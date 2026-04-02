import asyncio
import json
import logging
from typing import Dict, Any, cast

from ai import AIConversation, AIConversationEvent, AIConversationSettings, AIManager, AIMessageSource, AIReasoningCapability
from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback,
    AIToolResult, AIToolCall, AIToolOperationDefinition
)

from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_error import MindspaceError
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.column_manager_error import ColumnManagerError


class DelegateAITool(AITool):
    """
    AI delegation tool for LLM interaction.

    Allows delegating cognitive tasks to specialized AI instances. Creates focused AI 
    conversations for specific sub-tasks, expert analysis, or collaborative problem-solving.
    The delegated AI operates with controlled context optimized for the task, can use tools,
    and supports multi-turn collaboration through session persistence.
    """

    def __init__(self, column_manager: ColumnManager):
        """
        Initialize the delegate AI tool.

        Args:
            column_manager: Column manager for tab operations
        """
        self._column_manager = column_manager
        self._mindspace_manager = MindspaceManager()
        self._ai_manager = AIManager()
        self._logger = logging.getLogger("DelegateAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return self._build_definition_from_operations(
            name="delegate_ai",
            description_prefix=(
                "The delegate_ai tool lets you (the parent AI) delegate cognitive tasks to specialized child AI instances "
                "by creating a focused AI conversation for a specific task. The delegated AI has access to the same tools as "
                "you and can engage in multi-turn collaboration with you if you provide it with a session_id returned from a "
                "previous delegated conversation. Each call creates a temporary UI tab that closes after the response is "
                "returned\n\n"

                "Examples where delegation is useful:\n"
                "- Use AI capabilities from a different model, or with different settings\n"
                "- Seek analysis from a different perspective (you must provide the perspective in the task_prompt)\n"
                "- Use a subset of your current conversation context for more focused reasoning\n"
                "- Process a lot of context/tokens where you don't need all the context in your main conversation\n"
                "- Explore multiple approaches to a problem before converging on a solution in your main conversation\n"
                "- Give you a sounding board for ideas or to brainstorm with a different AI\n\n"

                "Important:\n"
                "- You MUST NOT delegate a task to a child AI that is largely the same task you have been given."
                "- You MUST tell the child AI that it is a child AI instance that has been tasked using the delegate_ai tool\n"
                "- You MUST tell the child AI that if it creates any files, it must inform you (the parent AI) about them\n"
                "- You must not use this tool to try to access any tools you do not have access to (if you cannot do "
                "something, neither can a child AI)\n"
                "- You must use the session_id parameter to continue a previous conversation\n"
                "- You must generate the task_prompt and that can be very expensive if you need to provide a lot of "
                "context in it. If you need to provide something that is already in the filesystem, tell the "
                "delegated AI where to read it and do not provide the contents in the task_prompt. If the content is not "
                "already in the filesystem, you must provide it in the task_prompt.\n"
                "- You can provide your current conversation context to a new child AI instance. This is very "
                "useful if your context has a lot of information that is relevant to the task, but this can also limit the "
                "amount of new context the child AI can process\n\n"

                "Returns: the delegated AI's response to the task_prompt, or an error message if the operation fails"
            ),
            additional_parameters=[
                AIToolParameter(
                    name="task_prompt",
                    type="string",
                    description="The complete prompt to send to the AI instance, including any context, "
                        "specialization instructions, format requirements, etc.",
                    required=True
                ),
                AIToolParameter(
                    name="session_id",
                    type="string",
                    description="Session ID to continue a previous conversation. Omit to start a new session with no context. "
                        "Set this to `current` to create a new session with the current conversation context.",
                    required=False
                ),
                AIToolParameter(
                    name="model",
                    type="string",
                    description="AI model to use for the delegated task",
                    required=False
                ),
                AIToolParameter(
                    name="temperature",
                    type="number",
                    description="Temperature setting 0.0-1.0 for creativity vs precision",
                    required=False
                )
            ]
        )

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Delegate tasks to specialized child AI instances for parallel or focused work."

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """
        Get operation definitions for this tool.

        Returns:
            Dictionary mapping operation names to their definitions
        """
        return {
            "delegate": AIToolOperationDefinition(
                name="delegate",
                handler=self._delegate,
                extract_context=None,
                allowed_parameters={"task_prompt", "session_id", "model", "temperature"},
                required_parameters={"task_prompt"},
                description="Delegate a task to a specialized child AI instance"
            )
        }

    def _validate_mindspace_access(self) -> None:
        """
        Validate that a mindspace is currently open.

        Raises:
            AIToolExecutionError: If no mindspace is open
        """
        if not self._mindspace_manager.has_mindspace():
            raise AIToolExecutionError(
                "No mindspace is currently open. AI delegation requires an active mindspace."
            )

    def _validate_and_resolve_session_id(self, session_id: str) -> str:
        """
        Validate session ID and resolve to conversation path.

        Args:
            session_id: Session ID (conversation path) to validate

        Returns:
            Resolved absolute path for the conversation

        Raises:
            AIToolExecutionError: If session_id is invalid or outside mindspace
        """
        if not session_id:
            raise AIToolExecutionError("Session ID parameter is required")

        # Remove leading separator if present
        if session_id.startswith("/"):
            session_id = session_id[1:]

        # Convert to absolute path via mindspace manager
        abs_path = self._mindspace_manager.get_absolute_path(session_id)

        # Verify the resolved path is still within mindspace
        relative_path = self._mindspace_manager.get_mindspace_relative_path(abs_path)
        if relative_path is None:
            raise AIToolExecutionError(f"Session ID is outside mindspace boundaries: {session_id}")

        return abs_path

    async def _delegate(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Delegate a task to a specialized child AI instance.

        Args:
            tool_call: Tool call containing task prompt and arguments
            requester_ref: AI model requesting the operation
            request_authorization: Function to call for user authorization

        Returns:
            AIToolResult containing the execution result and optional continuation

        Raises:
            AIToolExecutionError: If operation fails
            AIToolAuthorizationDenied: If user denies authorization
        """
        # Validate mindspace is open
        self._validate_mindspace_access()

        # Extract arguments
        arguments = tool_call.arguments
        task_prompt = self._get_required_str_value("task_prompt", arguments)

        session_id_arg = self._get_optional_str_value("session_id", arguments)
        model = self._get_optional_str_value("model", arguments)
        temperature = arguments.get("temperature")

        # Validate session_id if provided
        session_id = None
        if session_id_arg:
            if session_id_arg == "current":
                session_id = "current"

            else:
                session_id = self._validate_and_resolve_session_id(session_id_arg)

        # Validate temperature if provided
        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError("'temperature' must be a number")

            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError("'temperature' must be between 0.0 and 1.0")

        # Validate model exists if provided
        reasoning = None
        if model:
            ai_backends = self._ai_manager.get_backends()
            available_models = list(AIConversationSettings.iter_models_by_backends(ai_backends))

            if model not in available_models:
                raise AIToolExecutionError(
                    f"Model '{model}' is not available. Available models: {', '.join(available_models)}"
                )

            # Get reasoning capability from model
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        self._logger.debug("AI delegation requested with task: %s", task_prompt[:100])

        # Request user authorization before delegating
        session_info = "continue a previous session" if session_id == "current" else \
                       "continue an existing session" if session_id else "start a new session"
        model_info = f" using model '{model}'" if model else " using the default model"
        temp_info = f" with temperature {temperature}" if temperature is not None else ""

        context = (
            f"The AI wants to delegate a task to a child AI instance.\n\n"
            f"Session: {session_info}{model_info}{temp_info}\n\n"
            f"Task prompt:\n{task_prompt[:500]}{'...' if len(task_prompt) > 500 else ''}"
        )

        authorized = await request_authorization("delegate_ai", arguments, context, None, False)
        if not authorized:
            raise AIToolAuthorizationDenied("User denied permission to delegate AI task")

        ai_conversation: AIConversation = requester_ref

        try:
            return await self._delegate_task(
                tool_call, ai_conversation, task_prompt, session_id, model, temperature, reasoning
            )

        except (AIToolExecutionError, AIToolAuthorizationDenied):
            # Re-raise our own errors
            raise

        except Exception as e:
            self._logger.error("Unexpected error in AI delegation: %s", str(e), exc_info=True)
            raise AIToolExecutionError(f"AI delegation failed: {str(e)}") from e

    async def _delegate_task_continuation(
        self,
        child_ai_conversation: AIConversation,
        tab_guid: str,
        tool_call: AIToolCall
    ) -> AIToolResult:
        """
        Wait for a delegated task to complete and return the formatted result.

        Args:
            child_ai_conversation: The AIConversation of the delegated task
            tab_guid: The GUID of the display tab to close on completion
            tool_call: The tool call that initiated the delegation

        Returns:
            Formatted result string containing the AI's response or error information
        """
        completion_future: asyncio.Future[None] = asyncio.Future()

        async def on_completed() -> None:
            """Handle AIConversation COMPLETED event."""
            if not completion_future.done():
                completion_future.set_result(None)

        async def on_error(retries_exhausted: bool, _message: Any) -> None:
            """Handle AIConversation ERROR event."""
            if retries_exhausted and not completion_future.done():
                completion_future.set_result(None)

        child_ai_conversation.register_callback(AIConversationEvent.COMPLETED, on_completed)
        child_ai_conversation.register_callback(AIConversationEvent.ERROR, on_error)

        # Wait for the child conversation to finish
        await completion_future

        child_ai_conversation.unregister_callback(AIConversationEvent.COMPLETED, on_completed)
        child_ai_conversation.unregister_callback(AIConversationEvent.ERROR, on_error)

        # Get session_id from the tab before closing it
        tab = self._column_manager.get_tab_by_id(tab_guid)
        session_id = self._mindspace_manager.get_mindspace_relative_path(tab.path()) if tab else ""

        self._column_manager.close_tab_by_id(tab_guid)

        # Build result from the child AIConversation's history
        messages = child_ai_conversation.get_conversation_history().get_messages()
        last_message = messages[-1] if messages else None

        if not last_message or not last_message.completed:
            error_msg = "AI response was terminated early" if last_message else "No messages in conversation"
            self._logger.warning("Delegated AI task failed: %s", error_msg)
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Delegated AI task failed\ntab ID: {tab_guid}\nsession ID: {session_id}\nerror: {error_msg}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="delegate_ai",
                content="",
                error=f"Delegated AI task failed, session_id: {session_id}: error: {error_msg}",
                context="text"
            )

        if last_message.source == AIMessageSource.SYSTEM:
            error_msg = last_message.content
            self._logger.warning("Delegated AI task failed: %s", error_msg)
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Delegated AI task failed\ntab ID: {tab_guid}\nsession ID: {session_id}\nerror: {error_msg}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="delegate_ai",
                content="",
                error=f"Delegated AI task failed, session_id: {session_id}: error: {error_msg}",
                context="text"
            )

        response_content = last_message.content
        usage_info = last_message.usage.to_dict() if last_message.usage else None

        result_object = {
            "session_id": session_id,
            "status": "completed",
            "response": response_content,
            "usage": usage_info
        }

        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"Delegated AI task completed\ntab ID: {tab_guid}\nsession ID: {session_id}\nresponse: {response_content[:50]}..."
        )

        return AIToolResult(
            id=tool_call.id,
            name="delegate_ai",
            content=json.dumps(result_object, indent=2),
            context="json"
        )

    async def _delegate_task(
        self,
        tool_call: AIToolCall,
        parent_ai_conversation: AIConversation,
        task_prompt: str,
        session_id: str | None,
        model: str | None,
        temperature: float | None,
        reasoning_capability: AIReasoningCapability | None
    ) -> AIToolResult:
        """
        Delegate a task to an AI instance.

        Args:
            tool_call: Tool call containing operation name and arguments
            parent_ai_conversation: The parent AIConversation making the request
            task_prompt: The prompt to send to the delegated AI
            session_id: ID of the existing session (None for new session)
            model: AI model to use (None for default)
            temperature: Temperature setting (None for default)
            reasoning_capability: Reasoning capability setting

        Returns:
            AIToolResult with continuation for waiting on completion

        Raises:
            AIToolExecutionError: If operation fails
        """
        try:
            # Ensure conversations directory exists
            self._mindspace_manager.ensure_mindspace_dir("conversations")

            # Build settings for the child conversation
            mindspace_settings = self._mindspace_manager.settings()
            child_model = model or (mindspace_settings.model if mindspace_settings else None) or \
                parent_ai_conversation.conversation_settings().model
            child_temperature = temperature
            if child_temperature is None and mindspace_settings:
                child_temperature = mindspace_settings.temperature

            child_reasoning = reasoning_capability
            if child_reasoning is None and mindspace_settings:
                child_reasoning = mindspace_settings.reasoning

            child_settings = AIConversationSettings(
                model=child_model,
                temperature=child_temperature if AIConversationSettings.supports_temperature(child_model) else None,
                reasoning=child_reasoning or AIReasoningCapability.NO_REASONING
            )

            # Create the child AIConversation and configure it
            child_ai_conversation = AIConversation()
            child_ai_conversation.update_conversation_settings(child_settings)

            # Load parent history if session_id == "current"
            continuing_session = session_id == "current"
            if continuing_session:
                history = parent_ai_conversation.get_conversation_history()
                child_ai_conversation.load_message_history(history.get_messages())

            # Submit the prompt directly to the child AIConversation
            requester = parent_ai_conversation.conversation_settings().model

            # Create the display tab first so it registers event callbacks before we submit the prompt.
            # If we submit first, the MESSAGE_ADDED event for the user prompt fires before the
            # widget is listening, and the prompt never appears in the on-screen history.

            # Protect the parent conversation tab so the child display tab lands in a different column
            parent_tab = self._column_manager.find_tab_by_ai_conversation(parent_ai_conversation)
            if parent_tab:
                self._column_manager.protect_tab(parent_tab.tab_id())

            try:
                conversation_tab = self._column_manager.new_conversation(
                    child=True,
                    ai_conversation=child_ai_conversation
                )

            except ColumnManagerError as e:
                raise AIToolExecutionError(
                    f"Failed to create display tab for delegation: {str(e)}"
                ) from e

            if parent_tab:
                self._column_manager.unprotect_tab(parent_tab.tab_id())

            # Move the child conversation tab to the next column to the right of the parent
            if parent_tab:
                parent_info = self._column_manager.get_tab_info_by_id(parent_tab.tab_id())
                if parent_info:
                    target_column = min(cast(int, parent_info["column_index"]) + 1, 5)
                    try:
                        self._column_manager.move_tab_to_column(conversation_tab.tab_id(), target_column)

                    except ColumnManagerError:
                        pass  # Best effort — tab is still functional if move fails

            # Now submit the prompt — the widget is listening and will display the user message
            await child_ai_conversation.submit_message(requester, task_prompt)

            tab_guid = conversation_tab.tab_id()
            session_id = self._mindspace_manager.get_mindspace_relative_path(conversation_tab.path())
            assert session_id is not None, "Session ID should not be None after resolving path"

            # Log the delegation
            session_info = "continuing session" if continuing_session else "new session"
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI delegated task ({session_info})\ntab ID: {tab_guid}\nsession ID: {session_id}\nprompt: '{task_prompt[:50]}...'"
            )

            # Create a continuation task that waits for completion
            continuation_task = asyncio.create_task(
                self._delegate_task_continuation(child_ai_conversation, tab_guid, tool_call)
            )

            return AIToolResult(
                id=tool_call.id,
                name="delegate_ai",
                content=f"Delegated task to AI ({session_info}), session_id: {session_id}",
                continuation=continuation_task
            )

        except MindspaceError as e:
            raise AIToolExecutionError(f"Failed to create conversation directory: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Failed to delegate AI task: {str(e)}") from e
