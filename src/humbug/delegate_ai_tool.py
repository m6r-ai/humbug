import asyncio
import json
import logging
from typing import Dict, Any, cast

from ai import AIConversation, AIConversationSettings, AIManager, AIReasoningCapability
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
from humbug.tabs.conversation.conversation_tab import ConversationTab


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
                "- Parallelize problem solving (you must use parallel tool/function calls to do this)\n"
                "- Seek analysis from a different perspective (you must provide the perspective in the task_prompt)\n"
                "- Use a subset of your current conversation context for more focused reasoning\n"
                "- Process a lot of context/tokens where you don't need all the context in your main conversation\n"
                "- Explore multiple approaches to a problem before converging on a solution in your main conversation\n"
                "- Give you a sounding board for ideas or to brainstorm with a different AI\n\n"

                "Important:\n"
                "- You must not delegate a task to a child AI that is the same task you have been given unless you are "
                "significantly changing the context or requirements\n"
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
        _request_authorization: AIToolAuthorizationCallback
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

        ai_conversation = cast(AIConversation, requester_ref)

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
        conversation_tab: ConversationTab,
        tool_call: AIToolCall
    ) -> AIToolResult:
        """
        Wait for a delegated task to complete and return the formatted result.

        Args:
            conversation_tab: The conversation tab to wait for
            tool_call: The tool call that initiated the delegation

        Returns:
            Formatted result string containing the AI's response or error information
        """
        # Set up completion tracking
        completion_future: asyncio.Future[Dict[str, Any]] = asyncio.Future()

        def on_completion(result_dict: Dict[str, Any]) -> None:
            """Handle conversation completion."""
            if not completion_future.done():
                completion_future.set_result(result_dict)

        # Connect to completion signal
        conversation_tab.conversation_completed.connect(on_completion)

        tab_id = conversation_tab.tab_id()
        session_id = self._mindspace_manager.get_mindspace_relative_path(conversation_tab.path())

        # Wait for completion
        result = await completion_future

        self._column_manager.close_tab_by_id(conversation_tab.tab_id())

        # Return appropriate result
        success = result.get("success", False)
        if not success:
            error_msg = result.get("error", "Unknown error")
            self._logger.warning("Delegated AI task failed: %s", error_msg)
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Delegated AI task failed\ntab ID: {tab_id}\nsession ID: {session_id}\nerror: {error_msg}"
            )
            return AIToolResult(
                id=tool_call.id,
                name="delegate_ai",
                content="",
                error=f"Delegated AI task failed, session_id: {session_id}: error: {error_msg}",
                context="text"
            )

        response_content = result.get("content", "")
        usage_info = result.get("usage")

        # Create a structured response
        result_object = {
            "session_id": session_id,
            "status": "completed",
            "response": response_content,
            "usage": usage_info
        }

        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"Delegated AI task completed\ntab ID: {tab_id}\nsession ID: {session_id}\nresponse: {response_content[:50]}..."
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
        ai_conversation: AIConversation,
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
            ai_conversation: AIConversation instance for the request
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

            # Create or open conversation
            conversation_tab = None
            if session_id is None or session_id == "current":
                # Create new conversation
                history = ai_conversation.get_conversation_history() if session_id == "current" else None

                try:
                    conversation_tab = self._column_manager.new_conversation(
                        True, history, model, temperature, reasoning_capability
                    )

                except ColumnManagerError as e:
                    raise AIToolExecutionError(
                        f"Failed to create new conversation for delegation: {str(e)}"
                    ) from e

            else:
                # Open existing conversation
                conversation_tab = self._column_manager.open_conversation(session_id, False)
                if conversation_tab is None:
                    raise AIToolExecutionError(
                        f"Session '{session_id}' does not exist or is not a valid conversation."
                    )

            session_id = self._mindspace_manager.get_mindspace_relative_path(conversation_tab.path())
            assert session_id is not None, "Session ID should not be None after resolving path"

            # Update conversation settings if model or temperature provided
            conversation_settings = conversation_tab.conversation_settings()
            if model:
                conversation_settings.model = model

            if temperature is not None:
                conversation_settings.temperature = temperature

            if reasoning_capability:
                conversation_settings.reasoning = reasoning_capability

            conversation_tab.update_conversation_settings(conversation_settings)

            # Submit the task prompt
            conversation_tab.set_input_text(task_prompt)
            conversation_tab.submit_with_requester(ai_conversation.conversation_settings().model)

            # Log the delegation
            tab_id = conversation_tab.tab_id()
            session_info = "continuing session" if session_id else "new session"
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI delegated task ({session_info})\ntab ID: {tab_id}\nsession ID: {session_id}\nprompt: '{task_prompt[:50]}...'"
            )

            # Create a continuation task that waits for completion
            continuation_task = asyncio.create_task(
                self._delegate_task_continuation(conversation_tab, tool_call)
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
