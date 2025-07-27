import asyncio
import logging
from typing import Dict, Any, cast

from ai import AIConversation, AIConversationSettings, AIReasoningCapability
from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback,
    AIToolResult, AIToolCall
)

from humbug.column_manager import ColumnManager
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_error import MindspaceNotFoundError, MindspaceError
from humbug.tabs.conversation.conversation_tab import ConversationTab
from humbug.user.user_manager import UserManager


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
        self._user_manager = UserManager()
        self._logger = logging.getLogger("DelegateAITool")

    def get_definition(self) -> AIToolDefinition:
        """
        Get the tool definition.

        Returns:
            Tool definition with parameters and description
        """
        return AIToolDefinition(
            name="delegate_ai",
            description=(
                "The delegate_ai tool lets you (the parent AI) delegate cognitive tasks to other specialized child AI instances "
                "by creating a focused AI conversation for a specified task. The delegated AI has access to the same tools as "
                "you and can engage in multi-turn collaboration with you if you provide it with a session_id returned from a "
                "previous delegated conversation. Each call creates a temporary UI tab that closes after the response is "
                "returned.\n"
                "Delegation is useful for many reasons, such as:\n,"
                "- To use AI capabilities from a different model, or with different settings\n"
                "- To parallelize problem solving (you must use parallel tool/function calls to do this)\n"
                "- To seek analysis from a different perspective (you must provide the perspective in the task_prompt)\n"
                "- To use a subset of your current conversation context for more focused reasoning\n"
                "- To process a lot of context/tokens where you don't need all the context in your main conversation\n"
                "- To explore multiple approaches to a problem before converging on a solution in your main conversation\n"
                "- To give you a sounding board for ideas or to brainstorm with a different AI\n"
                "Important:\n"
                "- You must not use this tool if you would simply be giving the child AI the same task and with no significant "
                "differences in context or requirements\n"
                "- You must use the session_id parameter to continue a previous conversation\n"
                "- You will have to generate the task_prompt and that can be very expensive if you need to provide a lot of "
                "context in the task_prompt. If you need to provide something that is already in the filesystem, tell the "
                "delegated AI where to read it and do not provide the contents in the task_prompt. If the content is not "
                "already in the filesystem, you must provide it in the task_prompt.\n"
                "- You have the option to provide your current conversation context to a new child AI instance. This is very "
                "useful if your context has a lot of information that is relevant to the task, but this can also limit the "
                "amount of new context the child AI can process."
                "Returns: the delegated AI's response to the task_prompt, or an error message if the operation fails"
            ),
            parameters=[
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

    def _get_str_value_from_key(self, key: str, arguments: Dict[str, Any]) -> str:
        """
        Extract string value from arguments dictionary.

        Args:
            key: Key to extract from arguments
            arguments: Dictionary containing operation parameters

        Returns:
            String value for the given key

        Raises:
            AIToolExecutionError: If key is missing or value is not a string
        """
        if key not in arguments:
            raise AIToolExecutionError(f"No '{key}' argument provided")

        value = arguments[key]
        if not isinstance(value, str):
            raise AIToolExecutionError(f"'{key}' must be a string")

        return value

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

        try:
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

        except MindspaceNotFoundError as e:
            raise AIToolExecutionError(f"Mindspace error: {str(e)}") from e

        except Exception as e:
            raise AIToolExecutionError(f"Invalid session ID '{session_id}': {str(e)}") from e

    async def execute(
        self,
        tool_call: AIToolCall,
        requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """
        Execute the AI delegation with continuation support.

        Args:
            tool_call: Tool call containing task prompt and arguments
            requester: AI model requesting the operation
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
        task_prompt = self._get_str_value_from_key("task_prompt", arguments)

        session_id_arg = arguments.get("session_id")
        model = arguments.get("model")
        temperature = arguments.get("temperature")

        # Validate session_id if provided
        session_id = None
        if session_id_arg:
            if not isinstance(session_id_arg, str):
                raise AIToolExecutionError("'session_id' must be a string")

            if session_id_arg == "current":
                session_id = "current"

            else:
                session_id = self._validate_and_resolve_session_id(session_id_arg)

        # Validate model if provided
        if model and not isinstance(model, str):
            raise AIToolExecutionError("'model' must be a string")

        # Validate temperature if provided
        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError("'temperature' must be a number")

            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError("'temperature' must be between 0.0 and 1.0")

        # Validate model exists if provided
        reasoning = None
        if model:
            ai_backends = self._user_manager.get_ai_backends()
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

        try:
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
                    f"Delegated AI task failed, tab ID: {tab_id}, session ID: {session_id}, error: {error_msg}"
                )
                return AIToolResult(
                    id=tool_call.id,
                    name="delegate_ai",
                    content=f"Delegated AI task failed, session_id: {session_id}: error: {error_msg}"
                )

            response_content = result.get("content", "")
            usage_info = result.get("usage")

            # Create a formatted response
            result_parts = [f"Delegated AI task completed, session_id: {session_id}:\n{response_content}"]

            if usage_info:
                result_parts.append(
                    f"Token usage: {usage_info['prompt_tokens']} prompt + {usage_info['completion_tokens']} "
                    f"completion = {usage_info['total_tokens']} total"
                )

            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"Delegated AI task completed, tab ID: {tab_id}, session ID: {session_id}, response: {response_content[:50]}..."
            )

            return AIToolResult(
                id=tool_call.id,
                name="delegate_ai",
                content="\n\n".join(result_parts)
            )

        except Exception as e:
            self._logger.error("Error in AI delegation: %s", str(e), exc_info=True)
            self._column_manager.close_tab_by_id(conversation_tab.tab_id())

            return AIToolResult(
                id=tool_call.id,
                name="delegate_ai",
                content="Delegated AI task failed: " + str(e)
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
                conversation_tab = self._column_manager.new_conversation(
                    True, history, model, temperature, reasoning_capability
                )

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
            conversation_tab.submit_with_requester(conversation_settings.model)

            # Log the delegation
            tab_id = conversation_tab.tab_id()
            session_info = "continuing session" if session_id else "new session"
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.INFO,
                f"AI delegated task ({session_info}), tab ID: {tab_id}, session ID: {session_id}, prompt: '{task_prompt[:50]}...'"
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
