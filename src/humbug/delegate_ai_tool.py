import asyncio
import logging
from typing import Dict, Any

from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_model import ReasoningCapability
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
                "The delegate_ai tool lets you (the AI) delegate cognitive tasks to other specialized AI instances by creating "
                "a focused AI conversation for the specified task. The delegated AI has access to tools and can engage in "
                "multi-turn collaboration if you provide it with a session_id returned from a previous delegated conversation. "
                "Each call creates a temporary UI tab that closes after the response is returned. Delegation is useful for many "
                "reasons - e.g.:\n,"
                "- to use specialized AI capabilities\n"
                "- to parallelize problem solving\n"
                "- to seek expert analysis from a different perspective\n"
                "- to explore collaborative approaches to problem-solving\n"
                "- to use a subset context for more focused reasoning\n"
                "Important:\n"
                "- You must not use this tool if you have been delegated this task already\n"
                "- You must use the session_id parameter to continue a previous conversation\n"
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
                    description="Session ID to continue a previous conversation. Omit to start a new session.",
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
        requester: str,
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

        session_id = arguments.get("session_id")
        model = arguments.get("model")
        temperature = arguments.get("temperature")

        # Validate session_id if provided
        conversation_path = None
        if session_id:
            if not isinstance(session_id, str):
                raise AIToolExecutionError("'session_id' must be a string")

            conversation_path = self._validate_and_resolve_session_id(session_id)

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

        try:
            return await self._delegate_task(tool_call, requester, task_prompt, conversation_path, model, temperature, reasoning)

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
        requester: str,
        task_prompt: str,
        conversation_path: str | None,
        model: str | None,
        temperature: float | None,
        reasoning: ReasoningCapability | None
    ) -> AIToolResult:
        """
        Delegate a task to an AI instance.

        Args:
            tool_call: Tool call containing operation name and arguments
            requester: AI model requesting the operation
            task_prompt: The prompt to send to the delegated AI
            conversation_path: Path to existing conversation (None for new session)
            model: AI model to use (None for default)
            temperature: Temperature setting (None for default)
            reasoning: Reasoning capability setting

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
            is_new_session = conversation_path is None
            if is_new_session:
                # Create new conversation
                current_tab = self._column_manager.get_current_tab()
                assert isinstance(current_tab, ConversationTab), "Current tab must be a ConversationTab"
                conversation_tab = self._column_manager.new_conversation(current_tab, model, temperature, reasoning)

            else:
                # Open existing conversation
                conversation_tab = self._column_manager.open_conversation(conversation_path, False)
                if conversation_tab is None:
                    raise AIToolExecutionError(
                        f"Session '{conversation_path}' does not exist or is not a valid conversation."
                    )

            session_id = self._mindspace_manager.get_mindspace_relative_path(conversation_tab.path())
            if session_id is None:
                raise AIToolExecutionError(
                    f"Session '{conversation_path}' is outside the current mindspace boundaries."
                )

            # Submit the task prompt
            conversation_tab.set_input_text(task_prompt)
            conversation_tab.submit_with_requester(requester)

            # Log the delegation
            tab_id = conversation_tab.tab_id()
            session_info = "new session" if is_new_session else "continuing session"
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
