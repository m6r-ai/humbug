"""Enhanced AI conversation class with tool calling support."""

import asyncio
import json
import logging
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Set

from humbug.ai.ai_conversation_history import AIConversationHistory
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage
from humbug.ai.ai_message_source import AIMessageSource
from humbug.ai.ai_model import ReasoningCapability
from humbug.ai.ai_response import AIError
from humbug.ai.ai_tool_manager import AIToolManager, AIToolCall, AIToolResult
from humbug.ai.ai_usage import AIUsage
from humbug.user.user_manager import UserManager


class AIConversationEvent(Enum):
    """Events that can be emitted by the AIConversation class."""
    ERROR = auto()              # When an error occurs during request processing
    COMPLETED = auto()          # When a response is fully completed
    MESSAGE_ADDED = auto()      # When a new message is added to history
    MESSAGE_UPDATED = auto()    # When an existing message is updated
    MESSAGE_COMPLETED = auto()  # When an existing message has been completed
    TOOL_USED = auto()          # When a tool use message is added to history
    TOOL_APPROVAL_REQUIRED = auto()
                                # When tool calls need user approval


class AIConversation:
    """
    Handles AI conversation logic separate from the GUI.

    This class manages the communication with AI backends, maintains conversation
    history, and provides methods to start and cancel conversations.
    """

    def __init__(self) -> None:
        """Initialize the AIConversation."""
        self._logger = logging.getLogger("AIConversation")
        self._user_manager = UserManager()
        self._tool_manager = AIToolManager()
        self._settings = AIConversationSettings()
        self._conversation = AIConversationHistory()
        self._current_tasks: List[asyncio.Task] = []
        self._current_ai_message: AIMessage | None = None
        self._current_reasoning_message: AIMessage | None = None
        self._is_streaming = False

        # Tool approval state
        self._pending_tool_calls: List[AIToolCall] = []
        self._pending_tool_call_message: AIMessage | None = None

        # Callbacks for events
        self._callbacks: Dict[AIConversationEvent, Set[Callable]] = {
            event: set() for event in AIConversationEvent
        }

    def is_streaming(self) -> bool:
        """Check if the conversation is currently streaming a response."""
        return self._is_streaming

    def register_callback(self, event: AIConversationEvent, callback: Callable) -> None:
        """
        Register a callback for a specific event.

        Args:
            event: The event to register for
            callback: The callback function to call when the event occurs
        """
        self._callbacks[event].add(callback)

    def unregister_callback(self, event: AIConversationEvent, callback: Callable) -> None:
        """
        Unregister a callback for a specific event.

        Args:
            event: The event to unregister from
            callback: The callback function to remove
        """
        if callback in self._callbacks[event]:
            self._callbacks[event].remove(callback)

    async def _trigger_event(self, event: AIConversationEvent, *args: Any, **kwargs: Any) -> None:
        """
        Trigger all callbacks registered for an event.

        Args:
            event: The event to trigger
            *args: Arguments to pass to callbacks
            **kwargs: Keyword arguments to pass to callbacks
        """
        for callback in self._callbacks[event]:
            try:
                await callback(*args, **kwargs)

            except Exception:
                self._logger.exception("Error in callback for %s", event)

    def update_conversation_settings(self, new_settings: AIConversationSettings) -> None:
        """
        Update conversation settings.

        Args:
            new_settings: New settings to apply
        """
        self._settings = new_settings

    def conversation_settings(self) -> AIConversationSettings:
        """
        Get current conversation settings.

        Returns:
            Current conversation settings
        """
        return AIConversationSettings(
            model=self._settings.model,
            temperature=self._settings.temperature,
            reasoning=self._settings.reasoning
        )

    def get_conversation_history(self) -> AIConversationHistory:
        """
        Get the conversation history object.

        Returns:
            The conversation history object
        """
        return self._conversation

    def get_token_counts(self) -> Dict[str, int]:
        """
        Get the current token counts for status display.

        Returns:
            Dictionary with token count information
        """
        return self._conversation.get_token_counts()

    def load_message_history(self, messages: List[AIMessage]) -> None:
        """
        Load existing message history.

        Args:
            messages: List of AIMessage objects to load
        """
        self._conversation.clear()

        for message in messages:
            self._conversation.add_message(message)

            # Update settings if AI message
            if message.source == AIMessageSource.AI:
                if message.usage:
                    self._conversation.update_last_tokens(
                        message.usage.prompt_tokens,
                        message.usage.completion_tokens
                    )

                reasoning = message.reasoning_capability if message.reasoning_capability else ReasoningCapability.NO_REASONING
                if message.model:
                    self.update_conversation_settings(AIConversationSettings(
                        model=message.model,
                        temperature=message.temperature,
                        reasoning=reasoning
                    ))

    async def submit_message(self, message: AIMessage) -> None:
        """
        Submit a new user message and start AI response.

        Args:
            message: User message
        """
        # Add the user message to the conversation
        self._conversation.add_message(message)

        # Start AI response
        task = asyncio.create_task(self._start_ai())
        self._current_tasks.append(task)

        def task_done_callback(task: asyncio.Task) -> None:
            try:
                self._current_tasks.remove(task)

            except ValueError:
                self._logger.debug("Task already removed")

        task.add_done_callback(task_done_callback)

    async def _start_ai(self) -> None:
        """Start an AI response based on the conversation history."""
        stream = None
        settings = self.conversation_settings()

        try:
            self._logger.debug("=== Starting AI response streaming ===")

            # Get appropriate backend for conversation
            provider = AIConversationSettings.get_provider(settings.model)
            backend = self._user_manager.get_ai_backends().get(provider)

            if not backend:
                error_msg = f"No backend available for provider: {provider}"
                self._logger.error(error_msg)
                error_message = AIMessage.create(
                    AIMessageSource.SYSTEM,
                    error_msg,
                    error={"code": "backend_error", "message": error_msg}
                )
                self._conversation.add_message(error_message)
                await self._trigger_event(AIConversationEvent.ERROR, True, error_message)
                self._is_streaming = False
                return

            stream = backend.stream_message(self._conversation.get_messages(), self._settings)
            async for response in stream:
                await self._update_streaming_response(
                    reasoning=response.reasoning,
                    content=response.content,
                    usage=response.usage,
                    error=response.error,
                    tool_calls=response.tool_calls,
                    signature=response.signature,
                    readacted_reasoning=response.readacted_reasoning
                )

                if response.error and response.error.retries_exhausted:
                    return

            # If we get here and are still marked as streaming then we failed to get a
            # complete response before giving up.  This is a failure and should be handled as such.
            if self._is_streaming:
                self._logger.debug("AI response failed (likely timeout)")
                await self._update_streaming_response(
                    reasoning="",
                    content="",
                    error=AIError(
                        code="cancelled",
                        message="Server failed to complete response",
                        retries_exhausted=True,
                        details={"type": "CancelledError"}
                    )
                )
                return

        except asyncio.CancelledError:
            self._logger.debug("AI response cancelled")
            await self._update_streaming_response(
                reasoning="",
                content="",
                error=AIError(
                    code="cancelled",
                    message="Request cancelled by user",
                    retries_exhausted=True,
                    details={"type": "CancelledError"}
                )
            )
            return

        except Exception as e:
            self._logger.exception(
                "Error processing AI response with model %s", settings.model
            )
            await self._update_streaming_response(
                reasoning="",
                content="",
                error=AIError(
                    code="process_error",
                    message=str(e),
                    retries_exhausted=True,
                    details={"type": type(e).__name__}
                )
            )
            return

        finally:
            self._logger.debug("=== Finished AI response streaming ===")

            # Properly close the async generator if it exists
            if stream is not None:
                try:
                    await stream.aclose()

                except Exception as e:
                    # Log but don't propagate generator cleanup errors
                    self._logger.debug("Error during generator cleanup: %s", e)

    async def _process_pending_tool_calls(self, tool_calls: List[AIToolCall]) -> None:
        """Process tool calls with user approval requirement."""
        # Store pending tool calls for approval
        self._pending_tool_calls = tool_calls

        # Create and add tool call message
        tool_calls_dict = [tool_call.to_dict() for tool_call in tool_calls]
        content = f"""```json
{json.dumps(tool_calls_dict, indent=4)}
```"""
        tool_call_message = AIMessage.create(
            source=AIMessageSource.TOOL_CALL,
            content=content,
            tool_calls=tool_calls,
            completed=False
        )
        self._conversation.add_message(tool_call_message)
        self._pending_tool_call_message = tool_call_message

        await self._trigger_event(AIConversationEvent.TOOL_APPROVAL_REQUIRED, tool_call_message, tool_calls)

    async def approve_pending_tool_calls(self) -> None:
        """Execute approved tool calls."""
        if not self._pending_tool_calls or not self._pending_tool_call_message:
            return

        self._logger.debug("User approved tool calls, executing...")

        approved_message = self._conversation.update_message(
            self._pending_tool_call_message.id,
            content=self._pending_tool_call_message.content,
            completed=True
        )
        if approved_message:
            await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, approved_message)

        # Execute all tool calls
        tool_results = []
        for tool_call in self._pending_tool_calls:
            self._logger.debug("Executing approved tool call: %s", tool_call.name)
            tool_result = await self._tool_manager.execute_tool(tool_call)
            tool_results.append(tool_result)

        # Create and add tool result message (hidden from user, for audit trail)
        tool_results_dict = [tool_result.to_dict() for tool_result in tool_results]
        content = f"""```json
{json.dumps(tool_results_dict, indent=4)}
```"""
        tool_result_message = AIMessage.create(
            source=AIMessageSource.TOOL_RESULT,
            content=content,
            tool_results=tool_results,
            completed=True
        )
        self._conversation.add_message(tool_result_message)
        await self._trigger_event(AIConversationEvent.TOOL_USED, tool_result_message)

        # Create an explicit user message with tool results
        tool_response_message = AIMessage.create(
            AIMessageSource.USER,
            content="",
            tool_results=tool_results
        )
        self._conversation.add_message(tool_response_message)
        await self._trigger_event(AIConversationEvent.TOOL_USED, tool_response_message)

        # Clear pending state
        self._pending_tool_calls = []
        self._pending_tool_call_message = None

        # Automatically continue the conversation with tool results
        await self._start_ai()

    async def reject_pending_tool_calls(self, reason: str = "User rejected tool call") -> None:
        """Reject pending tool calls and return error to AI."""
        if not self._pending_tool_calls or not self._pending_tool_call_message:
            return

        self._logger.debug("User rejected tool calls: %s", reason)

        rejected_message = self._conversation.update_message(
            self._pending_tool_call_message.id,
            content=self._pending_tool_call_message.content,
            completed=True
        )
        if rejected_message:
            await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, rejected_message)

        # Create error results for each tool call
        tool_results = []
        for tool_call in self._pending_tool_calls:
            tool_result = AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=reason
            )
            tool_results.append(tool_result)

        # Create tool result message with errors
        tool_results_dict = [tool_result.to_dict() for tool_result in tool_results]
        content = f"""Tool results (rejected):
```json
{json.dumps(tool_results_dict, indent=4)}
```
"""
        tool_result_message = AIMessage.create(
            source=AIMessageSource.TOOL_RESULT,
            content=content,
            tool_results=tool_results,
            completed=True
        )
        self._conversation.add_message(tool_result_message)
        await self._trigger_event(AIConversationEvent.TOOL_USED, tool_result_message)

        # Create an explicit user message with error results
        tool_response_message = AIMessage.create(
            AIMessageSource.USER,
            content="",
            tool_results=tool_results
        )
        self._conversation.add_message(tool_response_message)
        await self._trigger_event(AIConversationEvent.TOOL_USED, tool_response_message)

        # Clear pending state
        self._pending_tool_calls = []
        self._pending_tool_call_message = None

        # Continue the conversation with error results
        await self._start_ai()

    async def _handle_error(self, error: AIError) -> None:
        """
        Handle errors that occur during AI response processing.

        Args:
            error: AIError object containing error details
        """
        # Only stop streaming if retries are exhausted
        if error.retries_exhausted:
            self._is_streaming = False

            # For cancellation, preserve the partial response first
            if self._current_reasoning_message:
                message = self._conversation.update_message(
                    self._current_reasoning_message.id,
                    content=self._current_reasoning_message.content,
                    completed=False
                )
                if message:
                    await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, message)

                self._current_reasoning_message = None

            if self._current_ai_message:
                message = self._conversation.update_message(
                    self._current_ai_message.id,
                    content=self._current_ai_message.content,
                    completed=False
                )
                if message:
                    await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, message)

                self._current_ai_message = None

        error_msg = error.message
        error_message = AIMessage.create(
            AIMessageSource.SYSTEM,
            error_msg,
            error={"code": error.code, "message": error.message, "details": error.details}
        )
        self._conversation.add_message(error_message)
        await self._trigger_event(AIConversationEvent.ERROR, error.retries_exhausted, error_message)

        # For cancellation, don't log as warning since it's user-initiated
        if error.code == "cancelled":
            self._logger.debug("AI response cancelled by user")

        else:
            self._logger.warning("AI response error: %s", error.message)

    async def _handle_content(
        self,
        reasoning: str,
        content: str,
        usage: AIUsage | None,
        tool_calls: List[AIToolCall] | None,
        signature: str | None = None,
        readacted_reasoning: str | None = None
    ) -> None:
        """
        Handle content updates from the AI response.

        Args:
            reasoning: Reasoning text from the AI response
            content: Main content of the AI response
            usage: Optional token usage information
            tool_calls: Optional list of tool calls made by the AI
            signature: Optional signature for the response
            readacted_reasoning: Optional readacted reasoning text
        """
        # We're handling a response.  Have we already seen part of this already?
        if self._current_ai_message:
            # Update existing message with new tool calls if provided
            if tool_calls:
                self._current_ai_message.tool_calls = tool_calls

            # Update existing message
            message = self._conversation.update_message(
                self._current_ai_message.id,
                content,
                usage=usage,
                completed=(usage is not None)
            )
            if message:
                await self._trigger_event(
                    AIConversationEvent.MESSAGE_UPDATED if usage is None else AIConversationEvent.MESSAGE_COMPLETED,
                    message
                )

            return

        # If we previously had reasoning from the AI then close that out
        if self._current_reasoning_message:
            reasoning_message = self._conversation.update_message(
                self._current_reasoning_message.id,
                reasoning,
                usage=usage,
                completed=True,
                signature=signature,
                readacted_reasoning=readacted_reasoning
            )
            await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, reasoning_message)
            self._current_reasoning_message = None

        # Create and add initial AI response message
        settings = self.conversation_settings()
        new_message = AIMessage.create(
            AIMessageSource.AI,
            content,
            model=settings.model,
            temperature=settings.temperature,
            reasoning_capability=settings.reasoning,
            completed=(usage is not None),
            tool_calls=tool_calls
        )
        self._conversation.add_message(new_message)
        await self._trigger_event(
            AIConversationEvent.MESSAGE_ADDED if usage is None else AIConversationEvent.TOOL_USED,
            new_message
        )
        self._current_ai_message = new_message

    async def _handle_reasoning(
        self,
        reasoning: str,
        usage: AIUsage | None,
        tool_calls: List[AIToolCall] | None,
        signature: str | None = None,
        readacted_reasoning: str | None = None
    ) -> None:
        """
        Handle reasoning updates from the AI response.

        Args:
            reasoning: Reasoning text from the AI response
            usage: Optional token usage information
            tool_calls: Optional list of tool calls made by the AI
            signature: Optional signature for the response
            readacted_reasoning: Optional readacted reasoning text
        """
        # We're handling reasoning from our AI.  Have we already seen part of this reasoning?
        if self._current_reasoning_message:
            # Update existing message
            message = self._conversation.update_message(
                self._current_reasoning_message.id,
                reasoning,
                usage=usage,
                completed=(usage is not None),
                signature=signature,
                readacted_reasoning=readacted_reasoning
            )
            if message:
                await self._trigger_event(
                    AIConversationEvent.MESSAGE_UPDATED if usage is None else AIConversationEvent.MESSAGE_COMPLETED,
                    message
                )

            return

        # Create and add initial message
        settings = self.conversation_settings()
        new_message = AIMessage.create(
            AIMessageSource.REASONING,
            reasoning,
            model=settings.model,
            temperature=settings.temperature,
            reasoning_capability=settings.reasoning,
            completed=(usage is not None),
            tool_calls=tool_calls,
            signature=signature,
            readacted_reasoning=readacted_reasoning
        )
        self._conversation.add_message(new_message)
        await self._trigger_event(
            AIConversationEvent.MESSAGE_ADDED if usage is None else AIConversationEvent.TOOL_USED,
            new_message
        )
        self._current_reasoning_message = new_message

    async def _handle_usage(self, reasoning: str, content: str, tool_calls: List[AIToolCall] | None) -> None:
        self._is_streaming = False

        if not content and not reasoning:
            settings = self.conversation_settings()
            message = AIMessage.create(
                AIMessageSource.AI,
                content="",
                model=settings.model,
                temperature=settings.temperature,
                reasoning_capability=settings.reasoning,
                completed=True,
                tool_calls=tool_calls
            )
            self._conversation.add_message(message)
            await self._trigger_event(AIConversationEvent.TOOL_USED, message)

        self._current_reasoning_message = None
        self._current_ai_message = None

        # Check for tool calls BEFORE marking conversation as completed
        if tool_calls:
            # Don't emit COMPLETED event yet - we need user approval first
            await self._process_pending_tool_calls(tool_calls)
            return

        await self._trigger_event(AIConversationEvent.COMPLETED)

    async def _update_streaming_response(
        self,
        reasoning: str,
        content: str,
        usage: AIUsage | None = None,
        error: AIError | None = None,
        tool_calls: List[AIToolCall] | None = None,
        signature: str | None = None,
        readacted_reasoning: str | None = None
    ) -> None:
        """
        Update the current AI response in the conversation.

        Args:
            reasoning: AI reasoning text
            content: AI response content
            usage: Optional token usage information
            error: Optional error information
            tool_calls: Optional list of tool calls made by the AI
        """
        if error:
            await self._handle_error(error)
            return

        if not self._is_streaming:
            self._is_streaming = True

        # Handle main content first
        if content:
            await self._handle_content(reasoning, content, usage, tool_calls, signature, readacted_reasoning)

        # If we have no content but have reasoning, handle that separately
        elif reasoning:
            await self._handle_reasoning(reasoning, usage, tool_calls, signature, readacted_reasoning)

        if usage:
            await self._handle_usage(reasoning, content, tool_calls)

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        for task in self._current_tasks:
            if not task.done():
                task.cancel()
