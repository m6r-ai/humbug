"""Enhanced AI conversation class with tool calling support."""

import asyncio
import logging
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Set

from humbug.ai.ai_conversation_history import AIConversationHistory
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_message import AIMessage
from humbug.ai.ai_message_source import AIMessageSource
from humbug.ai.ai_response import AIError
from humbug.ai.ai_tool_manager import AIToolManager, ToolCall
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

                if message.model:
                    self.update_conversation_settings(AIConversationSettings(
                        model=message.model,
                        temperature=message.temperature
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
        try:
            self._logger.debug("=== Starting new AI response ===")

            # Get appropriate backend for conversation
            settings = self.conversation_settings()
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

            stream = backend.stream_message(
                self._conversation.get_messages_for_context(),
                self._settings
            )

            async for response in stream:
                await self._update_streaming_response(
                    reasoning=response.reasoning,
                    content=response.content,
                    usage=response.usage,
                    error=response.error,
                    tool_calls=response.tool_calls
                )

                if response.error:
                    if response.error.retries_exhausted:
                        return

                    continue

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
            self._logger.debug("=== Finished AI response ===")

            # Properly close the async generator if it exists
            if stream is not None:
                try:
                    await stream.aclose()

                except Exception as e:
                    # Log but don't propagate generator cleanup errors
                    self._logger.debug("Error during generator cleanup: %s", e)

    async def _process_pending_tool_calls(self, tool_calls: List[ToolCall]) -> None:
        """Process any tool calls from the current AI message."""
        # Create and add tool call message (hidden from user, for audit trail)
        tool_call_message = AIMessage.create_tool_call_message(tool_calls=tool_calls)
        self._conversation.add_message(tool_call_message)
        await self._trigger_event(AIConversationEvent.TOOL_USED, tool_call_message)

        # Execute all tool calls
        tool_results = []
        for tool_call in tool_calls:
            self._logger.debug("Executing tool call: %s", tool_call.name)
            tool_result = await self._tool_manager.execute_tool(tool_call)
            tool_results.append(tool_result)

        # Create and add tool result message (hidden from user, for audit trail)
        tool_result_message = AIMessage.create_tool_result_message(tool_results=tool_results)
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

        # Automatically continue the conversation with tool results
        await self._continue_after_tool_execution()

    async def _continue_after_tool_execution(self) -> None:
        """Continue the AI conversation after tool execution with results."""
        try:
            self._logger.debug("Continuing conversation after tool execution")

            # Get appropriate backend for conversation
            settings = self.conversation_settings()
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

            # Continue streaming with the updated conversation history (including tool results)
            stream = backend.stream_message(
                self._conversation.get_messages_for_context(),
                self._settings
            )

            async for response in stream:
                await self._update_streaming_response(
                    reasoning=response.reasoning,
                    content=response.content,
                    usage=response.usage,
                    error=response.error,
                    tool_calls=response.tool_calls
                )

                if response.error and response.error.retries_exhausted:
                    return

        except Exception as e:
            self._logger.exception("Error continuing conversation after tool execution")
            await self._update_streaming_response(
                reasoning="",
                content="",
                error=AIError(
                    code="tool_continuation_error",
                    message=f"Failed to continue after tool execution: {str(e)}",
                    retries_exhausted=True,
                    details={"type": type(e).__name__}
                )
            )

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
        tool_calls: List[ToolCall] | None
    ) -> None:
        """
        Handle content updates from the AI response.

        Args:
            reasoning: Reasoning text from the AI response
            content: Main content of the AI response
            usage: Optional token usage information
            tool_calls: Optional list of tool calls made by the AI
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
                await self._trigger_event(AIConversationEvent.MESSAGE_UPDATED, message)

            return

        # If we previously had reasoning from the AI then close that out
        if self._current_reasoning_message:
            reasoning_message = self._conversation.update_message(
                self._current_reasoning_message.id,
                reasoning,
                usage=usage,
                completed=True
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
            completed=(usage is not None),
            tool_calls=tool_calls
        )
        self._conversation.add_message(new_message)
        await self._trigger_event(AIConversationEvent.MESSAGE_ADDED, new_message)
        self._current_ai_message = new_message

    async def _handle_reasoning(self, reasoning: str, usage: AIUsage | None = None) -> None:
        """
        Handle reasoning updates from the AI response.

        Args:
            reasoning: Reasoning text from the AI response
            usage: Optional token usage information
        """
        # We're handling reasoning from our AI.  Have we already seen part of this reasoning?
        if self._current_reasoning_message:
            # Update existing message
            message = self._conversation.update_message(
                self._current_reasoning_message.id,
                reasoning,
                usage=usage,
                completed=False
            )
            if message:
                await self._trigger_event(AIConversationEvent.MESSAGE_UPDATED, message)

            return

        # Create and add initial message
        settings = self.conversation_settings()
        new_message = AIMessage.create(
            AIMessageSource.REASONING,
            reasoning,
            model=settings.model,
            temperature=settings.temperature,
            completed=False
        )
        self._conversation.add_message(new_message)
        await self._trigger_event(AIConversationEvent.MESSAGE_ADDED, new_message)
        self._current_reasoning_message = new_message

    async def _handle_usage(self, reasoning: str, content: str, tool_calls: List[ToolCall] | None) -> None:
        self._is_streaming = False

        if not content and not reasoning:
            settings = self.conversation_settings()
            message = AIMessage.create(
                AIMessageSource.AI,
                content="",
                model=settings.model,
                temperature=settings.temperature,
                completed=True,
                tool_calls=tool_calls
            )
            self._conversation.add_message(message)
            await self._trigger_event(AIConversationEvent.TOOL_USED, message)

        self._current_reasoning_message = None
        self._current_ai_message = None

        # Check for tool calls BEFORE marking conversation as completed
        if tool_calls:
            # Don't emit COMPLETED event yet - we need to process tools first
            await self._process_pending_tool_calls(tool_calls)
            return

        await self._trigger_event(AIConversationEvent.COMPLETED)

    async def _update_streaming_response(
        self,
        reasoning: str,
        content: str,
        usage: AIUsage | None = None,
        error: AIError | None = None,
        tool_calls: List[ToolCall] | None = None
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
            await self._handle_content(reasoning, content, usage, tool_calls)

        # If we have no content but have reasoning, handle that separately
        elif reasoning:
            await self._handle_reasoning(reasoning, usage)

        if usage:
            await self._handle_usage(reasoning, content, tool_calls)

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        for task in self._current_tasks:
            if not task.done():
                task.cancel()
