"""Class to handle AI conversation separate from the GUI."""

import asyncio
import logging
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Set

from humbug.ai.ai_conversation_history import AIConversationHistory
from humbug.ai.ai_message import AIMessage
from humbug.ai.ai_message_source import AIMessageSource
from humbug.ai.ai_response import AIError
from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_usage import AIUsage
from humbug.transcript.transcript_error import TranscriptError
from humbug.transcript.transcript_handler import TranscriptHandler


class AIConversationEvent(Enum):
    """Events that can be emitted by the AIConversation class."""

    # AI response events
    REASONING_UPDATED = auto()  # When reasoning text is updated
    CONTENT_UPDATED = auto()    # When content is updated
    RESPONSE_ERROR = auto()     # When an error occurs during response
    RESPONSE_COMPLETED = auto() # When a response is fully completed

    # State change events
    STREAMING_STARTED = auto()  # When streaming begins
    STREAMING_STOPPED = auto()  # When streaming stops

    # Message events
    MESSAGE_ADDED = auto()      # When a new message is added to history
    MESSAGE_UPDATED = auto()    # When an existing message is updated

    # Cancellation and errors
    REQUEST_CANCELLED = auto()  # When a request is cancelled
    TRANSCRIPT_ERROR = auto()   # When there's an error writing to transcript


class AIConversation:
    """Handles AI conversation logic separate from the GUI.

    This class manages the communication with AI backends, maintains conversation
    history, and provides methods to start and cancel conversations.
    """

    def __init__(
        self,
        conversation_id: str,
        transcript_handler: TranscriptHandler,
        ai_backends: Dict[str, Any]
    ):
        """Initialize the AIConversation.

        Args:
            conversation_id: Unique identifier for this conversation
            transcript_handler: Handler for writing conversation to files
            ai_backends: Dictionary of available AI backends
        """
        self._logger = logging.getLogger("AIConversation")
        self._conversation_id = conversation_id
        self._transcript_handler = transcript_handler
        self._ai_backends = ai_backends

        self._settings = AIConversationSettings()
        self._conversation = AIConversationHistory(conversation_id)
        self._current_tasks: List[asyncio.Task] = []
        self._current_ai_message = None
        self._current_reasoning_message = None
        self._is_streaming = False
        self._last_submitted_message = None

        # Callbacks for events
        self._callbacks: Dict[AIConversationEvent, Set[Callable]] = {
            event: set() for event in AIConversationEvent
        }

    @property
    def conversation_id(self) -> str:
        """Get the conversation ID."""
        return self._conversation_id

    @property
    def is_streaming(self) -> bool:
        """Check if the conversation is currently streaming a response."""
        return self._is_streaming

    def register_callback(self, event: AIConversationEvent, callback: Callable) -> None:
        """Register a callback for a specific event.

        Args:
            event: The event to register for
            callback: The callback function to call when the event occurs
        """
        self._callbacks[event].add(callback)

    def unregister_callback(self, event: AIConversationEvent, callback: Callable) -> None:
        """Unregister a callback for a specific event.

        Args:
            event: The event to unregister from
            callback: The callback function to remove
        """
        if callback in self._callbacks[event]:
            self._callbacks[event].remove(callback)

    def _trigger_event(self, event: AIConversationEvent, *args, **kwargs) -> None:
        """Trigger all callbacks registered for an event.

        Args:
            event: The event to trigger
            *args: Arguments to pass to callbacks
            **kwargs: Keyword arguments to pass to callbacks
        """
        for callback in self._callbacks[event]:
            try:
                callback(*args, **kwargs)
            except Exception as e:
                self._logger.error(f"Error in callback for {event}: {e}")

    def update_conversation_settings(self, new_settings: AIConversationSettings) -> None:
        """Update conversation settings and associated backend.

        Args:
            new_settings: New settings to apply
        """
        self._settings = new_settings
        provider = AIConversationSettings.get_provider(new_settings.model)
        backend = self._ai_backends.get(provider)
        if backend:
            backend.update_conversation_settings(self._conversation_id, new_settings)

    def get_settings(self) -> AIConversationSettings:
        """Get current conversation settings.

        Returns:
            Current conversation settings
        """
        return AIConversationSettings(
            model=self._settings.model,
            temperature=self._settings.temperature,
            reasoning=self._settings.reasoning
        )

    def get_conversation_history(self) -> AIConversationHistory:
        """Get the conversation history object.

        Returns:
            The conversation history object
        """
        return self._conversation

    def get_token_counts(self) -> dict:
        """Get the current token counts for status display.

        Returns:
            Dictionary with token count information
        """
        return self._conversation.get_token_counts()

    async def _write_transcript(self, message: AIMessage) -> None:
        """Write messages to transcript file.

        Args:
            message: AIMessage to write to transcript
        """
        try:
            await self._transcript_handler.write([message.to_transcript_dict()])
        except TranscriptError as e:
            self._logger.error(f"Failed to write to transcript: {e}")
            self._trigger_event(AIConversationEvent.TRANSCRIPT_ERROR, str(e))

    def load_message_history(self, messages: List[AIMessage]) -> None:
        """Load existing message history from transcript.

        Args:
            messages: List of AIMessage objects to load
        """
        # Iterate over the messages
        for message in messages:
            self._conversation.add_message(message)
            self._trigger_event(AIConversationEvent.MESSAGE_ADDED, message)

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

    def _sanitize_input(self, text: str) -> str:
        """Strip control characters from input text, preserving newlines and tabs.

        Args:
            text: Text to sanitize

        Returns:
            Sanitized text
        """
        return ''.join(char for char in text if char == '\n' or char == '\t' or (ord(char) >= 32 and ord(char) != 127))

    async def submit_message(self, content: str) -> None:
        """Submit a new user message and start AI response.

        Args:
            content: User message content
        """
        if not content.strip():
            return

        content = self._sanitize_input(content.strip())
        self._last_submitted_message = content

        # Add the user message to the conversation
        message = AIMessage.create(AIMessageSource.USER, content)
        self._conversation.add_message(message)
        self._trigger_event(AIConversationEvent.MESSAGE_ADDED, message)
        await self._write_transcript(message)

        # Start AI response
        task = asyncio.create_task(self._start_ai())
        self._current_tasks.append(task)

        def task_done_callback(task):
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
            settings = self.get_settings()
            provider = AIConversationSettings.get_provider(settings.model)
            backend = self._ai_backends.get(provider)

            if not backend:
                error_msg = f"No backend available for provider: {provider}"
                self._logger.error(error_msg)
                error_message = AIMessage.create(
                    AIMessageSource.SYSTEM,
                    error_msg,
                    {"code": "backend_error", "message": error_msg}
                )
                self._conversation.add_message(error_message)
                self._trigger_event(AIConversationEvent.MESSAGE_ADDED, error_message)
                await self._write_transcript(error_message)

                if self._last_submitted_message:
                    self._trigger_event(AIConversationEvent.REQUEST_CANCELLED, self._last_submitted_message)
                    self._last_submitted_message = None

                self._is_streaming = False
                self._trigger_event(AIConversationEvent.STREAMING_STOPPED)
                return

            stream = backend.stream_message(
                self._conversation.get_messages_for_context(),
                self._conversation_id
            )

            async for response in stream:
                await self._update_streaming_response(
                    reasoning=response.reasoning,
                    content=response.content,
                    usage=response.usage,
                    error=response.error
                )

                if response.error:
                    if response.error.retries_exhausted:
                        if self._last_submitted_message:
                            self._trigger_event(AIConversationEvent.REQUEST_CANCELLED, self._last_submitted_message)
                            self._last_submitted_message = None
                        return

                    # We're retrying - continue to the next attempt
                    continue

            # If we get here and are still marked as streaming then we failed to get a
            # complete response before giving up. This is a failure and should be handled as such.
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
                if self._last_submitted_message:
                    self._trigger_event(AIConversationEvent.REQUEST_CANCELLED, self._last_submitted_message)
                    self._last_submitted_message = None
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
            if self._last_submitted_message:
                self._trigger_event(AIConversationEvent.REQUEST_CANCELLED, self._last_submitted_message)
                self._last_submitted_message = None
            return

        except Exception as e:
            self._logger.exception(
                f"Error processing AI response with model {settings.model}: {str(e)}"
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
            if self._last_submitted_message:
                self._trigger_event(AIConversationEvent.REQUEST_CANCELLED, self._last_submitted_message)
                self._last_submitted_message = None

        finally:
            self._logger.debug("=== Finished AI response ===")

            # Properly close the async generator if it exists
            if stream is not None:
                try:
                    await stream.aclose()
                except Exception as e:
                    # Log but don't propagate generator cleanup errors
                    self._logger.debug(f"Error during generator cleanup: {str(e)}")

    async def _update_streaming_response(
        self,
        reasoning: str,
        content: str,
        usage: Optional[AIUsage] = None,
        error: Optional[AIError] = None
    ) -> None:
        """Update the current AI response in the conversation.

        Args:
            reasoning: AI reasoning text
            content: AI response content
            usage: Optional token usage information
            error: Optional error information
        """
        if error:
            # Only stop streaming if retries are exhausted
            if error.retries_exhausted:
                self._is_streaming = False
                self._trigger_event(AIConversationEvent.STREAMING_STOPPED)

                # For cancellation, preserve the partial response first
                if self._current_reasoning_message:
                    message = self._conversation.update_message(
                        self._current_reasoning_message.id,
                        content=self._current_reasoning_message.content,
                        completed=False
                    )
                    if message:
                        await self._write_transcript(message)
                        self._trigger_event(AIConversationEvent.MESSAGE_UPDATED, message)

                    self._current_reasoning_message = None

                if self._current_ai_message:
                    message = self._conversation.update_message(
                        self._current_ai_message.id,
                        content=self._current_ai_message.content,
                        completed=False
                    )
                    if message:
                        await self._write_transcript(message)
                        self._trigger_event(AIConversationEvent.MESSAGE_UPDATED, message)

                    self._current_ai_message = None

            error_msg = error.message
            error_message = AIMessage.create(
                AIMessageSource.SYSTEM,
                error_msg,
                error={"code": error.code, "message": error.message, "details": error.details}
            )
            self._conversation.add_message(error_message)
            self._trigger_event(AIConversationEvent.MESSAGE_ADDED, error_message)
            self._trigger_event(AIConversationEvent.RESPONSE_ERROR, error)
            await self._write_transcript(error_message)

            # For cancellation, don't log as warning since it's user-initiated
            if error.code == "cancelled":
                self._logger.debug("AI response cancelled by user")
            else:
                self._logger.warning(f"AI response error: {error.message}")
            return

        if not self._is_streaming:
            self._is_streaming = True
            self._trigger_event(AIConversationEvent.STREAMING_STARTED)

        # Is our message an AI response or is it the AI reasoning?
        if content:
            # We're handling a response. Is this the first time we're seeing it?
            if not self._current_ai_message:
                # If we previously had reasoning from the AI then close that out
                if self._current_reasoning_message:
                    message = self._conversation.update_message(
                        self._current_reasoning_message.id,
                        reasoning,
                        usage=usage,
                        completed=True
                    )

                    await self._write_transcript(message)
                    self._trigger_event(AIConversationEvent.MESSAGE_UPDATED, message)
                    self._current_reasoning_message = None

                # Create and add initial AI response message
                settings = self.get_settings()
                message = AIMessage.create(
                    AIMessageSource.AI,
                    content,
                    model=settings.model,
                    temperature=settings.temperature,
                    completed=False
                )
                self._conversation.add_message(message)
                self._trigger_event(AIConversationEvent.MESSAGE_ADDED, message)
                self._current_ai_message = message
            else:
                # Update existing message
                message = self._conversation.update_message(
                    self._current_ai_message.id,
                    content,
                    usage=usage,
                    completed=(usage is not None)
                )
                if message:
                    self._trigger_event(AIConversationEvent.CONTENT_UPDATED, message)

            if usage:
                self._is_streaming = False
                self._trigger_event(AIConversationEvent.STREAMING_STOPPED)
                self._trigger_event(AIConversationEvent.RESPONSE_COMPLETED, self._current_ai_message)
                self._current_reasoning_message = None
                self._current_ai_message = None
                await self._write_transcript(message)
                return

            return

        if reasoning:
            # We're handling reasoning from our AI. Is it the first time we're seeing this?
            if not self._current_reasoning_message:
                # Create and add initial message
                settings = self.get_settings()
                message = AIMessage.create(
                    AIMessageSource.REASONING,
                    reasoning,
                    model=settings.model,
                    temperature=settings.temperature,
                    completed=False
                )
                self._conversation.add_message(message)
                self._trigger_event(AIConversationEvent.MESSAGE_ADDED, message)
                self._current_reasoning_message = message
            else:
                # Update existing message
                message = self._conversation.update_message(
                    self._current_reasoning_message.id,
                    reasoning,
                    usage=usage,
                    completed=False
                )
                if message:
                    self._trigger_event(AIConversationEvent.REASONING_UPDATED, message)

    def cancel_current_tasks(self) -> None:
        """Cancel any ongoing AI response tasks."""
        for task in self._current_tasks:
            if not task.done():
                task.cancel()
