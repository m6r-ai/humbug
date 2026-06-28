"""Enhanced AI conversation class with tool calling support."""

import asyncio
from collections.abc import Callable
from enum import Enum, auto
import json
import logging
from typing import Any

from ai.ai_conversation_history import AIConversationHistory
from ai.ai_conversation_settings import AIConversationSettings
from ai.ai_manager import AIManager
from ai.ai_message import AIMessage
from ai.ai_message_source import AIMessageSource
from ai.ai_model import AIReasoningCapability
from ai.ai_response import AIError
from ai.ai_usage import AIUsage
from ai_tool import AIToolManager, AIToolCall, AIToolResult, AIToolAuthorizationDenied
from syntax import ProgrammingLanguage, ProgrammingLanguageUtils


class ConversationState(Enum):
    """State of the conversation processing."""
    IDLE = auto()
    STREAMING_AI_RESPONSE = auto()
    EXECUTING_TOOLS = auto()
    WAITING_FOR_APPROVAL = auto()


class AIConversationEvent(Enum):
    """Events that can be emitted by the AIConversation class."""
    ERROR = auto()              # When an error occurs during request processing
    COMPLETED = auto()          # When a response is fully completed
    MESSAGE_ADDED = auto()      # When a new message is added to history
    MESSAGE_UPDATED = auto()    # When an existing message is updated
    MESSAGE_COMPLETED = auto()  # When an existing message has been completed
    MESSAGE_ADDED_AND_COMPLETED = auto()
                                # When a new message is added and completed immediately
    TOOL_APPROVAL_REQUIRED = auto()
                                # When tool calls need user approval
    STREAMING_UPDATE = auto()   # When a streaming response is updated
    AI_CONNECTED = auto()       # When AI connection is established
    RATE_LIMITED = auto()       # When a rate limit retry is in progress (not exhausted)


class AIConversation:
    """
    Handles AI conversation logic separate from the GUI.

    This class manages the communication with AI backends, maintains conversation
    history, and provides methods to start and cancel conversations.
    """

    def __init__(self) -> None:
        """Initialize the AIConversation."""
        self._logger = logging.getLogger("AIConversation")
        self._ai_manager = AIManager()
        self._tool_manager = AIToolManager()
        self._settings = AIConversationSettings()
        self._conversation = AIConversationHistory()
        self._current_tasks: list[asyncio.Task] = []
        self._current_ai_message: AIMessage | None = None
        self._current_reasoning_message: AIMessage | None = None
        self._is_streaming = False
        self._expecting_cancellation = False

        # Tool approval state
        self._pending_tool_calls: list[AIToolCall] = []
        self._pending_tool_call_message: AIMessage | None = None
        self._pending_authorization_future: asyncio.Future[bool] | None = None

        # Conversation state and interruption handling
        self._state = ConversationState.IDLE
        self._pending_user_messages: list[AIMessage] = []

        # Callbacks for events
        self._callbacks: dict[AIConversationEvent, set[Callable]] = {
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
            provider=self._settings.provider,
            temperature=self._settings.temperature,
            reasoning=self._settings.reasoning,
            reasoning_effort=self._settings.reasoning_effort,
        )

    def get_conversation_history(self) -> AIConversationHistory:
        """
        Get the conversation history object.

        Returns:
            The conversation history object
        """
        return self._conversation

    def get_token_counts(self) -> dict[str, int]:
        """
        Get the current token counts for status display.

        Returns:
            Dictionary with token count information
        """
        return self._conversation.get_token_counts()

    def load_message_history(self, messages: list[AIMessage]) -> None:
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
                reasoning = message.reasoning_capability if message.reasoning_capability else AIReasoningCapability.NO_REASONING
                if message.model and message.provider:
                    self.update_conversation_settings(AIConversationSettings(
                        model=message.model,
                        provider=message.provider,
                        temperature=message.temperature,
                        reasoning=reasoning,
                        reasoning_effort=message.reasoning_effort,
                    ))

    def load_history(self, history: AIConversationHistory) -> None:
        """
        Load a complete conversation history including messages and attachments.

        This is the preferred method when restoring a full AIConversationHistory
        object, as it preserves the attachment store.  Use load_message_history
        only when you have a bare message list with no attachments.

        Args:
            history: Full conversation history to restore
        """
        self._conversation.clear()
        self._conversation.restore_attachments(history.attachments())

        for message in history.get_messages():
            self._conversation.add_message(message)

            if message.source == AIMessageSource.AI:
                reasoning = message.reasoning_capability if message.reasoning_capability else AIReasoningCapability.NO_REASONING
                if message.model and message.provider:
                    self.update_conversation_settings(AIConversationSettings(
                        model=message.model,
                        provider=message.provider,
                        temperature=message.temperature,
                        reasoning=reasoning,
                        reasoning_effort=message.reasoning_effort,
                    ))

    async def submit_message(
        self,
        requester: str | None,
        user_message: str,
        attachment_guids: list[str] | None = None
    ) -> None:
        """
        Submit a user message to the conversation.

        If the conversation is currently executing tools, the message will be
        appended to the tool results as user feedback. If streaming an AI response,
        the message will be queued and processed after the current cycle completes.

        Args:
            requester: Name of the user submitting the message (or None)
            user_message: The user message to submit
            attachment_guids: Optional list of attachment GUIDs to associate with the message
        """
        settings = self.conversation_settings()

        # If we're actively processing, queue the message
        if self._state in (ConversationState.EXECUTING_TOOLS, ConversationState.STREAMING_AI_RESPONSE):
            self._logger.debug("Queuing message during active processing")
            message = AIMessage.create(
                AIMessageSource.USER_QUEUED,
                user_message,
                model=settings.model,
            provider=settings.provider,
                temperature=settings.temperature,
                reasoning_capability=settings.reasoning,
                reasoning_effort=settings.reasoning_effort,
            )
            self._pending_user_messages.append(message)
            await self._trigger_event(AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, message)
            return

        # Normal submission when idle
        message = AIMessage.create(
            AIMessageSource.USER,
            user_message,
            user_name=requester,
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning_capability=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
            attachments=attachment_guids if attachment_guids else None,
        )
        self._conversation.add_message(message)
        await self._trigger_event(AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, message)

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
        self._expecting_cancellation = False
        self._state = ConversationState.STREAMING_AI_RESPONSE
        stream = None
        settings = self.conversation_settings()

        try:
            self._logger.debug("Starting AI response streaming")

            # Get appropriate backend for conversation
            backend = self._ai_manager.get_backends().get(settings.provider)

            if not backend:
                error_msg = f"No backend available for provider: {settings.provider}"
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

            stream = backend.stream_message(self._conversation, self._settings)
            async for response in stream:
                await self._update_streaming_response(
                    reasoning=response.reasoning,
                    content=response.content,
                    usage=response.usage,
                    error=response.error,
                    tool_calls=response.tool_calls,
                    signature=response.signature,
                    redacted_reasoning=response.redacted_reasoning,
                    connected=response.connected
                )

                if response.error and response.error.retries_exhausted:
                    return

            # If we get here and are still marked as streaming then we failed to get a
            # complete response before giving up.  This is a failure and should be handled as such.
            if self._is_streaming:
                self._logger.debug("AI response failed (likely timeout)")
                await self._handle_error(
                    AIError(
                        code="cancelled",
                        message="Server failed to complete response",
                        retries_exhausted=True,
                        details={"type": "CancelledError"}
                    )
                )
                return

        except asyncio.CancelledError:
            self._logger.debug("AI response cancelled")
            await self._handle_error(
                AIError(
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
            await self._handle_error(
                AIError(
                    code="process_error",
                    message=str(e),
                    retries_exhausted=True,
                    details={"type": type(e).__name__}
                )
            )
            return

        finally:
            # Properly close the async generator if it exists
            if stream is not None:
                try:
                    await stream.aclose()

                except BaseException as e:
                    # Log but don't propagate generator cleanup errors
                    self._logger.debug("Error during generator cleanup: %s", e)

            self._state = ConversationState.IDLE

    async def _request_tool_authorization(
        self,
        tool_name: str,
        arguments: dict[str, Any],
        reason: str,
        context: str | None,
        destructive: bool
    ) -> bool:
        """
        Request authorization for a specific tool call during execution.

        This creates a single tool call and triggers the approval UI.

        Args:
            tool_name: Name of the tool requesting authorization
            arguments: Arguments being passed to the tool
            reason: Human-readable reason why authorization is needed
            context: Optional context information for the authorization
            destructive: Whether the tool call is considered destructive

        Returns:
            True if authorized, False if denied
        """
        # Before we request tool approval, check we don't have a queued message from the user.  If we do, we should process
        # that first since it may impact the tool call or even make it irrelevant.
        if self._pending_user_messages:
            self._logger.debug("Processing %d queued messages before requesting tool authorization",
                                len(self._pending_user_messages))
            return False

        self._logger.debug("Tool '%s' requesting authorization: %s", tool_name, reason)

        # Create a tool call for authorization
        tool_call = AIToolCall(
            id=f"auth_{tool_name}_{hash(str(arguments))}",
            name=tool_name,
            arguments=arguments
        )

        # Create a future to wait for the authorization result
        self._pending_authorization_future = asyncio.Future()

        try:
            # Trigger the approval UI
            await self._trigger_event(
                AIConversationEvent.TOOL_APPROVAL_REQUIRED,
                self._pending_tool_call_message,
                tool_call,
                reason,
                context,
                destructive
            )

            # Wait for the user's response
            result = await self._pending_authorization_future
            self._logger.debug("Tool '%s' authorization result: %s", tool_name, result)
            return result

        finally:
            self._pending_authorization_future = None

    def _extract_context(self, tool_call: AIToolCall) -> str | None:
        """
        Extract context information from the tool call.

        Args:
            tool_call: Tool call containing arguments and metadata

        Returns:
            Context string if available, otherwise None
        """
        tool = self._tool_manager.get_tool(tool_call.name)
        if tool is None:
            return None

        if not self._tool_manager.is_tool_enabled(tool_call.name):
            return None

        return tool.extract_context(tool_call)

    async def _execute_tool(
        self,
        tool_call: AIToolCall,
    ) -> AIToolResult:
        """
        Execute a tool call.

        Args:
            tool_call: The tool call to execute

        Returns:
            AIToolResult containing the execution result
        """
        tool = self._tool_manager.get_tool(tool_call.name)
        if tool is None:
            error_msg = f"Unknown tool: {tool_call.name}"
            self._logger.error(error_msg)
            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=error_msg
            )

        if not self._tool_manager.is_tool_enabled(tool_call.name):
            error_msg = f"Tool is disabled: {tool_call.name}"
            self._logger.error(error_msg)
            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=error_msg
            )

        try:
            self._logger.debug(
                "Executing tool '%s' with args %s",
                tool_call.name,
                tool_call.arguments
            )

            result = await tool.execute(tool_call, self, self._request_tool_authorization)

            self._logger.debug(
                "Tool '%s' executed successfully with args %s",
                tool_call.name,
                tool_call.arguments
            )

            return result

        except AIToolAuthorizationDenied as e:
            error_msg = f"Tool authorization denied: {str(e)}"
            self._logger.info(
                "Tool '%s' authorization denied with args %s: %s",
                tool_call.name,
                tool_call.arguments,
                str(e)
            )
            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=error_msg
            )

        except Exception as e:
            error_msg = f"Tool execution failed: {str(e)}"
            self._logger.exception(
                "Tool '%s' failed with args %s: %s",
                tool_call.name,
                tool_call.arguments,
                str(e)
            )

            return AIToolResult(
                id=tool_call.id,
                name=tool_call.name,
                content="",
                error=error_msg,
                context=f"`error` is:\n```text\n{str(e)}\n```"
            )

    async def _execute_tool_calls(self, tool_calls: list[AIToolCall]) -> None:
        """Execute tool calls with support for parallel execution via continuations."""
        self._state = ConversationState.EXECUTING_TOOLS
        self._logger.debug("Executing tool calls with continuation support...")

        # Execute all tool calls and collect results and continuations
        tool_results: list[AIToolResult] = []
        continuations = []

        for tool_call in tool_calls:
            self._logger.debug("Executing tool call: %s", tool_call.name)

            # Create and add tool call message
            tool_call_dict = tool_call.to_dict()
            content = f"""```json\n{json.dumps(tool_call_dict, indent=2)}\n```"""
            context = self._extract_context(tool_call)
            tool_call_message = AIMessage.create(
                source=AIMessageSource.TOOL_CALL,
                content=content,
                tool_calls=[tool_call],
                tool_call_context=context,
                completed=True
            )
            self._conversation.add_message(tool_call_message)

            # Store the message for potential completion during authorization
            self._pending_tool_call_message = tool_call_message

            await self._trigger_event(AIConversationEvent.MESSAGE_ADDED, tool_call_message)

            # Execute the tool call
            tool_result = await self._execute_tool(tool_call)
            tool_results.append(tool_result)

            # If the tool returned a continuation, collect it
            if tool_result.continuation:
                continuations.append(tool_result.continuation)

            # If our tool didn't require authorization we need to close out the tool call message
            if self._pending_tool_call_message:
                approved_message = self._conversation.update_message(
                    self._pending_tool_call_message.id,
                    content=self._pending_tool_call_message.content,
                    completed=True
                )
                if approved_message:
                    await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, approved_message)

                self._pending_tool_call_message = None

            if tool_result.continuation is None:
                # Create tool result message
                tool_result_dict = tool_result.to_dict()
                content = f"""```json\n{json.dumps(tool_result_dict, indent=2)}\n```"""

                # See if there's extra context to add for this tool result
                content_language = tool_result.context
                if content_language is not None:
                    # Before we use the context as a language, ensure it's valid
                    if ProgrammingLanguageUtils.from_name(content_language) == ProgrammingLanguage.TEXT:
                        content_language = "text"

                    if tool_result.error:
                        content += f"\n`error` is:\n```{content_language}\n{tool_result.error}\n```"

                    else:
                        content += f"\n`content` is:\n```{content_language}\n{tool_result.content}\n```"

                tool_result_message = AIMessage.create(
                    source=AIMessageSource.TOOL_RESULT,
                    content=content,
                    tool_results=[tool_result],
                    completed=True
                )
                self._conversation.add_message(tool_result_message)
                await self._trigger_event(AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, tool_result_message)

        # If we have continuations, wait for them to complete.  Then we update the tool results.
        if continuations:
            self._logger.debug("Waiting for %d tool continuations to complete...", len(continuations))
            continuation_results: list[AIToolResult] = await asyncio.gather(*continuations)
            self._logger.debug("Tool continuations completed: %s", continuation_results)

            # We need to iterate the continuation results and update the tool results
            for continuation in continuation_results:
                for tool_result in tool_results:
                    if tool_result.id == continuation.id:
                        tool_result.continuation = None
                        tool_result.content = continuation.content
                        tool_result.context = continuation.context
                        tool_result.error = continuation.error

                        # Create tool result message
                        tool_result_dict = tool_result.to_dict()
                        content = f"""```json\n{json.dumps(tool_result_dict, indent=2)}\n```"""

                        # See if there's extra context to add for this tool result
                        content_language = tool_result.context
                        if content_language is not None:
                            # Before we use the context as a language, ensure it's valid
                            if ProgrammingLanguageUtils.from_name(content_language) == ProgrammingLanguage.TEXT:
                                content_language = "text"

                        if tool_result.error:
                            content += f"\n`error` is:\n```{content_language}\n{tool_result.error}\n```"

                        else:
                            content += f"\n`content` is:\n```{content_language}\n{tool_result.content}\n```"

                        tool_result_message = AIMessage.create(
                            source=AIMessageSource.TOOL_RESULT,
                            content=content,
                            tool_results=[tool_result],
                            completed=True
                        )
                        self._conversation.add_message(tool_result_message)
                        await self._trigger_event(AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, tool_result_message)
                        break

        # Check for pending queued messages and append them to tool results
        queued_content = ""
        if self._pending_user_messages:
            self._logger.debug("Appending %d queued messages to tool results", len(self._pending_user_messages))

            # Collect queued messages
            queued_texts = []
            for msg in self._pending_user_messages:
                if msg.content:
                    queued_texts.append(msg.content)

            # Format as user feedback
            if queued_texts:
                queued_content = "\n\n".join([f"{text}" for text in queued_texts])

            # Clear the queue
            self._pending_user_messages.clear()

        # Create a specific user message with the tool results (and any queued messages)
        tool_response_message = AIMessage.create(
            AIMessageSource.USER,
            content=queued_content,
            tool_results=tool_results
        )
        self._conversation.add_message(tool_response_message)
        await self._trigger_event(AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, tool_response_message)

        # Continue the conversation with tool results
        self._state = ConversationState.IDLE
        await self._start_ai()

    async def approve_pending_tool_calls(self) -> None:
        """Approve the current pending tool authorization."""
        if not self._pending_authorization_future or self._pending_authorization_future.done():
            return

        self._logger.debug("User approved tool authorization")

        # Complete the authorization message
        if self._pending_tool_call_message:
            approved_message = self._conversation.update_message(
                self._pending_tool_call_message.id,
                content=self._pending_tool_call_message.content,
                completed=True
            )
            if approved_message:
                await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, approved_message)

            self._pending_tool_call_message = None

        # Signal approval
        self._pending_authorization_future.set_result(True)

    async def reject_pending_tool_calls(self, reason: str = "User rejected tool call") -> None:
        """Reject the current pending tool authorization."""
        if not self._pending_authorization_future or self._pending_authorization_future.done():
            return

        self._logger.debug("User rejected tool authorization: %s", reason)

        # Complete the authorization message
        if self._pending_tool_call_message:
            rejected_message = self._conversation.update_message(
                self._pending_tool_call_message.id,
                content=self._pending_tool_call_message.content,
                completed=True
            )
            if rejected_message:
                await self._trigger_event(AIConversationEvent.MESSAGE_COMPLETED, rejected_message)

            self._pending_tool_call_message = None

        # Signal denial
        self._pending_authorization_future.set_result(False)

    async def _handle_error(self, error: AIError) -> None:
        """
        Handle errors that occur during AI response processing.

        Args:
            error: AIError object containing error details
        """
        # For cancellation, don't log as warning since it's user-initiated
        notify = True
        if error.code == "cancelled":
            notify = not self._expecting_cancellation
            self._logger.debug("AI response cancelled by user")

        else:
            self._logger.warning("AI response error: %s", error.message)

        # Only stop streaming if retries are exhausted
        if not error.retries_exhausted:
            if error.code == "rate_limit":
                await self._trigger_event(AIConversationEvent.RATE_LIMITED, error.message)

            return

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

        if not notify:
            return

        error_msg = error.message
        error_message = AIMessage.create(
            AIMessageSource.SYSTEM,
            error_msg,
            error={"code": error.code, "message": error.message, "details": error.details}
        )
        self._conversation.add_message(error_message)
        await self._trigger_event(AIConversationEvent.ERROR, error.retries_exhausted, error_message)

    async def _handle_connection(self) -> None:
        """Handle AI connection established."""
        settings = self.conversation_settings()
        connection_message = AIMessage.create(
            AIMessageSource.AI_CONNECTED,
            "",
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning_capability=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
            completed=True
        )

        # Add to history first, then trigger events
        self._conversation.add_message(connection_message)
        await self._trigger_event(AIConversationEvent.AI_CONNECTED, connection_message)

    async def _handle_content(
        self,
        content: str,
        usage: AIUsage | None,
        tool_calls: list[AIToolCall] | None,
        redacted_reasoning: str | None = None
    ) -> None:
        """
        Handle content updates from the AI response.

        Args:
            content: Main content of the AI response
            usage: Optional token usage information
            tool_calls: Optional list of tool calls made by the AI
            redacted_reasoning: Optional redacted reasoning text
        """
        if self._current_ai_message:
            # Update existing message with new tool calls if provided
            if tool_calls:
                self._current_ai_message.tool_calls = tool_calls

            # Update existing message
            message = self._conversation.update_message(
                self._current_ai_message.id,
                content,
                usage=usage,
                completed=(usage is not None),
                redacted_reasoning=redacted_reasoning
            )
            if message:
                await self._trigger_event(
                    AIConversationEvent.MESSAGE_UPDATED if usage is None else AIConversationEvent.MESSAGE_COMPLETED,
                    message
                )

            return

        # Create and add initial AI response message
        settings = self.conversation_settings()
        new_message = AIMessage.create(
            AIMessageSource.AI,
            content,
            model=settings.model,
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning_capability=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
            completed=(usage is not None),
            tool_calls=tool_calls,
            redacted_reasoning=redacted_reasoning
        )
        self._conversation.add_message(new_message)
        await self._trigger_event(
            AIConversationEvent.MESSAGE_ADDED if usage is None else AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED,
            new_message
        )
        self._current_ai_message = new_message

    async def _handle_reasoning(
        self,
        reasoning: str,
        usage: AIUsage | None,
        tool_calls: list[AIToolCall] | None,
        signature: str | None = None,
        redacted_reasoning: str | None = None
    ) -> None:
        """
        Handle reasoning updates from the AI response.

        Args:
            reasoning: Reasoning text from the AI response
            usage: Optional token usage information
            tool_calls: Optional list of tool calls made by the AI
            signature: Optional signature for the response
            redacted_reasoning: Optional redacted reasoning text
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
                redacted_reasoning=redacted_reasoning
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
            provider=settings.provider,
            temperature=settings.temperature,
            reasoning_capability=settings.reasoning,
            reasoning_effort=settings.reasoning_effort,
            completed=(usage is not None),
            tool_calls=tool_calls,
            signature=signature,
            redacted_reasoning=redacted_reasoning
        )
        self._conversation.add_message(new_message)
        await self._trigger_event(
            AIConversationEvent.MESSAGE_ADDED if usage is None else AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED,
            new_message
        )
        self._current_reasoning_message = new_message

    async def _handle_usage(
        self,
        reasoning: str,
        content: str,
        tool_calls: list[AIToolCall] | None,
        redacted_reasoning: str | None = None
    ) -> None:
        """Finalize the AI response, handle tool calls, and emit completion events."""
        self._logger.debug("Finished AI response streaming")

        self._is_streaming = False

        if not content and not reasoning:
            settings = self.conversation_settings()
            message = AIMessage.create(
                AIMessageSource.AI,
                content="",
                model=settings.model,
                provider=settings.provider,
                temperature=settings.temperature,
                reasoning_capability=settings.reasoning,
                reasoning_effort=settings.reasoning_effort,
                completed=True,
                tool_calls=tool_calls,
                redacted_reasoning=redacted_reasoning
            )
            self._conversation.add_message(message)
            await self._trigger_event(AIConversationEvent.MESSAGE_ADDED_AND_COMPLETED, message)

        self._current_reasoning_message = None
        self._current_ai_message = None

        # Check for tool calls - execute them directly, authorization happens during execution if needed
        if tool_calls:
            await self._execute_tool_calls(tool_calls)
            return

        await self._trigger_event(AIConversationEvent.COMPLETED)

    async def _handle_pending_user_messages(self) -> None:
        """Process any user messages queued during AI response streaming."""
        # Check if we have any queued messages to process.
        if not self._pending_user_messages:
            return

        self._logger.debug("Processing %d queued messages after AI response completion",
                            len(self._pending_user_messages))

        # Collect queued messages
        queued_texts = []
        for msg in self._pending_user_messages:
            if msg.content:
                queued_texts.append(msg.content)

        # Format as user feedback
        queued_content = "\n\n".join([f"{text}" for text in queued_texts])

        # Clear the queue
        self._pending_user_messages.clear()

        # Create a specific user message with the queued messages
        queued_message = AIMessage.create(
            AIMessageSource.USER,
            content=queued_content
        )
        self._conversation.add_message(queued_message)
        await self._trigger_event(AIConversationEvent.MESSAGE_ADDED, queued_message)

        # Continue the conversation with tool results
        self._state = ConversationState.IDLE
        await self._start_ai()

    async def _update_streaming_response(
        self,
        reasoning: str,
        content: str,
        usage: AIUsage | None = None,
        error: AIError | None = None,
        tool_calls: list[AIToolCall] | None = None,
        signature: str | None = None,
        redacted_reasoning: str | None = None,
        connected: bool = False
    ) -> None:
        """
        Update the current AI response in the conversation.

        Args:
            reasoning: AI reasoning text
            content: AI response content
            usage: Optional token usage information
            error: Optional error information
            tool_calls: Optional list of tool calls made by the AI
            signature: Optional signature for the response
            redacted_reasoning: Optional redacted reasoning text
            connected: Whether this indicates AI connection established
        """
        if error:
            await self._handle_error(error)
            return

        # Handle connection signal
        if connected:
            await self._handle_connection()
            return

        if not self._is_streaming:
            self._is_streaming = True

        # Trigger streaming update event for visual feedback
        await self._trigger_event(AIConversationEvent.STREAMING_UPDATE)

        if reasoning:
            await self._handle_reasoning(reasoning, usage, tool_calls, signature, redacted_reasoning)

        if content:
            await self._handle_content(content, usage, tool_calls, redacted_reasoning)

        if usage:
            await self._handle_usage(reasoning, content, tool_calls, redacted_reasoning)
            await self._handle_pending_user_messages()

    def cancel_current_tasks(self, notify: bool = True) -> None:
        """Cancel any ongoing AI response tasks."""
        # If notify is False, we are expecting a cancellation and should not log it as unexpected
        self._expecting_cancellation = not notify

        # Cancel pending tool authorization if present
        if self._pending_authorization_future and not self._pending_authorization_future.done():
            self._logger.debug("Cancelling pending tool authorization due to task cancellation")
            loop = asyncio.get_event_loop()
            if loop.is_running():
                loop.create_task(self.reject_pending_tool_calls("Operation cancelled by user"))

        for task in self._current_tasks:
            if not task.done():
                task.cancel()

        # Cancel any active tool operations
        for tool_name in self._tool_manager.get_tool_names():
            tool = self._tool_manager.get_tool(tool_name)
            if tool:
                try:
                    tool.cancel()

                except Exception:
                    self._logger.exception("Error cancelling tool %s", tool_name)

        self._pending_user_messages.clear()

        self._state = ConversationState.IDLE

    async def retry_last_request(self) -> list[str]:
        """
        Retry the last request by removing all messages since the last user/tool input.

        This should only be called when the conversation is idle and the last message in history
        is a SYSTEM error message.  It removes that message plus any partial AI/reasoning/connected
        messages that were added before it, then restarts the AI from the last USER or TOOL_RESULT
        message.

        Returns:
            List of message IDs that were removed from history, so the caller can remove the
            corresponding widgets from the UI.
        """
        if self._state != ConversationState.IDLE:
            self._logger.warning("retry_last_request called while not idle; ignoring")
            return []

        messages = self._conversation.get_messages()
        if not messages:
            self._logger.warning("retry_last_request called with empty history; ignoring")
            return []

        last = messages[-1]
        if last.source != AIMessageSource.SYSTEM or not last.error:
            self._logger.warning("retry_last_request called but last message is not a SYSTEM error; ignoring")
            return []

        # Strip all messages back to (but not including) the last USER or TOOL_RESULT message.
        # That covers: SYSTEM(error), AI(partial), REASONING(partial), AI_CONNECTED.
        removed_ids: list[str] = []
        retry_sources = {AIMessageSource.USER, AIMessageSource.TOOL_RESULT}
        while self._conversation.get_messages() and self._conversation.get_messages()[-1].source not in retry_sources:
            removed = self._conversation.remove_last_message()
            if removed:
                removed_ids.append(removed.id)

        task = asyncio.create_task(self._start_ai())
        self._current_tasks.append(task)

        def task_done_callback(task: asyncio.Task) -> None:
            try:
                self._current_tasks.remove(task)

            except ValueError:
                self._logger.debug("Task already removed")

        task.add_done_callback(task_done_callback)

        return removed_ids

    def truncate_to_message(self, message_id: str) -> str | None:
        """
        Truncate the conversation history to just before the message with the given ID.

        Only user messages may be truncated to.  The attachment store is pruned so that
        only attachments referenced by the preserved messages are retained.

        Args:
            message_id: ID of the user message to truncate from (this message and
                everything after it will be removed)

        Returns:
            The content of the truncated message so the caller can restore it to the
            input box, or None if the message was not found or is not a user message.
        """
        messages = self._conversation.get_messages()
        index = next((i for i, m in enumerate(messages) if m.id == message_id), -1)
        if index < 0:
            self._logger.warning("truncate_to_message: message %s not found", message_id)
            return None

        if messages[index].source != AIMessageSource.USER:
            self._logger.warning("truncate_to_message: message %s is not a user message", message_id)
            return None

        truncated_content = messages[index].content
        preserved = messages[:index]

        # Collect attachment GUIDs referenced by the preserved messages
        referenced_guids: set[str] = set()
        for msg in preserved:
            if msg.attachments:
                referenced_guids.update(msg.attachments)

        # Rebuild the attachment store keeping only referenced attachments
        all_attachments = self._conversation.attachments()
        pruned_attachments = {
            guid: data for guid, data in all_attachments.items()
            if guid in referenced_guids
        }

        # Replace history with the preserved slice and pruned attachments
        self._conversation.clear()
        self._conversation.restore_attachments(pruned_attachments)
        for msg in preserved:
            self._conversation.add_message(msg)

        return truncated_content

    def fork_history_to_index(self, message_index: int) -> AIConversationHistory:
        """
        Return a copy of the conversation history up to (but not including) the given
        message index, with only the attachments referenced by those messages included.

        This does not modify the current conversation.  The caller is responsible for
        persisting the returned history.

        Args:
            message_index: Messages before this index are included in the fork.
                Pass None or len(messages) to fork the full conversation.

        Returns:
            A new AIConversationHistory containing the forked messages and attachments.
        """
        messages = self._conversation.get_messages()
        forked_messages = messages[:message_index]

        # Collect attachment GUIDs referenced by the forked messages
        referenced_guids: set[str] = set()
        for msg in forked_messages:
            if msg.attachments:
                referenced_guids.update(msg.attachments)

        all_attachments = self._conversation.attachments()
        forked_attachments = {
            guid: data for guid, data in all_attachments.items()
            if guid in referenced_guids
        }

        return AIConversationHistory(
            messages=forked_messages,
            version=self._conversation.version(),
            parent=self._conversation.parent(),
            attachments=forked_attachments
        )
