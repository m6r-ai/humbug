"""Delegate AI tool backend."""

import asyncio
import os
import json
import logging
from datetime import datetime, timezone
from typing import Any, Dict

from ai import (
    AIConversation, AIConversationEvent, AIConversationParent,
    AIConversationSettings, AIManager, AIMessageSource, AIReasoningCapability
)
from ai.ai_model import AIReasoningEffort
from ai_tool import (
    AIToolDefinition, AIToolParameter, AITool, AIToolExecutionError,
    AIToolAuthorizationDenied, AIToolAuthorizationCallback,
    AIToolResult, AIToolCall, AIToolOperationDefinition
)
from ai_transcript_conversation import AITranscriptConversation
from conversation_context.conversation_context import ConversationContext
from mindspace.mindspace import Mindspace
from mindspace.mindspace_log_level import MindspaceLogLevel


class DelegateAITool(AITool):
    """
    AI delegation tool for LLM interaction.

    Allows delegating cognitive tasks to specialized AI instances. Creates focused AI
    conversations for specific sub-tasks, expert analysis, or collaborative problem-solving.
    The delegated AI operates with controlled context optimized for the task, can use tools,
    and supports multi-turn collaboration through session persistence.
    """

    def __init__(
        self,
        mindspace: Mindspace,
    ) -> None:
        """
        Initialize the delegate AI tool.

        Args:
            mindspace: The active mindspace.
        """
        self._mindspace = mindspace
        self._ai_manager = AIManager()
        self._logger = logging.getLogger("DelegateAITool")
        self._context_ids: Dict[str, str] = {}

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return self._build_definition_from_operations(
            name="delegate_ai",
            description_prefix=(
                "The delegate_ai tool lets you (the parent AI) delegate tasks to specialized child AI instances "
                "by creating a focused AI conversation for a specific task. The delegated AI has access to the same tools as "
                "you and can engage in multi-turn collaboration with you if you provide it with a session_id returned from a "
                "previous delegated conversation. Each call creates a temporary UI tab that closes after the response is "
                "returned\n\n"

                "Examples where delegation is useful:\n"
                "- You want to use AI capabilities from a different model, or with different settings\n"
                "- You want to Seek analysis from a different perspective (you must provide the perspective in the task_prompt)\n"
                "- You want to process a lot of context/tokens where you don't need all the context in your main conversation\n"
                "- You want to xplore multiple approaches to a problem before converging on a solution in your main conversation\n"
                "- You want a sounding board for ideas or to brainstorm with a different AI\n\n"

                "Important:\n"
                "- Never delegate a task to a child AI that is largely the same task you have been given."
                "- You MUST tell the child AI that it is a child AI instance that has been tasked using the delegate_ai tool\n"
                "- You MUST tell the child AI that if it creates any files, it must inform you (the parent AI) about them\n"
                "- Never use this tool to try to access any tools you do not have access to (if you cannot do "
                "something, neither can a child AI)\n"
                "- You must use the session_id parameter to continue a previous conversation\n"
                "- You must generate the task_prompt and that can be very expensive if you need to provide a lot of "
                "context in it. If you need to provide something that is already in the filesystem, tell the "
                "delegated AI where to read it and do not provide the contents in the task_prompt. If the content is not "
                "already in the filesystem, you must provide it in the task_prompt.\n"
                "- When you delegate one or more tasks you will be blocked until the child/children AIs complete "
                "their work. You cannot work in parallel with any child AI\n"
                "- Never delegate a task where a child AIs lack of context may give poor results\n"
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
                    description="Session ID to continue a previous delegated conversation. Omit to start a new session.",
                    required=False
                ),
                AIToolParameter(
                    name="model",
                    type="string",
                    description="AI model to use for the delegated task",
                    required=False
                ),
                AIToolParameter(
                    name="provider",
                    type="string",
                    description="Provider to use for the delegated task. Required when the model name "
                        "is offered by more than one provider",
                    required=False
                ),
                AIToolParameter(
                    name="temperature",
                    type="number",
                    description="Temperature setting 0.0-1.0 for creativity vs precision",
                    required=False
                ),
                AIToolParameter(
                    name="reasoning_effort",
                    type="string",
                    description="Reasoning effort level for models that support it. "
                        f"One of: {', '.join(AIReasoningEffort.values())}. "
                        "Only valid for models with variable reasoning effort support.",
                    required=False
                )
            ]
        )

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return "Delegate tasks to specialized child AI instances for parallel or focused work."

    def get_operation_definitions(self) -> Dict[str, AIToolOperationDefinition]:
        """Get operation definitions for this tool."""
        return {
            "delegate": AIToolOperationDefinition(
                name="delegate",
                handler=self._delegate,
                extract_context=None,
                allowed_parameters={"task_prompt", "session_id", "model", "provider", "temperature", "reasoning_effort"},
                required_parameters={"task_prompt"},
                description="Delegate a task to a specialized child AI instance"
            )
        }

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
            requester_ref: The parent AIConversation making the request
            request_authorization: Callback for requesting user authorization

        Returns:
            AIToolResult with a continuation that resolves when the child finishes

        Raises:
            AIToolExecutionError: If operation fails
            AIToolAuthorizationDenied: If user denies authorization
        """
        arguments = tool_call.arguments
        task_prompt = self._get_required_str_value("task_prompt", arguments)
        session_id_arg = self._get_optional_str_value("session_id", arguments)
        model = self._get_optional_str_value("model", arguments)
        provider = self._get_optional_str_value("provider", arguments)
        temperature = arguments.get("temperature")
        reasoning_effort_arg = self._get_optional_str_value("reasoning_effort", arguments)

        # Resolve session_id
        session_path: str | None = None
        if session_id_arg:
            session_path = self._resolve_session_path(session_id_arg)

        # Validate temperature
        if temperature is not None:
            if not isinstance(temperature, (int, float)):
                raise AIToolExecutionError("'temperature' must be a number")

            if not 0.0 <= temperature <= 1.0:
                raise AIToolExecutionError("'temperature' must be between 0.0 and 1.0")

        # Resolve model and provider
        reasoning = AIReasoningCapability.NO_REASONING
        reasoning_effort: str | None = None
        effective_model: str = ""
        effective_provider: str = ""

        if provider and not model:
            raise AIToolExecutionError("'provider' requires 'model' to also be specified")

        if model:
            ai_backends = self._ai_manager.get_backends()
            available_keys = list(AIConversationSettings.iter_models_by_backends(ai_backends))
            candidate_keys = [
                k for k in available_keys if AIConversationSettings.get_display_name(k[0], k[1]) == model
            ]
            if provider:
                candidate_keys = [k for k in candidate_keys if k[1] == provider]

            if not candidate_keys:
                available_display = [
                    AIConversationSettings.get_display_name(m, p) for (m, p) in available_keys
                ]
                raise AIToolExecutionError(
                    f"Model '{model}'"
                    + (f" with provider '{provider}'" if provider else "")
                    + f" is not available. Available models: {', '.join(available_display)}"
                )

            if len(candidate_keys) > 1:
                providers = [k[1] for k in candidate_keys]
                raise AIToolExecutionError(
                    f"Model '{model}' is available from multiple providers: {', '.join(providers)}. "
                    f"Specify the 'provider' parameter to disambiguate."
                )

            effective_model, effective_provider = candidate_keys[0]
            model_config = AIConversationSettings.MODELS.get(candidate_keys[0])
            if model_config:
                reasoning = model_config.reasoning_capabilities

        # Validate reasoning_effort
        if reasoning_effort_arg is not None:
            if not AIReasoningEffort.is_valid(reasoning_effort_arg):
                raise AIToolExecutionError(
                    f"'reasoning_effort' must be one of: {', '.join(AIReasoningEffort.values())}"
                )

            if not effective_model and hasattr(requester_ref, "conversation_settings"):
                ref_settings = requester_ref.conversation_settings()
                effective_model = ref_settings.model
                effective_provider = ref_settings.provider

            if effective_model and effective_provider:
                supported = AIConversationSettings.get_supported_reasoning_efforts(
                    effective_model, effective_provider
                )
                if supported and reasoning_effort_arg not in supported:
                    raise AIToolExecutionError(
                        f"Model '{effective_model}' does not support reasoning_effort '{reasoning_effort_arg}'. "
                        f"Supported efforts: {', '.join(supported)}"
                    )

            reasoning_effort = reasoning_effort_arg

        self._logger.debug("AI delegation requested with task: %s", task_prompt[:100])

        # Request user authorization
        session_info = "continue an existing session" if session_path else "start a new session"
        model_info = f" using model '{model}'" + (f" ({provider})" if provider else "") \
            if model else " using the default model"

        temp_info = f" with temperature {temperature}" if temperature is not None else ""
        effort_info = f" with reasoning effort '{reasoning_effort}'" if reasoning_effort else ""
        context = (
            f"The AI wants to delegate a task to a child AI instance.\n\n"
            f"Session: {session_info}{model_info}{temp_info}{effort_info}\n\n"
            f"Task prompt:\n{task_prompt[:500]}{'...' if len(task_prompt) > 500 else ''}"
        )

        authorized = await request_authorization("delegate_ai", arguments, context, None, False)
        if not authorized:
            raise AIToolAuthorizationDenied("User denied permission to delegate AI task")

        # Fall back to parent model/provider if not specified
        parent_ai_conversation: AIConversation = requester_ref
        if not effective_model:
            ref_settings = parent_ai_conversation.conversation_settings()
            effective_model = ref_settings.model
            effective_provider = ref_settings.provider

        # Fall back to default temperature if not specified
        if temperature is None:
            temperature = self._get_default_temperature()

        try:
            return await self._run_delegation(
                tool_call, parent_ai_conversation, task_prompt, session_path,
                effective_model, effective_provider,
                temperature, reasoning, reasoning_effort
            )

        except (AIToolExecutionError, AIToolAuthorizationDenied):
            raise

        except Exception as e:
            self._logger.error("Unexpected error in AI delegation: %s", str(e), exc_info=True)
            raise AIToolExecutionError(f"AI delegation failed: {str(e)}") from e

    async def _run_delegation(
        self,
        tool_call: AIToolCall,
        parent_ai_conversation: AIConversation,
        task_prompt: str,
        session_path: str | None,
        model: str,
        provider: str,
        temperature: float | None,
        reasoning_capability: AIReasoningCapability,
        reasoning_effort: str | None
    ) -> AIToolResult:
        """
        Create the child conversation, notify the listener, submit the prompt,
        and return a continuation that waits for completion.

        Args:
            tool_call: The originating tool call
            parent_ai_conversation: The parent AIConversation
            task_prompt: Prompt to send to the child
            session_path: Absolute path to an existing transcript to resume, or None for a fresh session
            model: Model name to use
            provider: Provider name to use
            temperature: Temperature setting (None to use model default)
            reasoning_capability: Reasoning capability for the model
            reasoning_effort: Reasoning effort level (None for model default)

        Returns:
            AIToolResult with a continuation task

        Raises:
            AIToolExecutionError: If conversation setup fails
        """
        child_settings = AIConversationSettings(
            model=model,
            provider=provider,
            temperature=temperature if AIConversationSettings.supports_temperature(
                model, provider, reasoning_effort
            ) else None,
            reasoning=reasoning_capability,
            reasoning_effort=reasoning_effort,
        )

        child_ai_conversation = AIConversation()
        child_ai_conversation.update_conversation_settings(child_settings)

        # Establish parent/child relationship in the transcript metadata
        self._set_child_parent_metadata(child_ai_conversation, parent_ai_conversation, tool_call)

        # Generate the transcript path and wrap in a persistent conversation.
        # For a resumed session we use the existing path; for a new session we generate one.
        if session_path:
            transcript_path = session_path

        else:
            transcript_path = self._new_conversation_path()

        ai_transcript_conversation = AITranscriptConversation(transcript_path, child_ai_conversation)

        relative_session_id = self._mindspace.get_mindspace_relative_path(transcript_path) or transcript_path

        # Notify the listener synchronously so it can attach display callbacks
        # before we submit the prompt (and events start firing).
        self._open_conversation_context(
            ai_transcript_conversation,
            relative_session_id,
            parent_ai_conversation
        )

        # Now submit the prompt
        parent_settings = parent_ai_conversation.conversation_settings()
        requester = AIConversationSettings.get_display_name(parent_settings.model, parent_settings.provider)
        await child_ai_conversation.submit_message(requester, task_prompt)

        session_info = "resuming session" if session_path else "new session"
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"AI delegated task ({session_info})\nsession ID: {relative_session_id}\n"
            f"prompt: '{task_prompt[:50]}...'"
        )

        continuation_task = asyncio.create_task(
            self._wait_for_completion(
                child_ai_conversation, ai_transcript_conversation,
                relative_session_id, tool_call
            )
        )

        return AIToolResult(
            id=tool_call.id,
            name="delegate_ai",
            content=f"Delegated task to AI ({session_info}), session_id: {relative_session_id}",
            continuation=continuation_task
        )

    async def _wait_for_completion(
        self,
        child_ai_conversation: AIConversation,
        ai_transcript_conversation: AITranscriptConversation,
        session_id: str,
        tool_call: AIToolCall
    ) -> AIToolResult:
        """
        Wait for the child conversation to finish and return the formatted result.

        Args:
            child_ai_conversation: The child AIConversation
            ai_transcript_conversation: The transcript wrapper (kept alive for persistence)
            session_id: Project-relative session path
            tool_call: The originating tool call

        Returns:
            AIToolResult containing the child's final response or error information
        """
        # Hold a reference so the transcript handler's persistence callbacks remain
        # registered on child_ai_conversation for the lifetime of this coroutine.
        _ = ai_transcript_conversation
        completion_future: asyncio.Future[None] = asyncio.Future()

        async def on_completed() -> None:
            if not completion_future.done():
                completion_future.set_result(None)

        async def on_error(retries_exhausted: bool, _message: Any) -> None:
            if retries_exhausted and not completion_future.done():
                completion_future.set_result(None)

        child_ai_conversation.register_callback(AIConversationEvent.COMPLETED, on_completed)
        child_ai_conversation.register_callback(AIConversationEvent.ERROR, on_error)

        await completion_future

        child_ai_conversation.unregister_callback(AIConversationEvent.COMPLETED, on_completed)
        child_ai_conversation.unregister_callback(AIConversationEvent.ERROR, on_error)

        # Notify the listener that this session is done
        self._close_conversation_context(session_id)

        # Build the result from the child's message history
        messages = child_ai_conversation.get_conversation_history().get_messages()
        last_message = messages[-1] if messages else None

        if not last_message or not last_message.completed:
            error_msg = "AI response was terminated early" if last_message else "No messages in conversation"
            self._logger.warning("Delegated AI task failed: %s", error_msg)
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"Delegated AI task failed\nsession ID: {session_id}\nerror: {error_msg}"
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
            self._mindspace.add_interaction(
                MindspaceLogLevel.INFO,
                f"Delegated AI task failed\nsession ID: {session_id}\nerror: {error_msg}"
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

        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Delegated AI task completed\nsession ID: {session_id}\n"
            f"response: {response_content[:50]}..."
        )

        return AIToolResult(
            id=tool_call.id,
            name="delegate_ai",
            content=json.dumps(result_object, indent=2),
            context="json"
        )

    def _set_child_parent_metadata(
        self,
        child_ai_conversation: AIConversation,
        parent_ai_conversation: AIConversation,
        tool_call: AIToolCall
    ) -> None:
        """
        Record the parent/child relationship on the child conversation history.

        Finds the parent ai_response message that contains the triggering tool
        call, then sets its ID and the tool call ID on the child history so the
        relationship is written to the transcript on the child's first message.

        Args:
            child_ai_conversation: The child AIConversation to update
            parent_ai_conversation: The parent AIConversation making the delegation
            tool_call: The tool call that triggered this delegation
        """
        parent_message_id = None
        for message in parent_ai_conversation.get_conversation_history().get_messages():
            if message.tool_calls:
                for tc in message.tool_calls:
                    if tc.id == tool_call.id:
                        parent_message_id = message.id
                        break

            if parent_message_id:
                break

        if parent_message_id is None:
            self._logger.warning(
                "Could not find parent message for tool_call_id %s", tool_call.id
            )
            return

        child_ai_conversation.get_conversation_history().set_parent(
            AIConversationParent(message_id=parent_message_id, tool_call_id=tool_call.id)
        )

    def _resolve_session_path(self, session_id: str) -> str:
        """
        Validate a mindspace-relative session_id and return its absolute path.

        Args:
            session_id: Mindspace-relative path to an existing transcript file.

        Returns:
            Absolute path to the transcript file.

        Raises:
            AIToolExecutionError: If the session_id is invalid, attempts path traversal,
                or does not refer to an existing file within the mindspace.
        """
        if not session_id:
            raise AIToolExecutionError("session_id must not be empty")

        if session_id.startswith("/") or session_id.startswith(os.sep):
            session_id = session_id[1:]

        normalized = os.path.normpath(session_id)
        if normalized.startswith(".."):
            raise AIToolExecutionError(f"session_id attempts path traversal: {session_id}")

        abs_path = self._mindspace.get_absolute_path(normalized)
        relative = self._mindspace.get_mindspace_relative_path(abs_path)
        if relative is None:
            raise AIToolExecutionError(f"session_id is outside mindspace boundaries: {session_id}")

        if not os.path.isfile(abs_path):
            raise AIToolExecutionError(f"session_id does not refer to an existing file: {session_id}")

        return abs_path

    def _new_conversation_path(self) -> str:
        """
        Generate a unique absolute path for a new child conversation transcript.

        Returns:
            Absolute path for the new transcript file.
        """
        timestamp = datetime.now(timezone.utc)
        title = "dAI-" + timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
        filename = os.path.join("conversations", f"{title}.conv")
        self._mindspace.ensure_mindspace_dir("conversations")
        return self._mindspace.get_absolute_path(filename)

    def _get_default_temperature(self) -> float | None:
        """
        Return the default temperature from the current mindspace settings.

        Returns:
            Temperature value, or None if no mindspace settings are available.
        """
        settings = self._mindspace.settings()
        return settings.temperature if settings is not None else None

    def _open_conversation_context(
        self,
        child_transcript: AITranscriptConversation,
        session_id: str,
        parent_conversation: AIConversation
    ) -> None:
        """
        Open a context for the child conversation and record its context_id.

        Args:
            child_transcript: The child transcript conversation.
            session_id: Mindspace-relative session identifier.
            parent_conversation: The parent AIConversation.
        """
        parent_context_id: str | None = None
        for info in self._mindspace.contexts().list_all():
            model = self._mindspace.contexts().get_model(info.context_id, ConversationContext)
            if model is not None and model.ai_transcript_conversation().inner_conversation() is parent_conversation:
                parent_context_id = info.context_id
                break

        try:
            title = os.path.splitext(os.path.basename(child_transcript.path()))[0]
            context_id = self._mindspace.contexts().open(
                context_type="conversation",
                path=child_transcript.path(),
                title=title,
                initial_model=child_transcript,
                requester_id=parent_context_id or "",
            )
            self._context_ids[session_id] = context_id

        except Exception:
            pass

    def _close_conversation_context(self, session_id: str) -> None:
        """
        Close the context opened for a completed child conversation.

        Args:
            session_id: Mindspace-relative session identifier.
        """
        context_id = self._context_ids.pop(session_id, None)
        if context_id:
            self._mindspace.contexts().close(context_id)
