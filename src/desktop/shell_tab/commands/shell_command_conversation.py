"""Command for creating a new conversation tab from the system shell."""

from datetime import datetime, timezone
import os
from typing import List, Dict

from ai import AIConversation, AIConversationSettings, AIReasoningCapability, AIManager
from ai.ai_model import AIReasoningEffort
from ai_transcript_conversation import AITranscriptConversation
from mindspace.mindspace_error import MindspaceError
from mindspace.mindspace_log_level import MindspaceLogLevel
from syntax import Token, TokenType

from desktop.shell_tab.shell_command import ShellCommand
from desktop.shell_tab.shell_event_source import ShellEventSource


class ShellCommandConversation(ShellCommand):
    """Command to create a new conversation tab."""

    def __init__(self) -> None:
        """Initialize conversation command."""
        super().__init__()
        self._ai_manager = AIManager()

    def name(self) -> str:
        """Get the name of the command."""
        return "conversation"

    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return ["conv", "chat"]

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Starts a new conversation"

    def get_options_help(self) -> Dict[str, str]:
        """Get help text for supported options."""
        options = super().get_options_help()
        options["-m, --model"] = "AI model to use"
        options["-t, --temperature"] = "Temperature for the model (0.0 to 1.0)"
        options["-r, --reasoning-effort"] = f"Reasoning effort level ({', '.join(AIReasoningEffort.values())})"
        return options

    def get_option_value_count(self, option_name: str) -> int:
        """Determine how many values each option takes."""
        result = super().get_option_value_count(option_name)
        if result != 0:
            return result

        if option_name in ("-m", "--model"):
            return 1

        if option_name in ("-t", "--temperature"):
            return 1

        if option_name in ("-r", "--reasoning-effort"):
            return 1

        return 0

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        options = self._get_options(tokens)

        # Resolve model display name to (model, provider)
        effective_model: str | None = None
        effective_provider: str | None = None
        model_values = options.get("--model", []) or options.get("-m", [])
        if model_values:
            model_input = model_values[0]
            ai_backends = self._ai_manager.get_backends()
            available_keys = list(AIConversationSettings.iter_models_by_backends(ai_backends))
            matched_key = next(
                (k for k in available_keys if AIConversationSettings.get_display_name(k[0], k[1]) == model_input),
                None
            )
            if matched_key is None:
                available_display = [AIConversationSettings.get_display_name(m, p) for (m, p) in available_keys]
                self._history_manager.add_message(
                    ShellEventSource.ERROR,
                    f"Model '{model_input}' not found. Available models: {', '.join(available_display)}"
                )
                return False

            effective_model, effective_provider = matched_key

        # Get temperature if specified
        temperature_val = None
        temp_values = options.get("--temperature", []) or options.get("-t", [])
        if temp_values:
            try:
                temperature_val = float(temp_values[0])
                if temperature_val < 0.0 or temperature_val > 1.0:
                    self._history_manager.add_message(
                        ShellEventSource.ERROR,
                        "Temperature must be between 0.0 and 1.0"
                    )
                    return False

            except ValueError:
                self._history_manager.add_message(
                    ShellEventSource.ERROR,
                    "Temperature must be a valid number"
                )
                return False

        reasoning: AIReasoningCapability | None = None
        reasoning_effort: str | None = None

        effort_values = options.get("--reasoning-effort", []) or options.get("-r", [])
        if effort_values:
            effort_str = effort_values[0]
            if not AIReasoningEffort.is_valid(effort_str):
                self._history_manager.add_message(
                    ShellEventSource.ERROR,
                    f"Reasoning effort must be one of: {', '.join(AIReasoningEffort.values())}"
                )
                return False

            if effective_model and effective_provider:
                supported = AIConversationSettings.get_supported_reasoning_efforts(effective_model, effective_provider)
                if supported and effort_str not in supported:
                    self._history_manager.add_message(
                        ShellEventSource.ERROR,
                        f"Model '{effective_model}' does not support reasoning effort '{effort_str}'. "
                        f"Supported: {', '.join(supported)}"
                    )
                    return False

            reasoning_effort = effort_str

        if effective_model and effective_provider:
            model_config = AIConversationSettings.MODELS.get((effective_model, effective_provider))
            if model_config:
                reasoning = model_config.reasoning_capabilities

        requester_id = self._requester_id
        try:
            self._mindspace.ensure_mindspace_dir("conversations")
            timestamp = datetime.now(timezone.utc)
            title = timestamp.strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
            filename = os.path.join("conversations", f"{title}.conv")
            full_path = self._mindspace.get_absolute_path(filename)

            initial_model = None
            if effective_model and effective_provider:
                settings = self._mindspace.settings()
                conversation_settings = AIConversationSettings(
                    model=effective_model,
                    provider=effective_provider,
                    temperature=temperature_val if AIConversationSettings.supports_temperature(
                        effective_model, effective_provider, reasoning_effort
                    ) else None,
                    reasoning=reasoning or (settings.reasoning if settings else AIReasoningCapability.NO_REASONING),
                    reasoning_effort=reasoning_effort,
                )
                ai_conversation = AIConversation()
                ai_conversation.update_conversation_settings(conversation_settings)
                initial_model = AITranscriptConversation(full_path, ai_conversation)

            context_id = self._mindspace.contexts().open(
                context_type="conversation",
                path=full_path,
                title=title,
                initial_model=initial_model,
                requester_id=requester_id,
            )

        except MindspaceError as e:
            self._history_manager.add_message(ShellEventSource.ERROR, f"Failed to create conversation: {str(e)}")
            self._mindspace.add_interaction(
                MindspaceLogLevel.ERROR,
                f"Shell failed to create conversation: {str(e)}"
            )
            return False

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            "Started new conversation"
        )
        self._mindspace.add_interaction(
            MindspaceLogLevel.INFO,
            f"Shell created new conversation\ntab ID: {context_id}"
        )
        return True

    def _complete_model_names(self, partial_value: str) -> List[str]:
        """
        Complete model display names for -m/--model option.

        Args:
            partial_value: Partial model display name

        Returns:
            List of matching model display names
        """
        ai_backends = self._ai_manager.get_backends()
        models = []
        for (model_name, provider) in AIConversationSettings.iter_models_by_backends(ai_backends):
            display = AIConversationSettings.get_display_name(model_name, provider)
            if not partial_value or display.startswith(partial_value):
                models.append(display)

        return models

    def _complete_reasoning_efforts(self, partial_value: str) -> List[str]:
        """
        Complete reasoning effort values for -r/--reasoning-effort option.

        Args:
            partial_value: Partial effort string

        Returns:
            List of matching effort level strings
        """
        return [e for e in AIReasoningEffort.values() if not partial_value or e.startswith(partial_value)]

    def get_token_completions(
        self,
        current_token: Token,
        tokens: List[Token],
        cursor_token_index: int
    ) -> List[str]:
        """
        Get completions for the current token based on token information.

        Args:
            current_token: The token at cursor position
            tokens: All tokens in the command line
            cursor_token_index: Index of current_token in tokens list

        Returns:
            List of possible completions
        """
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        if current_token.type in (TokenType.ARGUMENT, TokenType.OPTION_VALUE) and cursor_token_index > 0:
            prev_token = tokens[cursor_token_index - 1]
            if prev_token.type == TokenType.OPTION:
                option_name = prev_token.value

                if option_name in ["-m", "--model"]:
                    return self._complete_model_names(current_token.value)

                if option_name in ["-t", "--temperature"]:
                    return [str(i / 10) for i in range(11) if str(i / 10).startswith(current_token.value)]

                if option_name in ["-r", "--reasoning-effort"]:
                    return self._complete_reasoning_efforts(current_token.value)

        return []
