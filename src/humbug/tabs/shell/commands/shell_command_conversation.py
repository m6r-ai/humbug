"""Command for creating a new conversation tab from the system shell."""

import logging
from typing import List, Dict, cast

from ai import AIConversationSettings, AIReasoningCapability
from ai.ai_model import AIReasoningEffort
from syntax import Token, TokenType

from humbug.mindspace.mindspace_error import MindspaceError
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.column_manager_error import ColumnManagerError
from humbug.tabs.shell.shell_command import ShellCommand
from humbug.tabs.shell.shell_event_source import ShellEventSource
from humbug.tabs.tab_base import TabBase
from humbug.user.user_manager import UserManager


class ShellCommandConversation(ShellCommand):
    """Command to create a new conversation tab."""

    def __init__(self, column_manager: ColumnManager) -> None:
        """
        Initialize conversation command.

        Args:
            create_conversation_callback: Callback to create a new conversation with optional model
        """
        super().__init__()
        self._column_manager = column_manager
        self._user_manager = UserManager()
        self._logger = logging.getLogger("ShellCommandConversation")

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
        # Get base class handling for common options
        result = super().get_option_value_count(option_name)
        if result != 0:
            return result

        # Handle command-specific options
        if option_name in ("-m", "--model"):
            return 1  # Takes exactly one value

        if option_name in ("-t", "--temperature"):
            return 1  # Takes exactly one value

        if option_name in ("-r", "--reasoning-effort"):
            return 1  # Takes exactly one value

        # Default for unknown options
        return 0

    def _execute_command(self, tokens: List[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        # Get options
        options = self._get_options(tokens)

        # Get model if specified
        model = None
        model_values = options.get("--model", []) or options.get("-m", [])
        if model_values:
            model = model_values[0]

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

        # Get reasoning effort if specified
        effort_values = options.get("--reasoning-effort", []) or options.get("-r", [])
        if effort_values:
            effort_str = effort_values[0]
            if not AIReasoningEffort.is_valid(effort_str):
                self._history_manager.add_message(
                    ShellEventSource.ERROR,
                    f"Reasoning effort must be one of: {', '.join(AIReasoningEffort.values())}"
                )
                return False

            if model:
                supported = AIConversationSettings.get_supported_reasoning_efforts(model)
                if supported and effort_str not in supported:
                    self._history_manager.add_message(
                        ShellEventSource.ERROR,
                        f"Model '{model}' does not support reasoning effort '{effort_str}'. "
                        f"Supported: {', '.join(supported)}"
                    )
                    return False

            reasoning_effort = effort_str

        if model:
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        # Create new conversation with model if specified
        current_tab = cast(TabBase, self._column_manager.get_current_tab())
        self._column_manager.protect_tab(current_tab.tab_id())

        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")
            conversation_tab = self._column_manager.new_conversation(
                False, None, model, temperature_val, reasoning, reasoning_effort
            )

        except (MindspaceError, ColumnManagerError) as e:
            self._history_manager.add_message(ShellEventSource.ERROR, f"Failed to create conversation: {str(e)}")
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.ERROR,
                f"Shell failed to create conversation: {str(e)}"
            )
            return False

        finally:
            self._column_manager.unprotect_tab(current_tab.tab_id())

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            "Started new conversation"
        )
        self._mindspace_manager.add_interaction(
            MindspaceLogLevel.INFO,
            f"Shell created new conversion\ntab ID: {conversation_tab.tab_id()}"
        )
        return True

    def _complete_model_names(self, partial_value: str) -> List[str]:
        """
        Complete model names for -m/--model option.

        Args:
            partial_value: Partial model name

        Returns:
            List of matching model names
        """
        ai_backends = self._user_manager.get_ai_backends()
        models = []
        for model in AIConversationSettings.iter_models_by_backends(ai_backends):
            if not partial_value or model.startswith(partial_value):
                models.append(model)

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
        # Handle option completions
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        # Check if we're completing a model name for -m/--model option
        if current_token.type in (TokenType.ARGUMENT, TokenType.OPTION_VALUE) and cursor_token_index > 0:
            prev_token = tokens[cursor_token_index - 1]
            if prev_token.type == TokenType.OPTION:
                option_name = prev_token.value

                # Check if this is the -m/--model option
                if option_name in ["-m", "--model"]:
                    return self._complete_model_names(current_token.value)

                # Check if we're completing a temperature value for -t/--temperature option
                if option_name in ["-t", "--temperature"]:
                    # Temperature should be a float between 0.0 and 1.0
                    return [str(i / 10) for i in range(11) if str(i / 10).startswith(current_token.value)]

                # Check if we're completing a reasoning effort for -r/--reasoning-effort option
                if option_name in ["-r", "--reasoning-effort"]:
                    return self._complete_reasoning_efforts(current_token.value)

        # No completions for other arguments
        return []
