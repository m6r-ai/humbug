"""Command for creating a new conversation tab from the system shell."""

import logging
from typing import List, Dict

from ai import AIConversationSettings, AIReasoningCapability
from syntax import Token, TokenType

from humbug.tabs.column_manager import ColumnManager
from humbug.tabs.column_manager_error import ColumnManagerError
from humbug.mindspace.mindspace_error import MindspaceError
from humbug.mindspace.mindspace_log_level import MindspaceLogLevel
from humbug.tabs.shell.shell_command import ShellCommand
from humbug.tabs.shell.shell_message_source import ShellMessageSource
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
                        ShellMessageSource.ERROR,
                        "Temperature must be between 0.0 and 1.0"
                    )
                    return False

            except ValueError:
                self._history_manager.add_message(
                    ShellMessageSource.ERROR,
                    "Temperature must be a valid number"
                )
                return False

        reasoning: AIReasoningCapability | None = None
        if model:
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        # Create new conversation with model if specified
        self._column_manager.protect_current_tab(True)
        try:
            self._mindspace_manager.ensure_mindspace_dir("conversations")
            conversation_tab = self._column_manager.new_conversation(False, None, model, temperature_val, reasoning)

        except (MindspaceError, ColumnManagerError) as e:
            self._history_manager.add_message(ShellMessageSource.ERROR, f"Failed to create conversation: {str(e)}")
            self._mindspace_manager.add_interaction(
                MindspaceLogLevel.ERROR,
                f"Shell failed to create conversation: {str(e)}"
            )
            return False

        finally:
            self._column_manager.protect_current_tab(False)

        self._history_manager.add_message(
            ShellMessageSource.SUCCESS,
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

        # No completions for other arguments
        return []
