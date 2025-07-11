"""Command to create a new conversation with a Metaphor file."""

import logging
import os
from typing import Callable, Dict, List

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.ai.ai_model import ReasoningCapability
from humbug.gui.tab.shell.shell_command import ShellCommand
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_message_source import MindspaceMessageSource
from humbug.syntax.lexer import Token, TokenType
from humbug.user.user_manager import UserManager


class ShellCommandM6rc(ShellCommand):
    """Command to create a new conversation with a Metaphor file."""

    def __init__(
        self,
        create_m6rc_conversation_callback: Callable[[str, List[str], str | None, float | None, ReasoningCapability | None, bool], bool]
    ) -> None:
        """
        Initialize the command.

        Args:
            create_metaphor_conversation_callback: Callback to create conversation with file path and optional model
        """
        super().__init__()
        self._logger = logging.getLogger("ShellCommandM6rc")
        self._create_m6rc_conversation = create_m6rc_conversation_callback
        self._user_manager = UserManager()
        self._mindspace_manager = MindspaceManager()

    def name(self) -> str:
        return "m6rc"

    def help_text(self) -> str:
        return "Starts a new Metaphor conversation"

    def get_options_help(self) -> Dict[str, str]:
        """Get help text for supported options."""
        options = super().get_options_help()
        options["-m, --model"] = "AI model to use"
        options["-t, --temperature"] = "Temperature for the model (0.0 to 1.0)"
        options["-j, --submit"] = "Automatically submit the metaphor prompt"
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

        if option_name in ("-j", "--submit"):
            return 0  # Flag with no values

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
        # Get positional arguments
        args = self._get_positional_arguments(tokens)
        if not args:
            self._mindspace_manager.add_interaction(
                MindspaceMessageSource.ERROR,
                "No file path provided"
            )
            return False

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
                    self._mindspace_manager.add_interaction(
                        MindspaceMessageSource.ERROR,
                        "Temperature must be between 0.0 and 1.0"
                    )
                    return False

            except ValueError:
                self._mindspace_manager.add_interaction(
                    MindspaceMessageSource.ERROR,
                    "Temperature must be a valid number"
                )
                return False

        # Check if should auto-submit
        should_submit = "--submit" in options or "-j" in options

        reasoning: ReasoningCapability | None = None
        if model:
            model_config = AIConversationSettings.MODELS.get(model)
            if model_config:
                reasoning = model_config.reasoning_capabilities

        try:
            # Check if the path exists.  Convert to absolute path if it's relative
            file_path = self._mindspace_manager.get_absolute_path(args[0])
            if not os.path.exists(file_path):
                self._mindspace_manager.add_interaction(
                    MindspaceMessageSource.ERROR,
                    f"File not found: {file_path}"
                )
                return False

            if not self._create_m6rc_conversation(file_path, args, model, temperature_val, reasoning, should_submit):
                return False

            self._mindspace_manager.add_interaction(
                MindspaceMessageSource.SUCCESS,
                f"Started Metaphor conversation from {file_path}"
            )
            return True

        except Exception as e:
            self._logger.error("Failed to create Metaphor conversation: %s", str(e), exc_info=True)
            self._mindspace_manager.add_interaction(
                MindspaceMessageSource.ERROR,
                f"Failed to create Metaphor conversation: {str(e)}"
            )
            return False

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

        # For regular arguments, complete file paths
        return self._get_mindspace_path_completions(current_token.value)
