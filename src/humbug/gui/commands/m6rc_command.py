"""Command to create a new conversation with a Metaphor file."""

import os
from typing import Callable, Dict, List

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command import SystemCommand
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.syntax.command.command_lexer import Token, TokenType
from humbug.user.user_manager import UserManager


class M6rcCommand(SystemCommand):
    """Command to create a new conversation with a Metaphor file."""

    def __init__(self, create_m6rc_conversation_callback: Callable[[str, str | None], bool]) -> None:
        """
        Initialize the command.

        Args:
            create_metaphor_conversation_callback: Callback to create conversation with file path and optional model
        """
        super().__init__()
        self._create_m6rc_conversation = create_m6rc_conversation_callback
        self._user_manager = UserManager()
        self._mindspace_manager = MindspaceManager()

    @property
    def name(self) -> str:
        return "m6rc"

    @property
    def help_text(self) -> str:
        return "Create a new conversation from a Metaphor file"

    def get_options_help(self) -> Dict[str, str]:
        """Get help text for supported options."""
        options = super().get_options_help()
        options["-m, --model"] = "Specify the AI model to use"
        return options

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
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                "No file path provided. Usage: m6rc [options] <path/to/file.m6r>"
            )
            return False

        # Get file path from first positional argument
        file_path = args[0]

        # Get options
        options = self._get_options(tokens)

        # Get model if specified
        model = options.get("model")

        try:
            # Check if the path exists
            # Convert to absolute path if it's relative
            if not os.path.isabs(file_path):
                file_path = self._mindspace_manager.get_mindspace_path(file_path)

            if not os.path.exists(file_path):
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.ERROR,
                    f"File not found: {file_path}"
                )
                return False

            if not self._create_m6rc_conversation(file_path, model):
                return False

            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.SUCCESS,
                f"New Metaphor conversation started from {file_path}"
            )
            return True

        except Exception as e:
            self._logger.error("Failed to create Metaphor conversation: %s", str(e), exc_info=True)
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
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
        print("Current token:", current_token)
        # Handle option completions
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        # Check if we're completing a model name for -m/--model option
        if current_token.type == TokenType.ARGUMENT and cursor_token_index > 0:
            prev_token = tokens[cursor_token_index - 1]
            if prev_token.type == TokenType.OPTION:
                # Check if this is the -m/--model option
                option_name = prev_token.value
                if option_name in ["-m", "--model"]:
                    return self._complete_model_names(current_token.value)

        # For regular arguments, complete file paths with .m6r extension
        return self._get_mindspace_path_completions(current_token.value, file_extension=".m6r")
