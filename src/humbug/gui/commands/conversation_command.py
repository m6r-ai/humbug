"""Command for creating a new conversation tab from the system terminal."""

import logging
from typing import List, Callable, Dict

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command import SystemCommand
from humbug.mindspace.system.system_message_source import SystemMessageSource
from humbug.syntax.command.command_lexer import Token, TokenType
from humbug.user.user_manager import UserManager


class ConversationCommand(SystemCommand):
    """Command to create a new conversation tab."""

    def __init__(self, create_conversation_callback: Callable[[str | None], str | None]) -> None:
        """
        Initialize conversation command.

        Args:
            create_conversation_callback: Callback to create a new conversation with optional model
        """
        super().__init__()
        self._create_conversation = create_conversation_callback
        self._mindspace_manager = MindspaceManager()
        self._user_manager = UserManager()
        self._logger = logging.getLogger("ConversationCommand")

    @property
    def name(self) -> str:
        """Get the name of the command."""
        return "conversation"

    @property
    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return ["conv", "chat"]

    @property
    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Creates a new conversation tab."

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
        # Get options
        options = self._get_options(tokens)

        # Get model if specified
        model = options.get("model")

        try:
            # Create new conversation with model if specified
            conversation_id = self._create_conversation(model)
            if conversation_id:
                # Success message would include model info if specified
                msg = f"Created new conversation: {conversation_id}"
                if model:
                    msg += f" with model {model}"

                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.SUCCESS,
                    msg
                )
                return True

            # Creation failed for some reason
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                "Failed to create new conversation"
            )
            return False

        except Exception as e:
            self._logger.exception("Failed to create conversation: %s", str(e))
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Failed to create conversation: {str(e)}"
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
        if current_token.type == TokenType.ARGUMENT and cursor_token_index > 0:
            prev_token = tokens[cursor_token_index - 1]
            if prev_token.type == TokenType.OPTION:
                # Check if this is the -m/--model option
                option_name = prev_token.value
                if option_name in ["-m", "--model"]:
                    return self._complete_model_names(current_token.value)

        # No completions for other arguments
        return []
