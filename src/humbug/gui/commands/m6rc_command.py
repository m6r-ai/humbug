from typing import Callable, List

from humbug.ai.ai_conversation_settings import AIConversationSettings
from humbug.gui.command_options import CommandOptionDescriptor, CommandOptionsRegistry, CommandOptionParser
from humbug.mindspace.system.system_command import SystemCommand
from humbug.user.user_manager import UserManager


class M6rcCommand(SystemCommand):
    """Command to create a new conversation with a Metaphor file."""

    def __init__(self, create_metaphor_conversation_callback: Callable[[str, str | None], None]) -> None:
        """
        Initialize the command.

        Args:
            create_metaphor_conversation_callback: Callback to create conversation with file path and optional model
        """
        self._create_metaphor_conversation = create_metaphor_conversation_callback
        self._user_manager = UserManager()

    @property
    def name(self) -> str:
        return "m6rc"

    @property
    def help_text(self) -> str:
        return "Create a new conversation from a Metaphor file"

    def setup_options(self) -> CommandOptionsRegistry:
        """Set up command options."""
        options = super().setup_options()

        # Add model option
        options.add_option(CommandOptionDescriptor(
            short_name="m",
            long_name="model",
            help_text="Specify the AI model to use",
            takes_value=True,
            value_description="MODEL"
        ))

        # Add model name completer
        options.add_value_completer("model", self._complete_model_names)

        return options

    def _complete_model_names(self, partial_value: str) -> List[str]:
        """
        Complete model names for -m/--model option.

        Args:
            partial_value: Partial model name

        Returns:
            List of matching model names
        """
        ai_backends = self._user_manager.get_ai_backends()
        print(f"AI backends: {ai_backends}")
        models = []
        for model in AIConversationSettings.iter_models_by_backends(ai_backends):
            if not partial_value or model.startswith(partial_value):
                models.append(model)

        print(f"Model completions: {models}")
        return models

    def _execute_command(self, parser: CommandOptionParser, args: str) -> bool:
        """
        Execute the command with parsed options.

        Args:
            parser: The option parser with parsed options
            args: Remaining arguments after option parsing

        Returns:
            True if command executed successfully, False otherwise
        """
        if not args.strip():
            return False

        # Get model if specified
        model = parser.get_option("model")

        self._create_metaphor_conversation(args.strip(), model)
        return True

    def get_completions(self, partial_args: str) -> List[str]:
        """
        Get completions for partial arguments.

        Args:
            partial_args: Partial command arguments

        Returns:
            List of possible completions
        """
        # First check for option completions using the base implementation
        print(f"Partial args: {partial_args}")
        option_completions = super().get_completions(partial_args)
        if option_completions:
            return option_completions

        # Could implement file path completion here for the Metaphor file
        return []
