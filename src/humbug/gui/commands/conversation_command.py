"""Command for creating a new conversation tab from the system terminal."""

import logging
from typing import List

from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.system.system_command import SystemCommand
from humbug.mindspace.system.system_message_source import SystemMessageSource


class ConversationCommand(SystemCommand):
    """Command to create a new conversation tab."""

    def __init__(self, create_conversation_callback) -> None:
        """
        Initialize conversation command.

        Args:
            create_conversation_callback: Callback to create a new conversation
        """
        self._create_conversation = create_conversation_callback
        self._mindspace_manager = MindspaceManager()
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

    def execute(self, args: str) -> bool:
        """
        Execute the command with the given arguments.

        Args:
            args: Command arguments as a string

        Returns:
            True if command executed successfully, False otherwise
        """
        if not self._mindspace_manager.has_mindspace():
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                "Cannot create conversation: no mindspace is open."
            )
            return False

        try:
            # Create new conversation and report success
            conversation_id = self._create_conversation()
            if conversation_id:
                self._mindspace_manager.add_system_interaction(
                    SystemMessageSource.SUCCESS,
                    f"Created new conversation: {conversation_id}"
                )
                return True

            return False

        except Exception as e:
            self._logger.exception("Failed to create conversation: %s", str(e))
            self._mindspace_manager.add_system_interaction(
                SystemMessageSource.ERROR,
                f"Failed to create conversation: {str(e)}"
            )
            return False
