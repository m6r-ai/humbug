from abc import ABC, abstractmethod
from typing import List

class SystemCommand(ABC):
    """Base class for system commands."""

    @property
    @abstractmethod
    def name(self) -> str:
        """Get the name of the command."""

    @property
    def aliases(self) -> List[str]:
        """Get alternate names for the command."""
        return []

    @property
    def help_text(self) -> str:
        """Get the help text for the command."""
        return ""

    @abstractmethod
    def execute(self, args: str) -> bool:
        """
        Execute the command with the given arguments.
        
        Args:
            args: Command arguments as a string
            
        Returns:
            True if command executed successfully, False otherwise
        """

    def get_completions(self, _partial_args: str) -> List[str]:
        """
        Get completions for partial arguments.
        
        Args:
            partial_args: Partial command arguments
            
        Returns:
            List of possible completions
        """
        return []
