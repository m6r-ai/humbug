"""Directory tracking for mindspaces."""

from dataclasses import dataclass
import os


@dataclass
class MindspaceDirectoryTracking:
    """Container for tracked directory paths."""
    file_dialog: str
    conversations: str

    @classmethod
    def create_default(cls, mindspace_path: str) -> 'MindspaceDirectoryTracking':
        """Create default tracking with mindspace root path."""
        return cls(
            file_dialog=mindspace_path,
            conversations=os.path.join(mindspace_path, "conversations")
        )

    def to_dict(self) -> dict[str, str]:
        """Convert to serializable dictionary."""
        return {
            "fileDialog": self.file_dialog,
            "conversations": self.conversations
        }

    @classmethod
    def from_dict(cls, data: dict[str, str], mindspace_path: str) -> 'MindspaceDirectoryTracking':
        """Create from dictionary, falling back to mindspace path if stored paths invalid."""
        return cls(
            file_dialog=data.get("fileDialog", mindspace_path),
            conversations=data.get("conversations", os.path.join(mindspace_path, "conversations"))
        )
