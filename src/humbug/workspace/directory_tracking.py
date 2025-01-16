"""Directory tracking for workspaces."""

from dataclasses import dataclass
import os
from typing import Dict


@dataclass
class DirectoryTracking:
    """Container for tracked directory paths."""
    file_dialog: str  # Last directory used for file dialogs
    conversations: str  # Last directory used for conversations

    @classmethod
    def create_default(cls, workspace_path: str) -> 'DirectoryTracking':
        """Create default tracking with workspace root path."""
        return cls(
            file_dialog=workspace_path,
            conversations=os.path.join(workspace_path, "conversations")
        )

    def to_dict(self) -> Dict[str, str]:
        """Convert to serializable dictionary."""
        return {
            "fileDialog": self.file_dialog,
            "conversations": self.conversations
        }

    @classmethod
    def from_dict(cls, data: Dict[str, str], workspace_path: str) -> 'DirectoryTracking':
        """Create from dictionary, falling back to workspace path if stored paths invalid."""
        return cls(
            file_dialog=data.get("fileDialog", workspace_path),
            conversations=data.get("conversations", os.path.join(workspace_path, "conversations"))
        )
