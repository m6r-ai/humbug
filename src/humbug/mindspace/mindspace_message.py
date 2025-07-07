from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Dict
import uuid

from humbug.mindspace.mindspace_message_source import MindspaceMessageSource


@dataclass
class MindspaceMessage:
    """Represents a single shell message in the interaction history."""
    message_id: str
    source: MindspaceMessageSource
    content: str
    timestamp: datetime

    @classmethod
    def create(
        cls,
        source: MindspaceMessageSource,
        content: str,
        timestamp: datetime | None = None
    ) -> 'MindspaceMessage':
        """Create a new message with generated ID and current timestamp."""
        if timestamp is None:
            timestamp = datetime.now(timezone.utc)

        return cls(
            message_id=str(uuid.uuid4()),
            source=source,
            content=content,
            timestamp=timestamp
        )

    def to_dict(self) -> Dict:
        """Convert message to dictionary for storage."""
        return {
            "message_id": self.message_id,
            "source": self.source.value,
            "content": self.content,
            "timestamp": self.timestamp.isoformat()
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'MindspaceMessage':
        """Create a MindspaceMessage instance from dictionary."""
        return cls(
            message_id=data["message_id"],
            source=MindspaceMessageSource(data["source"]),
            content=data["content"],
            timestamp=datetime.fromisoformat(data["timestamp"])
        )
