from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Dict
import uuid

from humbug.mindspace.mindspace_log_level import MindspaceLogLevel


@dataclass
class MindspaceMessage:
    """Represents a single shell message in the interaction history."""
    message_id: str
    level: MindspaceLogLevel
    content: str
    timestamp: datetime

    @classmethod
    def create(
        cls,
        level: MindspaceLogLevel,
        content: str,
        timestamp: datetime | None = None
    ) -> 'MindspaceMessage':
        """Create a new message with generated ID and current timestamp."""
        if timestamp is None:
            timestamp = datetime.now(timezone.utc)

        return cls(
            message_id=str(uuid.uuid4()),
            level=level,
            content=content,
            timestamp=timestamp
        )

    def to_dict(self) -> Dict:
        """Convert message to dictionary for storage."""
        return {
            "message_id": self.message_id,
            "level": self.level.value,
            "content": self.content,
            "timestamp": self.timestamp.isoformat()
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'MindspaceMessage':
        """Create a MindspaceMessage instance from dictionary."""
        return cls(
            message_id=data["message_id"],
            level=MindspaceLogLevel(data["level"]),
            content=data["content"],
            timestamp=datetime.fromisoformat(data["timestamp"])
        )
