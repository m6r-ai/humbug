from dataclasses import dataclass
from datetime import datetime
import uuid
from typing import Dict

from humbug.mindspace.system.system_message_source import SystemMessageSource


@dataclass
class SystemMessage:
    """Represents a single system message in the interaction history."""
    id: str
    source: SystemMessageSource
    content: str
    timestamp: datetime

    @classmethod
    def create(
        cls,
        source: SystemMessageSource,
        content: str,
        timestamp: datetime | None = None
    ) -> 'SystemMessage':
        """Create a new message with generated ID and current timestamp."""
        if timestamp is None:
            timestamp = datetime.utcnow()

        return cls(
            id=str(uuid.uuid4()),
            source=source,
            content=content,
            timestamp=timestamp
        )

    def to_dict(self) -> Dict:
        """Convert message to dictionary for storage."""
        return {
            "id": self.id,
            "source": self.source.value,
            "content": self.content,
            "timestamp": self.timestamp.isoformat()
        }

    @classmethod
    def from_dict(cls, data: Dict) -> 'SystemMessage':
        """Create a SystemMessage instance from dictionary."""
        return cls(
            id=data["id"],
            source=SystemMessageSource(data["source"]),
            content=data["content"],
            timestamp=datetime.fromisoformat(data["timestamp"])
        )
