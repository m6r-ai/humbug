"""Shell message data structure for shell command history."""

from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Dict
import uuid

from humbug.tabs.shell.shell_event_source import ShellEventSource


@dataclass
class ShellEvent:
    """Represents a single shell message in the command history."""
    message_id: str
    source: ShellEventSource
    content: str
    timestamp: datetime

    @classmethod
    def create(
        cls,
        source: ShellEventSource,
        content: str,
        timestamp: datetime | None = None
    ) -> 'ShellEvent':
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
    def from_dict(cls, data: Dict) -> 'ShellEvent':
        """Create a ShellEvent instance from dictionary."""
        return cls(
            message_id=data["message_id"],
            source=ShellEventSource(data["source"]),
            content=data["content"],
            timestamp=datetime.fromisoformat(data["timestamp"])
        )
