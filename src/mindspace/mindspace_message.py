from dataclasses import dataclass
from datetime import datetime, timezone

from mindspace.mindspace_log_level import MindspaceLogLevel


@dataclass
class MindspaceMessage:
    """Represents a single message in the mindspace interaction log."""
    sequence: int
    level: MindspaceLogLevel
    content: str
    timestamp: datetime

    @classmethod
    def create(
        cls,
        sequence: int,
        level: MindspaceLogLevel,
        content: str,
        timestamp: datetime | None = None
    ) -> 'MindspaceMessage':
        """Create a message with the given sequence number and current timestamp."""
        if timestamp is None:
            timestamp = datetime.now(timezone.utc)

        return cls(
            sequence=sequence,
            level=level,
            content=content,
            timestamp=timestamp
        )

    def to_dict(self) -> dict:
        """Convert message to dictionary for storage."""
        return {
            "sequence": self.sequence,
            "level": self.level.value,
            "content": self.content,
            "timestamp": self.timestamp.isoformat()
        }

    @classmethod
    def from_dict(cls, data: dict) -> 'MindspaceMessage':
        """Create a MindspaceMessage instance from dictionary."""
        return cls(
            sequence=int(data["sequence"]),
            level=MindspaceLogLevel(data["level"]),
            content=data["content"],
            timestamp=datetime.fromisoformat(data["timestamp"])
        )
