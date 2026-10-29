from dataclasses import dataclass, field
from datetime import datetime, timezone

from mindspace.mindspace_log_level import MindspaceLogLevel


@dataclass
class MindspaceMessage:
    """Represents a single message in the mindspace interaction log."""

    CONVERSATION_CONTEXT = "conversation"

    sequence: int
    level: MindspaceLogLevel
    content: str
    timestamp: datetime
    context: dict[str, str] = field(default_factory=dict)

    @classmethod
    def create(
        cls,
        sequence: int,
        level: MindspaceLogLevel,
        content: str,
        timestamp: datetime | None = None,
        context: dict[str, str] | None = None
    ) -> 'MindspaceMessage':
        """Create a message with the given sequence number and current timestamp."""
        if timestamp is None:
            timestamp = datetime.now(timezone.utc)

        return cls(
            sequence=sequence,
            level=level,
            content=content,
            timestamp=timestamp,
            context=dict(context) if context else {}
        )

    def to_dict(self) -> dict:
        """Convert message to dictionary for storage."""
        record: dict[str, object] = {
            "sequence": self.sequence,
            "level": self.level.value,
            "content": self.content,
            "timestamp": self.timestamp.isoformat()
        }

        if self.context:
            record["context"] = dict(self.context)

        return record

    @classmethod
    def from_dict(cls, data: dict) -> 'MindspaceMessage':
        """Create a MindspaceMessage instance from dictionary."""
        context = data.get("context")

        return cls(
            sequence=int(data["sequence"]),
            level=MindspaceLogLevel(data["level"]),
            content=data["content"],
            timestamp=datetime.fromisoformat(data["timestamp"]),
            context=cls._parse_context(context)
        )

    @staticmethod
    def _parse_context(context: object) -> dict[str, str]:
        """Return the context held in a stored record, ignoring anything malformed."""
        if not isinstance(context, dict):
            return {}

        return {str(key): str(value) for key, value in context.items()}
