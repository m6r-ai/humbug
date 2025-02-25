"""Conversation state management for the Humbug application."""

from dataclasses import dataclass
from datetime import datetime
from typing import Dict, Optional
import uuid

from humbug.conversation.message_source import MessageSource
from humbug.conversation.usage import Usage


@dataclass
class Message:
    """Represents a single message in the conversation."""
    id: str
    source: MessageSource
    content: str
    timestamp: datetime
    usage: Optional[Usage] = None
    error: Optional[Dict] = None
    model: Optional[str] = None
    temperature: Optional[float] = None
    completed: bool = True

    # Map between MessageSource enum and transcript type strings
    _SOURCE_TYPE_MAP = {
        MessageSource.USER: "user_message",
        MessageSource.AI: "ai_response",
        MessageSource.REASONING: "ai_reasoning",
        MessageSource.SYSTEM: "system_message"
    }
    _TYPE_SOURCE_MAP = {v: k for k, v in _SOURCE_TYPE_MAP.items()}

    @classmethod
    def create(
        cls,
        source: MessageSource,
        content: str,
        usage: Optional[Usage] = None,
        error: Optional[Dict] = None,
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        completed: bool = True,
        timestamp: Optional[datetime] = None
    ) -> 'Message':
        """Create a new message with generated ID and current timestamp."""
        if timestamp:
            msg_timestamp = datetime.fromisoformat(timestamp)
        else:
            msg_timestamp = datetime.utcnow()

        return cls(
            id=str(uuid.uuid4()),
            source=source,
            content=content,
            timestamp=msg_timestamp,
            usage=usage,
            error=error,
            model=model,
            temperature=temperature,
            completed=completed
        )

    def to_transcript_dict(self) -> Dict:
        """Convert message to transcript format."""
        message = {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "type": self._SOURCE_TYPE_MAP[self.source],
            "content": self.content,
            "completed": self.completed
        }

        # Always include these fields, even if None
        message["usage"] = self.usage.to_dict() if self.usage else None
        message["error"] = self.error
        message["model"] = self.model
        message["temperature"] = self.temperature

        return message

    @classmethod
    def from_transcript_dict(cls, data: Dict) -> 'Message':
        """Create a Message instance from transcript dictionary format.

        Args:
            data: Dictionary containing message data

        Returns:
            New Message instance

        Raises:
            ValueError: If required fields are missing or invalid
        """
        # Validate required fields
        required_fields = ["id", "timestamp", "type", "content"]
        missing_fields = [f for f in required_fields if f not in data]
        if missing_fields:
            raise ValueError(f"Missing required fields: {', '.join(missing_fields)}")

        # Convert message type to source
        msg_type = data["type"]
        if msg_type not in cls._TYPE_SOURCE_MAP:
            raise ValueError(f"Invalid message type: {msg_type}")
        source = cls._TYPE_SOURCE_MAP[msg_type]

        # Parse timestamp
        try:
            timestamp = datetime.fromisoformat(data["timestamp"])
        except ValueError as e:
            raise ValueError(f"Invalid timestamp format: {data['timestamp']}") from e

        # Parse usage data if present
        usage = None
        if data.get("usage"):
            try:
                usage_data = data["usage"]
                usage = Usage(
                    prompt_tokens=usage_data["prompt_tokens"],
                    completion_tokens=usage_data["completion_tokens"],
                    total_tokens=usage_data["total_tokens"]
                )
            except (KeyError, TypeError) as e:
                raise ValueError(f"Invalid usage data format: {data['usage']}") from e

        return cls(
            id=data["id"],
            source=source,
            content=data["content"],
            timestamp=timestamp,
            usage=usage,
            error=data.get("error"),
            model=data.get("model"),
            temperature=data.get("temperature"),
            completed=data.get("completed", True)
        )
