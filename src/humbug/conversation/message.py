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

    @classmethod
    def create(
        cls,
        source: MessageSource, content: str,
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
            "type": self._get_transcript_type(),
            "content": self.content,
            "completed": self.completed
        }

        if self.usage:
            message["usage"] = self.usage.to_dict()
        if self.error:
            message["error"] = self.error

        # Add AI-specific fields only for AI responses
        if self.source == MessageSource.AI:
            if self.model is not None:
                message["model"] = self.model
            if self.temperature is not None:
                message["temperature"] = self.temperature

        return message

    def _get_transcript_type(self) -> str:
        """Map message source to transcript type."""
        return {
            MessageSource.USER: "user_message",
            MessageSource.AI: "ai_response",
            MessageSource.SYSTEM: "system_message"
        }[self.source]
