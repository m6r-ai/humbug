"""Conversation state management for the Humbug application."""

from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional
import uuid


class MessageSource(Enum):
    """Enumeration of possible message sources."""
    USER = "user"
    AI = "ai"
    SYSTEM = "system"


@dataclass
class Usage:
    """Token usage information."""
    prompt_tokens: int
    completion_tokens: int
    total_tokens: int

    def to_dict(self) -> Dict:
        """Convert usage to dictionary for transcript."""
        return {
            "prompt_tokens": self.prompt_tokens,
            "completion_tokens": self.completion_tokens,
            "total_tokens": self.total_tokens
        }


@dataclass
class Message:
    """Represents a single message in the conversation."""
    id: str
    source: MessageSource
    content: str
    timestamp: datetime
    usage: Optional[Usage] = None
    error: Optional[Dict] = None

    @classmethod
    def create(cls, source: MessageSource, content: str, 
               usage: Optional[Usage] = None, error: Optional[Dict] = None) -> 'Message':
        """Create a new message with generated ID and current timestamp."""
        return cls(
            id=str(uuid.uuid4()),
            source=source,
            content=content,
            timestamp=datetime.utcnow(),
            usage=usage,
            error=error
        )

    def to_transcript_dict(self) -> Dict:
        """Convert message to transcript format."""
        message = {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "type": self._get_transcript_type(),
            "content": self.content
        }
        if self.usage:
            message["usage"] = self.usage.to_dict()
        if self.error:
            message["error"] = self.error
        return message

    def _get_transcript_type(self) -> str:
        """Map message source to transcript type."""
        return {
            MessageSource.USER: "user_message",
            MessageSource.AI: "ai_response",
            MessageSource.SYSTEM: "system_message"
        }[self.source]


class ConversationHistory:
    """Manages the conversation history and state."""

    def __init__(self):
        """Initialize empty conversation history."""
        self.messages: List[Message] = []
        self.total_input_tokens: int = 0
        self.total_output_tokens: int = 0

    def add_message(self, message: Message) -> None:
        """Add a message to the history and update token counts."""
        self.messages.append(message)
        if message.usage:
            self.total_input_tokens += message.usage.prompt_tokens
            self.total_output_tokens += message.usage.completion_tokens

    def get_messages_for_context(self) -> List[str]:
        """Get messages formatted for AI context."""
        return [msg.content for msg in self.messages 
                if msg.source in (MessageSource.USER, MessageSource.AI)]

    def get_messages_for_transcript(self) -> List[Dict]:
        """Get messages formatted for transcript writing."""
        return [msg.to_transcript_dict() for msg in self.messages]

    def get_token_counts(self) -> Dict[str, int]:
        """Get current token usage counts."""
        return {
            "input": self.total_input_tokens,
            "output": self.total_output_tokens
        }
