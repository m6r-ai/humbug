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
    conversation_id: str
    usage: Optional[Usage] = None
    error: Optional[Dict] = None
    model: Optional[str] = None
    temperature: Optional[float] = None
    completed: bool = True

    @classmethod
    def create(cls, conversation_id: str, source: MessageSource, content: str,
               usage: Optional[Usage] = None, error: Optional[Dict] = None,
               model: Optional[str] = None, temperature: Optional[float] = None,
               completed: bool = True) -> 'Message':
        """Create a new message with generated ID and current timestamp."""
        return cls(
            id=str(uuid.uuid4()),
            source=source,
            content=content,
            timestamp=datetime.utcnow(),
            conversation_id=conversation_id,
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
            "conversation_id": self.conversation_id,
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


class ConversationHistory:
    """Manages the conversation history and state."""

    def __init__(self, conversation_id: str):
        """Initialize empty conversation history."""
        self.conversation_id = conversation_id
        self.messages: List[Message] = []
        self.total_input_tokens: int = 0
        self.total_output_tokens: int = 0

    def add_message(self, message: Message) -> None:
        """Add a message to the history."""
        self.messages.append(message)

    def update_message(self, message_id: str, content: str, usage: Optional[Usage] = None, completed: bool = None) -> Optional[Message]:
        """Update an existing message and return the updated message."""
        for message in self.messages:
            if message.id == message_id:
                message.content = content
                if usage is not None:
                    old_usage = message.usage
                    message.usage = usage
                    # Only update token counts if we didn't have usage before
                    if old_usage is None:
                        self.total_input_tokens += usage.prompt_tokens
                        self.total_output_tokens += usage.completion_tokens
                if completed is not None:
                    message.completed = completed
                return message
        return None

    def recalculate_token_counts(self) -> None:
        """Recalculate total token counts from all messages."""
        self.total_input_tokens = 0
        self.total_output_tokens = 0
        for message in self.messages:
            if message.usage:
                self.total_input_tokens += message.usage.prompt_tokens
                self.total_output_tokens += message.usage.completion_tokens

    def get_messages_for_context(self) -> List[str]:
        """Get messages formatted for AI context."""
        result = []
        i = 0
        while i < len(self.messages):
            if self.messages[i].source == MessageSource.USER:
                # Found a user message, look for corresponding AI response
                user_msg = self.messages[i]
                ai_msg = None
                if i + 1 < len(self.messages) and self.messages[i + 1].source == MessageSource.AI:
                    ai_msg = self.messages[i + 1]

                # Only include the exchange if:
                # 1. It's a user message without an AI response yet (current exchange)
                # 2. Or it's a completed exchange without errors
                if (ai_msg is None) or (ai_msg.completed and not ai_msg.error):
                    result.append(user_msg.content)
                    if ai_msg:
                        result.append(ai_msg.content)
                        i += 1  # Skip the AI message since we've handled it

            i += 1  # Move to next message

        return result

    def get_messages_for_transcript(self) -> List[Dict]:
        """Get messages formatted for transcript writing."""
        return [msg.to_transcript_dict() for msg in self.messages]

    def get_token_counts(self) -> Dict[str, int]:
        """Get current token usage counts."""
        return {
            "input": self.total_input_tokens,
            "output": self.total_output_tokens
        }
