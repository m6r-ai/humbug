"""Conversation message support."""

from dataclasses import dataclass
from datetime import datetime
from typing import Dict, List
import uuid

from humbug.ai.ai_message_source import AIMessageSource
from humbug.ai.ai_model import ReasoningCapability
from humbug.ai.ai_tool_manager import AIToolCall, AIToolResult
from humbug.ai.ai_usage import AIUsage


@dataclass
class AIMessage:
    """
    Represents a single message in the conversation.

    These messsages represent an abstraction of the actual messages we send and receive.  We want them
    to be independent of the actual backend, so we can swap backends mid-conversation without losing context.
    """
    id: str
    source: AIMessageSource
    content: str
    timestamp: datetime
    usage: AIUsage | None = None
    error: Dict | None = None
    model: str | None = None
    temperature: float | None = None
    reasoning_capability: ReasoningCapability | None = None
    completed: bool = True
    tool_calls: List[AIToolCall] | None = None
    tool_results: List[AIToolResult] | None = None
    signature: str | None = None
    readacted_reasoning: str | None = None

    # Map between AIMessageSource enum and transcript type strings
    _SOURCE_TYPE_MAP = {
        AIMessageSource.USER: "user_message",
        AIMessageSource.AI: "ai_response",
        AIMessageSource.REASONING: "ai_reasoning",
        AIMessageSource.SYSTEM: "system_message",
        AIMessageSource.TOOL_CALL: "tool_call",
        AIMessageSource.TOOL_RESULT: "tool_result"
    }
    _TYPE_SOURCE_MAP = {v: k for k, v in _SOURCE_TYPE_MAP.items()}

    @classmethod
    def create(
        cls,
        source: AIMessageSource,
        content: str,
        usage: AIUsage | None = None,
        error: Dict | None = None,
        model: str | None = None,
        temperature: float | None = None,
        reasoning_capability: ReasoningCapability | None = None,
        completed: bool = True,
        timestamp: datetime | None = None,
        tool_calls: List[AIToolCall] | None = None,
        tool_results: List[AIToolResult] | None = None,
        signature: str | None = None,
        readacted_reasoning: str | None = None
    ) -> 'AIMessage':
        """Create a new message with generated ID and current timestamp."""
        if timestamp is None:
            timestamp = datetime.utcnow()

        return cls(
            id=str(uuid.uuid4()),
            source=source,
            content=content,
            timestamp=timestamp,
            usage=usage,
            error=error,
            model=model,
            temperature=temperature,
            reasoning_capability=reasoning_capability,
            completed=completed,
            tool_calls=tool_calls,
            tool_results=tool_results,
            signature=signature,
            readacted_reasoning=readacted_reasoning
        )

    def copy(self) -> 'AIMessage':
        """Create a deep copy of the message."""
        return AIMessage(
            id=self.id,  # We keep the same ID for tracking
            source=self.source,
            content=self.content,
            timestamp=self.timestamp,
            usage=self.usage.copy() if self.usage else None,
            error=self.error.copy() if self.error else None,
            model=self.model,
            temperature=self.temperature,
            reasoning_capability=self.reasoning_capability,
            completed=self.completed,
            tool_calls=self.tool_calls.copy() if self.tool_calls else None,
            tool_results=self.tool_results.copy() if self.tool_results else None,
            signature=self.signature,
            readacted_reasoning=self.readacted_reasoning
        )

    def is_hidden_from_user(self) -> bool:
        """Check if this message should be hidden from the user interface."""
        return self.source in (AIMessageSource.TOOL_CALL, AIMessageSource.TOOL_RESULT)

    def to_transcript_dict(self) -> Dict:
        """Convert message to transcript format."""
        message = {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "type": self._SOURCE_TYPE_MAP[self.source],
            "content": self.content,
            "completed": self.completed
        }

        if self.usage:
            message["usage"] = self.usage.to_dict()

        if self.error:
            message["error"] = self.error

        if self.model:
            message["model"] = self.model

        if self.reasoning_capability:
            message["reasoning_capability"] = self.reasoning_capability.value

        if self.temperature is not None:
            message["temperature"] = self.temperature

        if self.signature:
            message["signature"] = self.signature

        if self.readacted_reasoning:
            message["readacted_reasoning"] = self.readacted_reasoning

        # Add tool-specific fields
        if self.tool_calls:
            message["tool_calls"] = [
                {
                    "id": call.id,
                    "name": call.name,
                    "arguments": call.arguments
                }
                for call in self.tool_calls
            ]

        if self.tool_results:
            message["tool_results"] = [
                {
                    "tool_call_id": result.tool_call_id,
                    "name": result.name,
                    "content": result.content,
                    "error": result.error
                }
                for result in self.tool_results
            ]

        return message

    @classmethod
    def from_transcript_dict(cls, data: Dict) -> 'AIMessage':
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

        # Parse reasoning capability if present
        reasoning_capability = None
        if data.get("reasoning_capability"):
            try:
                reasoning_value = data["reasoning_capability"]
                reasoning_capability = ReasoningCapability(reasoning_value)

            except ValueError as e:
                raise ValueError(f"Invalid reasoning capability: {data['reasoning_capability']}") from e

        # Parse usage data if present
        usage = None
        if data.get("usage"):
            try:
                usage_data = data["usage"]
                usage = AIUsage(
                    prompt_tokens=usage_data["prompt_tokens"],
                    completion_tokens=usage_data["completion_tokens"],
                    total_tokens=usage_data["total_tokens"]
                )

            except (KeyError, TypeError) as e:
                raise ValueError(f"Invalid usage data format: {data['usage']}") from e

        # Parse tool calls if present
        tool_calls = None
        if data.get("tool_calls"):
            try:
                tool_calls = [
                    AIToolCall(
                        id=call_data["id"],
                        name=call_data["name"],
                        arguments=call_data["arguments"]
                    )
                    for call_data in data["tool_calls"]
                ]

            except (KeyError, TypeError) as e:
                raise ValueError(f"Invalid tool_calls data format: {data['tool_calls']}") from e

        # Parse tool results if present
        tool_results = None
        if data.get("tool_results"):
            try:
                tool_results = [
                    AIToolResult(
                        tool_call_id=result_data["tool_call_id"],
                        name=result_data["name"],
                        content=result_data["content"],
                        error=result_data.get("error")
                    )
                    for result_data in data["tool_results"]
                ]

            except (KeyError, TypeError) as e:
                raise ValueError(f"Invalid tool_results data format: {data['tool_results']}") from e

        return cls(
            id=data["id"],
            source=source,
            content=data["content"],
            timestamp=timestamp,
            usage=usage,
            error=data.get("error", None),
            model=data.get("model", None),
            temperature=data.get("temperature", None),
            reasoning_capability=reasoning_capability,
            completed=data.get("completed", True),
            tool_calls=tool_calls,
            tool_results=tool_results,
            signature=data.get("signature", None),
            readacted_reasoning=data.get("readacted_reasoning", None)
        )
