"""Load and validate conversation transcripts."""

import json
from datetime import datetime
from typing import Dict, List, Optional, Tuple
import uuid

from humbug.conversation.message import Message
from humbug.conversation.message_source import MessageSource
from humbug.conversation.usage import Usage


class TranscriptLoader:
    """Handles loading and validating conversation transcripts."""

    @staticmethod
    def load_transcript(filename: str) -> Tuple[List[Message], Optional[str], Optional[Dict]]:
        """
        Load and validate a transcript file.

        Args:
            filename: Path to transcript file

        Returns:
            Tuple containing:
            - List of Message objects
            - Error message if validation failed, None if successful
            - Metadata dictionary if successful, None if failed

        Raises:
            FileNotFoundError: If file doesn't exist
            json.JSONDecodeError: If file isn't valid JSON
        """
        with open(filename, 'r', encoding='utf-8') as f:
            data = json.load(f)

        # Basic structure validation
        if not isinstance(data, dict):
            return [], "Invalid transcript format: root must be object", None

        if "metadata" not in data or "conversation" not in data:
            return [], "Invalid transcript format: missing required fields", None

        metadata = data["metadata"]
        if not isinstance(metadata, dict):
            return [], "Invalid transcript format: metadata must be object", None

        if "version" not in metadata:
            return [], "Invalid transcript format: missing version", None

        conversation = data["conversation"]
        if not isinstance(conversation, list):
            return [], "Invalid transcript format: conversation must be array", None

        # Convert transcript messages to Message objects
        messages = []
        for msg in conversation:
            try:
                # Required fields
                if not all(k in msg for k in ["id", "timestamp", "type", "content"]):
                    return [], f"Message missing required field: {msg}", None

                # Parse timestamp
                try:
                    timestamp = datetime.fromisoformat(msg["timestamp"])
                except ValueError:
                    return [], f"Invalid timestamp format: {msg['timestamp']}", None

                # Map message type to source
                type_to_source = {
                    "user_message": MessageSource.USER,
                    "ai_response": MessageSource.AI,
                    "system_message": MessageSource.SYSTEM
                }
                if msg["type"] not in type_to_source:
                    return [], f"Invalid message type: {msg['type']}", None
                
                source = type_to_source[msg["type"]]

                # Handle usage data if present
                usage = None
                if "usage" in msg:
                    usage_data = msg["usage"]
                    if not all(k in usage_data for k in ["prompt_tokens", "completion_tokens", "total_tokens"]):
                        return [], f"Invalid usage data format: {usage_data}", None
                    usage = Usage(
                        prompt_tokens=usage_data["prompt_tokens"],
                        completion_tokens=usage_data["completion_tokens"],
                        total_tokens=usage_data["total_tokens"]
                    )

                # Create message object
                message = Message(
                    id=msg["id"],
                    source=source,
                    content=msg["content"],
                    timestamp=timestamp,
                    usage=usage,
                    error=msg.get("error"),
                    model=msg.get("model"),
                    temperature=msg.get("temperature"),
                    completed=msg.get("completed", True)
                )
                messages.append(message)

            except Exception as e:
                return [], f"Error parsing message: {str(e)}", None

        return messages, None, metadata