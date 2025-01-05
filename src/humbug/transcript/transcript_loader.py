"""Load and validate conversation transcripts."""

import json
from typing import Dict, List, Optional, Tuple

from humbug.conversation.message import Message


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
                message = Message.from_transcript_dict(msg)
                messages.append(message)
            except ValueError as e:
                return [], f"Error parsing message: {str(e)}", None

        return messages, None, metadata
