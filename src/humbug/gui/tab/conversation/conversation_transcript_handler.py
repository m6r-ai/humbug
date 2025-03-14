"""Unified transcript handling for conversations."""

from dataclasses import dataclass
import json
import logging
import os
from datetime import datetime
from typing import Dict, List, Optional

from humbug.ai.ai_message import AIMessage
from humbug.gui.tab.conversation.conversation_transcript_error import (
    ConversationTranscriptFormatError, ConversationTranscriptIOError
)


class FloatOneDecimalEncoder(json.JSONEncoder):
    """JSON encoder that formats floats to one decimal place."""
    def default(self, o):
        """
        Convert object to JSON serializable form.

        Args:
            o: Object to convert

        Returns:
            JSON serializable representation
        """
        if isinstance(o, float):
            return round(o, 1)

        return super().default(o)


@dataclass
class ConversationTranscriptData:
    """Container for transcript data and metadata."""
    messages: List[AIMessage]
    timestamp: datetime
    version: str


class ConversationTranscriptHandler:
    """Handles reading and writing conversation transcripts."""

    def __init__(self, filename: str, timestamp: Optional[datetime] = None):
        """
        Initialize transcript handler for a conversation.

        Args:
            filename: Full path to transcript file
            timestamp: ISO format timestamp for new conversations

        Raises:
            ValueError: If creating new transcript without timestamp
        """
        self._filename = filename
        self._timestamp = timestamp
        self._logger = logging.getLogger("ConversationTranscriptHandler")

        if not os.path.exists(filename):
            if timestamp is None:
                raise ValueError("Timestamp required when creating new transcript")
            self._initialize_file()

    def update_path(self, new_path: str):
        """Update the transcript file path.

        Args:
            new_path: New path for the transcript file
        """
        self._filename = new_path

    def _initialize_file(self) -> None:
        """
        Initialize a new transcript file with metadata.

        Raises:
            ConversationTranscriptIOError: If file creation fails
        """
        metadata = {
            "metadata": {
                "timestamp": self._timestamp.isoformat(),
                "version": "0.1",
            },
            "conversation": []
        }

        try:
            with open(self._filename, 'w', encoding='utf-8') as f:
                json.dump(metadata, f, indent=2, cls=FloatOneDecimalEncoder)
        except Exception as e:
            raise ConversationTranscriptIOError(
                f"Failed to create transcript file: {str(e)}"
            ) from e

    def _validate_transcript_data(self, data: Dict) -> None:
        """
        Validate transcript file structure.

        Args:
            data: Loaded transcript data

        Raises:
            ConversationTranscriptFormatError: If transcript format is invalid
        """
        if not isinstance(data, dict):
            raise ConversationTranscriptFormatError("Root element must be object")

        if "metadata" not in data or "conversation" not in data:
            raise ConversationTranscriptFormatError("Missing required fields")

        metadata = data["metadata"]
        if not isinstance(metadata, dict):
            raise ConversationTranscriptFormatError("Metadata must be object")

        if "version" not in metadata:
            raise ConversationTranscriptFormatError("Missing version in metadata")

        if "timestamp" not in metadata:
            raise ConversationTranscriptFormatError("Missing timestamp in metadata")

        if not isinstance(data["conversation"], list):
            raise ConversationTranscriptFormatError("Conversation must be array")

    def read(self) -> ConversationTranscriptData:
        """
        Read and validate transcript file.

        Returns:
            ConversationTranscriptData containing messages and metadata

        Raises:
            ConversationTranscriptFormatError: If transcript format is invalid
            ConversationTranscriptIOError: If file operations fail
        """
        try:
            with open(self._filename, 'r', encoding='utf-8') as f:
                data = json.load(f)
        except json.JSONDecodeError as e:
            raise ConversationTranscriptFormatError(f"Invalid JSON: {str(e)}") from e
        except Exception as e:
            raise ConversationTranscriptIOError(f"Failed to read transcript: {str(e)}") from e

        self._validate_transcript_data(data)

        # Convert transcript messages to AIMessage objects
        messages = []
        for msg in data["conversation"]:
            try:
                message = AIMessage.from_transcript_dict(msg)
                messages.append(message)
            except ValueError as e:
                raise ConversationTranscriptFormatError(f"Invalid message format: {str(e)}") from e

        try:
            timestamp = datetime.fromisoformat(data["metadata"]["timestamp"])
        except ValueError as e:
            raise ConversationTranscriptFormatError(f"Invalid timestamp format: {str(e)}") from e

        return ConversationTranscriptData(
            messages=messages,
            timestamp=timestamp,
            version=data["metadata"]["version"]
        )

    async def write(self, messages: List[Dict]) -> None:
        """
        Write messages to transcript file.

        Args:
            messages: List of message dictionaries to append

        Raises:
            ConversationTranscriptIOError: If file operations fail
        """
        try:
            # Read current content
            with open(self._filename, 'r', encoding='utf-8') as f:
                data = json.load(f)

            # Add new messages
            data["conversation"].extend(messages)

            # Write to temp file then rename for atomic operation
            temp_file = f"{self._filename}.tmp"
            with open(temp_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, cls=FloatOneDecimalEncoder)

            # Atomic replace
            os.replace(temp_file, self._filename)

        except Exception as e:
            # Create backup of current file if possible
            try:
                if os.path.exists(self._filename):
                    backup = f"{self._filename}.backup"
                    os.replace(self._filename, backup)
                    self._logger.info("Created transcript backup: %s", backup)
            except Exception as backup_error:
                self._logger.error(
                    "Failed to create backup file: %s",
                    str(backup_error)
                )

            raise ConversationTranscriptIOError(
                f"Failed to write transcript: {str(e)}",
                details={"backup_created": os.path.exists(f"{self._filename}.backup")}
            ) from e
