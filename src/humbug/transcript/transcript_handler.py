"""Unified transcript handling for conversations."""

from dataclasses import dataclass
import json
import logging
import os
from datetime import datetime
from typing import Dict, List, Optional

from humbug.ai.ai_message import AIMessage
from humbug.transcript.transcript_error import (
    TranscriptFormatError, TranscriptIOError
)
from humbug.transcript.float_one_decimal_encoder import FloatOneDecimalEncoder


@dataclass
class TranscriptData:
    """Container for transcript data and metadata."""
    messages: List[AIMessage]
    timestamp: datetime
    version: str


class TranscriptHandler:
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
        self._logger = logging.getLogger("TranscriptHandler")

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
            TranscriptIOError: If file creation fails
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
            raise TranscriptIOError(
                f"Failed to create transcript file: {str(e)}"
            ) from e

    def _validate_transcript_data(self, data: Dict) -> None:
        """
        Validate transcript file structure.

        Args:
            data: Loaded transcript data

        Raises:
            TranscriptFormatError: If transcript format is invalid
        """
        if not isinstance(data, dict):
            raise TranscriptFormatError("Root element must be object")

        if "metadata" not in data or "conversation" not in data:
            raise TranscriptFormatError("Missing required fields")

        metadata = data["metadata"]
        if not isinstance(metadata, dict):
            raise TranscriptFormatError("Metadata must be object")

        if "version" not in metadata:
            raise TranscriptFormatError("Missing version in metadata")

        if "timestamp" not in metadata:
            raise TranscriptFormatError("Missing timestamp in metadata")

        if not isinstance(data["conversation"], list):
            raise TranscriptFormatError("Conversation must be array")

    def read(self) -> TranscriptData:
        """
        Read and validate transcript file.

        Returns:
            TranscriptData containing messages and metadata

        Raises:
            TranscriptFormatError: If transcript format is invalid
            TranscriptIOError: If file operations fail
        """
        try:
            with open(self._filename, 'r', encoding='utf-8') as f:
                data = json.load(f)
        except json.JSONDecodeError as e:
            raise TranscriptFormatError(f"Invalid JSON: {str(e)}") from e
        except Exception as e:
            raise TranscriptIOError(f"Failed to read transcript: {str(e)}") from e

        self._validate_transcript_data(data)

        # Convert transcript messages to AIMessage objects
        messages = []
        for msg in data["conversation"]:
            try:
                message = AIMessage.from_transcript_dict(msg)
                messages.append(message)
            except ValueError as e:
                raise TranscriptFormatError(f"Invalid message format: {str(e)}") from e

        try:
            timestamp = datetime.fromisoformat(data["metadata"]["timestamp"])
        except ValueError as e:
            raise TranscriptFormatError(f"Invalid timestamp format: {str(e)}") from e

        return TranscriptData(
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
            TranscriptIOError: If file operations fail
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

            raise TranscriptIOError(
                f"Failed to write transcript: {str(e)}",
                details={"backup_created": os.path.exists(f"{self._filename}.backup")}
            ) from e
