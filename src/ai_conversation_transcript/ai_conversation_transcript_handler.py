"""Unified transcript handling for conversations."""

import json
import logging
import os
import time
from typing import Dict, Any

from ai import AIMessage, AIConversationHistory

from ai_conversation_transcript.ai_conversation_transcript_error import (
    AIConversationTranscriptFormatError, AIConversationTranscriptIOError
)


class FloatOneDecimalEncoder(json.JSONEncoder):
    """JSON encoder that formats floats to one decimal place."""
    def default(self, o: Any) -> Any:
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


class AIConversationTranscriptHandler:
    """Handles reading and writing conversation transcripts."""

    def __init__(self, filename: str):
        """
        Initialize transcript handler for a conversation.

        Args:
            filename: Full path to transcript file
        """
        self._filename = filename
        self._logger = logging.getLogger("AIConversationTranscriptHandler")

        if not os.path.exists(filename):
            self._initialize_file()

    def get_path(self) -> str:
        """Get the transcript file path.

        Returns:
            str: Full path to the transcript file
        """
        return self._filename

    def _atomic_replace(self, temp_file: str, target_file: str) -> None:
        """
        Atomically replace target file with temp file, with retry logic.

        This method implements retry logic to handle transient file locking issues
        that can occur on any platform, but are particularly common on Windows.

        Args:
            temp_file: Path to temporary file
            target_file: Path to target file to replace

        Raises:
            OSError: If replacement fails after all retries
        """
        max_retries = 4
        retry_delay = 0.05  # seconds

        for attempt in range(max_retries):
            try:
                os.replace(temp_file, target_file)
                return

            except PermissionError as e:
                if attempt < max_retries - 1:
                    self._logger.warning(
                        "File replace failed (attempt %d/%d), retrying: %s",
                        attempt + 1,
                        max_retries,
                        str(e)
                    )
                    time.sleep(retry_delay * (attempt + 1))  # Linear backoff
                    continue

                # All retries exhausted
                raise e

    def set_path(self, new_path: str) -> None:
        """Set the transcript file path.

        Args:
            new_path: New path for the transcript file
        """
        self._filename = new_path

    def _initialize_file(self) -> None:
        """
        Initialize a new transcript file with metadata.

        Raises:
            AIConversationTranscriptIOError: If file creation fails
        """
        metadata = {
            "metadata": {
                "version": "0.1",
                "parent": None
            },
            "conversation": []
        }

        try:
            with open(self._filename, 'w', encoding='utf-8') as f:
                json.dump(metadata, f, indent=2, cls=FloatOneDecimalEncoder)

        except Exception as e:
            raise AIConversationTranscriptIOError(
                f"Failed to create transcript file: {str(e)}"
            ) from e

    def _validate_transcript_data(self, data: Dict) -> None:
        """
        Validate transcript file structure.

        Args:
            data: Loaded transcript data

        Raises:
            AIConversationTranscriptFormatError: If transcript format is invalid
        """
        if not isinstance(data, dict):
            raise AIConversationTranscriptFormatError("Root element must be object")

        if "metadata" not in data or "conversation" not in data:
            raise AIConversationTranscriptFormatError("Missing required fields")

        metadata = data["metadata"]
        if not isinstance(metadata, dict):
            raise AIConversationTranscriptFormatError("Metadata must be object")

        if "version" not in metadata:
            raise AIConversationTranscriptFormatError("Missing version in metadata")

        if not isinstance(data["conversation"], list):
            raise AIConversationTranscriptFormatError("Conversation must be array")

    def read(self) -> AIConversationHistory:
        """
        Read and validate transcript file.

        Returns:
            AIConversationHistory containing messages and metadata

        Raises:
            AIConversationTranscriptFormatError: If transcript format is invalid
            AIConversationTranscriptIOError: If file operations fail
        """
        try:
            with open(self._filename, 'r', encoding='utf-8') as f:
                data = json.load(f)

        except json.JSONDecodeError as e:
            raise AIConversationTranscriptFormatError(f"Invalid JSON: {str(e)}") from e

        except Exception as e:
            raise AIConversationTranscriptIOError(f"Failed to read transcript: {str(e)}") from e

        self._validate_transcript_data(data)

        # Convert transcript messages to AIMessage objects
        messages = []
        for msg in data["conversation"]:
            try:
                message = AIMessage.from_transcript_dict(msg)
                messages.append(message)

            except ValueError as e:
                raise AIConversationTranscriptFormatError(f"Invalid message format: {str(e)}") from e

        return AIConversationHistory(
            messages=messages,
            version=data["metadata"]["version"],
            parent=data["metadata"].get("parent", None)
        )

    def write(self, history: AIConversationHistory) -> None:
        """
        Write a conversation history to the transcript file.

        Args:
            history: AIConversationHistory object containing messages and metadata

        Raises:
            AIConversationTranscriptFormatError: If transcript format is invalid
            AIConversationTranscriptIOError: If file operations fail
        """
        try:
            # Convert messages to transcript format
            transcript_messages = [msg.to_transcript_dict() for msg in history.get_messages()]

            # Create the full transcript structure
            data = {
                "metadata": {
                    "version": history.version(),
                    "parent": history.parent()
                },
                "conversation": transcript_messages
            }

            # Write to temp file then rename for atomic operation
            temp_file = f"{self._filename}.tmp"
            with open(temp_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, cls=FloatOneDecimalEncoder)

            # Atomic replace
            self._atomic_replace(temp_file, self._filename)

        except Exception as e:
            # Clean up temp file if it exists
            try:
                temp_file = f"{self._filename}.tmp"
                if os.path.exists(temp_file):
                    os.remove(temp_file)

            except Exception as backup_error:
                self._logger.error("Failed to clean up temp file: %s", str(backup_error))

            raise AIConversationTranscriptIOError(
                f"Failed to replace messages in transcript: {str(e)}"
            ) from e
