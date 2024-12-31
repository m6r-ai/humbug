"""Transcript writer for individual conversations."""

from datetime import datetime
import json
import os
import sys
from typing import Dict, List

from humbug.transcript.float_one_decimal_encoder import FloatOneDecimalEncoder


class TranscriptWriter:
    """Handles writing a single conversation transcript."""

    def __init__(self, filename: str, timestamp: datetime):
        """Initialize transcript writer for a conversation.

        Args:
            filename: Full path to transcript file
            timestamp: ISO format timestamp for the conversation start
        """
        self._filename = filename
        self._timestamp: datetime = timestamp

        # Only initialize if file doesn't exist
        if not os.path.exists(filename):
            self._initialize_file()

    def _initialize_file(self) -> None:
        """Initialize a new transcript file with metadata."""
        metadata = {
            "metadata": {
                "timestamp": self._timestamp.isoformat(),
                "version": "0.1",
            },
            "conversation": []
        }
        with open(self._filename, 'w', encoding='utf-8') as f:
            json.dump(metadata, f, indent=2, cls=FloatOneDecimalEncoder)

    async def write(self, messages: List[Dict]) -> None:
        """Write messages to the transcript file.

        Args:
            messages: List of message dictionaries to append
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
            print(f"Error writing transcript: {str(e)}", file=sys.stderr)
            # Create backup of current file if possible
            try:
                if os.path.exists(self._filename):
                    backup = f"{self._filename}.backup"
                    os.replace(self._filename, backup)
            except Exception:
                pass

    def close(self) -> None:
        """Close the transcript file."""
        # Nothing to do as we don't keep file handles open
