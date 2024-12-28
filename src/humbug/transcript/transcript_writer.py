"""Transcript writer for individual conversations."""

import asyncio
from datetime import datetime
import json
import os
from typing import Dict, List

from humbug.transcript.float_one_decimal_encoder import FloatOneDecimalEncoder

class TranscriptWriter:
    """Handles writing a single conversation transcript."""

    def __init__(self, filename: str, conversation_number: int):
        """Initialize transcript writer for a conversation.

        Args:
            filename: Full path to transcript file
            conversation_number: Sequential number for this conversation
        """
        self._filename = filename
        self._initialize_file(conversation_number)

    def _initialize_file(self, conversation_number: int) -> None:
        """Initialize a new transcript file with metadata."""
        metadata = {
            "metadata": {
                "timestamp": datetime.utcnow().isoformat(),
                "version": "0.1",
                "conversation_number": conversation_number
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
            for message in messages:
                data["conversation"].append(message)

            # Write to temp file then rename for atomic operation
            temp_file = f"{self._filename}.tmp"
            with open(temp_file, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, cls=FloatOneDecimalEncoder)

            os.replace(temp_file, self._filename)

        except Exception as e:
            print(f"Error writing transcript: {str(e)}", file=sys.stderr)
            # If this fails, the chat tab will still have the content in memory
            # Next write will try again with all messages

    def close(self) -> None:
        """Close the transcript file."""
        # Nothing to do as we don't keep file handles open
        pass
