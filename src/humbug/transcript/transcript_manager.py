"""Manager for transcript files."""

import os
from datetime import datetime

class TranscriptManager:
    """Manages transcript file operations."""

    @staticmethod
    def generate_conversation_id() -> str:
        """Generate a unique conversation ID based on current UTC time."""
        return datetime.utcnow().strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]

    @staticmethod
    def ensure_conversations_directory() -> None:
        """Create conversations directory if it doesn't exist."""
        os.makedirs("conversations", exist_ok=True)

    @staticmethod
    def generate_transcript_filename() -> str:
        """Generate a transcript filename based on current UTC time."""
        timestamp = datetime.utcnow().strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
        return os.path.join("conversations", f"{timestamp}.conv")

    @staticmethod
    def get_conversation_number() -> int:
        """Get the next conversation number by counting existing files."""
        try:
            files = os.listdir("conversations")
            return len([f for f in files if f.endswith('.conv')]) + 1
        except FileNotFoundError:
            return 1
