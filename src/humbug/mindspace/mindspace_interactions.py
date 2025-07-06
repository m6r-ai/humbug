import json
import logging
import os
from typing import List

from humbug.mindspace.mindspace_message import MindspaceMessage


class MindspaceInteractions:
    """Manages the mindspace interaction history."""

    MAX_MESSAGES = 100

    def __init__(self) -> None:
        """Initialize empty interaction history."""
        self._messages: List[MindspaceMessage] = []
        self._logger = logging.getLogger("SystemInteraction")

    def add_message(self, message: MindspaceMessage) -> None:
        """Add a message to the history, maintaining the max message limit."""
        self._messages.append(message)

        # Trim to the maximum number of messages
        if len(self._messages) > self.MAX_MESSAGES:
            self._messages = self._messages[-self.MAX_MESSAGES:]

    def get_messages(self) -> List[MindspaceMessage]:
        """Get a copy of all messages in the interaction history."""
        return self._messages.copy()

    def save(self, file_path: str) -> None:
        """Save interaction history to a JSON file."""
        messages_data = [msg.to_dict() for msg in self._messages]

        with open(file_path, 'w', encoding='utf-8') as f:
            json.dump({"messages": messages_data}, f, indent=2)

    def load(self, file_path: str) -> None:
        """Load interaction history from a JSON file."""
        if not os.path.exists(file_path):
            self._messages = []
            return

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                data = json.load(f)

            messages_data = data.get("messages", [])
            self._messages = [MindspaceMessage.from_dict(msg) for msg in messages_data]

            # Ensure we don't exceed the maximum
            if len(self._messages) > self.MAX_MESSAGES:
                self._messages = self._messages[-self.MAX_MESSAGES:]

        except (json.JSONDecodeError, KeyError):
            self._logger.exception("Failed to load shell interaction history")

            # If there's an error loading, start with an empty history
            self._messages = []

    def clear(self) -> None:
        """Clear the interaction history."""
        self._messages = []
