"""Shell history management for shell command history."""

import json
import logging
import os
from typing import List

from humbug.gui.tab.shell.shell_message import ShellMessage
from humbug.gui.tab.shell.shell_message_source import ShellMessageSource


class ShellHistory:
    """Manages shell command history storage and retrieval."""

    def __init__(self, max_messages: int = 1000) -> None:
        """
        Initialize shell history manager.

        Args:
            max_messages: Maximum number of messages to keep in history
        """
        self._messages: List[ShellMessage] = []
        self._max_messages = max_messages
        self._logger = logging.getLogger("ShellHistory")

    def add_message(self, message: ShellMessage) -> None:
        """
        Add a message to the history.

        Args:
            message: The ShellMessage to add
        """
        self._messages.append(message)

        # Trim history if it exceeds maximum size
        if len(self._messages) > self._max_messages:
            self._messages = self._messages[-self._max_messages:]

    def get_messages(self) -> List[ShellMessage]:
        """
        Get all messages in chronological order.

        Returns:
            List of ShellMessage objects
        """
        return self._messages.copy()

    def clear(self) -> None:
        """Clear all messages from history."""
        self._messages.clear()

    def save(self, file_path: str) -> None:
        """
        Save history to a JSON file.

        Args:
            file_path: Path to save the history file

        Raises:
            OSError: If file cannot be written
        """
        try:
            # Ensure directory exists
            os.makedirs(os.path.dirname(file_path), exist_ok=True)

            # Convert messages to dictionaries
            data = {
                "messages": [msg.to_dict() for msg in self._messages],
                "max_messages": self._max_messages
            }

            # Write to file
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=2, ensure_ascii=False)

        except OSError as e:
            self._logger.error("Failed to save shell history to %s: %s", file_path, str(e))
            raise

    def load(self, file_path: str) -> None:
        """
        Load history from a JSON file.

        Args:
            file_path: Path to load the history file from

        Raises:
            OSError: If file cannot be read
            json.JSONDecodeError: If file contains invalid JSON
        """
        if not os.path.exists(file_path):
            self._logger.debug("Shell history file does not exist: %s", file_path)
            return

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                data = json.load(f)

            # Load messages
            self._messages = []
            for msg_data in data.get("messages", []):
                try:
                    message = ShellMessage.from_dict(msg_data)
                    self._messages.append(message)
                except (KeyError, ValueError) as e:
                    self._logger.warning("Skipping invalid message in history: %s", str(e))
                    continue

            # Update max_messages if specified in file
            if "max_messages" in data:
                self._max_messages = data["max_messages"]

            self._logger.debug("Loaded %d messages from shell history", len(self._messages))

        except json.JSONDecodeError as e:
            self._logger.error("Failed to parse shell history file %s: %s", file_path, str(e))
            raise

        except OSError as e:
            self._logger.error("Failed to load shell history from %s: %s", file_path, str(e))
            raise

    def get_user_commands(self) -> List[str]:
        """
        Get list of user commands from history (newest first).

        Returns:
            List of user command strings
        """
        user_commands = []

        # Process messages from newest to oldest
        for message in reversed(self._messages):
            if message.source == ShellMessageSource.USER:
                content = message.content.strip()
                if content and content not in user_commands:
                    user_commands.append(content)

        return user_commands
