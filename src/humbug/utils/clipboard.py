"""Clipboard integration for copy/paste operations."""

import logging
import platform
import subprocess
from typing import Optional


class ClipboardHandler:
    """Handles system clipboard operations."""

    def __init__(self):
        """Initialize clipboard handler."""
        self.logger = logging.getLogger("ClipboardHandler")
        self.system = platform.system()

        if self.system == "Windows":
            try:
                import win32clipboard
                self._win32clipboard = win32clipboard
            except ImportError:
                self.logger.warning("win32clipboard not available - clipboard support disabled")
                self._win32clipboard = None

    def strip_ansi(self, text: str) -> str:
        """Remove ANSI escape sequences from text.

        Args:
            text: Text containing ANSI sequences

        Returns:
            Clean text without ANSI sequences
        """
        result = []
        i = 0
        n = len(text)

        while i < n:
            if text[i] == '\x1b':  # ESC character
                i += 1
                if i < n and text[i] == '[':
                    # Skip until end of escape sequence (next letter)
                    i += 1
                    while i < n and not ('A' <= text[i] <= 'Z' or 'a' <= text[i] <= 'z'):
                        i += 1
                    i += 1  # Skip the final character
                    continue
            else:
                result.append(text[i])
            i += 1

        return ''.join(result)

    def copy_to_clipboard(self, text: str) -> bool:
        """Copy text to system clipboard.

        Args:
            text: Text to copy

        Returns:
            True if copy succeeded, False otherwise
        """
        if not text:
            return False

        # Clean the text before copying
        clean_text = self.strip_ansi(text)
        if not clean_text:
            return False

        try:
            if self.system == "Darwin":  # macOS
                process = subprocess.Popen(
                    ["pbcopy"],
                    stdin=subprocess.PIPE,
                    close_fds=True
                )
                process.communicate(clean_text.encode())
                return process.returncode == 0

            elif self.system == "Windows":
                if not self._win32clipboard:
                    return False

                self._win32clipboard.OpenClipboard()
                self._win32clipboard.EmptyClipboard()
                self._win32clipboard.SetClipboardText(clean_text)
                self._win32clipboard.CloseClipboard()
                return True

            else:  # Linux/Unix
                # Try xclip first
                try:
                    process = subprocess.Popen(
                        ["xclip", "-selection", "clipboard"],
                        stdin=subprocess.PIPE,
                        close_fds=True
                    )
                    process.communicate(clean_text.encode())
                    if process.returncode == 0:
                        return True
                except FileNotFoundError:
                    pass

                # Fall back to xsel
                try:
                    process = subprocess.Popen(
                        ["xsel", "--input", "--clipboard"],
                        stdin=subprocess.PIPE,
                        close_fds=True
                    )
                    process.communicate(clean_text.encode())
                    return process.returncode == 0
                except FileNotFoundError:
                    self.logger.warning("Neither xclip nor xsel found - clipboard copy disabled")
                    return False

        except Exception as e:
            self.logger.error(f"Error copying to clipboard: {e}")
            return False

    def paste_from_clipboard(self) -> Optional[str]:
        """Paste text from system clipboard.

        Returns:
            Clipboard text if available, None otherwise
        """
        try:
            if self.system == "Darwin":  # macOS
                process = subprocess.Popen(
                    ["pbpaste"],
                    stdout=subprocess.PIPE,
                    close_fds=True
                )
                output, _ = process.communicate()
                return output.decode() if process.returncode == 0 else None

            elif self.system == "Windows":
                if not self._win32clipboard:
                    return None

                self._win32clipboard.OpenClipboard()
                try:
                    text = self._win32clipboard.GetClipboardData()
                    return text
                except Exception:
                    return None
                finally:
                    self._win32clipboard.CloseClipboard()

            else:  # Linux/Unix
                # Try xclip first
                try:
                    process = subprocess.Popen(
                        ["xclip", "-selection", "clipboard", "-output"],
                        stdout=subprocess.PIPE,
                        close_fds=True
                    )
                    output, _ = process.communicate()
                    if process.returncode == 0:
                        return output.decode()
                except FileNotFoundError:
                    pass

                # Fall back to xsel
                try:
                    process = subprocess.Popen(
                        ["xsel", "--output", "--clipboard"],
                        stdout=subprocess.PIPE,
                        close_fds=True
                    )
                    output, _ = process.communicate()
                    if process.returncode == 0:
                        return output.decode()
                except FileNotFoundError:
                    self.logger.warning("Neither xclip nor xsel found - clipboard paste disabled")
                    return None

        except Exception as e:
            self.logger.error(f"Error pasting from clipboard: {e}")
            return None
