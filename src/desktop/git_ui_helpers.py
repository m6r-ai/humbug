"""Shared helpers for the git UI (relative time formatting and diff colouring)."""

import time

from PySide6.QtGui import QSyntaxHighlighter, QTextCharFormat, QTextDocument

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


def relative_time(timestamp: int) -> str:
    """
    Return a short human-readable age like '3d ago' for an epoch timestamp.

    Args:
        timestamp: Epoch seconds; 0 or falsy yields an empty string.

    Returns:
        A compact relative-time string.
    """
    if not timestamp:
        return ""

    delta = max(0, int(time.time()) - timestamp)
    for unit, secs in (("y", 31536000), ("mo", 2592000), ("d", 86400), ("h", 3600), ("m", 60)):
        if delta >= secs:
            return f"{delta // secs}{unit} ago"

    return "just now"


class DiffHighlighter(QSyntaxHighlighter):
    """Colours unified-diff lines by prefix (adds, deletes, hunks, meta)."""

    def __init__(self, document: QTextDocument, style_manager: StyleManager) -> None:
        super().__init__(document)
        self._style_manager = style_manager

    def refresh(self) -> None:
        """Re-highlight after a theme change."""
        self.rehighlight()

    def highlightBlock(self, text: str) -> None:
        if text.startswith("@@"):
            role = ColorRole.TEXT_HEADING

        elif text.startswith(("diff ", "index ", "--- ", "+++ ", "new file",
                              "deleted file", "similarity", "rename ")):
            role = ColorRole.TEXT_INACTIVE

        elif text.startswith("+"):
            role = ColorRole.VCS_ADDED

        elif text.startswith("-"):
            role = ColorRole.VCS_DELETED

        else:
            return

        fmt = QTextCharFormat()
        fmt.setForeground(self._style_manager.get_color(role))
        self.setFormat(0, len(text), fmt)
