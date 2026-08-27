"""Line-based text document model, stdlib only."""

from collections.abc import Callable
from difflib import unified_diff
import logging
import re
from typing import Any

from diff import DiffApplicationResult
from editor_context.editor_context_diff_applier import EditorContextDiffApplier


class EditorContextDocument:
    """
    Line-based text model using stdlib only.

    The document is stored as a list of lines.  A trailing newline is
    represented by an empty string as the final element, mirroring
    ``str.splitlines()`` semantics:

        "a\\nb\\nc"    -> ["a", "b", "c"]          (no trailing newline)
        "a\\nb\\nc\\n" -> ["a", "b", "c", ""]      (trailing newline)
        ""             -> []                       (empty)
        "\\n"          -> ["", ""]                 (one blank line + trailing newline)

    The document owns file path and saved-content state so ``is_modified``
    and ``get_diff`` work without a frontend.
    """

    def __init__(self, path: str = "") -> None:
        """
        Initialise an empty document.

        Args:
            path: File path associated with the document, or empty string.
        """
        self._lines: list[str] = []
        self._path: str = path
        self._saved_content: str = ""
        self._listeners: list[Callable[[], None]] = []
        self._logger = logging.getLogger("EditorContextDocument")

    def to_plain_text(self) -> str:
        """Return the full document content as a single string."""
        return '\n'.join(self._lines)

    def block_count(self) -> int:
        """
        Return the number of content lines.

        A trailing empty string represents a trailing newline, not a
        content line, and is excluded from the count.
        """
        if not self._lines:
            return 0

        if len(self._lines) == 1 and self._lines[0] == '':
            return 1

        if self._lines[-1] == '':
            return len(self._lines) - 1

        return len(self._lines)

    def get_text_range(
        self,
        start_line: int | None = None,
        end_line: int | None = None,
    ) -> str:
        """
        Get text from the document, optionally limited to a line range.

        Args:
            start_line: Starting line number (1-indexed, inclusive), or None
                for the start of the document.
            end_line: Ending line number (1-indexed, inclusive), or None for
                the end of the document.

        Returns:
            Text content for the specified range.

        Raises:
            ValueError: If the line numbers are invalid.
        """
        if start_line is None and end_line is None:
            return self.to_plain_text()

        total = self.block_count()

        if start_line is None:
            start_line = 1

        if end_line is None:
            end_line = total

        if start_line < 1:
            raise ValueError(f"start_line must be >= 1, got {start_line}")

        if end_line < start_line:
            raise ValueError(f"end_line ({end_line}) must be >= start_line ({start_line})")

        if start_line > total:
            raise ValueError(
                f"start_line ({start_line}) exceeds document length ({total} lines)"
            )

        end_line = min(end_line, total)

        selected = self._lines[start_line - 1:end_line]
        return '\n'.join(selected)

    def get_line(self, line_num: int) -> str:
        """
        Return a single line (1-indexed).

        Raises:
            ValueError: If the line number is out of range.
        """
        total = self.block_count()

        if line_num < 1:
            raise ValueError(f"line_num must be >= 1, got {line_num}")

        if line_num > total:
            raise ValueError(f"line_num ({line_num}) exceeds document length ({total} lines)")

        return self._lines[line_num - 1]

    def get_lines(self, start_line: int, count: int) -> list[str]:
        """
        Return *count* lines starting at *start_line* (1-indexed).

        Returns fewer lines if the document is shorter than requested.
        """
        idx = max(start_line - 1, 0)

        end = idx + count

        return self._lines[idx:end]

    def find_all_occurrences(
        self,
        search_text: str,
        case_sensitive: bool = False,
        regexp: bool = False,
    ) -> list[dict[str, Any]]:
        """
        Find all occurrences of text in the document.

        Args:
            search_text:   Text or regular expression to search for.
            case_sensitive: Whether the search is case-sensitive.
            regexp:        If True, treat search_text as a regular expression.

        Returns:
            List of dicts with line (1-indexed), column (1-indexed),
            match_text, and context keys.

        Raises:
            ValueError: If regexp is True and search_text is not a valid regex.
        """
        if not search_text:
            return []

        matches: list[dict[str, Any]] = []

        if regexp:
            flags = 0 if case_sensitive else re.IGNORECASE

            try:
                pattern = re.compile(search_text, flags)

            except re.error as e:
                raise ValueError(f"Invalid regular expression: {e}") from e

            for line_idx, line_text in enumerate(self._lines):
                for m in pattern.finditer(line_text):
                    matches.append({
                        'line': line_idx + 1,
                        'column': m.start() + 1,
                        'match_text': m.group(),
                        'context': line_text,
                    })

        else:
            search_str = search_text if case_sensitive else search_text.lower()

            for line_idx, line_text in enumerate(self._lines):
                haystack = line_text if case_sensitive else line_text.lower()
                pos = 0

                while True:
                    pos = haystack.find(search_str, pos)

                    if pos == -1:
                        break

                    matches.append({
                        'line': line_idx + 1,
                        'column': pos + 1,
                        'match_text': line_text[pos:pos + len(search_text)],
                        'context': line_text,
                    })
                    pos += 1

        return matches

    def set_text(self, text: str) -> None:
        """
        Replace the entire document content.

        This is the primary sync entry point for a frontend widget pushing
        user edits to the model.
        """
        parts = text.split('\n')
        # An empty string splits to [''] but should produce an empty document
        if parts == ['']:
            parts = []

        self._lines = parts
        self._notify_listeners()

    def replace_text(self, start_line: int, end_line: int, new_text: str) -> None:
        """
        Replace lines start_line..end_line (1-indexed, inclusive) with new_text.

        The new text may contain multiple lines.
        """
        new_lines = new_text.split('\n')
        self._lines[start_line - 1:end_line] = new_lines
        self._notify_listeners()

    def apply_diff(self, diff_text: str) -> DiffApplicationResult:
        """
        Apply a unified diff to the document.

        Atomic — on failure the document is unchanged.

        Returns:
            DiffApplicationResult with operation status.

        Raises:
            DiffParseError, DiffMatchError, DiffValidationError,
            DiffApplicationError: On diff processing failures.
        """
        applier = EditorContextDiffApplier(confidence_threshold=0.75, search_window=50)
        original_lines = list(self._lines)

        try:
            result = applier.apply_diff(diff_text, self._lines)

        except Exception:
            self._lines = original_lines
            raise

        if result.success:
            self._notify_listeners()

        else:
            self._lines = original_lines

        return result

    def path(self) -> str:
        """Return the file path, or empty string for untitled."""
        return self._path

    def set_path(self, path: str) -> None:
        """Set the file path."""
        self._path = path

    def is_modified(self) -> bool:
        """Return True if the buffer differs from the saved content."""
        return self.to_plain_text() != self._saved_content

    def saved_content(self) -> str:
        """Return the content as last saved (or loaded from disk)."""
        return self._saved_content

    def mark_saved(self) -> None:
        """Record that the current content has been saved to disk."""
        self._saved_content = self.to_plain_text()

    def load_from_disk(self) -> None:
        """
        Read the file at the current path into the document.

        Sets the saved content so ``is_modified`` returns False.

        Raises:
            OSError: If the file cannot be read.
        """
        with open(self._path, 'r', encoding='utf-8') as f:
            content = f.read()

        self._lines = content.split('\n')
        self._saved_content = content
        self._notify_listeners()

    def save_to_disk(self) -> None:
        """
        Write the current content to the file path.

        Calls ``mark_saved`` on success.

        Raises:
            OSError: If the file cannot be written.
        """
        content = self.to_plain_text()

        with open(self._path, 'w', encoding='utf-8') as f:
            f.write(content)

        self._saved_content = content

    def get_diff(self, context_lines: int = 3) -> str:
        """
        Generate a unified diff between the saved content and the current buffer.

        Returns:
            Unified diff string, or empty string if there are no changes or
            the file has never been saved.
        """
        if not self._path:
            return ''

        current_content = self.to_plain_text()

        if current_content == self._saved_content:
            return ''

        saved_lines = self._saved_content.splitlines(keepends=True)
        current_lines = current_content.splitlines(keepends=True)
        diff_lines = unified_diff(
            saved_lines,
            current_lines,
            fromfile=f"a/{self._path}",
            tofile=f"b/{self._path}",
            n=context_lines,
        )
        return ''.join(diff_lines)

    def add_listener(self, callback: Callable[[], None]) -> None:
        """Register a listener called on any content mutation."""
        self._listeners.append(callback)

    def remove_listener(self, callback: Callable[[], None]) -> None:
        """Unregister a listener."""
        if callback in self._listeners:
            self._listeners.remove(callback)

    def _notify_listeners(self) -> None:
        """Invoke all registered listeners."""
        for callback in self._listeners:
            try:
                callback()

            except Exception:
                self._logger.exception("Error in EditorContextDocument listener")
