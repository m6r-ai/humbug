"""Editor context model."""

from collections.abc import Callable
import logging
from typing import Any

from diff import DiffParseError, DiffMatchError, DiffValidationError, DiffApplicationError
from editor_context.editor_document import EditorDocument


class EditorContext:
    """
    Model-layer context for an open editor tab.

    Owns an ``EditorDocument`` (a plain-Python line-based model) and
    implements the data operations the AI tool layer needs: reading,
    searching, diffing, and applying diffs.

    Operations that depend on a frontend widget's cursor or viewport
    (get_cursor_info, get_selected_text, goto_line, save) are delegated to
    optional callbacks supplied by the frontend at construction time.  A
    headless backend supplies ``None`` for these callbacks and the methods
    return sensible defaults or operate on the document directly.
    """

    def __init__(
        self,
        context_id: str,
        document: EditorDocument,
        get_cursor_info_cb: Callable[[], dict[str, Any]] | None = None,
        get_selected_text_cb: Callable[[], str] | None = None,
        get_editor_info_cb: Callable[[], dict[str, Any]] | None = None,
        save_cb: Callable[[], bool] | None = None,
        on_goto_line: Callable[[int, int], None] | None = None,
        on_apply_diff: Callable[[str], dict[str, Any]] | None = None,
    ) -> None:
        """
        Initialise the editor context.

        Args:
            context_id: Stable identifier issued by the ContextRegistry.
            document: The ``EditorDocument`` model that owns the text content
                and file state.
            get_cursor_info_cb: Optional callable returning cursor position
                and selection info.  Provided by the Qt EditorWidget; a
                headless backend leaves this ``None``.
            get_selected_text_cb: Optional callable returning the currently
                selected text.  Provided by the Qt EditorWidget.
            get_editor_info_cb: Optional callable returning editor metadata
                (language, untitled_number, etc.).  Provided by the Qt
                EditorWidget.  When ``None``, a minimal dict is built from the
                document.
            save_cb: Optional callable that saves the document to disk and
                returns ``True`` on success.  When ``None``, the document's
                ``save_to_disk`` is used directly.
            on_goto_line: Optional callable(line, column) that scrolls the
                viewport to the target position.  A headless backend leaves
                this ``None`` and the method validates only.
            on_apply_diff: Optional callable(diff_text) -> result dict that
                applies the diff via the frontend widget (which also handles
                cursor and scroll).  When ``None``, the diff is applied
                directly to the ``EditorDocument``.
        """
        self._context_id = context_id
        self._document = document
        self._get_cursor_info_cb = get_cursor_info_cb
        self._get_selected_text_cb = get_selected_text_cb
        self._get_editor_info_cb = get_editor_info_cb
        self._save_cb = save_cb
        self._on_goto_line = on_goto_line
        self._on_apply_diff = on_apply_diff
        self._logger = logging.getLogger("EditorContext")

    def context_id(self) -> str:
        """Return the stable context identifier."""
        return self._context_id

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
        return self._document.get_text_range(start_line, end_line)

    def get_cursor_info(self) -> dict[str, Any]:
        """
        Return current cursor position and selection information.

        Delegates to the frontend callback if supplied.  In headless mode,
        returns a default cursor state (line 1, column 1, no selection).

        Returns:
            Dictionary with line, column, has_selection, and optional
            selection_start/end and selected_text keys.
        """
        if self._get_cursor_info_cb is not None:
            return self._get_cursor_info_cb()

        return {
            'line': 1,
            'column': 1,
            'has_selection': False,
        }

    def get_editor_info(self) -> dict[str, Any]:
        """
        Return editor metadata and document information.

        Delegates to the frontend callback if supplied.  In headless mode,
        builds a minimal dict from the ``EditorDocument``.

        Returns:
            Dictionary with line_count, language, language_id, encoding,
            is_modified, file_path, and untitled_number.
        """
        if self._get_editor_info_cb is not None:
            return self._get_editor_info_cb()

        return {
            'line_count': self._document.block_count(),
            'language': '',
            'language_id': '',
            'encoding': 'UTF-8',
            'is_modified': self._document.is_modified(),
            'file_path': self._document.path(),
            'untitled_number': None,
        }

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
        return self._document.find_all_occurrences(search_text, case_sensitive, regexp)

    def get_selected_text(self) -> str:
        """
        Return the currently selected text.

        Delegates to the frontend callback if supplied.  In headless mode,
        returns an empty string.

        Returns:
            Selected text, or empty string if no selection.
        """
        if self._get_selected_text_cb is not None:
            return self._get_selected_text_cb()

        return ''

    def get_diff(self, context_lines: int = 3) -> str:
        """
        Generate a unified diff between the saved file and the current buffer.

        Reads ``is_modified`` and ``file_path`` from the ``EditorDocument``
        so this works without a frontend callback.

        Args:
            context_lines: Number of context lines to include (default 3).

        Returns:
            Unified diff string, or empty string if there are no changes or
            the file has never been saved.
        """
        if not self._document.is_modified():
            return ''

        return self._document.get_diff(context_lines)

    def apply_diff(self, diff_text: str) -> dict[str, Any]:
        """
        Apply a unified diff to the editor content.

        This operation is atomic — either all hunks apply successfully or
        none do.

        If an ``on_apply_diff`` callback is supplied (frontend path), the
        callback applies the diff via the frontend widget and returns a
        result dict.  Otherwise the diff is applied directly to the
        ``EditorDocument`` (headless path).

        Args:
            diff_text: Unified diff format text.

        Returns:
            Dictionary with success, message, hunks_applied, and
            error_details keys.
        """
        if self._on_apply_diff is not None:
            return self._on_apply_diff(diff_text)

        try:
            result = self._document.apply_diff(diff_text)

        except (DiffParseError, DiffMatchError, DiffValidationError, DiffApplicationError) as e:
            error_details = getattr(e, 'error_details', None) or {
                'phase': 'diff_application',
                'reason': str(e),
            }
            return {
                'success': False,
                'message': str(e),
                'error_details': error_details,
            }

        return {
            'success': result.success,
            'message': result.message,
            'hunks_applied': result.hunks_applied,
            'error_details': result.error_details,
        }

    def save(self) -> bool:
        """
        Save the current editor content to file.

        Delegates to the frontend save callback if supplied.  In headless
        mode, saves directly via ``EditorDocument.save_to_disk``.

        Returns:
            True if the save was successful.
        """
        if self._save_cb is not None:
            return self._save_cb()

        try:
            self._document.save_to_disk()
            return True

        except OSError:
            return False

    def goto_line(self, line: int, column: int = 1) -> None:
        """
        Request that the frontend move the cursor to a specific line and column.

        Validates the position against the document, then fires the
        ``on_goto_line`` callback if one was supplied.  A headless backend
        leaves the callback as ``None`` and this method validates only.

        Args:
            line:   Target line number (1-indexed).
            column: Target column number (1-indexed, default 1).

        Raises:
            ValueError: If the line or column is out of range.
        """
        total_lines = self._document.block_count()

        if line < 1:
            raise ValueError(f"line must be >= 1, got {line}")

        if line > total_lines:
            raise ValueError(f"line ({line}) exceeds document length ({total_lines} lines)")

        if column < 1:
            raise ValueError(f"column must be >= 1, got {column}")

        line_text = self._document.get_line(line)
        line_length = len(line_text)

        if column > line_length + 1:
            raise ValueError(f"column ({column}) exceeds line length ({line_length})")

        if self._on_goto_line is not None:
            self._on_goto_line(line, column)
