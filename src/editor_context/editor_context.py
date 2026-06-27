"""Editor context model."""

from collections.abc import Callable
from difflib import unified_diff
import logging
from typing import Any

from PySide6.QtCore import QRegularExpression
from PySide6.QtGui import QTextCursor, QTextDocument

from diff import DiffParseError, DiffMatchError, DiffValidationError, DiffApplicationError
from editor_context.editor_diff_applier import EditorDiffApplier


class EditorContext:
    """
    Model-layer context for an open editor tab.

    Owns the QTextDocument and implements the data operations the AI tool
    layer needs: reading, searching, diffing, and applying diffs.

    Operations that depend on the widget's cursor or viewport (get_cursor_info,
    get_selected_text, goto_line, save) are delegated to callbacks supplied by
    EditorWidget at construction time.  A headless test harness can supply
    stubs; a CLI would supply None for the viewport callbacks.
    """

    def __init__(
        self,
        context_id: str,
        document: QTextDocument,
        get_cursor_info_cb: Callable[[], dict[str, Any]],
        get_selected_text_cb: Callable[[], str],
        get_editor_info_cb: Callable[[], dict[str, Any]],
        save_cb: Callable[[], bool],
        on_goto_line: Callable[[int, int], None] | None = None,
        on_apply_diff: Callable[[str], dict[str, Any]] | None = None,
    ) -> None:
        """
        Initialise the editor context.

        Args:
            context_id: Stable identifier issued by the ContextRegistry.
            document: The shared QTextDocument.  EditorWidget calls
                setDocument(document) so both share the same backing store.
            get_cursor_info_cb: Callable() -> dict, provided by EditorWidget.
            get_selected_text_cb: Callable() -> str, provided by EditorWidget.
            get_editor_info_cb: Callable() -> dict, provided by EditorWidget.
            save_cb: Callable() -> bool, provided by EditorWidget.
            on_goto_line: Optional callable(line, column) that scrolls the Qt
                viewport to the target position.  The Qt EditorWidget supplies
                this; a CLI would pass None.
            on_apply_diff: Optional callable(diff_text) -> result dict that
                applies the diff via the Qt EditorWidget (which also handles
                scrolling).  The Qt EditorWidget supplies this; a CLI would pass None.
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
        if start_line is None and end_line is None:
            return self._document.toPlainText()

        total_lines = self._document.blockCount()

        if start_line is None:
            start_line = 1

        if end_line is None:
            end_line = total_lines

        if start_line < 1:
            raise ValueError(f"start_line must be >= 1, got {start_line}")

        if end_line < start_line:
            raise ValueError(f"end_line ({end_line}) must be >= start_line ({start_line})")

        if start_line > total_lines:
            raise ValueError(
                f"start_line ({start_line}) exceeds document length ({total_lines} lines)"
            )

        end_line = min(end_line, total_lines)

        start_block = self._document.findBlockByLineNumber(start_line - 1)
        end_block = self._document.findBlockByLineNumber(end_line - 1)

        if not start_block.isValid() or not end_block.isValid():
            raise ValueError("Invalid line range")

        cursor = QTextCursor(start_block)
        cursor.setPosition(
            end_block.position() + end_block.length() - 1,
            QTextCursor.MoveMode.KeepAnchor,
        )

        text = cursor.selectedText()
        return text.replace('\u2029', '\n')

    def get_cursor_info(self) -> dict[str, Any]:
        """
        Return current cursor position and selection information.

        Delegates to the EditorWidget callback so the live cursor state is
        reflected.

        Returns:
            Dictionary with line, column, has_selection, and optional
            selection_start/end and selected_text keys.
        """
        return self._get_cursor_info_cb()

    def get_editor_info(self) -> dict[str, Any]:
        """
        Return editor metadata and document information.

        Delegates to the EditorWidget callback so live state (is_modified,
        file_path, syntax) is reflected.

        Returns:
            Dictionary with line_count, language, language_id, encoding,
            is_modified, file_path, and untitled_number.
        """
        return self._get_editor_info_cb()

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
        find_flags = QTextDocument.FindFlag(0)
        if case_sensitive:
            find_flags |= QTextDocument.FindFlag.FindCaseSensitively

        if regexp:
            pattern_flags = QRegularExpression.PatternOption(0)
            if not case_sensitive:
                pattern_flags |= QRegularExpression.PatternOption.CaseInsensitiveOption

            pattern = QRegularExpression(search_text, pattern_flags)
            if not pattern.isValid():
                raise ValueError(f"Invalid regular expression: {pattern.errorString()}")

            cursor = QTextCursor(self._document)
            while True:
                cursor = self._document.find(pattern, cursor, find_flags)
                if cursor.isNull():
                    break

                matches.append({
                    'line': cursor.blockNumber() + 1,
                    'column': cursor.columnNumber() + 1,
                    'match_text': cursor.selectedText(),
                    'context': cursor.block().text(),
                })

        else:
            cursor = QTextCursor(self._document)
            while True:
                cursor = self._document.find(search_text, cursor, find_flags)
                if cursor.isNull():
                    break

                matches.append({
                    'line': cursor.blockNumber() + 1,
                    'column': cursor.columnNumber() + 1,
                    'match_text': cursor.selectedText(),
                    'context': cursor.block().text(),
                })

        return matches

    def get_selected_text(self) -> str:
        """
        Return the currently selected text.

        Delegates to the EditorWidget callback so the live selection is
        reflected.

        Returns:
            Selected text, or empty string if no selection.
        """
        return self._get_selected_text_cb()

    def get_diff(self, context_lines: int = 3) -> str:
        """
        Generate a unified diff between the saved file and the current buffer.

        Delegates to the EditorWidget callback for live state (path,
        last_save_content, is_modified).

        Args:
            context_lines: Number of context lines to include (default 3).

        Returns:
            Unified diff string, or empty string if there are no changes or
            the file has never been saved.
        """
        info = self._get_editor_info_cb()
        file_path = info.get('file_path', '')
        is_modified = info.get('is_modified', False)

        if not is_modified:
            return ''

        current_content = self._document.toPlainText()

        # Read the saved content from disk to diff against
        if not file_path:
            return ''

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                saved_content = f.read()

        except OSError:
            return ''

        if current_content == saved_content:
            return ''

        saved_lines = saved_content.splitlines(keepends=True)
        current_lines = current_content.splitlines(keepends=True)
        diff_lines = unified_diff(
            saved_lines,
            current_lines,
            fromfile=f"a/{file_path}",
            tofile=f"b/{file_path}",
            n=context_lines,
        )
        return ''.join(diff_lines)

    def apply_diff(self, diff_text: str) -> dict[str, Any]:
        """
        Apply a unified diff to the editor content.

        This operation is atomic — either all hunks apply successfully or
        none do.

        Args:
            diff_text: Unified diff format text.

        Returns:
            Dictionary with success, message, hunks_applied, and
            error_details keys.
        """
        if self._on_apply_diff is not None:
            return self._on_apply_diff(diff_text)

        diff_applier = EditorDiffApplier(confidence_threshold=0.75, search_window=50)
        cursor = QTextCursor(self._document)

        try:
            result = diff_applier.apply_diff(diff_text, self._document, cursor=cursor)

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

        Delegates to the EditorWidget save callback.

        Returns:
            True if the save was successful.
        """
        return self._save_cb()

    def goto_line(self, line: int, column: int = 1) -> None:
        """
        Request that the frontend move the cursor to a specific line and column.

        Fires the on_goto_line callback if one was supplied.  A CLI frontend
        would supply None and this becomes a no-op.

        Args:
            line:   Target line number (1-indexed).
            column: Target column number (1-indexed, default 1).

        Raises:
            ValueError: If the line or column is out of range.
        """
        total_lines = self._document.blockCount()

        if line < 1:
            raise ValueError(f"line must be >= 1, got {line}")

        if line > total_lines:
            raise ValueError(f"line ({line}) exceeds document length ({total_lines} lines)")

        if column < 1:
            raise ValueError(f"column must be >= 1, got {column}")

        target_block = self._document.findBlockByLineNumber(line - 1)
        if not target_block.isValid():
            raise ValueError(f"Invalid line number: {line}")

        line_length = target_block.length() - 1
        if column > line_length + 1:
            raise ValueError(f"column ({column}) exceeds line length ({line_length})")

        if self._on_goto_line is not None:
            self._on_goto_line(line, column)
