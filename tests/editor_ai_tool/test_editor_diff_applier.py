"""Tests for EditorDiffApplier trailing newline and diff application."""
from PySide6.QtGui import QTextCursor, QTextDocument

from desktop.editor_tab.editor_diff_applier import EditorDiffApplier


def blocks_to_text(document: QTextDocument) -> str:
    """Reconstruct file content from blocks, preserving trailing newline state."""
    parts: list[str] = []
    block = document.firstBlock()
    while block.isValid():
        parts.append(block.text())
        block = block.next()

    # An empty trailing block represents a trailing newline
    if parts and parts[-1] == "":
        return "\n".join(parts[:-1]) + "\n"

    return "\n".join(parts)


class TestEditorDiffApplierTrailingNewline:
    """Test trailing newline handling in the Qt document model."""

    def test_add_trailing_newline(self, qapp):
        """A marker on the old side only adds a trailing newline."""
        document = QTextDocument()
        document.setPlainText("line1\nline2\nline3")

        applier = EditorDiffApplier()
        cursor = QTextCursor(document)
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
\\ No newline at end of file
+line3
"""
        result = applier.apply_diff(diff_text, document, cursor=cursor)

        assert result.success
        assert blocks_to_text(document) == "line1\nline2\nline3\n"

    def test_remove_trailing_newline(self, qapp):
        """A marker on the new side only removes the trailing newline."""
        document = QTextDocument()
        document.setPlainText("line1\nline2\nline3\n")

        applier = EditorDiffApplier()
        cursor = QTextCursor(document)
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
+line3
\\ No newline at end of file
"""
        result = applier.apply_diff(diff_text, document, cursor=cursor)

        assert result.success
        assert blocks_to_text(document) == "line1\nline2\nline3"

    def test_no_marker_preserves_trailing_newline(self, qapp):
        """Without a marker, the original trailing newline is preserved."""
        document = QTextDocument()
        document.setPlainText("line1\nline2\nline3\n")

        applier = EditorDiffApplier()
        cursor = QTextCursor(document)
        diff_text = """@@ -1,3 +1,3 @@
 line1
-line2
+lineX
 line3
"""
        result = applier.apply_diff(diff_text, document, cursor=cursor)

        assert result.success
        assert blocks_to_text(document) == "line1\nlineX\nline3\n"

    def test_no_marker_preserves_absent_trailing_newline(self, qapp):
        """Without a marker, a file with no trailing newline stays without one."""
        document = QTextDocument()
        document.setPlainText("line1\nline2\nline3")

        applier = EditorDiffApplier()
        cursor = QTextCursor(document)
        diff_text = """@@ -1,3 +1,3 @@
 line1
-line2
+lineX
 line3
"""
        result = applier.apply_diff(diff_text, document, cursor=cursor)

        assert result.success
        assert blocks_to_text(document) == "line1\nlineX\nline3"

    def test_marker_both_sides_keeps_no_trailing_newline(self, qapp):
        """A marker on both sides means the result has no trailing newline."""
        document = QTextDocument()
        document.setPlainText("line1\nline2\nline3")

        applier = EditorDiffApplier()
        cursor = QTextCursor(document)
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
\\ No newline at end of file
+line3
\\ No newline at end of file
"""
        result = applier.apply_diff(diff_text, document, cursor=cursor)

        assert result.success
        assert blocks_to_text(document) == "line1\nline2\nline3"
