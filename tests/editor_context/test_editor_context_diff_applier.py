"""
Tests for EditorContextDiffMatcher and EditorContextDiffApplier.
"""

import pytest

from diff import DiffMatchError, DiffValidationError
from editor_context.editor_context_diff_applier import EditorContextDiffApplier, EditorContextDiffMatcher


def apply_diff(document: list[str], diff_text: str) -> list[str]:
    """Apply a diff to a list[str] document and return the result."""
    applier = EditorContextDiffApplier(confidence_threshold=0.75, search_window=50)
    applier.apply_diff(diff_text, document)
    return document


class TestEditorContextDiffApplierBasic:
    """Test basic diff application."""

    def test_apply_simple_addition(self):
        """A single line is added between two existing lines."""
        doc = ["a", "c"]
        diff = """@@ -1,2 +1,3 @@
 a
+b
 c
"""
        result = apply_diff(doc, diff)
        assert result == ["a", "b", "c"]

    def test_apply_simple_deletion(self):
        """A single line is removed."""
        doc = list("abcd")
        diff = """@@ -1,2 +1,1 @@
 a
-b
 c
"""
        result = apply_diff(doc, diff)
        assert result == list("acd")

    def test_apply_simple_replacement(self):
        """A single line is replaced."""
        doc = list("abc")
        diff = """@@ -1,3 +1,3 @@
 a
-b
+c
"""
        result = apply_diff(doc, diff)
        assert result == list("acc")

    def test_apply_multiple_hunks(self):
        """Multiple non-overlapping hunks are applied."""
        doc = list("abcdef")
        diff = """@@ -1,2 +1,2 @@
 a
-b
+B
@@ -5,2 +5,2 @@
 e
-f
+F
"""
        result = apply_diff(doc, diff)
        assert result == ["a", "B", "c", "d", "e", "F"]


class TestEditorContextDiffApplierTrailingNewline:
    """Test trailing newline handling, mirroring the Qt tests."""

    def test_add_trailing_newline(self):
        """A marker on the old side only adds a trailing newline."""
        doc = list("line1\nline2\nline3".split('\n'))  # ["line1", "line2", "line3"]
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
\\ No newline at end of file
+line3
"""
        result = apply_diff(doc, diff_text)
        assert '\n'.join(result) == "line1\nline2\nline3\n"

    def test_remove_trailing_newline(self):
        """A marker on the new side only removes the trailing newline."""
        doc = list("line1\nline2\nline3\n".split('\n'))  # ["line1", "line2", "line3", ""]
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
+line3
\\ No newline at end of file
"""
        result = apply_diff(doc, diff_text)
        assert '\n'.join(result) == "line1\nline2\nline3"

    def test_no_marker_preserves_trailing_newline(self):
        """Without a marker, the original trailing newline is preserved."""
        doc = list("line1\nline2\nline3\n".split('\n'))  # ["line1", "line2", "line3", ""]
        diff_text = """@@ -1,3 +1,3 @@
 line1
-line2
+lineX
 line3
"""
        result = apply_diff(doc, diff_text)
        assert '\n'.join(result) == "line1\nlineX\nline3\n"

    def test_no_marker_preserves_absent_trailing_newline(self):
        """Without a marker, a file with no trailing newline stays without one."""
        doc = list("line1\nline2\nline3".split('\n'))  # ["line1", "line2", "line3"]
        diff_text = """@@ -1,3 +1,3 @@
 line1
-line2
+lineX
 line3
"""
        result = apply_diff(doc, diff_text)
        assert '\n'.join(result) == "line1\nlineX\nline3"

    def test_marker_both_sides_keeps_no_trailing_newline(self):
        """A marker on both sides means the result has no trailing newline."""
        doc = list("line1\nline2\nline3".split('\n'))  # ["line1", "line2", "line3"]
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
\\ No newline at end of file
+line3
\\ No newline at end of file
"""
        result = apply_diff(doc, diff_text)
        assert '\n'.join(result) == "line1\nline2\nline3"


class TestEditorContextDiffApplierDryRun:
    """Test dry-run mode."""

    def test_dry_run_does_not_modify(self):
        """Dry run validates without modifying the document."""
        doc = list("abc")
        diff = """@@ -1,3 +1,3 @@
 a
-b
+B
 c
"""
        applier = EditorContextDiffApplier()
        result = applier.apply_diff(diff, doc, dry_run=True)
        assert result.success
        assert result.hunks_applied == 1
        assert doc == list("abc")  # unchanged


class TestEditorContextDiffApplierEmptyFile:
    """Test diffing against an empty document."""

    def test_apply_to_empty_file(self):
        """A diff that adds all lines to an empty file."""
        doc: list[str] = []
        diff = """@@ -0,0 +1,2 @@
+line1
+line2
"""
        result = apply_diff(doc, diff)
        assert '\n'.join(result) == "line1\nline2"


class TestEditorContextDiffApplierOverlapping:
    """Test overlap detection."""

    def test_overlapping_hunks_raise(self):
        """Overlapping hunks raise DiffValidationError."""
        doc = list("abcde")
        diff = """@@ -1,3 +1,3 @@
 a
-b
+B
 c
@@ -2,3 +2,3 @@
 b
-c
+C
 d
"""
        applier = EditorContextDiffApplier()
        with pytest.raises(DiffValidationError):
            applier.apply_diff(diff, doc)


class TestEditorContextDiffMatcher:
    """Test EditorContextDiffMatcher line counting."""

    def test_line_count_no_trailing_newline(self):
        """Line count excludes trailing newline sentinel."""
        matcher = EditorContextDiffMatcher()
        assert matcher._get_document_line_count(["a", "b", "c"]) == 3

    def test_line_count_with_trailing_newline(self):
        """Trailing empty string is not counted as a line."""
        matcher = EditorContextDiffMatcher()
        assert matcher._get_document_line_count(["a", "b", "c", ""]) == 3

    def test_line_count_empty(self):
        """Empty list has zero lines."""
        matcher = EditorContextDiffMatcher()
        assert matcher._get_document_line_count([]) == 0

    def test_line_count_single_empty_line(self):
        """A single empty string is one line."""
        matcher = EditorContextDiffMatcher()
        assert matcher._get_document_line_count([""]) == 1

    def test_get_document_lines(self):
        """_get_document_lines returns a slice of the list."""
        matcher = EditorContextDiffMatcher()
        doc = ["a", "b", "c", "d", "e"]
        assert matcher._get_document_lines(doc, 1, 3) == ["a", "b", "c"]
        assert matcher._get_document_lines(doc, 3, 2) == ["c", "d"]

    def test_get_document_lines_out_of_range(self):
        """_get_document_lines returns fewer lines if document is shorter."""
        matcher = EditorContextDiffMatcher()
        doc = ["a", "b"]
        assert matcher._get_document_lines(doc, 1, 10) == ["a", "b"]
