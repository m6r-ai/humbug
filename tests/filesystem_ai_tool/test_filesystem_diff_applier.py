"""
Tests for FilesystemDiffApplier.
"""
from unittest.mock import patch

import pytest

from diff import DiffApplicationError
from filesystem_ai_tool.filesystem_diff_applier import FilesystemDiffApplier


class TestFilesystemDiffApplierApplicationError:
    """Test DiffApplicationError is raised when application fails after a successful match."""

    def test_delete_beyond_document_end_raises_application_error(self):
        """Test that FilesystemDiffApplier raises DiffApplicationError when a deletion
        targets a line beyond the end of the document."""
        applier = FilesystemDiffApplier()

        # The hunk matches at line 1 (both deletion lines exist at match time),
        # but we patch _get_document_lines on the matcher to report the document
        # as having 2 lines, then supply only 1 line to _apply_hunk so the second
        # deletion runs off the end.
        document = ["line 1", "line 2"]

        diff_text = """@@ -1,2 +1,0 @@
-line 1
-line 2
"""

        # Shrink the document to 1 line after matching succeeds, so the second
        # deletion in _apply_hunk hits the out-of-bounds guard.
        original_apply_hunk = applier._apply_hunk

        def truncating_apply_hunk(hunk, location, doc, context):
            doc.pop()  # Remove last line before application begins
            original_apply_hunk(hunk, location, doc, context)

        with patch.object(applier, '_apply_hunk', truncating_apply_hunk):
            with pytest.raises(DiffApplicationError) as exc_info:
                applier.apply_diff(diff_text, document)

        assert "Cannot delete line" in str(exc_info.value)


class TestFilesystemDiffApplierTrailingNewline:
    """Test trailing newline handling via the no-newline marker."""

    def test_add_trailing_newline(self):
        """A marker on the old side only adds a trailing newline to the result."""
        applier = FilesystemDiffApplier()

        original = "line1\nline2\nline3"  # no trailing newline
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
\\ No newline at end of file
+line3
"""
        result, modified = applier.apply_diff_to_file(diff_text, original)

        assert result.success
        assert modified == "line1\nline2\nline3\n"

    def test_remove_trailing_newline(self):
        """A marker on the new side only removes the trailing newline from the result."""
        applier = FilesystemDiffApplier()

        original = "line1\nline2\nline3\n"  # has trailing newline
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
+line3
\\ No newline at end of file
"""
        result, modified = applier.apply_diff_to_file(diff_text, original)

        assert result.success
        assert modified == "line1\nline2\nline3"

    def test_no_marker_preserves_trailing_newline(self):
        """Without a marker, the original trailing newline state is preserved."""
        applier = FilesystemDiffApplier()

        original = "line1\nline2\nline3\n"  # has trailing newline
        diff_text = """@@ -1,3 +1,3 @@
 line1
-line2
+lineX
 line3
"""
        result, modified = applier.apply_diff_to_file(diff_text, original)

        assert result.success
        assert modified == "line1\nlineX\nline3\n"

    def test_no_marker_preserves_absent_trailing_newline(self):
        """Without a marker, a file with no trailing newline stays without one."""
        applier = FilesystemDiffApplier()

        original = "line1\nline2\nline3"  # no trailing newline
        diff_text = """@@ -1,3 +1,3 @@
 line1
-line2
+lineX
 line3
"""
        result, modified = applier.apply_diff_to_file(diff_text, original)

        assert result.success
        assert modified == "line1\nlineX\nline3"

    def test_marker_both_sides_keeps_no_trailing_newline(self):
        """A marker on both sides means the result has no trailing newline."""
        applier = FilesystemDiffApplier()

        original = "line1\nline2\nline3"  # no trailing newline
        diff_text = """@@ -1,3 +1,3 @@
 line1
 line2
-line3
\\ No newline at end of file
+line3
\\ No newline at end of file
"""
        result, modified = applier.apply_diff_to_file(diff_text, original)

        assert result.success
        assert modified == "line1\nline2\nline3"
