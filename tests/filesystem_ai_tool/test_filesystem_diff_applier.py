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
