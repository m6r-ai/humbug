"""Tests for diff applier."""

import pytest

from diff.diff_applier import DiffApplier
from diff.diff_exceptions import DiffMatchError, DiffValidationError, DiffParseError
from diff.diff_types import DiffApplicationResult


class TestDiffApplierBasic:
    """Test basic diff application functionality."""

    def test_apply_simple_replacement(self, simple_applier, helpers):
        """Test applying a simple replacement diff."""
        document = helpers.create_simple_document([
            "line 1",
            "old line",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-old line
+new line
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 1
        assert document == ["line 1", "new line", "line 3"]

    def test_apply_insertion(self, simple_applier, helpers):
        """Test applying an insertion diff."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2"
        ])

        diff_text = """@@ -2,0 +2,1 @@
+new line
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 1
        assert document == ["line 1", "new line", "line 2"]

    def test_apply_deletion(self, simple_applier, helpers):
        """Test applying a deletion diff."""
        document = helpers.create_simple_document([
            "line 1",
            "line to delete",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,0 @@
-line to delete
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 1
        assert document == ["line 1", "line 3"]

    def test_apply_with_context(self, simple_applier, helpers):
        """Test applying diff with context lines."""
        document = helpers.create_simple_document([
            "context 1",
            "old line",
            "context 2"
        ])

        diff_text = """@@ -1,3 +1,3 @@
 context 1
-old line
+new line
 context 2
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document == ["context 1", "new line", "context 2"]

    def test_apply_multiple_hunks(self, simple_applier, helpers):
        """Test applying diff with multiple hunks."""
        document = helpers.create_simple_document([
            "line 1",
            "old 1",
            "line 3",
            "line 4",
            "old 2",
            "line 6"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-old 1
+new 1
@@ -5,1 +5,1 @@
-old 2
+new 2
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 2
        assert document == ["line 1", "new 1", "line 3", "line 4", "new 2", "line 6"]


class TestDiffApplierDryRun:
    """Test dry run functionality."""

    def test_dry_run_validates_without_applying(self, simple_applier, helpers):
        """Test dry run validates but doesn't apply changes."""
        document = helpers.create_simple_document([
            "line 1",
            "old line",
            "line 3"
        ])
        original_document = document.copy()

        diff_text = """@@ -2,1 +2,1 @@
-old line
+new line
"""

        result = simple_applier.apply_diff(diff_text, document, dry_run=True)

        assert result.success is True
        assert result.hunks_applied == 1
        assert "validation successful" in result.message.lower()
        # Document should be unchanged
        assert document == original_document

    def test_dry_run_detects_match_failure(self, simple_applier, helpers):
        """Test dry run detects match failures."""
        document = helpers.create_simple_document([
            "line 1",
            "different line",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-old line
+new line
"""

        with pytest.raises(DiffMatchError):
            simple_applier.apply_diff(diff_text, document, dry_run=True)

        # Document should be unchanged
        assert document == ["line 1", "different line", "line 3"]

    def test_dry_run_detects_overlapping_hunks(self, simple_applier, helpers):
        """Test dry run detects overlapping hunks."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3",
            "line 4"
        ])

        # Overlapping hunks
        diff_text = """@@ -2,2 +2,2 @@
-line 2
-line 3
+new 2
+new 3
@@ -3,2 +3,2 @@
-line 3
-line 4
+new 3
+new 4
"""

        with pytest.raises(DiffValidationError):
            simple_applier.apply_diff(diff_text, document, dry_run=True)


class TestDiffApplierErrorHandling:
    """Test error handling."""

    def test_parse_error_on_invalid_diff(self, simple_applier, helpers):
        """Test that invalid diff raises parse error."""
        document = helpers.create_simple_document(["line 1"])

        diff_text = """@@ invalid @@
-old
+new
"""

        with pytest.raises(DiffParseError):
            simple_applier.apply_diff(diff_text, document)

    def test_match_error_on_missing_context(self, simple_applier, helpers):
        """Test that missing context raises match error."""
        document = helpers.create_simple_document([
            "line 1",
            "completely different",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-expected line
+new line
"""

        with pytest.raises(DiffMatchError) as exc_info:
            simple_applier.apply_diff(diff_text, document)

        # Check error details
        error = exc_info.value
        assert error.error_details is not None
        assert error.error_details['phase'] == 'matching'
        assert error.error_details['failed_hunk'] == 1

    def test_match_error_includes_suggestions(self, simple_applier, helpers):
        """Test that match error includes helpful suggestions."""
        document = helpers.create_simple_document([
            "line 1",
            "different content",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-old content
+new content
"""

        with pytest.raises(DiffMatchError) as exc_info:
            simple_applier.apply_diff(diff_text, document)

        error = exc_info.value
        assert 'suggestion' in error.error_details
        assert 'context' in error.error_details['suggestion'].lower()

    def test_validation_error_on_overlapping_hunks(self, simple_applier, helpers):
        """Test that overlapping hunks raise validation error."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3",
            "line 4",
            "line 5"
        ])

        # Create overlapping hunks
        diff_text = """@@ -2,3 +2,3 @@
-line 2
-line 3
-line 4
+new 2
+new 3
+new 4
@@ -4,2 +4,2 @@
-line 4
-line 5
+new 4
+new 5
"""

        with pytest.raises(DiffValidationError) as exc_info:
            simple_applier.apply_diff(diff_text, document)

        error = exc_info.value
        assert error.error_details['phase'] == 'validation'
        assert 'overlapping' in error.error_details['reason'].lower()

    def test_empty_diff_raises_error(self, simple_applier, helpers):
        """Test that empty diff raises error."""
        document = helpers.create_simple_document(["line 1"])

        with pytest.raises(DiffParseError, match="Empty diff"):
            simple_applier.apply_diff("", document)


class TestDiffApplierFuzzyMatching:
    """Test fuzzy matching during application."""

    def test_apply_with_fuzzy_match(self, simple_applier, helpers):
        """Test applying diff with fuzzy matching."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3",
            "target line",  # Actually at line 4, not expected line 2
            "line 5"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-target line
+modified line
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document[3] == "modified line"  # Applied at actual location

    def test_apply_with_whitespace_tolerance(self, simple_applier, helpers):
        """Test applying diff with whitespace differences."""
        document = helpers.create_simple_document([
            "line 1",
            "  line 2  ",  # Extra whitespace
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-line 2
+new line 2
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert "new line 2" in document

    def test_fuzzy_match_within_search_window(self, simple_applier_custom, helpers):
        """Test fuzzy matching within configured search window."""
        applier = simple_applier_custom(search_window=10)

        document = helpers.create_simple_document(
            ["line " + str(i) for i in range(1, 21)]
        )
        document[14] = "target line"  # At line 15

        diff_text = """@@ -10,1 +10,1 @@
-target line
+modified line
"""

        result = applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document[14] == "modified line"

    def test_fuzzy_match_outside_search_window_fails(self, simple_applier_custom, helpers):
        """Test fuzzy matching fails outside search window."""
        applier = simple_applier_custom(search_window=2)

        document = helpers.create_simple_document(
            ["line " + str(i) for i in range(1, 21)]
        )
        document[14] = "target line"  # At line 15, far from expected line 5

        diff_text = """@@ -5,1 +5,1 @@
-target line
+modified line
"""

        with pytest.raises(DiffMatchError):
            applier.apply_diff(diff_text, document)


class TestDiffApplierHunkOrdering:
    """Test hunk ordering and overlap detection."""

    def test_hunks_applied_bottom_to_top(self, simple_applier, helpers):
        """Test that hunks are applied from bottom to top."""
        document = helpers.create_simple_document([
            "line 1",
            "old 1",
            "line 3",
            "old 2",
            "line 5"
        ])

        # Hunks in top-to-bottom order in diff
        diff_text = """@@ -2,1 +2,1 @@
-old 1
+new 1
@@ -4,1 +4,1 @@
-old 2
+new 2
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        # Both hunks should be applied correctly
        assert document == ["line 1", "new 1", "line 3", "new 2", "line 5"]

    def test_overlapping_hunks_detected(self, simple_applier, helpers):
        """Test that overlapping hunks are detected."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3",
            "line 4"
        ])

        # Hunks that overlap
        diff_text = """@@ -2,2 +2,2 @@
-line 2
-line 3
+new 2
+new 3
@@ -3,2 +3,2 @@
-line 3
-line 4
+new 3
+new 4
"""

        with pytest.raises(DiffValidationError) as exc_info:
            simple_applier.apply_diff(diff_text, document)

        assert 'overlap' in str(exc_info.value).lower()

    def test_adjacent_hunks_allowed(self, simple_applier, helpers):
        """Test that adjacent (non-overlapping) hunks are allowed."""
        document = helpers.create_simple_document([
            "line 1",
            "old 1",
            "old 2",
            "line 4"
        ])

        # Adjacent but non-overlapping hunks
        diff_text = """@@ -2,1 +2,1 @@
-old 1
+new 1
@@ -3,1 +3,1 @@
-old 2
+new 2
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document == ["line 1", "new 1", "new 2", "line 4"]


class TestDiffApplierComplexScenarios:
    """Test complex application scenarios."""

    def test_apply_multiple_operations(self, simple_applier, helpers):
        """Test applying diff with multiple operation types."""
        document = helpers.create_simple_document([
            "line 1",
            "to delete",
            "line 3",
            "to replace",
            "line 5"
        ])

        diff_text = """@@ -2,1 +2,0 @@
-to delete
@@ -4,1 +3,2 @@
-to replace
+replacement
+insertion
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 2
        expected = ["line 1", "line 3", "replacement", "insertion", "line 5"]
        assert document == expected

    def test_apply_to_large_document(self, simple_applier, helpers):
        """Test applying diff to large document."""
        document = helpers.create_simple_document(
            ["line " + str(i) for i in range(1, 101)]
        )

        diff_text = """@@ -50,1 +50,1 @@
-line 50
+modified line 50
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document[49] == "modified line 50"

    def test_apply_with_empty_lines(self, simple_applier, helpers):
        """Test applying diff with empty lines."""
        document = helpers.create_simple_document([
            "line 1",
            "",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-
+new line
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document == ["line 1", "new line", "line 3"]

    def test_apply_preserves_whitespace(self, simple_applier, helpers):
        """Test that application preserves whitespace."""
        document = helpers.create_simple_document([
            "line 1",
            "    indented",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-    indented
+    new indented
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document[1] == "    new indented"

    def test_atomic_application(self, simple_applier, helpers):
        """Test that application is atomic (all or nothing)."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3"
        ])
        original_document = document.copy()

        # Second hunk will fail to match
        diff_text = """@@ -1,1 +1,1 @@
-line 1
+new line 1
@@ -2,1 +2,1 @@
-nonexistent line
+new line 2
"""

        with pytest.raises(DiffMatchError):
            simple_applier.apply_diff(diff_text, document)

        # Document should be unchanged (atomic failure)
        assert document == original_document


class TestDiffApplierConfiguration:
    """Test applier configuration."""

    def test_custom_confidence_threshold(self, simple_applier_custom, helpers):
        """Test custom confidence threshold."""
        applier = simple_applier_custom(confidence_threshold=0.95)

        document = helpers.create_simple_document([
            "similar but not exact line"
        ])

        diff_text = """@@ -1,1 +1,1 @@
-similar but not exact
+new line
"""

        # Should fail with high confidence threshold
        with pytest.raises(DiffMatchError):
            applier.apply_diff(diff_text, document)

    def test_lenient_confidence_threshold(self, simple_applier_custom, helpers):
        """Test lenient confidence threshold allows fuzzy matches."""
        applier = simple_applier_custom(confidence_threshold=0.75)

        document = helpers.create_simple_document([
            "line 1",
            "target line",  # At line 2, but diff expects at line 5
            "line 3"
        ])

        # Diff expects target at line 5, but it's actually at line 2
        diff_text = """@@ -5,1 +5,1 @@
-target line
+new line
"""

        # Should succeed - fuzzy matching finds it at line 2 with exact match (confidence 1.0)
        result = applier.apply_diff(diff_text, document)
        assert result.success is True
        assert document[1] == "new line"


class TestDiffApplierResultMessages:
    """Test result messages and reporting."""

    def test_success_message_includes_hunk_count(self, simple_applier, helpers):
        """Test success message includes hunk count."""
        document = helpers.create_simple_document([
            "old 1",
            "old 2",
            "old 3"
        ])

        diff_text = """@@ -1,1 +1,1 @@
-old 1
+new 1
@@ -2,1 +2,1 @@
-old 2
+new 2
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert "2" in result.message
        assert result.hunks_applied == 2

    def test_match_error_includes_details(self, simple_applier, helpers):
        """Test match error includes detailed information."""
        document = helpers.create_simple_document([
            "line 1",
            "different",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-expected
+new
"""

        with pytest.raises(DiffMatchError) as exc_info:
            simple_applier.apply_diff(diff_text, document)

        error = exc_info.value
        assert error.error_details['failed_hunk'] == 1
        assert error.error_details['total_hunks'] == 1
        assert 'expected_context' in error.error_details
        assert 'best_match' in error.error_details

    def test_validation_error_includes_details(self, simple_applier, helpers):
        """Test validation error includes detailed information."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3"
        ])

        diff_text = """@@ -1,2 +1,2 @@
-line 1
-line 2
+new 1
+new 2
@@ -2,2 +2,2 @@
-line 2
-line 3
+new 2
+new 3
"""

        with pytest.raises(DiffValidationError) as exc_info:
            simple_applier.apply_diff(diff_text, document)

        error = exc_info.value
        assert 'hunk1_range' in error.error_details
        assert 'hunk2_range' in error.error_details
