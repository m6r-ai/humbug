"""Tests for diff data types."""

import pytest

from diff.diff_types import DiffLine, DiffHunk, MatchResult, DiffApplicationResult


class TestDiffLine:
    """Test DiffLine dataclass."""

    def test_create_context_line(self):
        """Test creating a context line."""
        line = DiffLine(' ', 'def foo():')
        assert line.type == ' '
        assert line.content == 'def foo():'

    def test_create_deletion_line(self):
        """Test creating a deletion line."""
        line = DiffLine('-', 'old content')
        assert line.type == '-'
        assert line.content == 'old content'

    def test_create_addition_line(self):
        """Test creating an addition line."""
        line = DiffLine('+', 'new content')
        assert line.type == '+'
        assert line.content == 'new content'

    def test_equality(self):
        """Test DiffLine equality."""
        line1 = DiffLine(' ', 'content')
        line2 = DiffLine(' ', 'content')
        line3 = DiffLine('-', 'content')
        line4 = DiffLine(' ', 'different')

        assert line1 == line2
        assert line1 != line3
        assert line1 != line4

    def test_empty_content(self):
        """Test DiffLine with empty content."""
        line = DiffLine('+', '')
        assert line.type == '+'
        assert line.content == ''


class TestDiffHunk:
    """Test DiffHunk dataclass."""

    def test_create_simple_hunk(self):
        """Test creating a simple hunk."""
        lines = [
            DiffLine(' ', 'context'),
            DiffLine('-', 'old'),
            DiffLine('+', 'new'),
        ]
        hunk = DiffHunk(10, 2, 10, 2, lines)

        assert hunk.old_start == 10
        assert hunk.old_count == 2
        assert hunk.new_start == 10
        assert hunk.new_count == 2
        assert len(hunk.lines) == 3

    def test_create_insertion_hunk(self):
        """Test creating an insertion-only hunk."""
        lines = [
            DiffLine('+', 'new line 1'),
            DiffLine('+', 'new line 2'),
        ]
        hunk = DiffHunk(10, 0, 10, 2, lines)

        assert hunk.old_count == 0
        assert hunk.new_count == 2
        assert all(line.type == '+' for line in hunk.lines)

    def test_create_deletion_hunk(self):
        """Test creating a deletion-only hunk."""
        lines = [
            DiffLine('-', 'old line 1'),
            DiffLine('-', 'old line 2'),
        ]
        hunk = DiffHunk(10, 2, 10, 0, lines)

        assert hunk.old_count == 2
        assert hunk.new_count == 0
        assert all(line.type == '-' for line in hunk.lines)

    def test_empty_hunk(self):
        """Test creating a hunk with no lines."""
        hunk = DiffHunk(1, 0, 1, 0, [])
        assert len(hunk.lines) == 0

    def test_equality(self):
        """Test DiffHunk equality."""
        lines1 = [DiffLine(' ', 'content')]
        lines2 = [DiffLine(' ', 'content')]
        lines3 = [DiffLine('-', 'content')]

        hunk1 = DiffHunk(10, 1, 10, 1, lines1)
        hunk2 = DiffHunk(10, 1, 10, 1, lines2)
        hunk3 = DiffHunk(10, 1, 10, 1, lines3)
        hunk4 = DiffHunk(20, 1, 20, 1, lines1)

        assert hunk1 == hunk2
        assert hunk1 != hunk3
        assert hunk1 != hunk4


class TestMatchResult:
    """Test MatchResult dataclass."""

    def test_create_successful_match(self):
        """Test creating a successful match result."""
        result = MatchResult(
            success=True,
            location=42,
            confidence=1.0,
            actual_lines=['line 1', 'line 2']
        )

        assert result.success is True
        assert result.location == 42
        assert result.confidence == 1.0
        assert result.actual_lines == ['line 1', 'line 2']

    def test_create_failed_match(self):
        """Test creating a failed match result."""
        result = MatchResult(
            success=False,
            location=10,
            confidence=0.5,
            actual_lines=[]
        )

        assert result.success is False
        assert result.location == 10
        assert result.confidence == 0.5
        assert result.actual_lines == []

    def test_partial_confidence(self):
        """Test match result with partial confidence."""
        result = MatchResult(
            success=False,
            location=15,
            confidence=0.73,
            actual_lines=['similar', 'but not exact']
        )

        assert result.success is False
        assert 0.0 <= result.confidence <= 1.0
        assert result.confidence == 0.73

    def test_equality(self):
        """Test MatchResult equality."""
        result1 = MatchResult(True, 10, 1.0, ['line'])
        result2 = MatchResult(True, 10, 1.0, ['line'])
        result3 = MatchResult(False, 10, 1.0, ['line'])

        assert result1 == result2
        assert result1 != result3


class TestDiffApplicationResult:
    """Test DiffApplicationResult dataclass."""

    def test_create_successful_result(self):
        """Test creating a successful application result."""
        result = DiffApplicationResult(
            success=True,
            message='Successfully applied 3 hunk(s)',
            hunks_applied=3
        )

        assert result.success is True
        assert 'Successfully' in result.message
        assert result.hunks_applied == 3
        assert result.error_details is None

    def test_create_failed_result(self):
        """Test creating a failed application result."""
        error_details = {
            'phase': 'matching',
            'failed_hunk': 2,
            'reason': 'Could not locate hunk'
        }
        result = DiffApplicationResult(
            success=False,
            message='Failed to apply diff',
            hunks_applied=1,
            error_details=error_details
        )

        assert result.success is False
        assert result.hunks_applied == 1
        assert result.error_details is not None
        assert result.error_details['phase'] == 'matching'
        assert result.error_details['failed_hunk'] == 2

    def test_default_values(self):
        """Test default values for optional fields."""
        result = DiffApplicationResult(
            success=True,
            message='Test'
        )

        assert result.hunks_applied == 0
        assert result.error_details is None

    def test_error_details_structure(self):
        """Test various error detail structures."""
        # Matching error
        match_error = {
            'phase': 'matching',
            'failed_hunk': 1,
            'expected_location': 10,
            'best_match': {'location': 12, 'confidence': 0.6}
        }
        result1 = DiffApplicationResult(False, 'Match failed', error_details=match_error)
        assert result1.error_details['phase'] == 'matching'

        # Validation error
        validation_error = {
            'phase': 'validation',
            'reason': 'Overlapping hunks'
        }
        result2 = DiffApplicationResult(False, 'Validation failed', error_details=validation_error)
        assert result2.error_details['phase'] == 'validation'

    def test_equality(self):
        """Test DiffApplicationResult equality."""
        result1 = DiffApplicationResult(True, 'Success', 3)
        result2 = DiffApplicationResult(True, 'Success', 3)
        result3 = DiffApplicationResult(False, 'Success', 3)

        assert result1 == result2
        assert result1 != result3
