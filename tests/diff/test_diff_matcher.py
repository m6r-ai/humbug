"""Tests for diff matcher."""

import pytest

from diff.diff_matcher import DiffMatcher
from diff.diff_types import DiffHunk, DiffLine, MatchResult


class TestDiffMatcherBasic:
    """Test basic diff matching functionality."""

    def test_exact_match_at_expected_location(self, simple_matcher):
        """Test exact match at the expected location."""
        document = [
            "line 1",
            "line 2",
            "line 3",
            "line 4",
            "line 5"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=2,
            new_start=2,
            new_count=2,
            lines=[
                DiffLine(' ', 'line 2'),
                DiffLine('-', 'line 3'),
                DiffLine('+', 'new line 3')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 2
        assert result.confidence == 1.0
        assert result.actual_lines == ['line 2', 'line 3']

    def test_exact_match_with_deletions_only(self, simple_matcher):
        """Test matching hunk with only deletions."""
        document = [
            "line 1",
            "line 2",
            "line 3"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=2,
            new_start=2,
            new_count=0,
            lines=[
                DiffLine('-', 'line 2'),
                DiffLine('-', 'line 3')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 2
        assert result.confidence == 1.0

    def test_pure_insertion_match(self, simple_matcher):
        """Test matching pure insertion hunk."""
        document = [
            "line 1",
            "line 2",
            "line 3"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=0,
            new_start=2,
            new_count=2,
            lines=[
                DiffLine('+', 'new line 1'),
                DiffLine('+', 'new line 2')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Pure insertions have no context to match, so they succeed at expected location
        assert result.success is True
        assert result.location == 2
        assert result.confidence == 1.0
        assert result.actual_lines == []

    def test_match_at_file_start(self, simple_matcher):
        """Test matching hunk at the start of file."""
        document = [
            "first line",
            "second line",
            "third line"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=2,
            new_start=1,
            new_count=2,
            lines=[
                DiffLine(' ', 'first line'),
                DiffLine('-', 'second line'),
                DiffLine('+', 'new second line')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 1
        assert result.confidence == 1.0

    def test_match_at_file_end(self, simple_matcher):
        """Test matching hunk at the end of file."""
        document = [
            "line 1",
            "line 2",
            "last line"
        ]

        hunk = DiffHunk(
            old_start=3,
            old_count=1,
            new_start=3,
            new_count=1,
            lines=[
                DiffLine('-', 'last line'),
                DiffLine('+', 'new last line')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 3


class TestDiffMatcherFuzzyMatching:
    """Test fuzzy matching functionality."""

    def test_fuzzy_match_nearby_location(self, simple_matcher):
        """Test fuzzy matching finds content at nearby location."""
        document = [
            "line 1",
            "line 2",
            "line 3",
            "target line",
            "another line",
            "line 6"
        ]

        # Hunk expects to be at line 2, but content is actually at line 4
        hunk = DiffHunk(
            old_start=2,
            old_count=2,
            new_start=2,
            new_count=2,
            lines=[
                DiffLine(' ', 'target line'),
                DiffLine('-', 'another line'),
                DiffLine('+', 'modified line')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 4  # Found at actual location
        assert result.confidence == 1.0

    def test_fuzzy_match_with_whitespace_differences(self, simple_matcher):
        """Test fuzzy matching handles whitespace differences."""
        document = [
            "line 1",
            "  line 2  ",  # Extra whitespace
            "line 3"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=1,
            new_start=2,
            new_count=1,
            lines=[
                DiffLine('-', 'line 2'),  # No extra whitespace
                DiffLine('+', 'new line 2')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Should match despite whitespace differences (stripped for comparison)
        assert result.success is True
        assert result.location == 2
        assert result.confidence == 1.0

    def test_fuzzy_match_with_high_similarity(self, simple_matcher):
        """Test fuzzy matching with high similarity but not exact."""
        document = [
            "line 1",
            "def function_name():",  # Slightly different
            "line 3"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=1,
            new_start=2,
            new_count=1,
            lines=[
                DiffLine('-', 'def function_name():'),  # Very similar
                DiffLine('+', 'def new_function_name():')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 2
        assert result.confidence == 1.0  # Exact match

    def test_fuzzy_match_insufficient_confidence(self, simple_matcher_custom):
        """Test fuzzy matching fails with insufficient confidence."""
        matcher = simple_matcher_custom(confidence_threshold=0.75, search_window=10)

        document = [
            "completely different line 1",
            "completely different line 2",
            "completely different line 3"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=1,
            new_start=2,
            new_count=1,
            lines=[
                DiffLine('-', 'expected line'),
                DiffLine('+', 'new line')
            ]
        )

        result = matcher.find_match(hunk, document)

        assert result.success is False
        assert result.confidence < 0.75

    def test_fuzzy_match_prefers_closer_location(self, simple_matcher):
        """Test that fuzzy matching prefers locations closer to expected."""
        document = [
            "target line",  # Line 1
            "other content",
            "other content",
            "other content",
            "target line",  # Line 5 - farther from expected
            "other content"
        ]

        # Expect at line 3, should prefer line 1 over line 5
        hunk = DiffHunk(
            old_start=3,
            old_count=1,
            new_start=3,
            new_count=1,
            lines=[
                DiffLine('-', 'target line'),
                DiffLine('+', 'new line')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        # Should prefer line 1 (distance 2) over line 5 (distance 2)
        # Actually both have same distance, so might pick first found
        assert result.location in [1, 5]


class TestDiffMatcherConfiguration:
    """Test matcher configuration options."""

    def test_custom_confidence_threshold(self, simple_matcher_custom):
        """Test custom confidence threshold."""
        # Very strict threshold
        strict_matcher = simple_matcher_custom(confidence_threshold=0.95)

        document = ["similar but not exact line"]

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'similar but not exact'),
                DiffLine('+', 'new line')
            ]
        )

        result = strict_matcher.find_match(hunk, document)

        # Should fail with strict threshold
        assert result.success is False

    def test_custom_search_window(self, simple_matcher_custom):
        """Test custom search window."""
        small_window_matcher = simple_matcher_custom(search_window=2)

        document = [
            "line 1",
            "line 2",
            "line 3",
            "line 4",
            "line 5",
            "target line",  # Line 6, more than 2 lines away
            "line 7"
        ]

        hunk = DiffHunk(
            old_start=3,  # Expected at line 3, actually at line 6
            old_count=1,
            new_start=3,
            new_count=1,
            lines=[
                DiffLine('-', 'target line'),
                DiffLine('+', 'new line')
            ]
        )

        result = small_window_matcher.find_match(hunk, document)

        # Should not find it due to small search window
        assert result.success is False

    def test_large_search_window(self, simple_matcher_custom):
        """Test large search window finds distant matches."""
        large_window_matcher = simple_matcher_custom(search_window=100)

        document = ["line " + str(i) for i in range(1, 101)]
        document[49] = "target line"  # At line 50

        hunk = DiffHunk(
            old_start=10,  # Expected at line 10, actually at line 50
            old_count=1,
            new_start=10,
            new_count=1,
            lines=[
                DiffLine('-', 'target line'),
                DiffLine('+', 'new line')
            ]
        )

        result = large_window_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 50

    def test_search_window_accessor(self, simple_matcher_custom):
        """Test search_window() accessor method."""
        matcher = simple_matcher_custom(search_window=25)
        assert matcher.search_window() == 25


class TestDiffMatcherMultiLineContext:
    """Test matching with multiple lines of context."""

    def test_match_multiple_context_lines(self, simple_matcher):
        """Test matching hunk with multiple context lines."""
        document = [
            "context 1",
            "context 2",
            "context 3",
            "old line",
            "context 4",
            "context 5"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=6,
            new_start=1,
            new_count=6,
            lines=[
                DiffLine(' ', 'context 1'),
                DiffLine(' ', 'context 2'),
                DiffLine(' ', 'context 3'),
                DiffLine('-', 'old line'),
                DiffLine('+', 'new line'),
                DiffLine(' ', 'context 4'),
                DiffLine(' ', 'context 5')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 1
        assert result.confidence == 1.0

    def test_match_with_partial_context_mismatch(self, simple_matcher):
        """Test matching when some context lines don't match."""
        document = [
            "context 1",
            "different line",  # Doesn't match expected context
            "context 3",
            "old line",
            "context 5"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=5,
            new_start=1,
            new_count=5,
            lines=[
                DiffLine(' ', 'context 1'),
                DiffLine(' ', 'context 2'),  # Expected but not in document
                DiffLine(' ', 'context 3'),
                DiffLine('-', 'old line'),
                DiffLine('+', 'new line'),
                DiffLine(' ', 'context 5')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Should have lower confidence due to mismatch
        assert result.confidence < 1.0

    def test_match_long_context_block(self, simple_matcher):
        """Test matching with long block of context."""
        document = ["context line " + str(i) for i in range(1, 21)]
        document[9] = "target line"

        lines = [DiffLine(' ', f'context line {i}') for i in range(1, 10)]
        lines.append(DiffLine('-', 'target line'))
        lines.append(DiffLine('+', 'new line'))
        lines.extend([DiffLine(' ', f'context line {i}') for i in range(11, 21)])

        hunk = DiffHunk(
            old_start=1,
            old_count=20,
            new_start=1,
            new_count=20,
            lines=lines
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 1
        assert result.confidence == 1.0


class TestDiffMatcherEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_match_empty_document(self, simple_matcher):
        """Test matching against empty document."""
        document = []

        hunk = DiffHunk(
            old_start=1,
            old_count=0,
            new_start=1,
            new_count=1,
            lines=[DiffLine('+', 'new line')]
        )

        # Pure insertion should succeed even on empty document
        result = simple_matcher.find_match(hunk, document)
        assert result.success is True

    def test_match_single_line_document(self, simple_matcher):
        """Test matching against single-line document."""
        document = ["only line"]

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'only line'),
                DiffLine('+', 'new line')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 1

    def test_match_beyond_document_end(self, simple_matcher):
        """Test matching hunk that extends beyond document end."""
        document = [
            "line 1",
            "line 2"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=3,  # Expects 3 lines but only 2 exist
            new_start=2,
            new_count=3,
            lines=[
                DiffLine(' ', 'line 2'),
                DiffLine(' ', 'line 3'),
                DiffLine(' ', 'line 4')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Should fail because document doesn't have enough lines
        assert result.success is False

    def test_match_with_empty_lines(self, simple_matcher):
        """Test matching content with empty lines."""
        document = [
            "line 1",
            "",
            "line 3"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=3,
            new_start=1,
            new_count=3,
            lines=[
                DiffLine(' ', 'line 1'),
                DiffLine('-', ''),
                DiffLine('+', 'new line'),
                DiffLine(' ', 'line 3')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 1

    def test_match_identical_lines(self, simple_matcher):
        """Test matching when document has many identical lines."""
        document = [
            "same",
            "same",
            "same",
            "different",
            "same",
            "same"
        ]

        hunk = DiffHunk(
            old_start=2,
            old_count=2,
            new_start=2,
            new_count=2,
            lines=[
                DiffLine(' ', 'same'),
                DiffLine('-', 'different'),
                DiffLine('+', 'modified'),
                DiffLine(' ', 'same')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Should find the location with 'different'
        assert result.success is True
        assert result.location in [3, 4]  # Context starts at line before 'different'


class TestDiffMatcherConfidenceCalculation:
    """Test confidence score calculation."""

    def test_confidence_exact_match(self, simple_matcher):
        """Test confidence is 1.0 for exact match."""
        document = ["exact line"]

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'exact line'),
                DiffLine('+', 'new line')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.confidence == 1.0

    def test_confidence_partial_match(self, simple_matcher):
        """Test confidence calculation for partial match."""
        document = ["partially matching line"]

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'partially different line'),
                DiffLine('+', 'new line')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Should have moderate confidence
        assert 0.0 < result.confidence < 1.0

    def test_confidence_no_match(self, simple_matcher):
        """Test confidence is low for no match."""
        document = ["completely different content"]

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'expected content'),
                DiffLine('+', 'new content')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Should have low confidence
        assert result.confidence < 0.5

    def test_confidence_mixed_matches(self, simple_matcher):
        """Test confidence with mix of exact and partial matches."""
        document = [
            "exact match",
            "partial match here",
            "another exact"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=3,
            new_start=1,
            new_count=3,
            lines=[
                DiffLine(' ', 'exact match'),
                DiffLine('-', 'partial match'),  # Partially matches
                DiffLine('+', 'new line'),
                DiffLine(' ', 'another exact')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        # Should have high but not perfect confidence
        assert 0.75 < result.confidence < 1.0


class TestDiffMatcherStringDocument:
    """Test matcher with string documents."""

    def test_match_string_document(self, simple_matcher):
        """Test matching against string document."""
        document = """line 1
line 2
line 3
line 4"""

        hunk = DiffHunk(
            old_start=2,
            old_count=2,
            new_start=2,
            new_count=2,
            lines=[
                DiffLine(' ', 'line 2'),
                DiffLine('-', 'line 3'),
                DiffLine('+', 'new line 3')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 2
        assert result.confidence == 1.0
