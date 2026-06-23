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
        """Test that ambiguous matches (multiple exact matches) are rejected."""
        document = [
            "target line",  # Line 1
            "other content",
            "other content",
            "other content",
            "target line",  # Line 5 - farther from expected
            "other content"
        ]

        # Two exact matches within the window: ambiguity must be flagged.
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

        assert result.success is False
        assert result.ambiguous_locations == [1, 5]


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


class TestDiffMatcherOutOfRange:
    """Test the full-file exact search and out_of_range_location reporting."""

    def test_out_of_range_location_populated_when_match_outside_window(self, simple_matcher_custom):
        """Test that out_of_range_location is set when an exact match exists
        outside the search window."""
        matcher = simple_matcher_custom(search_window=2)

        document = ["line " + str(i) for i in range(1, 20)]
        document[14] = "unique target line"  # At line 15, well outside window around line 1

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'unique target line'),
                DiffLine('+', 'replacement')
            ]
        )

        result = matcher.find_match(hunk, document)

        assert result.success is False
        assert result.out_of_range_location == 15

    def test_out_of_range_location_none_when_no_match_anywhere(self, simple_matcher_custom):
        """Test that out_of_range_location is None when content does not exist
        anywhere in the document."""
        matcher = simple_matcher_custom(search_window=2)

        document = ["line 1", "line 2", "line 3"]

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'completely absent line'),
                DiffLine('+', 'replacement')
            ]
        )

        result = matcher.find_match(hunk, document)

        assert result.success is False
        assert result.out_of_range_location is None

    def test_bare_at_at_hunk_matches_via_full_file_search(self, simple_matcher):
        """Test that a bare @@ hunk (old_count=0, new_count=0 sentinel) succeeds
        by finding the content anywhere in the file."""
        document = ["line 1", "line 2", "target content", "line 4"]

        hunk = DiffHunk(
            old_start=1,
            old_count=0,   # sentinel: location unknown
            new_start=1,
            new_count=0,
            lines=[
                DiffLine('-', 'target content'),
                DiffLine('+', 'replacement')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 3
        assert result.confidence == 1.0


class TestDiffMatcherDefaultLineCount:
    """Test the base DiffMatcher._get_document_line_count default implementation."""

    def test_default_line_count_via_get_document_lines(self):
        """Test that the default _get_document_line_count works correctly by
        using a matcher subclass that does not override it."""
        from diff.diff_matcher import DiffMatcher
        from typing import Any, List

        class MinimalMatcher(DiffMatcher):
            """Matcher that relies entirely on the base _get_document_line_count."""

            def _get_document_lines(self, document: Any, start_line: int, count: int) -> List[str]:
                lines = document
                start = start_line - 1
                return lines[start:start + count]

        matcher = MinimalMatcher()
        document = ["a", "b", "c", "d", "e"]

        hunk = DiffHunk(
            old_start=3,
            old_count=1,
            new_start=3,
            new_count=1,
            lines=[
                DiffLine('-', 'c'),
                DiffLine('+', 'C')
            ]
        )

        result = matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 3


class TestDiffMatcherAmbiguity:
    """Test ambiguity detection when multiple exact matches exist."""

    def test_ambiguous_within_search_window(self, simple_matcher):
        """Two exact matches within the search window are flagged as ambiguous."""
        document = [
            "filler 1",
            "duplicate",
            "filler 2",
            "duplicate",
            "filler 3"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'duplicate'),
                DiffLine('+', 'new')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is False
        assert result.ambiguous_locations is not None
        assert sorted(result.ambiguous_locations) == [2, 4]

    def test_ambiguous_with_context_lines(self, simple_matcher):
        """Ambiguity detected when context + deletion lines match multiple places."""
        document = [
            "header",
            "same context",
            "same target",
            "footer",
            "same context",
            "same target",
            "end"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=2,
            new_start=1,
            new_count=2,
            lines=[
                DiffLine(' ', 'same context'),
                DiffLine('-', 'same target'),
                DiffLine('+', 'new target')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is False
        assert result.ambiguous_locations is not None
        assert sorted(result.ambiguous_locations) == [2, 5]

    def test_no_ambiguity_when_unique_context_disambiguates(self, simple_matcher):
        """Extra unique context lines eliminate ambiguity."""
        document = [
            "header",
            "unique A",
            "target",
            "footer",
            "unique B",
            "target",
            "end"
        ]

        hunk = DiffHunk(
            old_start=1,
            old_count=2,
            new_start=1,
            new_count=2,
            lines=[
                DiffLine(' ', 'unique A'),
                DiffLine('-', 'target'),
                DiffLine('+', 'new')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 2
        assert result.ambiguous_locations is None

    def test_ambiguous_out_of_range_normal_hunk(self, simple_matcher_custom):
        """Multiple out-of-range matches for a normal hunk are flagged as ambiguous."""
        matcher = simple_matcher_custom(search_window=2)

        document = ["filler " + str(i) for i in range(1, 30)]
        document[9] = "duplicate"   # line 10
        document[19] = "duplicate"  # line 20

        hunk = DiffHunk(
            old_start=1,
            old_count=1,
            new_start=1,
            new_count=1,
            lines=[
                DiffLine('-', 'duplicate'),
                DiffLine('+', 'new')
            ]
        )

        result = matcher.find_match(hunk, document)

        assert result.success is False
        assert result.ambiguous_locations is not None
        assert sorted(result.ambiguous_locations) == [10, 20]

    def test_ambiguous_bare_at_at_hunk(self, simple_matcher):
        """A bare @@ hunk with multiple matches anywhere is flagged as ambiguous."""
        document = ["line 1", "target", "line 3", "target", "line 5"]

        hunk = DiffHunk(
            old_start=1,
            old_count=0,
            new_start=1,
            new_count=0,
            lines=[
                DiffLine('-', 'target'),
                DiffLine('+', 'replacement')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is False
        assert result.ambiguous_locations is not None
        assert sorted(result.ambiguous_locations) == [2, 4]

    def test_bare_at_at_single_match_still_succeeds(self, simple_matcher):
        """A bare @@ hunk with a unique match still succeeds."""
        document = ["line 1", "unique target", "line 3"]

        hunk = DiffHunk(
            old_start=1,
            old_count=0,
            new_start=1,
            new_count=0,
            lines=[
                DiffLine('-', 'unique target'),
                DiffLine('+', 'replacement')
            ]
        )

        result = simple_matcher.find_match(hunk, document)

        assert result.success is True
        assert result.location == 2
        assert result.ambiguous_locations is None
