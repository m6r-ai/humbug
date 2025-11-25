"""Abstract diff applier."""

from abc import ABC, abstractmethod
from typing import Any, List, Tuple

from diff.diff_exceptions import DiffMatchError, DiffValidationError
from diff.diff_matcher import DiffMatcher
from diff.diff_parser import DiffParser
from diff.diff_types import DiffApplicationResult, DiffHunk, MatchResult


class DiffApplier(ABC):
    """Abstract base class for applying unified diffs."""

    def __init__(
        self,
        confidence_threshold: float = 0.75,
        search_window: int = 50
    ):
        """
        Initialize the diff applier.

        Args:
            confidence_threshold: Minimum confidence required for matches
            search_window: Lines to search above/below expected position
        """
        self._parser = DiffParser()
        self._confidence_threshold = confidence_threshold
        self._search_window = search_window

    @abstractmethod
    def _create_matcher(self) -> DiffMatcher:
        """
        Create appropriate matcher for this applier type.

        Returns:
            DiffMatcher instance for this applier type
        """

    @abstractmethod
    def _apply_hunk(
        self,
        hunk: DiffHunk,
        location: int,
        document: Any,
        context: Any
    ) -> None:
        """
        Apply a single hunk to the document.

        Args:
            hunk: The hunk to apply
            location: Line number where to apply (1-indexed)
            document: Document to modify (type varies by implementation)
            context: Additional context needed for application (varies by implementation)
        """

    def apply_diff(
        self,
        diff_text: str,
        document: Any,
        dry_run: bool = False,
        **kwargs: Any
    ) -> DiffApplicationResult:
        """
        Apply a unified diff to a document.

        This operation is atomic - either all hunks apply successfully or none do.

        Args:
            diff_text: Unified diff format text
            document: Document to modify (type varies by implementation)
            dry_run: If True, validate but don't apply changes
            **kwargs: Additional implementation-specific parameters

        Returns:
            DiffApplicationResult with operation status
        """
        # Parse the diff
        hunks = self._parser.parse(diff_text)

        # Phase 1: Locate all hunks
        matcher = self._create_matcher()
        hunk_locations: List[Tuple[DiffHunk, MatchResult]] = []

        for idx, hunk in enumerate(hunks):
            match_result = matcher.find_match(hunk, document)

            if not match_result.success:
                # Build detailed error information
                expected_lines = [
                    line.content for line in hunk.lines
                    if line.type in (' ', '-')
                ]

                search_window = matcher.search_window()
                error_details = {
                    'phase': 'matching',
                    'failed_hunk': idx + 1,
                    'total_hunks': len(hunks),
                    'reason': 'Could not locate hunk with sufficient confidence',
                    'expected_location': hunk.old_start,
                    'expected_context': expected_lines,
                    'searched_range': [
                        max(1, hunk.old_start - search_window),
                        hunk.old_start + search_window
                    ],
                    'best_match': {
                        'location': match_result.location,
                        'confidence': round(match_result.confidence, 2),
                        'actual_context': match_result.actual_lines
                    },
                    'suggestion': 'Context lines do not match. Consider reading the current '
                        'content and regenerating the diff.'
                }

                raise DiffMatchError(
                    f'Could not locate hunk {idx + 1} with sufficient confidence',
                    error_details
                )

            hunk_locations.append((hunk, match_result))

        # Phase 2: Sort hunks by location (bottom to top) and check for overlaps
        hunk_locations.sort(key=lambda x: x[1].location, reverse=True)
        self._check_for_overlaps(hunk_locations)

        # If dry run, return success without applying
        if dry_run:
            return DiffApplicationResult(
                success=True,
                message=f'Diff validation successful: {len(hunks)} hunk(s) can be applied',
                hunks_applied=len(hunks)
            )

        # Phase 3: Apply all hunks
        context = kwargs.get('context')
        for hunk, match_result in hunk_locations:
            self._apply_hunk(hunk, match_result.location, document, context)

        return DiffApplicationResult(
            success=True,
            message=f'Successfully applied {len(hunks)} hunk(s)',
            hunks_applied=len(hunks)
        )

    def _check_for_overlaps(
        self,
        hunk_locations: List[Tuple[DiffHunk, MatchResult]]
    ) -> None:
        """
        Check if any hunks would overlap when applied.

        Args:
            hunk_locations: List of (hunk, match_result) tuples, sorted by location

        Raises:
            DiffValidationError: If overlaps found
        """
        for i in range(len(hunk_locations) - 1):
            hunk1, match1 = hunk_locations[i]
            hunk2, match2 = hunk_locations[i + 1]

            # Calculate the range each hunk affects.
            # Since hunks are sorted in reverse order (high to low),
            # hunk2 comes BEFORE hunk1 in the file, so check if hunk2 overlaps into hunk1
            hunk2_end = match2.location + hunk2.old_count - 1
            hunk1_start = match1.location

            if hunk2_end >= hunk1_start:
                error_details = {
                    'phase': 'validation',
                    'reason': 'Overlapping hunks detected',
                    'hunk1_range': [hunk1_start, hunk1_start + hunk1.old_count - 1],
                    'hunk2_range': [match2.location, hunk2_end],
                    'suggestion': 'Hunks affect overlapping line ranges. Regenerate diff with non-overlapping changes.'
                }

                raise DiffValidationError('Hunks would overlap when applied', error_details)
