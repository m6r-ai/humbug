"""
Unified diff parsing, matching, and application.

This package provides a reusable framework for applying unified diffs
to various document types (Qt text documents, files, strings, etc.).
"""

from diff.diff_applier import DiffApplier
from diff.diff_exceptions import (
    DiffApplicationError,
    DiffError,
    DiffMatchError,
    DiffParseError,
    DiffValidationError,
)
from diff.diff_matcher import DiffMatcher
from diff.diff_parser import DiffParser
from diff.diff_types import (
    DiffApplicationResult,
    DiffHunk,
    DiffLine,
    MatchResult,
)

__all__ = [
    # Exceptions
    'DiffError',
    'DiffParseError',
    'DiffMatchError',
    'DiffApplicationError',
    'DiffValidationError',
    # Types
    'DiffLine',
    'DiffHunk',
    'MatchResult',
    'DiffApplicationResult',
    # Core classes
    'DiffParser',
    'DiffMatcher',
    'DiffApplier',
]
