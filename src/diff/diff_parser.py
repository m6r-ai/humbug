"""Unified diff parsing."""

import re
from typing import List

from diff.diff_exceptions import DiffParseError
from diff.diff_types import DiffLine, DiffHunk

# Unicode characters that visually resemble dash prefix characters and are
# commonly substituted by LLMs when generating diffs.
_DASH_LOOKALIKES = frozenset([
    '\u2012',  # figure dash
    '\u2013',  # en dash
    '\u2014',  # em dash
    '\u2015',  # horizontal bar
    '\u2212',  # minus sign
    '\ufe58',  # small em dash
    '\ufe63',  # small hyphen-minus
    '\uff0d',  # fullwidth hyphen-minus
])


class DiffParser:
    """Parser for unified diff format."""

    def parse(self, diff_text: str) -> List[DiffHunk]:
        """
        Parse unified diff text into structured hunks.

        Args:
            diff_text: Unified diff format text

        Returns:
            List of parsed hunks

        Raises:
            DiffParseError: If parsing fails
        """
        if not diff_text or not diff_text.strip():
            raise DiffParseError("Empty diff provided")

        lines = diff_text.splitlines()
        hunks: List[DiffHunk] = []

        # Skip file headers (--- and +++ lines) if present
        start_idx = 0
        while start_idx < len(lines):
            line = lines[start_idx]
            if line.startswith('---') or line.startswith('+++'):
                start_idx += 1
                continue

            break

        # Parse hunks
        i = start_idx
        while i < len(lines):
            line = lines[i]

            # Look for hunk header: @@ -old_start,old_count +new_start,new_count @@
            if line.startswith('@@'):
                hunk = self._parse_hunk(lines, i)
                hunks.append(hunk)
                # Skip past the lines we just parsed
                i += 1 + len(hunk.lines)

            else:
                # Skip non-hunk lines (could be additional headers or context)
                i += 1

        if not hunks:
            raise DiffParseError("No valid hunks found in diff")

        return hunks

    def _parse_hunk(self, lines: List[str], start_idx: int) -> DiffHunk:
        """
        Parse a single hunk starting at the given index.

        Args:
            lines: All lines from the diff
            start_idx: Index of the @@ line

        Returns:
            Parsed hunk

        Raises:
            DiffParseError: If hunk parsing fails
        """
        header = lines[start_idx]

        # Parse hunk header: @@ -old_start,old_count +new_start,new_count @@
        match = re.match(r'^@@\s+-(\d+)(?:,(\d+))?\s+\+(\d+)(?:,(\d+))?\s+@@', header)
        if not match:
            # A bare @@ with no line numbers is treated as "location unknown".
            # old_start=1 causes the matcher's full-file fuzzy search to find
            # the correct position from the context/deletion lines alone.
            if not re.match(r'^@@\s*$', header):
                raise DiffParseError(f"Invalid hunk header format: {header}")

            old_start = 1
            old_count = 0
            new_start = 1
            new_count = 0

        else:
            old_start = int(match.group(1))
            old_count = int(match.group(2)) if match.group(2) else 1
            new_start = int(match.group(3))
            new_count = int(match.group(4)) if match.group(4) else 1

        # Parse hunk lines
        hunk_lines: List[DiffLine] = []
        i = start_idx + 1

        while i < len(lines):
            line = lines[i]

            # Stop at next hunk or end of diff
            if line.startswith('@@'):
                break

            # Stop at next file header (--- or +++)
            if line.startswith('---') or line.startswith('+++'):
                break

            # Skip empty lines
            if not line:
                i += 1
                continue

            # Reject lookalike dash characters used as a deletion prefix
            if line[0] in _DASH_LOOKALIKES:
                raise DiffParseError(
                    f"Invalid diff prefix character U+{ord(line[0]):04X} "
                    f"resembles a dash but is not ASCII '-'. "
                    f"Use '-' for deletions, '+' for additions, ' ' for context."
                )

            # Parse diff line
            if line.startswith(' '):
                hunk_lines.append(DiffLine(' ', line[1:]))

            elif line.startswith('-'):
                hunk_lines.append(DiffLine('-', line[1:]))

            elif line.startswith('+'):
                hunk_lines.append(DiffLine('+', line[1:]))

            elif line.startswith('\\'):
                # "\ No newline at end of file" - ignore for now
                pass

            else:
                # Treat as context line if it doesn't have a prefix
                # This handles cases where the AI might forget the space prefix
                hunk_lines.append(DiffLine(' ', line))

            i += 1

        return DiffHunk(old_start, old_count, new_start, new_count, hunk_lines)
