"""Unified diff parsing."""

import re

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

    def parse(self, diff_text: str) -> list[DiffHunk]:
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
        hunks: list[DiffHunk] = []

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
                # Skip past the lines we just parsed.  Add 1 for the @@ header
                # and count raw hunk body lines (including any "\ No newline"
                # markers, which produce no DiffLine).
                i += 1 + self._hunk_body_line_count(lines, i)

            else:
                # Skip non-hunk lines (could be additional headers or context)
                i += 1

        if not hunks:
            raise DiffParseError("No valid hunks found in diff")

        return hunks

    def _hunk_body_line_count(self, lines: list[str], hunk_idx: int) -> int:
        """
        Count the raw body lines belonging to the hunk at hunk_idx.

        The body starts immediately after the @@ header.  When the hunk
        header specifies line counts, --- and +++ lines are treated as
        content (not file headers) until both old and new line quotas are
        consumed — only then do they act as terminators.  This prevents
        content lines that happen to start with --- or +++ from being
        mistaken for file headers.  For a bare @@ header (no counts), the
        body ends at the next @@, file header (--- / +++), or end of diff.
        Backslash-prefixed no-newline markers are included in the count so
        the outer parse loop can skip past them correctly.

        Args:
            lines: All lines from the diff
            hunk_idx: Index of the @@ header line

        Returns:
            Number of raw body lines in this hunk
        """
        header = lines[hunk_idx]
        match = re.match(r'^@@\s+-(\d+)(?:,(\d+))?\s+\+(\d+)(?:,(\d+))?\s+@@', header)

        if not match:
            # Bare @@ header: no counts to guide us, use terminators.
            count = 0
            i = hunk_idx + 1

            while i < len(lines):
                line = lines[i]

                if line.startswith('@@') or line.startswith('---') or line.startswith('+++'):
                    break

                count += 1
                i += 1

            return count

        old_count = int(match.group(2)) if match.group(2) else 1
        new_count = int(match.group(4)) if match.group(4) else 1

        count = 0
        old_consumed = 0
        new_consumed = 0
        i = hunk_idx + 1

        while i < len(lines):
            line = lines[i]

            if line.startswith('@@'):
                break

            # --- and +++ are treated as file headers (terminators) only after
            # both line quotas are consumed.  Before that, they are content lines.
            if (line.startswith('---') or line.startswith('+++')) and old_consumed >= old_count and new_consumed >= new_count:
                break

            if not line:
                # Empty lines don't count toward either quota.
                count += 1
                i += 1
                continue

            if line.startswith('\\'):
                # No-newline marker: counts as a raw line but not toward quotas.
                count += 1
                i += 1
                continue

            if line.startswith(' '):
                old_consumed += 1
                new_consumed += 1

            elif line.startswith('-'):
                old_consumed += 1

            elif line.startswith('+'):
                new_consumed += 1

            count += 1
            i += 1

        return count

    def _parse_hunk(self, lines: list[str], start_idx: int) -> DiffHunk:
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
        has_counts = True
        match = re.match(r'^@@\s+-(\d+)(?:,(\d+))?\s+\+(\d+)(?:,(\d+))?\s+@@', header)
        if not match:
            # A bare @@ with no line numbers is treated as "location unknown".
            # old_start=1 causes the matcher's full-file fuzzy search to find
            # the correct position from the context/deletion lines alone.
            if not re.match(r'^@@\s*$', header):
                raise DiffParseError(f"Invalid hunk header format: {header}")

            has_counts = False
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
        hunk_lines: list[DiffLine] = []
        old_no_newline = False
        new_no_newline = False
        old_consumed = 0
        new_consumed = 0
        i = start_idx + 1

        while i < len(lines):
            line = lines[i]

            # Stop at next hunk header.
            if line.startswith('@@'):
                break

            # Stop at file header (--- or +++) only after both line quotas are
            # consumed.  Before that, such lines are content lines — this
            # prevents lines like "+++ b/file" inside a hunk body from being
            # mistaken for a file header.  For bare @@ headers (has_counts is
            # False, quotas are 0), this always terminates as before.
            if ((line.startswith('---') or line.startswith('+++')) and
                    (not has_counts or (old_consumed >= old_count and new_consumed >= new_count))):
                break

            # Skip empty lines
            if not line:
                i += 1
                continue

            # "\ No newline at end of file" marker.  This applies to the line
            # immediately preceding it: a '-' line means the old file's last
            # line has no trailing newline, a '+' line means the new file's
            # last line has no trailing newline, and a context line means both.
            if line.startswith('\\'):
                if hunk_lines:
                    prev_type = hunk_lines[-1].type
                    if prev_type == '-':
                        old_no_newline = True

                    elif prev_type == '+':
                        new_no_newline = True

                    else:
                        # Context line: both sides share the same last line
                        old_no_newline = True
                        new_no_newline = True

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
                old_consumed += 1
                new_consumed += 1

            elif line.startswith('-'):
                hunk_lines.append(DiffLine('-', line[1:]))
                old_consumed += 1

            elif line.startswith('+'):
                hunk_lines.append(DiffLine('+', line[1:]))
                new_consumed += 1

            else:
                # Treat as context line if it doesn't have a prefix
                # This handles cases where the AI might forget the space prefix
                hunk_lines.append(DiffLine(' ', line))
                old_consumed += 1
                new_consumed += 1

            i += 1

        return DiffHunk(
            old_start, old_count, new_start, new_count, hunk_lines,
            old_no_newline=old_no_newline, new_no_newline=new_no_newline
        )
