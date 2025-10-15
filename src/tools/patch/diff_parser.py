"""
Unified Diff Parser - Parse unified diff format into structured data.

Supports standard unified diff format as produced by diff -u or git diff.
"""

import re
from typing import List, Dict, Tuple, Optional, Any
from dataclasses import dataclass


@dataclass
class DiffHeader:
    """Represents the header of a unified diff."""
    old_file: str
    new_file: str

    def __repr__(self) -> str:
        return f"DiffHeader(old={self.old_file}, new={self.new_file})"


@dataclass
class HunkHeader:
    """Represents a hunk header line (@@ -start,count +start,count @@)."""
    old_start: int
    old_count: int
    new_start: int
    new_count: int

    def __repr__(self) -> str:
        return f"HunkHeader(-{self.old_start},{self.old_count} +{self.new_start},{self.new_count})"


class UnifiedDiffParser:
    """
    Parser for unified diff format.

    Handles:
    - File headers (---, +++)
    - Hunk headers (@@ ... @@)
    - Context lines (starting with space)
    - Deletion lines (starting with -)
    - Addition lines (starting with +)
    """

    # Regex patterns for diff format
    OLD_FILE_PATTERN = re.compile(r'^---\s+(.+)$')
    NEW_FILE_PATTERN = re.compile(r'^\+\+\+\s+(.+)$')
    HUNK_HEADER_PATTERN = re.compile(r'^@@\s+-(\d+)(?:,(\d+))?\s+\+(\d+)(?:,(\d+))?\s+@@')

    def __init__(self) -> None:
        """Initialize the parser."""
        self.current_file: Optional[str] = None
        self.current_header: Optional[DiffHeader] = None

    def parse(self, diff_text: str) -> Tuple[Optional[str], List[Dict[str, Any]]]:
        """
        Parse unified diff text into structured hunks.

        Args:
            diff_text: Unified diff text (multi-line string)

        Returns:
            Tuple of (filename, list of hunk dictionaries)
            Each hunk dictionary has:
                - start_line: int (0-based line number)
                - old_count: int
                - new_count: int
                - changes: List[Tuple[str, str]] where first element is type
                  ('context', 'delete', 'insert') and second is the line text

        Raises:
            ValueError: If diff format is invalid
        """
        lines = diff_text.split('\n')

        # Parse file headers
        header = self._parse_headers(lines)
        if not header:
            raise ValueError("Could not find file headers (--- and +++) in diff")

        self.current_header = header

        # Determine target filename (prefer new file, fall back to old)
        target_file = self._extract_filename(header.new_file)
        if not target_file or target_file == '/dev/null':
            target_file = self._extract_filename(header.old_file)

        # Parse hunks
        hunks = self._parse_hunks(lines)

        if not hunks:
            raise ValueError("No hunks found in diff")

        return target_file, hunks

    def _parse_headers(self, lines: List[str]) -> Optional[DiffHeader]:
        """
        Parse file headers from diff.

        Args:
            lines: All lines from diff

        Returns:
            DiffHeader or None if not found
        """
        old_file = None
        new_file = None

        for line in lines:
            if not old_file:
                match = self.OLD_FILE_PATTERN.match(line)
                if match:
                    old_file = match.group(1)
                    continue

            if old_file and not new_file:
                match = self.NEW_FILE_PATTERN.match(line)
                if match:
                    new_file = match.group(1)
                    break

        if old_file and new_file:
            return DiffHeader(old_file=old_file, new_file=new_file)

        return None

    def _extract_filename(self, path: str) -> str:
        """
        Extract filename from diff path (handles a/, b/ prefixes, timestamps, etc.).

        Args:
            path: Path from diff header

        Returns:
            Cleaned filename
        """
        # Remove git-style a/ or b/ prefix
        if path.startswith('a/') or path.startswith('b/'):
            path = path[2:]

        # Remove timestamp if present (space followed by date/time)
        # Example: "file.py	2024-01-01 12:00:00"
        parts = path.split('\t')
        if parts:
            path = parts[0]

        # Also handle space-separated timestamps
        parts = path.split()
        if parts:
            path = parts[0]

        return path.strip()

    def _parse_hunks(self, lines: List[str]) -> List[Dict[str, Any]]:
        """
        Parse all hunks from diff lines.

        Args:
            lines: All lines from diff

        Returns:
            List of hunk dictionaries
        """
        hunks = []
        i = 0

        # Skip to first hunk header
        while i < len(lines):
            if self.HUNK_HEADER_PATTERN.match(lines[i]):
                break

            i += 1

        # Parse each hunk
        while i < len(lines):
            hunk_match = self.HUNK_HEADER_PATTERN.match(lines[i])
            if not hunk_match:
                i += 1
                continue

            # Parse hunk header
            hunk_header = self._parse_hunk_header(hunk_match)

            # Parse hunk body
            i += 1
            hunk_changes, i = self._parse_hunk_body(lines, i, hunk_header)

            # Create hunk dictionary
            # Convert to 0-based indexing for consistency
            hunk = {
                'start_line': hunk_header.old_start - 1,
                'old_count': hunk_header.old_count,
                'new_count': hunk_header.new_count,
                'changes': hunk_changes
            }

            hunks.append(hunk)

        return hunks

    def _parse_hunk_header(self, match: re.Match) -> HunkHeader:
        """
        Parse hunk header from regex match.

        Args:
            match: Regex match object from HUNK_HEADER_PATTERN

        Returns:
            HunkHeader object
        """
        old_start = int(match.group(1))
        old_count = int(match.group(2)) if match.group(2) else 1
        new_start = int(match.group(3))
        new_count = int(match.group(4)) if match.group(4) else 1

        return HunkHeader(
            old_start=old_start,
            old_count=old_count,
            new_start=new_start,
            new_count=new_count
        )

    def _parse_hunk_body(
        self,
        lines: List[str],
        start_idx: int,
        header: HunkHeader
    ) -> Tuple[List[Tuple[str, str]], int]:
        """
        Parse hunk body (context, deletions, insertions).

        Args:
            lines: All diff lines
            start_idx: Starting index for this hunk body
            header: Hunk header information

        Returns:
            Tuple of (changes list, next line index)
        """
        changes = []
        i = start_idx

        # Count lines processed to know when hunk ends
        old_lines_processed = 0
        new_lines_processed = 0

        while i < len(lines):
            line = lines[i]

            # Check if we've reached the next hunk or end of diff
            if self.HUNK_HEADER_PATTERN.match(line):
                break

            # Empty line or line with just whitespace might end hunk
            if not line or (line and line[0] not in ' +-\\'):
                # Check if we've processed expected number of lines
                if (old_lines_processed >= header.old_count and new_lines_processed >= header.new_count):
                    break

                # Otherwise, might be an empty context line
                if line == '':
                    changes.append(('context', ''))
                    old_lines_processed += 1
                    new_lines_processed += 1
                    i += 1
                    continue

                # Unknown line, skip it
                i += 1
                continue

            prefix = line[0]
            content = line[1:] if len(line) > 1 else ''

            if prefix == ' ':
                # Context line
                changes.append(('context', content))
                old_lines_processed += 1
                new_lines_processed += 1

            elif prefix == '-':
                # Deletion
                changes.append(('delete', content))
                old_lines_processed += 1

            elif prefix == '+':
                # Insertion
                changes.append(('insert', content))
                new_lines_processed += 1

            elif prefix == '\\':
                # "\ No newline at end of file" - ignore
                pass

            i += 1

            # Check if we've processed all expected lines
            if (old_lines_processed >= header.old_count and new_lines_processed >= header.new_count):
                break

        return changes, i

    def parse_file(self, diff_file_path: str) -> Tuple[Optional[str], List[Dict[str, Any]]]:
        """
        Parse a unified diff file.

        Args:
            diff_file_path: Path to diff file

        Returns:
            Tuple of (filename, list of hunks)
        """
        with open(diff_file_path, 'r', encoding='utf-8') as f:
            diff_text = f.read()

        return self.parse(diff_text)
