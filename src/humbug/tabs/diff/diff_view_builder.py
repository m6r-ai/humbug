"""Converts full file content and parsed diff hunks into side-by-side row descriptors."""

from typing import List

from diff.diff_types import DiffHunk

from humbug.tabs.diff.diff_row import DiffRow, DiffRowType


class DiffViewBuilder:
    """
    Transforms two full file line lists and a list of DiffHunk objects into a flat
    list of DiffRow objects suitable for side-by-side rendering.

    Both the old (HEAD) and new (working tree) line lists are required so that
    context lines outside hunks can be emitted in full, allowing a syntax
    highlighter to process each pane's document from top to bottom with correct
    parser-state propagation.

    Row types and their pane content:

    - CONTEXT : unchanged line — left and right carry identical text.
    - CHANGED : paired removal/addition — left has old text, right has new text.
    - REMOVED : line exists only on the left — right_text is empty string (blank
                line in the right pane).
    - ADDED   : line exists only on the right — left_text is empty string (blank
                line in the left pane).

    For a block where there are more removed lines than added lines, the surplus
    removed lines become REMOVED rows (real text on left, blank on right).  For
    the opposite case, surplus added lines become ADDED rows (blank on left, real
    text on right).
    """

    def build(self, old_lines: List[str], new_lines: List[str], hunks: List[DiffHunk]) -> List[DiffRow]:
        """
        Build the flat row list for a complete side-by-side diff.

        Args:
            old_lines: All lines of the old (HEAD) file, without trailing newlines.
            new_lines: All lines of the new (working tree) file, without trailing newlines.
            hunks: Parsed hunks from DiffParser, in file order.

        Returns:
            Ordered list of DiffRow objects ready for rendering.
        """
        rows: List[DiffRow] = []

        # Cursors into each line list (0-indexed internally; hunk numbers are 1-indexed).
        old_pos = 0
        new_pos = 0

        for hunk in hunks:
            # Emit context rows for lines before this hunk.
            old_context_end = hunk.old_start - 1
            new_context_end = hunk.new_start - 1

            while old_pos < old_context_end and new_pos < new_context_end:
                rows.append(DiffRow(
                    row_type=DiffRowType.CONTEXT,
                    left_text=old_lines[old_pos],
                    right_text=new_lines[new_pos],
                    left_line_no=old_pos + 1,
                    right_line_no=new_pos + 1,
                ))
                old_pos += 1
                new_pos += 1

            # Collect removed and added runs within this hunk.
            removed: List[str] = []
            added: List[str] = []

            for diff_line in hunk.lines:
                if diff_line.type == ' ':
                    self._flush_run(rows, removed, added, old_pos, new_pos)
                    old_pos += len(removed)
                    new_pos += len(added)
                    removed = []
                    added = []

                    rows.append(DiffRow(
                        row_type=DiffRowType.CONTEXT,
                        left_text=old_lines[old_pos],
                        right_text=new_lines[new_pos],
                        left_line_no=old_pos + 1,
                        right_line_no=new_pos + 1,
                    ))
                    old_pos += 1
                    new_pos += 1

                elif diff_line.type == '-':
                    removed.append(diff_line.content)

                elif diff_line.type == '+':
                    added.append(diff_line.content)

            self._flush_run(rows, removed, added, old_pos, new_pos)
            old_pos += len(removed)
            new_pos += len(added)

        # Emit any remaining context lines after the last hunk.
        while old_pos < len(old_lines) and new_pos < len(new_lines):
            rows.append(DiffRow(
                row_type=DiffRowType.CONTEXT,
                left_text=old_lines[old_pos],
                right_text=new_lines[new_pos],
                left_line_no=old_pos + 1,
                right_line_no=new_pos + 1,
            ))
            old_pos += 1
            new_pos += 1

        return rows

    def _flush_run(
        self,
        rows: List[DiffRow],
        removed: List[str],
        added: List[str],
        old_start: int,
        new_start: int,
    ) -> None:
        """
        Emit rows for a paired run of removed and added lines.

        Lines are paired 1-to-1 as CHANGED rows for as long as both sides have
        content.  Surplus lines on the longer side become REMOVED or ADDED rows,
        which carry an empty string on the opposite side.  This keeps both panes
        at the same document block count.

        Args:
            rows: Row list to append to.
            removed: Removed-line contents for this run.
            added: Added-line contents for this run.
            old_start: 0-indexed position of the first removed line.
            new_start: 0-indexed position of the first added line.
        """
        if not removed and not added:
            return

        paired = min(len(removed), len(added))

        for i in range(paired):
            rows.append(DiffRow(
                row_type=DiffRowType.CHANGED,
                left_text=removed[i],
                right_text=added[i],
                left_line_no=old_start + i + 1,
                right_line_no=new_start + i + 1,
            ))

        for i in range(paired, len(removed)):
            rows.append(DiffRow(
                row_type=DiffRowType.REMOVED,
                left_text=removed[i],
                right_text="",
                left_line_no=old_start + i + 1,
                right_line_no=None,
            ))

        for i in range(paired, len(added)):
            rows.append(DiffRow(
                row_type=DiffRowType.ADDED,
                left_text="",
                right_text=added[i],
                left_line_no=None,
                right_line_no=new_start + i + 1,
            ))
