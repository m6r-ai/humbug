"""Converts parsed diff hunks into side-by-side row descriptors."""

from typing import List

from diff.diff_types import DiffHunk

from humbug.tabs.diff.diff_row import DiffRow, DiffRowType


class DiffViewBuilder:
    """Transforms a list of DiffHunk objects into a flat list of DiffRow objects.

    Each DiffRow represents one line in both the left (old) and right (new) pane
    of the side-by-side view.  The renderer iterates the row list once and draws
    one line per pane per row, so both panes always advance together and a single
    shared scrollbar value keeps them in sync.

    Row types and their pane content:

    - CONTEXT : unchanged line — left and right carry identical text.
    - CHANGED : paired removal/addition — left has old text, right has new text.
    - REMOVED : line exists only on the left — right_text is empty.
    - ADDED   : line exists only on the right — left_text is empty.
    - FILLER  : completely blank on both sides, reserved for future use such as
                inter-hunk separators.
    - HEADER  : hunk header (@@ ... @@) shown across the full width.

    REMOVED and ADDED rows already encode a blank on the opposite side, so both
    panes advance by the same number of rows without needing extra FILLER rows.
    """

    def build(self, hunks: List[DiffHunk]) -> List[DiffRow]:
        """
        Build the flat row list for a complete diff.

        Args:
            hunks: Parsed hunks from DiffParser, in file order.

        Returns:
            Ordered list of DiffRow objects ready for rendering.
        """
        rows: List[DiffRow] = []

        for hunk in hunks:
            self._emit_header(rows, hunk)
            self._emit_hunk_body(rows, hunk)

        return rows

    def _emit_header(self, rows: List[DiffRow], hunk: DiffHunk) -> None:
        """Emit a HEADER row for the hunk's @@ line."""
        header_text = (
            f"@@ -{hunk.old_start},{hunk.old_count} "
            f"+{hunk.new_start},{hunk.new_count} @@"
        )
        rows.append(DiffRow(
            row_type=DiffRowType.HEADER,
            left_text=header_text,
            right_text="",
            left_line_no=None,
            right_line_no=None,
        ))

    def _emit_hunk_body(self, rows: List[DiffRow], hunk: DiffHunk) -> None:
        """Emit all content rows for a single hunk.

        Walks the hunk lines, accumulating runs of '-' and '+' lines.  Each
        time a context line is encountered (or the hunk ends) the accumulated
        run is flushed into paired rows.
        """
        old_line = hunk.old_start
        new_line = hunk.new_start

        removed_run: List[str] = []
        added_run: List[str] = []

        run_old_start = old_line
        run_new_start = new_line

        for diff_line in hunk.lines:
            if diff_line.type == ' ':
                self._flush_run(rows, removed_run, added_run, run_old_start, run_new_start)
                old_line += len(removed_run)
                new_line += len(added_run)
                removed_run = []
                added_run = []

                rows.append(DiffRow(
                    row_type=DiffRowType.CONTEXT,
                    left_text=diff_line.content,
                    right_text=diff_line.content,
                    left_line_no=old_line,
                    right_line_no=new_line,
                ))
                old_line += 1
                new_line += 1
                run_old_start = old_line
                run_new_start = new_line

            elif diff_line.type == '-':
                removed_run.append(diff_line.content)

            elif diff_line.type == '+':
                added_run.append(diff_line.content)

        self._flush_run(rows, removed_run, added_run, run_old_start, run_new_start)

    def _flush_run(
        self,
        rows: List[DiffRow],
        removed: List[str],
        added: List[str],
        old_start: int,
        new_start: int,
    ) -> None:
        """Emit rows for a paired run of removed and added lines.

        Lines are paired 1-to-1 as CHANGED rows for as long as both sides have
        content.  Surplus lines on the longer side become REMOVED or ADDED rows,
        which carry an empty string on the opposite side, keeping both panes at
        the same row count for this run.

        Args:
            rows: Row list to append to.
            removed: Removed-line contents for this run.
            added: Added-line contents for this run.
            old_start: 1-indexed line number of the first removed line.
            new_start: 1-indexed line number of the first added line.
        """
        if not removed and not added:
            return

        paired = min(len(removed), len(added))

        for i in range(paired):
            rows.append(DiffRow(
                row_type=DiffRowType.CHANGED,
                left_text=removed[i],
                right_text=added[i],
                left_line_no=old_start + i,
                right_line_no=new_start + i,
            ))

        for i in range(paired, len(removed)):
            rows.append(DiffRow(
                row_type=DiffRowType.REMOVED,
                left_text=removed[i],
                right_text="",
                left_line_no=old_start + i,
                right_line_no=None,
            ))

        for i in range(paired, len(added)):
            rows.append(DiffRow(
                row_type=DiffRowType.ADDED,
                left_text="",
                right_text=added[i],
                left_line_no=None,
                right_line_no=new_start + i,
            ))
