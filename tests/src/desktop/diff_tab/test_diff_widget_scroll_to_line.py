"""Tests for scrolling a diff view to a working-tree line."""

from desktop.diff_tab.diff_row import DiffRow, DiffRowType, DiffViewMode
from desktop.diff_tab.diff_widget import DiffWidget


def _row(row_type: DiffRowType, left_line: int | None, right_line: int | None) -> DiffRow:
    return DiffRow(
        row_type=row_type,
        left_text="",
        right_text="",
        left_line_no=left_line,
        right_line_no=right_line,
    )


def _make_widget(rows: list[DiffRow]) -> DiffWidget:
    widget = DiffWidget("", mode=DiffViewMode.INLINE)
    widget.resize(800, 600)
    widget._rows = rows  # pylint: disable=protected-access
    return widget


class TestDiffWidgetScrollToLine:
    """Tests for DiffWidget.scroll_to_line."""

    def test_returns_false_when_no_rows(self, qapp) -> None:
        """An empty diff cannot be scrolled."""
        widget = _make_widget([])

        assert widget.scroll_to_line(1) is False

    def test_returns_true_when_rows_present(self, qapp) -> None:
        """A non-empty diff reports a successful scroll request."""
        widget = _make_widget([_row(DiffRowType.CONTEXT, 1, 1)])

        assert widget.scroll_to_line(1) is True

    def test_exact_line_selects_its_row(self, qapp) -> None:
        """A line matching a row's right-side line number selects that row."""
        rows = [
            _row(DiffRowType.CONTEXT, 1, 1),
            _row(DiffRowType.CONTEXT, 2, 2),
            _row(DiffRowType.CONTEXT, 3, 3),
        ]
        widget = _make_widget(rows)

        widget.scroll_to_line(3)

        expected = widget._primary_pane().target_scroll_for_block(2)  # pylint: disable=protected-access
        assert widget._smooth_scroll_target == expected  # pylint: disable=protected-access

    def test_line_between_rows_selects_preceding_row(self, qapp) -> None:
        """A line with no exact row selects the nearest preceding right-side row."""
        rows = [
            _row(DiffRowType.CONTEXT, 1, 1),
            _row(DiffRowType.REMOVED, 2, None),
            _row(DiffRowType.ADDED, None, 2),
        ]
        widget = _make_widget(rows)

        widget.scroll_to_line(2)

        expected = widget._primary_pane().target_scroll_for_block(2)  # pylint: disable=protected-access
        assert widget._smooth_scroll_target == expected  # pylint: disable=protected-access

    def test_line_before_first_row_selects_first_row(self, qapp) -> None:
        """A line before any right-side row falls back to the first row."""
        rows = [
            _row(DiffRowType.ADDED, None, 5),
            _row(DiffRowType.CONTEXT, 6, 6),
        ]
        widget = _make_widget(rows)

        widget.scroll_to_line(1)

        expected = widget._primary_pane().target_scroll_for_block(0)  # pylint: disable=protected-access
        assert widget._smooth_scroll_target == expected  # pylint: disable=protected-access

    def test_line_beyond_last_row_selects_last_right_row(self, qapp) -> None:
        """A line past the diff selects the last row carrying a right-side line."""
        rows = [
            _row(DiffRowType.CONTEXT, 1, 1),
            _row(DiffRowType.CONTEXT, 2, 2),
        ]
        widget = _make_widget(rows)

        widget.scroll_to_line(999)

        expected = widget._primary_pane().target_scroll_for_block(1)  # pylint: disable=protected-access
        assert widget._smooth_scroll_target == expected  # pylint: disable=protected-access
