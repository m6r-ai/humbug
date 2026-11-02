"""Tests for translating a diff pane click into an editor line and column."""

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


def _capture(widget: DiffWidget) -> list[tuple[int, int]]:
    emitted: list[tuple[int, int]] = []
    widget.open_in_editor_requested.connect(lambda line, column: emitted.append((line, column)))
    return emitted


class TestDiffWidgetOpenInEditor:
    """Tests for the line and column emitted when a pane row is opened in the editor."""

    def test_clicked_row_keeps_its_column(self, qapp) -> None:
        """A row that supplies its own working-tree line keeps the clicked column."""
        widget = _make_widget([_row(DiffRowType.CONTEXT, 1, 1)])
        emitted = _capture(widget)

        widget._on_right_pane_open_in_editor_requested(0, 42)  # pylint: disable=protected-access

        assert emitted == [(1, 42)]

    def test_remapped_row_discards_the_column(self, qapp) -> None:
        """A row whose line is remapped to a later row resets the column to the line start."""
        rows = [
            _row(DiffRowType.REMOVED, 1, None),
            _row(DiffRowType.CONTEXT, 2, 2),
        ]
        widget = _make_widget(rows)
        emitted = _capture(widget)

        widget._on_right_pane_open_in_editor_requested(0, 101)  # pylint: disable=protected-access

        assert emitted == [(2, 1)]

    def test_left_pane_remapped_row_discards_the_column(self, qapp) -> None:
        """A left-pane row whose line is remapped resets the column to the line start."""
        rows = [
            _row(DiffRowType.REMOVED, 1, None),
            _row(DiffRowType.CONTEXT, 2, 2),
        ]
        widget = _make_widget(rows)
        emitted = _capture(widget)

        widget._on_left_pane_open_in_editor_requested(0, 101)  # pylint: disable=protected-access

        assert emitted == [(2, 1)]

    def test_inline_pane_remapped_row_discards_the_column(self, qapp) -> None:
        """An inline-pane row whose line is remapped resets the column to the line start."""
        rows = [
            _row(DiffRowType.REMOVED, 1, None),
            _row(DiffRowType.CONTEXT, 2, 2),
        ]
        widget = _make_widget(rows)
        emitted = _capture(widget)

        widget._on_inline_pane_open_in_editor_requested(0, 101)  # pylint: disable=protected-access

        assert emitted == [(2, 1)]

    def test_row_without_any_right_line_falls_back_to_line_one(self, qapp) -> None:
        """When no later row carries a working-tree line, line 1 is used with column 1."""
        widget = _make_widget([_row(DiffRowType.REMOVED, 1, None)])
        emitted = _capture(widget)

        widget._on_right_pane_open_in_editor_requested(0, 7)  # pylint: disable=protected-access

        assert emitted == [(1, 1)]
