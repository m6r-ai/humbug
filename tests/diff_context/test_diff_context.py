"""Tests for the diff context model."""

from diff_context import DiffContext


class TestDiffContext:
    """Tests for DiffContext."""

    def test_context_id_is_returned(self) -> None:
        """The context id supplied at construction is returned."""
        context = DiffContext("ctx-1", "/tmp/example.py")

        assert context.context_id() == "ctx-1"

    def test_save_content_state_persists_path(self) -> None:
        """Only the path is persisted, since the diff is regenerated on restore."""
        context = DiffContext("ctx-1", "/tmp/example.py")

        assert context.save_content_state() == {"path": "/tmp/example.py"}

    def test_get_info_reports_path_and_filename(self) -> None:
        """The info dictionary reports the path and its basename."""
        context = DiffContext("ctx-1", "/tmp/example.py")

        assert context.get_info() == {
            "path": "/tmp/example.py",
            "filename": "example.py",
        }

    def test_scroll_to_line_invokes_callback(self) -> None:
        """Requesting a scroll fires the callback with the line."""
        received: list[int] = []

        def record(line: int) -> bool:
            received.append(line)
            return True

        context = DiffContext("ctx-1", "/tmp/example.py", on_scroll_to_line=record)

        result = context.scroll_to_line(42)

        assert result is True
        assert received == [42]

    def test_scroll_to_line_without_callback_is_noop(self) -> None:
        """Without a callback the scroll request reports failure and does nothing."""
        context = DiffContext("ctx-1", "/tmp/example.py")

        assert context.scroll_to_line(42) is False

    def test_scroll_to_line_reports_callback_result(self) -> None:
        """The callback's return value is propagated to the caller."""
        context = DiffContext("ctx-1", "/tmp/example.py", on_scroll_to_line=lambda _line: False)

        assert context.scroll_to_line(42) is False
