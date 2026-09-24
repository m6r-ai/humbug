"""Tests for the preview context model."""

from preview_context import PreviewContext


class TestPreviewContextScrollToLine:
    """Tests for PreviewContext.scroll_to_line."""

    def test_scroll_to_line_invokes_callback(self) -> None:
        """Requesting a scroll fires the callback with the line."""
        received: list[int] = []

        def record(line: int) -> bool:
            received.append(line)
            return True

        context = PreviewContext(
            "ctx-1", "/tmp/example.md", [], on_scroll_to_line=record
        )

        result = context.scroll_to_line(12)

        assert result is True
        assert received == [12]

    def test_scroll_to_line_without_callback_is_noop(self) -> None:
        """Without a callback the scroll request reports failure and does nothing."""
        context = PreviewContext("ctx-1", "/tmp/example.md", [])

        assert context.scroll_to_line(12) is False

    def test_scroll_to_line_reports_callback_result(self) -> None:
        """The callback's return value is propagated to the caller."""
        context = PreviewContext(
            "ctx-1", "/tmp/example.md", [], on_scroll_to_line=lambda _line: False
        )

        assert context.scroll_to_line(12) is False
