"""Tests for the editor widget's view-state codec."""

from desktop.editor_tab.editor_widget import EditorWidget


class TestEditorViewState:
    """Tests for EditorWidget.create_view_state and restore_view_state."""

    def test_view_state_does_not_include_content(self, qapp, mindspace_manager) -> None:
        """View state describes the view only, never the document content."""
        widget = EditorWidget(mindspace_manager)
        widget.setPlainText("hello")

        state = widget.create_view_state()

        assert "content" not in state

    def test_view_state_captures_syntax_and_scroll(self, qapp, mindspace_manager) -> None:
        """Syntax and scroll offsets are part of the view state."""
        widget = EditorWidget(mindspace_manager)
        widget.setPlainText("line one\nline two\nline three")
        widget.verticalScrollBar().setValue(2)

        state = widget.create_view_state()

        assert "syntax" in state
        assert state["vertical_scroll"] == 2
        assert "horizontal_scroll" in state
        assert "cursor" in state

    def test_restore_view_state_restores_scroll(self, qapp, mindspace_manager) -> None:
        """A saved scroll offset is reapplied."""
        widget = EditorWidget(mindspace_manager)
        widget.setPlainText("\n".join(f"line {i}" for i in range(200)))

        widget.restore_view_state({"vertical_scroll": 5, "horizontal_scroll": 0})

        assert widget.verticalScrollBar().value() == 5

    def test_restore_view_state_ignores_empty_state(self, qapp, mindspace_manager) -> None:
        """An empty state dictionary is a no-op."""
        widget = EditorWidget(mindspace_manager)
        widget.setPlainText("hello")

        widget.restore_view_state({})

        assert widget.toPlainText() == "hello"

    def test_restore_view_state_does_not_change_content(self, qapp, mindspace_manager) -> None:
        """Restoring view state leaves the document content untouched."""
        widget = EditorWidget(mindspace_manager)
        widget.setPlainText("original")

        widget.restore_view_state({"vertical_scroll": 0, "cursor": {"line": 0, "column": 0}})

        assert widget.toPlainText() == "original"
