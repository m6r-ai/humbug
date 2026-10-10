"""Tests for the shell widget's view-state codec."""

# pylint: disable=wrong-import-position
from desktop.shell_tab.shell_widget import ShellWidget


class TestShellViewState:
    """Tests for ShellWidget.create_view_state and restore_view_state."""

    def test_view_state_includes_input_draft(self, mindspace_sandbox) -> None:
        """The unsaved input draft is part of the view state."""
        widget = ShellWidget("t1")
        widget.set_input_text("a half-written command")

        state = widget.create_view_state()

        assert state["content"] == "a half-written command"

    def test_restore_view_state_restores_input_draft(self, mindspace_sandbox) -> None:
        """A saved input draft is reapplied to the input box."""
        widget = ShellWidget("t1")

        widget.restore_view_state({"content": "restored command"})

        assert widget._input.to_plain_text() == "restored command"  # pylint: disable=protected-access

    def test_migration_state_carries_input_draft(self, mindspace_sandbox) -> None:
        """Migration state carries the unsaved input across a column move."""
        widget = ShellWidget("t1")
        widget.set_input_text("move me")

        state = widget.create_migration_state()

        assert state["content"] == "move me"

    def test_restore_migration_state_restores_input_draft(self, mindspace_sandbox) -> None:
        """Restoring migration state reapplies the unsaved input."""
        widget = ShellWidget("t1")

        widget.restore_migration_state({"content": "moved draft"})

        assert widget._input.to_plain_text() == "moved draft"  # pylint: disable=protected-access
