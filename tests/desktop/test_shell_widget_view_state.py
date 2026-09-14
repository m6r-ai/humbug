"""Tests for the shell widget's view-state codec."""

import pytest

# pylint: disable=wrong-import-position
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.shell_tab.shell_widget import ShellWidget
from desktop.user.user_manager import UserManager


@pytest.fixture
def shell_env(qapp, tmp_path, monkeypatch):
    """Open a real mindspace in a sandboxed HOME."""
    home_dir = tmp_path / "home"
    home_dir.mkdir()
    monkeypatch.setenv("HOME", str(home_dir))

    MindspaceManager._instance = None
    UserManager._instance = None

    mgr = MindspaceManager()
    mgr._home_config = str(tmp_path / "mindspace.json")  # pylint: disable=protected-access
    ms_path = str(tmp_path / "mindspace")
    mgr.create_mindspace(ms_path, [])
    mgr.open_mindspace(ms_path)

    yield mgr

    MindspaceManager._instance = None
    UserManager._instance = None


class TestShellViewState:
    """Tests for ShellWidget.create_view_state and restore_view_state."""

    def test_view_state_includes_input_draft(self, shell_env) -> None:
        """The unsaved input draft is part of the view state."""
        widget = ShellWidget("t1")
        widget.set_input_text("a half-written command")

        state = widget.create_view_state()

        assert state["content"] == "a half-written command"

    def test_restore_view_state_restores_input_draft(self, shell_env) -> None:
        """A saved input draft is reapplied to the input box."""
        widget = ShellWidget("t1")

        widget.restore_view_state({"content": "restored command"})

        assert widget._input.to_plain_text() == "restored command"  # pylint: disable=protected-access

    def test_migration_state_carries_input_draft(self, shell_env) -> None:
        """Migration state carries the unsaved input across a column move."""
        widget = ShellWidget("t1")
        widget.set_input_text("move me")

        state = widget.create_migration_state()

        assert state["content"] == "move me"

    def test_restore_migration_state_restores_input_draft(self, shell_env) -> None:
        """Restoring migration state reapplies the unsaved input."""
        widget = ShellWidget("t1")

        widget.restore_migration_state({"content": "moved draft"})

        assert widget._input.to_plain_text() == "moved draft"  # pylint: disable=protected-access
