"""Tests for the Trash panel's rail button visibility toggle."""

import pytest

# pylint: disable=wrong-import-position
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar_manager.sidebar_manager import SidebarManager
from desktop.trash_sidebar.trash_sidebar import TrashSidebar
from desktop.user.user_manager import UserManager


def _wire_trash_sidebar(_panel: SidebarBase, _mgr: SidebarManager) -> None:
    """No signals to wire for this test."""


@pytest.fixture
def manager(qapp, tmp_path, monkeypatch):  # pylint: disable=unused-argument
    """A SidebarManager with the Trash panel registered, hidden-by-default."""
    home_dir = tmp_path / "home"
    home_dir.mkdir()
    monkeypatch.setenv("HOME", str(home_dir))

    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access

    mgr = SidebarManager()
    mgr.register_panel(
        "trash", "trash", TrashSidebar, _wire_trash_sidebar, visibility_signal="visibility_requested"
    )
    yield mgr

    mgr.deleteLater()
    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access


class TestTrashPanelVisibility:
    """The Trash rail button starts hidden and follows visibility_requested."""

    def test_trash_button_starts_hidden(self, manager):
        button = manager._panel_buttons["trash"]  # pylint: disable=protected-access
        assert button.isHidden()

    def test_emitting_true_shows_the_button(self, manager):
        panel = manager.get_panel("trash")
        assert isinstance(panel, TrashSidebar)

        panel.visibility_requested.emit(True)

        button = manager._panel_buttons["trash"]  # pylint: disable=protected-access
        assert not button.isHidden()

    def test_emitting_false_hides_it_again(self, manager):
        panel = manager.get_panel("trash")
        assert isinstance(panel, TrashSidebar)

        panel.visibility_requested.emit(True)
        panel.visibility_requested.emit(False)

        button = manager._panel_buttons["trash"]  # pylint: disable=protected-access
        assert button.isHidden()
