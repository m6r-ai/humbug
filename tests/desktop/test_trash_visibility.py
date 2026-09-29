"""Tests for the Trash panel's rail button placement."""

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
    """A SidebarManager with the Trash panel registered in the bottom rail group."""
    home_dir = tmp_path / "home"
    home_dir.mkdir()
    monkeypatch.setenv("HOME", str(home_dir))

    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access

    mgr = SidebarManager()
    mgr.register_panel(
        "trash", "trash", TrashSidebar, _wire_trash_sidebar, place_at_bottom=True
    )
    yield mgr

    mgr.deleteLater()
    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access


class TestTrashPanelVisibility:
    """The Trash rail button is always visible and sits in the bottom rail group."""

    def test_trash_button_is_visible(self, manager):
        button = manager._panel_buttons["trash"]  # pylint: disable=protected-access
        assert not button.isHidden()

    def test_trash_button_sits_above_settings(self, manager):
        layout = manager._rail_layout  # pylint: disable=protected-access
        trash_button = manager._panel_buttons["trash"]  # pylint: disable=protected-access
        settings_button = manager._settings_button  # pylint: disable=protected-access
        assert layout.indexOf(trash_button) == layout.indexOf(settings_button) - 1

    def test_trash_button_sits_below_carousel(self, manager):
        layout = manager._rail_layout  # pylint: disable=protected-access
        trash_button = manager._panel_buttons["trash"]  # pylint: disable=protected-access
        carousel_button = manager._tab_carousel_button  # pylint: disable=protected-access
        assert layout.indexOf(trash_button) == layout.indexOf(carousel_button) + 1
