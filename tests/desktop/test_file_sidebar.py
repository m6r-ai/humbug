"""Tests for the Files panel header create buttons."""

import os

import pytest

# pylint: disable=wrong-import-position
from desktop.file_sidebar.file_sidebar import FileSidebar
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.user.user_manager import UserManager


@pytest.fixture
def sidebar_env(qapp, tmp_path, monkeypatch):  # pylint: disable=unused-argument
    """Open a real mindspace in a sandboxed HOME and return (mindspace_manager, mindspace_path)."""
    home_dir = tmp_path / "home"
    home_dir.mkdir()
    monkeypatch.setenv("HOME", str(home_dir))

    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access

    mgr = MindspaceManager()
    mgr._home_config = str(tmp_path / "mindspace.json")  # pylint: disable=protected-access
    ms_path = str(tmp_path / "mindspace")
    mgr.create_mindspace(ms_path, [])
    mgr.open_mindspace(ms_path)

    yield mgr, ms_path

    MindspaceManager._instance = None  # pylint: disable=protected-access
    UserManager._instance = None  # pylint: disable=protected-access


@pytest.fixture
def sidebar(sidebar_env):
    """A FileSidebar configured for the sandboxed mindspace."""
    _mgr, ms_path = sidebar_env
    widget = FileSidebar()
    widget.set_mindspace(ms_path)
    yield widget
    widget.deleteLater()


class TestNewFileAndFolderButtons:
    """The header's create-file and create-folder buttons track mindspace availability."""

    def test_buttons_start_disabled_with_no_mindspace(self, qapp):  # pylint: disable=unused-argument
        widget = FileSidebar()
        assert not widget._new_file_button.isEnabled()  # pylint: disable=protected-access
        assert not widget._new_folder_button.isEnabled()  # pylint: disable=protected-access
        widget.deleteLater()

    def test_buttons_enabled_once_mindspace_is_set(self, sidebar):
        assert sidebar._new_file_button.isEnabled()  # pylint: disable=protected-access
        assert sidebar._new_folder_button.isEnabled()  # pylint: disable=protected-access

    def test_buttons_disabled_when_mindspace_is_cleared(self, sidebar):
        sidebar.set_mindspace("")
        assert not sidebar._new_file_button.isEnabled()  # pylint: disable=protected-access
        assert not sidebar._new_folder_button.isEnabled()  # pylint: disable=protected-access

    def test_new_file_button_creates_file_in_mindspace_root(self, sidebar, sidebar_env):
        _mgr, ms_path = sidebar_env

        sidebar._new_file_button.click()  # pylint: disable=protected-access

        assert os.path.exists(os.path.join(ms_path, "New File.txt"))

    def test_new_folder_button_creates_folder_in_mindspace_root(self, sidebar, sidebar_env):
        _mgr, ms_path = sidebar_env

        sidebar._new_folder_button.click()  # pylint: disable=protected-access

        assert os.path.isdir(os.path.join(ms_path, "New Folder"))
