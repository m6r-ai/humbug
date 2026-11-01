"""Tests for the Files panel header create buttons."""

import os

import pytest
from PySide6.QtCore import QPoint, Qt
from PySide6.QtTest import QTest

# pylint: disable=wrong-import-position
from desktop.file_sidebar.file_sidebar import FileSidebar


@pytest.fixture
def sidebar_env(mindspace_sandbox):
    """The sandboxed mindspace, as (mindspace_manager, mindspace_path)."""
    yield mindspace_sandbox.manager, mindspace_sandbox.path


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


class TestHeaderButtonsCreateInTheSelectedFolder:
    """The header's create buttons act on the selection, as the context menu does."""

    @pytest.fixture
    def src_dir(self, sidebar_env):
        """A folder holding one file."""
        _mgr, ms_path = sidebar_env
        src = os.path.join(ms_path, "src")
        os.makedirs(src)
        with open(os.path.join(src, "main.py"), "w", encoding="utf-8") as f:
            f.write("")

        return src

    def _select(self, sidebar, path: str) -> None:
        source_index = sidebar._fs_model.index(path)  # pylint: disable=protected-access
        index = sidebar._filter_model.mapFromSource(source_index)  # pylint: disable=protected-access
        assert index.isValid()
        sidebar._tree_view.setCurrentIndex(index)  # pylint: disable=protected-access

    def test_new_file_goes_in_the_selected_folder(self, sidebar, src_dir):
        self._select(sidebar, src_dir)

        sidebar._new_file_button.click()  # pylint: disable=protected-access

        assert os.path.exists(os.path.join(src_dir, "New File.txt"))

    def test_new_folder_goes_beside_the_selected_file(self, sidebar, src_dir):
        self._select(sidebar, os.path.join(src_dir, "main.py"))

        sidebar._new_folder_button.click()  # pylint: disable=protected-access

        assert os.path.isdir(os.path.join(src_dir, "New Folder"))

    def test_clicking_empty_space_goes_back_to_the_root(self, sidebar, sidebar_env, src_dir):
        """Clicking below the items deselects, as in other file trees."""
        _mgr, ms_path = sidebar_env
        sidebar.resize(300, 600)
        sidebar.show()
        self._select(sidebar, src_dir)
        assert sidebar._tree_view.folder_for_new_item(ms_path) == src_dir  # pylint: disable=protected-access
        viewport = sidebar._tree_view.viewport()  # pylint: disable=protected-access

        QTest.mouseClick(viewport, Qt.MouseButton.LeftButton, Qt.KeyboardModifier.NoModifier, QPoint(5, viewport.height() - 5))
        sidebar._new_folder_button.click()  # pylint: disable=protected-access

        assert os.path.isdir(os.path.join(ms_path, "New Folder"))

    def test_escape_goes_back_to_the_root(self, sidebar, sidebar_env, src_dir):
        """A tree too full to have empty space can still be deselected."""
        _mgr, ms_path = sidebar_env
        self._select(sidebar, src_dir)
        assert sidebar._tree_view.folder_for_new_item(ms_path) == src_dir  # pylint: disable=protected-access

        QTest.keyClick(sidebar._tree_view, Qt.Key.Key_Escape)  # pylint: disable=protected-access
        sidebar._new_folder_button.click()  # pylint: disable=protected-access

        assert os.path.isdir(os.path.join(ms_path, "New Folder"))

    def test_clicking_the_root_crumb_goes_back_to_the_root(self, sidebar, sidebar_env, src_dir):
        """The root crumb is always shown, however full the tree is."""
        _mgr, ms_path = sidebar_env
        self._select(sidebar, src_dir)
        assert sidebar._tree_view.folder_for_new_item(ms_path) == src_dir  # pylint: disable=protected-access

        sidebar._bc_container.scroll_to_path(ms_path)  # pylint: disable=protected-access
        sidebar._new_folder_button.click()  # pylint: disable=protected-access

        assert os.path.isdir(os.path.join(ms_path, "New Folder"))
