"""Tests for the geometry of the sidebar tree's inline rename editor."""

import os

import pytest
from PySide6.QtCore import QRect

# pylint: disable=wrong-import-position
from desktop.file_sidebar.file_sidebar import FileSidebar
from desktop.sidebar.sidebar_tree_inline_editor import SidebarTreeInlineEditor


@pytest.fixture
def sidebar_with_subfolder(mindspace_sandbox):
    """A showing FileSidebar whose one folder holds more files than fit on screen."""
    path = mindspace_sandbox.path
    subfolder = os.path.join(path, "subfolder")
    os.makedirs(subfolder)
    for i in range(40):
        with open(os.path.join(subfolder, f"file_{i:02d}.txt"), "w", encoding="utf-8") as f:
            f.write("")

    widget = FileSidebar()
    widget.set_mindspace(path)
    widget.apply_style()
    widget.resize(360, 240)
    widget.show()
    widget._tree_view.expand(widget._get_tree_index_for_path(subfolder))  # pylint: disable=protected-access
    yield widget, subfolder
    widget.deleteLater()


def _start_creating_file(sidebar: FileSidebar, folder: str, settle) -> None:
    """Create a file in the given folder and leave its name open for editing."""
    sidebar._start_new_file_creation(folder)  # pylint: disable=protected-access
    settle()


def _edited_row(sidebar: FileSidebar) -> tuple[SidebarTreeInlineEditor, QRect]:
    """Return the inline editor and the visual rectangle of the row it edits."""
    tree_view = sidebar._tree_view  # pylint: disable=protected-access
    editors = tree_view.viewport().findChildren(SidebarTreeInlineEditor)
    assert len(editors) == 1

    pending = sidebar._pending_new_item  # pylint: disable=protected-access
    assert pending

    index = sidebar._get_tree_index_for_path(pending[2])  # pylint: disable=protected-access
    assert index.isValid()

    return editors[0], tree_view.visualRect(index)


def _scroll_row_into_view(sidebar: FileSidebar, settle) -> None:
    """Scroll the tree so the row being edited sits well inside the viewport."""
    tree_view = sidebar._tree_view  # pylint: disable=protected-access
    _editor, visual_rect = _edited_row(sidebar)
    scroll_bar = tree_view.verticalScrollBar()
    scroll_bar.setValue(scroll_bar.value() + visual_rect.top() - 60)
    settle()


class TestInlineEditorGeometry:
    """The inline editor occupies the text area of its row and never covers the icon."""

    def test_editor_starts_after_the_icon_of_its_row(self, sidebar_with_subfolder, settle):
        sidebar, folder = sidebar_with_subfolder
        _start_creating_file(sidebar, folder, settle)

        editor, visual_rect = _edited_row(sidebar)

        assert editor.geometry().top() == visual_rect.top()
        assert editor.geometry().height() == visual_rect.height()
        assert editor.geometry().left() > visual_rect.left()
        assert editor.geometry().right() == visual_rect.right()

    def test_editor_stays_after_the_icon_when_the_tree_scrolls(self, sidebar_with_subfolder, settle):
        sidebar, folder = sidebar_with_subfolder
        _start_creating_file(sidebar, folder, settle)
        _scroll_row_into_view(sidebar, settle)

        editor, visual_rect = _edited_row(sidebar)

        assert editor.geometry().top() == visual_rect.top()
        assert editor.geometry().left() > visual_rect.left()
        assert editor.geometry().right() == visual_rect.right()
