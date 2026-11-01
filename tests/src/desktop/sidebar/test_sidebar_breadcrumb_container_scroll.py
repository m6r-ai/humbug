"""Tests that the breadcrumb container's scrollbar follows the tree's own scrolling."""

import os

import pytest

# pylint: disable=wrong-import-position
from desktop.file_sidebar.file_sidebar import FileSidebar


@pytest.fixture
def sidebar_with_folder(mindspace_sandbox, settle):
    """A showing FileSidebar with a folder holding more files than fit on screen."""
    path = mindspace_sandbox.path
    subfolder = os.path.join(path, "subfolder")
    os.makedirs(subfolder)
    for i in range(60):
        with open(os.path.join(subfolder, f"file_{i:02d}.txt"), "w", encoding="utf-8") as f:
            f.write("")

    widget = FileSidebar()
    widget.set_mindspace(path)
    widget.apply_style()
    widget.resize(360, 240)
    widget.show()
    widget._tree_view.expand(widget._get_tree_index_for_path(subfolder))  # pylint: disable=protected-access
    settle()
    yield widget, subfolder
    widget.deleteLater()


def _scroll_positions(sidebar: FileSidebar) -> tuple[int, int]:
    """Return the tree's scroll position and the external scrollbar's position."""
    tree_value = sidebar._tree_view.verticalScrollBar().value()  # pylint: disable=protected-access
    container = sidebar._bc_container  # pylint: disable=protected-access
    return tree_value, container._scrollbar.value()  # pylint: disable=protected-access


class TestScrollbarFollowsTree:
    """The external scrollbar always holds the tree's real position."""

    def test_scrollbar_follows_a_scroll_the_tree_performs_itself(self, sidebar_with_folder, settle):
        sidebar, _folder = sidebar_with_folder
        tree_view = sidebar._tree_view  # pylint: disable=protected-access
        container = sidebar._bc_container  # pylint: disable=protected-access
        _tree_value, before = _scroll_positions(sidebar)

        tree_view.verticalScrollBar().setValue(tree_view.verticalScrollBar().maximum())
        settle()

        tree_value, after = _scroll_positions(sidebar)
        bc_h = max(0, container._breadcrumb_rows - 1) * container._row_height  # pylint: disable=protected-access
        assert tree_value > 0
        assert after == tree_value - bc_h
        assert after != before

    def test_creating_an_item_leaves_the_scrollbar_where_the_tree_is(self, sidebar_with_folder, settle):
        sidebar, folder = sidebar_with_folder
        container = sidebar._bc_container  # pylint: disable=protected-access

        sidebar._start_new_file_creation(folder)  # pylint: disable=protected-access
        settle()

        tree_value, external_value = _scroll_positions(sidebar)
        bc_h = max(0, container._breadcrumb_rows - 1) * container._row_height  # pylint: disable=protected-access
        assert external_value == max(0, tree_value - bc_h)
