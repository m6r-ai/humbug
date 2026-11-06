"""Tests that the breadcrumb bar follows the zoom factor vertically."""

import json
import os

import pytest

# pylint: disable=wrong-import-position
from desktop.conversation_sidebar.conversation_sidebar import ConversationSidebar
from desktop.file_sidebar.file_sidebar import FileSidebar
from desktop.style_manager import StyleManager


@pytest.fixture
def style_manager(qapp):  # pylint: disable=unused-argument
    """The style manager singleton, left unzoomed after the test."""
    manager = StyleManager()
    yield manager
    manager.set_zoom(1.0)


@pytest.fixture
def populated_mindspace(mindspace_sandbox):
    """The sandboxed mindspace, with content so both sidebar trees have rows."""
    folder = os.path.join(mindspace_sandbox.path, "src")
    os.makedirs(folder)
    with open(os.path.join(folder, "main.py"), "w", encoding="utf-8") as f:
        f.write("")

    conversation = os.path.join(mindspace_sandbox.conversations_dir, "one.conv")
    with open(conversation, "w", encoding="utf-8") as f:
        json.dump({
            "metadata": {"version": "0.1", "parent": None},
            "conversation": [{"id": "m1", "source": "user"}],
        }, f)

    return mindspace_sandbox


@pytest.fixture(params=[FileSidebar, ConversationSidebar], ids=["files", "conversations"])
def sidebar(request, populated_mindspace, settle):
    """A showing sidebar whose breadcrumb bar has something to show."""
    widget = request.param()
    widget.set_mindspace(populated_mindspace.path)
    widget.apply_style()
    widget.resize(320, 480)
    widget.show()
    settle()

    yield widget
    widget.deleteLater()


def breadcrumb_bar(sidebar):
    """Return the sidebar's breadcrumb bar."""
    return sidebar._breadcrumb_bar  # pylint: disable=protected-access


def breadcrumb_height(sidebar) -> int:
    """Return the height the breadcrumb bar is laid out at."""
    return breadcrumb_bar(sidebar).height()


def breadcrumb_row_height(sidebar) -> int:
    """Return the height of one breadcrumb row at the current zoom factor."""
    bar = breadcrumb_bar(sidebar)
    return bar.rowHeight(bar.indexAt(bar.rect().topLeft()))


class TestBreadcrumbBarHeight:
    """The breadcrumb bar is laid out tall enough for its rows at the current zoom factor."""

    def test_the_bar_is_showing_before_the_zoom_factor_changes(self, sidebar):
        assert breadcrumb_height(sidebar) > 0

    def test_the_bar_grows_when_zooming_in(self, sidebar, style_manager, settle):
        before = breadcrumb_height(sidebar)

        style_manager.set_zoom(1.5)
        sidebar.apply_style()
        settle()

        assert breadcrumb_height(sidebar) > before

    def test_the_bar_shrinks_back_when_zooming_out(self, sidebar, style_manager, settle):
        before = breadcrumb_height(sidebar)
        style_manager.set_zoom(1.5)
        sidebar.apply_style()
        settle()

        style_manager.set_zoom(1.0)
        sidebar.apply_style()
        settle()

        assert breadcrumb_height(sidebar) == before

    def test_the_rows_fit_after_zooming_in(self, sidebar, style_manager, settle):
        style_manager.set_zoom(1.5)
        sidebar.apply_style()
        settle()

        assert breadcrumb_height(sidebar) >= breadcrumb_row_height(sidebar)
