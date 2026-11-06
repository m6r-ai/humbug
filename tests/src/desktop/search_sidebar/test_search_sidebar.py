"""Tests for the search pane's zoom-aware layout."""

import pytest
from PySide6.QtCore import QPoint

# pylint: disable=wrong-import-position
from desktop.search_sidebar.search_sidebar import SearchSidebar
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.style_manager import StyleManager


@pytest.fixture
def style_manager(qapp):  # pylint: disable=unused-argument
    """The style manager singleton, left unzoomed after the test."""
    manager = StyleManager()
    yield manager
    manager.set_zoom(1.0)


@pytest.fixture
def sidebar(style_manager, settle):  # pylint: disable=unused-argument
    """A showing search pane with no mindspace open."""
    widget = SearchSidebar()
    widget.apply_style()
    widget.resize(320, 480)
    widget.show()
    settle()

    yield widget
    widget.deleteLater()


def left_edge(sidebar, widget) -> int:
    """Return the left edge of widget in the search pane's coordinates."""
    return widget.mapTo(sidebar, QPoint(0, 0)).x()


def search_input_rule(stylesheet: str) -> str:
    """Return the search field's rule from the pane stylesheet."""
    start = stylesheet.index("QLineEdit#_search_input {")
    return stylesheet[start:stylesheet.index("}", start)]


class TestSearchPaneIndent:
    """The search field and the section title share one left edge."""

    def test_the_search_field_starts_where_the_section_title_starts(self, sidebar):
        title = sidebar._header._title_label  # pylint: disable=protected-access

        assert left_edge(sidebar, sidebar._search_input) == left_edge(sidebar, title)  # pylint: disable=protected-access

    def test_the_shared_indent_is_the_title_inset(self, sidebar):
        expected = StyleManager().scale(SidebarSectionHeader.TITLE_INSET)

        assert left_edge(sidebar, sidebar._search_input) == expected  # pylint: disable=protected-access

    def test_the_shared_indent_grows_with_the_zoom_factor(self, sidebar, style_manager, settle):
        before = left_edge(sidebar, sidebar._search_input)  # pylint: disable=protected-access

        style_manager.set_zoom(1.5)
        sidebar.apply_style()
        settle()

        assert left_edge(sidebar, sidebar._search_input) == round(before * 1.5)  # pylint: disable=protected-access

    def test_the_option_buttons_keep_the_same_indent_as_the_field(self, sidebar):
        field_margins = sidebar._search_layout.contentsMargins()  # pylint: disable=protected-access
        option_margins = sidebar._options_layout.contentsMargins()  # pylint: disable=protected-access

        assert option_margins.left() == field_margins.left()
        assert option_margins.right() == field_margins.right()

    def test_the_status_text_is_indented_like_the_section_title(self, sidebar):
        style_manager = StyleManager()
        inset = style_manager.scale(SidebarSectionHeader.TITLE_INSET)

        assert f"padding: {round(12 * style_manager.zoom_factor())}px {inset}px" in sidebar.styleSheet()

    def test_the_text_inside_the_field_is_indented_by_the_zoom_factor(self, sidebar, style_manager):
        style_manager.set_zoom(1.5)
        sidebar.apply_style()

        assert "padding: 6px;" in search_input_rule(sidebar.styleSheet())
