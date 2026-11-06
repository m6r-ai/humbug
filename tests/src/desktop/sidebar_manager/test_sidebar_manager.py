"""Tests for the sidebar manager's zoom-aware metrics."""

import pytest

# pylint: disable=wrong-import-position
from desktop.sidebar_manager.sidebar_manager import SidebarManager
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.style_manager import StyleManager


@pytest.fixture
def style_manager(qapp):  # pylint: disable=unused-argument
    """The style manager singleton, left unzoomed after the test."""
    manager = StyleManager()
    yield manager
    manager.set_zoom(1.0)


@pytest.fixture
def sidebar(style_manager):  # pylint: disable=unused-argument
    """A sidebar manager with no panels registered."""
    widget = SidebarManager()
    widget.apply_style()
    yield widget
    widget.deleteLater()


def header_rule(stylesheet: str) -> str:
    """Return the mindspace name header rule from a sidebar stylesheet."""
    start = stylesheet.index("QPushButton#_header_widget {")
    return stylesheet[start:stylesheet.index("}", start)]


class TestMindspaceNameHeaderIndent:
    """The mindspace name shares the panel type label's indent, at every zoom factor."""

    def test_the_indent_is_the_title_inset_when_not_zoomed(self, sidebar):
        inset = StyleManager().scale(SidebarSectionHeader.TITLE_INSET)

        assert f"padding: 10px {inset}px 6px {inset}px;" in header_rule(sidebar.styleSheet())

    def test_the_indent_grows_with_the_zoom_factor(self, sidebar, style_manager):
        style_manager.set_zoom(1.5)
        sidebar.apply_style()
        inset = round(SidebarSectionHeader.TITLE_INSET * 1.5)

        assert f"padding: 15px {inset}px 9px {inset}px;" in header_rule(sidebar.styleSheet())


class TestExpandedSidebarWidth:
    """The recorded sidebar width is independent of the zoom factor."""

    def test_the_width_grows_with_the_zoom_factor(self, sidebar, style_manager):
        style_manager.set_zoom(1.5)

        assert sidebar.expanded_sidebar_width() == 480

    def test_a_recorded_width_is_reported_unchanged_at_the_zoom_it_was_recorded(self, sidebar, style_manager):
        style_manager.set_zoom(1.5)
        sidebar.set_expanded_sidebar_width(480)

        assert sidebar.expanded_sidebar_width() == 480

    def test_a_recorded_width_rescales_when_the_zoom_factor_changes(self, sidebar, style_manager):
        style_manager.set_zoom(1.5)
        sidebar.set_expanded_sidebar_width(480)
        style_manager.set_zoom(1.0)

        assert sidebar.expanded_sidebar_width() == 320
