"""Tests for the sidebar manager's zoom-aware metrics and rail button tooltips."""

import pytest

# pylint: disable=wrong-import-position
from desktop.language.language_code import LanguageCode
from desktop.language.language_manager import LanguageManager
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar_manager.sidebar_manager import SidebarManager
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.style_manager import StyleManager


class _StubPanel(SidebarBase):
    """A panel with no content, used to exercise panel registration."""


def _wire_nothing(_panel: SidebarBase, _manager: SidebarManager) -> None:
    """Register a panel without connecting any of its signals."""


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


class TestRailButtonTooltips:
    """Resting the pointer on a rail icon names the panel that icon opens."""

    @pytest.fixture
    def panel_sidebar(self, sidebar):
        """A sidebar manager with one tooltipped panel registered."""
        sidebar.register_panel(
            "files", "files", _StubPanel, _wire_nothing, tooltip_key="mindspace_files",
        )
        yield sidebar

    def test_the_tooltip_names_the_panel(self, panel_sidebar):
        button = panel_sidebar.panel_button("files")

        assert button is not None
        assert button.toolTip() == "Files"

    def test_the_tooltip_follows_a_language_change(self, panel_sidebar):
        language_manager = LanguageManager()
        original_language = language_manager.current_language()
        language_manager.set_language(LanguageCode.FR)

        try:
            button = panel_sidebar.panel_button("files")

            assert button is not None
            assert button.toolTip() == "Fichiers"

        finally:
            language_manager.set_language(original_language)
