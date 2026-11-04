"""Tests for the main window splitter keeping the sidebar in proportion with the zoom factor."""

import pytest
from PySide6.QtCore import Qt
from PySide6.QtWidgets import QWidget

# pylint: disable=wrong-import-position
from desktop.main_window_splitter import MainWindowSplitter
from desktop.sidebar_manager.sidebar_manager import SidebarManager
from desktop.style_manager import StyleManager


@pytest.fixture
def style_manager(qapp):  # pylint: disable=unused-argument
    """The style manager singleton, left unzoomed after the test."""
    manager = StyleManager()
    yield manager
    manager.set_zoom(1.0)


@pytest.fixture
def splitter(style_manager):  # pylint: disable=unused-argument
    """A splitter holding a sidebar beside a tab area, with the sidebar expanded."""
    sidebar = SidebarManager()
    sidebar.apply_style()

    widget = MainWindowSplitter(Qt.Orientation.Horizontal)
    widget.addWidget(sidebar)
    widget.addWidget(QWidget())
    widget.resize(1200, 800)
    widget.show()
    widget.setSizes([sidebar.expanded_sidebar_width(), 1200 - sidebar.expanded_sidebar_width()])

    yield widget
    widget.deleteLater()


def sidebar_of(splitter):
    """Return the sidebar manager held by the splitter."""
    widget = splitter.widget(0)
    assert isinstance(widget, SidebarManager)
    return widget


class TestZoomResizesTheSidebar:
    """Zooming moves the splitter so the sidebar grows and shrinks with the text it holds."""

    def test_the_sidebar_grows_when_the_zoom_factor_increases(self, splitter, style_manager):
        style_manager.set_zoom(1.5)
        sidebar_of(splitter).apply_style()

        splitter.apply_sidebar_width()

        assert splitter.sizes()[0] == 480

    def test_the_sidebar_shrinks_when_the_zoom_factor_returns_to_normal(self, splitter, style_manager):
        style_manager.set_zoom(1.5)
        sidebar_of(splitter).apply_style()
        splitter.apply_sidebar_width()

        style_manager.set_zoom(1.0)
        sidebar_of(splitter).apply_style()
        splitter.apply_sidebar_width()

        assert splitter.sizes()[0] == 320

    def test_a_collapsed_sidebar_tracks_the_rail_width(self, splitter, style_manager):
        sidebar_of(splitter).set_collapsed(True)

        style_manager.set_zoom(1.5)
        sidebar_of(splitter).apply_style()
        splitter.apply_sidebar_width()

        assert splitter.sizes()[0] == sidebar_of(splitter).rail_width()
