"""Tests for icon scaling, including the uniform variant used by mixed lists."""
# pylint: disable=protected-access

import pytest
from PySide6.QtGui import QIcon, QRegion

from desktop.color_theme import ColorTheme
from desktop.quick_switcher.quick_switcher_widget import QuickSwitcherWidget
from desktop.style_manager import StyleManager


ICON_SIZE = 64

# Icons whose artwork is padded very differently in the icon set: "close" and the
# arrows sit inside wide margins, "usage" and "cog" reach almost to the edge.
MIXED_ICONS = ["close", "arrow-right", "arrow-left", "usage", "cog", "editor", "conversation", "log"]


def artwork_fill(pixmap) -> float:
    """Return the share of the pixmap's box its visible artwork spans."""
    bounds = QRegion(pixmap.mask()).boundingRect()
    return max(bounds.width(), bounds.height()) / pixmap.width()


@pytest.fixture
def style_manager(qapp):
    """The shared StyleManager."""
    return StyleManager()


class TestUniformIconScaling:
    """Tests for icons that have to sit in a column together."""

    def test_mixed_icons_end_up_the_same_size(self, style_manager):
        """A list of mixed icons must not look like a list of mixed sizes."""
        fills = [
            artwork_fill(style_manager.scale_icon_uniform(name, ICON_SIZE))
            for name in MIXED_ICONS
        ]

        assert max(fills) - min(fills) < 0.02

    def test_the_icon_set_is_not_uniform_without_it(self, style_manager):
        """The plain scaling keeps each icon's own proportions, which do vary."""
        fills = [
            artwork_fill(style_manager.scale_icon(name, ICON_SIZE))
            for name in MIXED_ICONS
        ]

        assert max(fills) - min(fills) > 0.2

    def test_the_pixmap_is_the_requested_size(self, style_manager):
        """Normalising the artwork must not change the box the icon occupies."""
        pixmap = style_manager.scale_icon_uniform("close", ICON_SIZE)
        plain = style_manager.scale_icon("close", ICON_SIZE)

        assert pixmap.size() == plain.size()

    def test_a_missing_icon_falls_back_to_plain_scaling(self, style_manager):
        """An icon with no artwork has no bounds to normalise, and must not raise."""
        pixmap = style_manager.scale_icon_uniform("no-such-icon", ICON_SIZE)

        assert pixmap.width() == style_manager.scale_icon("no-such-icon", ICON_SIZE).width()

    def test_changing_zoom_discards_the_old_size(self, style_manager):
        """An icon cached for the previous zoom must not be served at the new one."""
        original_zoom = style_manager.zoom_factor()
        style_manager.scale_icon_uniform("close", ICON_SIZE)
        stale_key = ("close", int(ICON_SIZE * original_zoom))
        assert stale_key in style_manager._uniform_icon_cache

        try:
            style_manager.set_zoom(original_zoom * 1.5)

            assert stale_key not in style_manager._uniform_icon_cache

        finally:
            style_manager.set_zoom(original_zoom)

    def test_changing_theme_re_renders_the_icon(self, style_manager):
        """An icon must be redrawn in the new theme's colours rather than served from cache."""
        original_theme = style_manager.user_color_theme()

        try:
            # Both themes are set explicitly: SYSTEM resolves to one of them, so
            # switching from it to its own resolved mode would change nothing.
            style_manager.set_color_theme(ColorTheme.LIGHT)
            light = style_manager.scale_icon_uniform("close", ICON_SIZE).toImage()

            style_manager.set_color_theme(ColorTheme.DARK)
            dark = style_manager.scale_icon_uniform("close", ICON_SIZE).toImage()

            assert dark != light

        finally:
            style_manager.set_color_theme(original_theme)


class TestQuickSwitcherIconRefresh:
    """Tests that the overlay's own icon caches follow a style change."""

    def test_the_file_icon_cache_is_dropped(self, qapp, host, style_manager):
        """Icons cached for the old theme and zoom must not survive a style change."""
        switcher = QuickSwitcherWidget(host)
        try:
            provider = switcher._file_icon_provider
            provider._cached_icons["stale-key"] = QIcon()

            switcher._on_style_changed()

            assert "stale-key" not in provider._cached_icons

        finally:
            # The overlay listens for style changes, so one left alive here would
            # repopulate the icon caches that the tests below assert are empty.
            switcher.setParent(None)  # type: ignore[call-overload]
            switcher.deleteLater()
            qapp.processEvents()
