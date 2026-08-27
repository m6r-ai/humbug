"""Tests for SidebarTreeIconProvider's Pinned section icon."""

from PySide6.QtCore import QFileInfo

# pylint: disable=wrong-import-position
from desktop.sidebar.sidebar_tree_icon_provider import SidebarTreeIconProvider


class TestPinnedSectionIcon:
    """Tests for pinned_section_icon()."""

    def test_returns_nonnull_icon(self, qapp):
        """pinned_section_icon() returns a non-null icon with a valid pixmap."""
        provider = SidebarTreeIconProvider()

        icon = provider.pinned_section_icon()

        assert not icon.isNull()
        assert not icon.pixmap(16, 16).isNull()

    def test_is_cached(self, qapp):
        """Calling pinned_section_icon() twice returns the same cached instance."""
        provider = SidebarTreeIconProvider()

        first = provider.pinned_section_icon()
        second = provider.pinned_section_icon()

        assert first is second

    def test_update_icons_clears_cache(self, qapp):
        """update_icons() drops the cached icon so it regenerates on next use."""
        provider = SidebarTreeIconProvider()

        first = provider.pinned_section_icon()
        provider.update_icons()
        second = provider.pinned_section_icon()

        assert first is not second

    def test_distinct_from_a_file_icon(self, qapp):
        """pinned_section_icon() renders differently from an ordinary file icon."""
        provider = SidebarTreeIconProvider()
        size = round(16 * provider._style_manager.zoom_factor())  # pylint: disable=protected-access

        pinned_image = provider.pinned_section_icon().pixmap(size, size).toImage()
        file_image = provider.icon(QFileInfo("/tmp/chat.conv")).pixmap(size, size).toImage()

        assert pinned_image != file_image
