"""Tests for the Palette hierarchy."""

import pytest

from desktop.color_role import ColorRole
from desktop.palette import (
    COLOR_BLIND_PALETTE,
    DARK_PALETTE,
    LIGHT_PALETTE,
    FixedPalette,
    OverlayPalette,
)


def _full_fixed(color: str = "#aabbcc") -> dict:
    """Build a flat colour table covering every ColorRole."""
    return {role: color for role in ColorRole}


class TestFixedPalette:
    def test_resolves_color(self):
        palette = FixedPalette(_full_fixed())
        assert palette.resolve(ColorRole.TEXT_PRIMARY) == "#aabbcc"

    def test_raises_on_missing_role(self):
        incomplete = {role: "#aabbcc" for role in ColorRole if role != ColorRole.TEXT_PRIMARY}
        with pytest.raises(ValueError, match="TEXT_PRIMARY"):
            FixedPalette(incomplete)

    def test_raises_lists_all_missing_roles(self):
        incomplete = {
            role: "#aabbcc"
            for role in ColorRole
            if role not in (ColorRole.TEXT_PRIMARY, ColorRole.BACKGROUND_PRIMARY)
        }
        with pytest.raises(ValueError) as exc_info:
            FixedPalette(incomplete)
        message = str(exc_info.value)
        assert "TEXT_PRIMARY" in message
        assert "BACKGROUND_PRIMARY" in message

    def test_accepts_complete_table(self):
        FixedPalette(_full_fixed())


class TestOverlayPalette:
    def test_falls_through_to_base_when_no_override(self):
        base = FixedPalette(_full_fixed("#111111"))
        overlay = OverlayPalette(base, {})
        assert overlay.resolve(ColorRole.TEXT_PRIMARY) == "#111111"

    def test_override_takes_precedence(self):
        base = FixedPalette(_full_fixed("#111111"))
        overlay = OverlayPalette(base, {ColorRole.TEXT_PRIMARY: "#ffffff"})
        assert overlay.resolve(ColorRole.TEXT_PRIMARY) == "#ffffff"

    def test_override_does_not_affect_other_role(self):
        base = FixedPalette(_full_fixed("#111111"))
        overlay = OverlayPalette(base, {ColorRole.TEXT_PRIMARY: "#ffffff"})
        assert overlay.resolve(ColorRole.BACKGROUND_PRIMARY) == "#111111"

    def test_overrides_returns_override_table(self):
        base = FixedPalette(_full_fixed())
        overrides = {ColorRole.TEXT_PRIMARY: "#ffffff"}
        overlay = OverlayPalette(base, overrides)
        assert overlay.overrides() is overrides

    def test_empty_string_override_falls_through(self):
        base = FixedPalette(_full_fixed("#111111"))
        overlay = OverlayPalette(base, {ColorRole.TEXT_PRIMARY: ""})
        assert overlay.resolve(ColorRole.TEXT_PRIMARY) == "#111111"


class TestBuiltinPalettes:
    def test_dark_palette_covers_all_roles(self):
        for role in ColorRole:
            assert DARK_PALETTE.resolve(role), f"DARK_PALETTE missing {role.name}"

    def test_light_palette_covers_all_roles(self):
        for role in ColorRole:
            assert LIGHT_PALETTE.resolve(role), f"LIGHT_PALETTE missing {role.name}"

    def test_color_blind_palette_covers_all_roles(self):
        for role in ColorRole:
            assert COLOR_BLIND_PALETTE.resolve(role), f"COLOR_BLIND_PALETTE missing {role.name}"

    def test_dark_and_light_differ(self):
        differences = [
            role for role in ColorRole
            if DARK_PALETTE.resolve(role) != LIGHT_PALETTE.resolve(role)
        ]
        assert differences, "Dark and light palettes should differ for at least some roles"
