"""Palette abstractions and built-in palette instances for application colour themes."""

from typing import Dict

from desktop.color_role import ColorRole


class Palette:
    """Base class for application colour palettes."""

    def resolve(self, role: ColorRole) -> str:
        """Return the hex colour string for the given role."""
        raise NotImplementedError


class FixedPalette(Palette):
    """A palette backed by a flat role-to-colour mapping.

    Raises ValueError at construction time if any ColorRole is missing,
    so gaps are caught immediately rather than silently falling back.
    """

    def __init__(self, colors: Dict[ColorRole, str]) -> None:
        missing = [role for role in ColorRole if role not in colors]
        if missing:
            missing_names = ", ".join(r.name for r in missing)
            raise ValueError(f"FixedPalette is missing ColorRole entries: {missing_names}")

        self._colors = colors

    def resolve(self, role: ColorRole) -> str:
        return self._colors[role]


class OverlayPalette(Palette):
    """A palette that applies per-role overrides on top of a base palette.

    Used for the custom theme: most roles fall through to the base palette,
    only the overridden roles return the custom colour.
    """

    def __init__(self, base: FixedPalette, overrides: Dict[ColorRole, str]) -> None:
        self._base = base
        self._overrides = overrides

    def resolve(self, role: ColorRole) -> str:
        override = self._overrides.get(role)
        if override:
            return override

        return self._base.resolve(role)

    def overrides(self) -> Dict[ColorRole, str]:
        """Return the current override table."""
        return self._overrides


_DARK_COLORS: Dict[ColorRole, str] = {
    # Brand colours
    ColorRole.BRAND_PRIMARY: "#9b87f5",
    ColorRole.BRAND_GRADIENT_START: "#29c5ff",
    ColorRole.BRAND_GRADIENT_END: "#c050ff",
    ColorRole.BRAND_ICON_BG_START: "#1e0e50",
    ColorRole.BRAND_ICON_BG_END: "#060612",

    # Background colours
    ColorRole.BACKGROUND_PRIMARY: "#080808",
    ColorRole.BACKGROUND_SECONDARY: "#282828",
    ColorRole.BACKGROUND_GRADIENT_START: "#080808",
    ColorRole.BACKGROUND_GRADIENT_END: "#080808",
    ColorRole.BACKGROUND_TERTIARY: "#060606",
    ColorRole.BACKGROUND_TERTIARY_HOVER: "#303030",
    ColorRole.BACKGROUND_TERTIARY_PRESSED: "#505050",
    ColorRole.BACKGROUND_DIALOG: "#1e1e1e",
    ColorRole.LOGO_BACKGROUND: "#303030",

    # Text colours
    ColorRole.TEXT_PRIMARY: "#d8d8d8",
    ColorRole.TEXT_BRIGHT: "#ffffff",
    ColorRole.TEXT_HEADING: "#ffe0a0",
    ColorRole.TEXT_HEADING_BRIGHT: "#ffe8a0",
    ColorRole.TEXT_DISABLED: "#707070",
    ColorRole.TEXT_SELECTED: "#404058",
    ColorRole.TEXT_FOUND: "#885050",
    ColorRole.TEXT_FOUND_DIM: "#583838",
    ColorRole.TEXT_RECOMMENDED: "#ffffff",
    ColorRole.TEXT_LINK: "#5080ff",
    ColorRole.TEXT_INACTIVE: "#909090",
    ColorRole.TEXT_EPHEMERAL: "#e0a080",
    ColorRole.TEXT_EPHEMERAL_INACTIVE: "#a06040",
    ColorRole.TEXT_ERROR: "#e03020",
    ColorRole.TEXT_ERROR_INACTIVE: "#e06050",
    ColorRole.TEXT_SUCCESS: "#4ade80",

    # Edit box colours
    ColorRole.EDIT_BOX_BORDER: "#6060c0",
    ColorRole.EDIT_BOX_BACKGROUND: "#242454",
    ColorRole.EDIT_BOX_ERROR: "#c03020",

    # Mindspace colours
    ColorRole.MINDSPACE_BACKGROUND: "#101010",
    ColorRole.MINDSPACE_NAME_BACKGROUND: "#383838",
    ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER: "#585858",
    ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED: "#707070",
    ColorRole.MINDSPACE_HEADING: "#242424",
    ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND: "#181818",
    ColorRole.MINDSPACE_FOLDER: "#4B89DC",
    ColorRole.MINDSPACE_FOLDER_BREADCRUMB: "#F0C0C8",

    # Tab colours
    ColorRole.TAB_BAR_BACKGROUND: "#1c1c1c",
    ColorRole.TAB_BACKGROUND_ACTIVE: "#000000",
    ColorRole.TAB_BACKGROUND_INACTIVE: "#303030",
    ColorRole.TAB_BACKGROUND_HOVER: "#242454",
    ColorRole.TAB_BACKGROUND_UPDATED: "#3c2054",
    ColorRole.TAB_BORDER_ACTIVE: "#e03826",

    # Button colours
    ColorRole.BUTTON_BACKGROUND: "#303030",
    ColorRole.BUTTON_BACKGROUND_PRESSED: "#505050",
    ColorRole.BUTTON_BACKGROUND_HOVER: "#404040",
    ColorRole.BUTTON_SECONDARY_BACKGROUND: "#2c2c2c",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED: "#505050",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER: "#404040",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED: "#2050c0",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED: "#4070e0",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER: "#3060d0",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE: "#c03020",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED: "#e05040",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER: "#d04030",
    ColorRole.BUTTON_BACKGROUND_EDIT: "#b07010",
    ColorRole.BUTTON_BACKGROUND_EDIT_PRESSED: "#906000",
    ColorRole.BUTTON_BACKGROUND_EDIT_HOVER: "#c08020",
    ColorRole.BUTTON_BACKGROUND_DISABLED: "#282828",

    # Switch colours
    ColorRole.SWITCH_TRACK_ON: "#5a6893",
    ColorRole.SWITCH_TRACK_OFF: "#303030",
    ColorRole.SWITCH_TRACK_DISABLED_ON: "#3d4670",
    ColorRole.SWITCH_TRACK_BORDER: "#505050",
    ColorRole.SWITCH_KNOB: "#d8d8d8",
    ColorRole.SWITCH_KNOB_DISABLED: "#909090",

    # Menu elements
    ColorRole.MENU_BACKGROUND: "#2d2d2d",
    ColorRole.MENU_HOVER: "#3060d0",
    ColorRole.MENU_BORDER: "#606060",
    ColorRole.COMBO_ITEM_HOVER: "#484848",

    # Splitter bars
    ColorRole.SPLITTER: "#606060",
    ColorRole.TAB_SPLITTER: "#040404",

    # Scroll bar elements
    ColorRole.SCROLLBAR_BACKGROUND: "#2d2d2d",
    ColorRole.SCROLLBAR_HANDLE: "#404040",

    # Code block border
    ColorRole.CODE_BORDER: "#282828",

    # Blockquote elements
    ColorRole.BLOCKQUOTE_BORDER: "#65614c",
    ColorRole.BLOCKQUOTE_BACKGROUND: "#26261d",

    # Table elements
    ColorRole.TABLE_BORDER: "#808080",
    ColorRole.TABLE_HEADER_BACKGROUND: "#484838",

    # Message colours
    ColorRole.MESSAGE_BACKGROUND: "#121212",
    ColorRole.MESSAGE_BACKGROUND_HOVER: "#404040",
    ColorRole.MESSAGE_BACKGROUND_PRESSED: "#585858",
    ColorRole.MESSAGE_USER_BACKGROUND: "#282828",
    ColorRole.MESSAGE_USER_BACKGROUND_HOVER: "#484848",
    ColorRole.MESSAGE_USER_BACKGROUND_PRESSED: "#606060",
    # The input background has an alpha channel so it allows a small amount of what's underneath it to show through.
    ColorRole.MESSAGE_INPUT_BACKGROUND: "#ea383838",
    ColorRole.MESSAGE_ATTACHMENT_BACKGROUND: "#383838",
    ColorRole.MESSAGE_BORDER: "#303030",
    ColorRole.MESSAGE_USER_BORDER: "#484848",
    ColorRole.MESSAGE_INPUT_BORDER: "#707070",
    ColorRole.MESSAGE_SPOTLIGHTED: "#788ca0",
    ColorRole.MESSAGE_USER: "#7090e0",
    ColorRole.MESSAGE_AI: "#80c080",
    ColorRole.MESSAGE_REASONING: "#808080",
    ColorRole.MESSAGE_TOOL_CALL: "#808080",
    ColorRole.MESSAGE_TOOL_RESULT: "#808080",
    ColorRole.MESSAGE_USER_QUEUED: "#a0a0a0",
    ColorRole.MESSAGE_SYSTEM_ERROR: "#c08080",
    ColorRole.MESSAGE_SYSTEM_SUCCESS: "#80c080",
    ColorRole.MESSAGE_SYNTAX: "#a07850",
    ColorRole.MESSAGE_STREAMING: "#c0a080",
    ColorRole.MESSAGE_TRACE: "#a0a0a0",
    ColorRole.MESSAGE_INFORMATION: "#80b0f0",
    ColorRole.MESSAGE_WARNING: "#f0c040",
    ColorRole.MESSAGE_ERROR: "#ff6060",

    # Status bar elements
    ColorRole.STATUS_BAR_BACKGROUND: "#121212",
    ColorRole.CANARY_BACKGROUND: "#802020",

    # Close button states
    ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER: "#e03030",

    # Drop target highlights
    ColorRole.DROP_TARGET_HIGHLIGHT: "#142454",
    ColorRole.DROP_TARGET_SEPARATOR_HIGHLIGHT: "#a8b8f8",

    # Line numbers
    ColorRole.LINE_NUMBER: "#606060",

    # Diff view colours
    ColorRole.DIFF_REMOVED_BACKGROUND: "#402020",
    ColorRole.DIFF_ADDED_BACKGROUND: "#204020",
    ColorRole.DIFF_CHANGED_BACKGROUND: "#202050",
    ColorRole.DIFF_HUNK_LINE_NUMBER: "#e0e0e0",

    # VCS status colours
    ColorRole.VCS_MODIFIED: "#f0c040",
    ColorRole.VCS_ADDED: "#68b068",
    ColorRole.VCS_DELETED: "#f08080",
    ColorRole.VCS_RENAMED: "#8080c0",

    # Usage model colours
    ColorRole.USAGE_MODEL_COLOR_1: "#4C9BE8",
    ColorRole.USAGE_MODEL_COLOR_2: "#4EC994",
    ColorRole.USAGE_MODEL_COLOR_3: "#F5A623",
    ColorRole.USAGE_MODEL_COLOR_4: "#E85D75",
    ColorRole.USAGE_MODEL_COLOR_5: "#9B8AF5",
    ColorRole.USAGE_MODEL_COLOR_6: "#F5724D",
    ColorRole.USAGE_MODEL_COLOR_7: "#50C8D8",
    ColorRole.USAGE_MODEL_COLOR_8: "#A8CC5E",

    # Syntax highlighting
    ColorRole.SYNTAX_ERROR: "#ff0000",
    ColorRole.SYNTAX_01: "#80e0d0",
    ColorRole.SYNTAX_02: "#f0f0f0",
    ColorRole.SYNTAX_03: "#68c068",
    ColorRole.SYNTAX_04: "#ffa0eb",
    ColorRole.SYNTAX_05: "#808080",
    ColorRole.SYNTAX_06: "#90e0e8",
    ColorRole.SYNTAX_07: "#e0e080",
    ColorRole.SYNTAX_08: "#b090f0",
    ColorRole.SYNTAX_09: "#90e0e8",
    ColorRole.SYNTAX_10: "#d070d0",
    ColorRole.SYNTAX_11: "#80a0f0",
    ColorRole.SYNTAX_12: "#f08080",
    ColorRole.SYNTAX_13: "#ffc0eb",
    ColorRole.SYNTAX_14: "#f0e060",
    ColorRole.SYNTAX_15: "#70e0e8",
    ColorRole.SYNTAX_16: "#88d048",
    ColorRole.SYNTAX_17: "#c0c0c0",
    ColorRole.SYNTAX_18: "#80b080",
    ColorRole.SYNTAX_19: "#c87050",
    ColorRole.SYNTAX_20: "#c05040",
    ColorRole.SYNTAX_21: "#30c090",

    # Terminal basic colors
    ColorRole.TERM_BLACK: "#000000",
    ColorRole.TERM_RED: "#cd0000",
    ColorRole.TERM_GREEN: "#00cd00",
    ColorRole.TERM_YELLOW: "#cdcd00",
    ColorRole.TERM_BLUE: "#0000ee",
    ColorRole.TERM_MAGENTA: "#cd00cd",
    ColorRole.TERM_CYAN: "#00cdcd",
    ColorRole.TERM_WHITE: "#e5e5e5",

    # Terminal bright colors
    ColorRole.TERM_BRIGHT_BLACK: "#7f7f7f",
    ColorRole.TERM_BRIGHT_RED: "#ff0000",
    ColorRole.TERM_BRIGHT_GREEN: "#00ff00",
    ColorRole.TERM_BRIGHT_YELLOW: "#ffff00",
    ColorRole.TERM_BRIGHT_BLUE: "#5c5cff",
    ColorRole.TERM_BRIGHT_MAGENTA: "#ff00ff",
    ColorRole.TERM_BRIGHT_CYAN: "#00ffff",
    ColorRole.TERM_BRIGHT_WHITE: "#ffffff",
}

_LIGHT_COLORS: Dict[ColorRole, str] = {
    # Brand colours
    ColorRole.BRAND_PRIMARY: "#6248e8",
    ColorRole.BRAND_GRADIENT_START: "#29c5ff",
    ColorRole.BRAND_GRADIENT_END: "#c050ff",
    ColorRole.BRAND_ICON_BG_START: "#1e0e50",
    ColorRole.BRAND_ICON_BG_END: "#060612",

    # Background colours
    ColorRole.BACKGROUND_PRIMARY: "#f4f4f4",
    ColorRole.BACKGROUND_SECONDARY: "#e4e4e4",
    ColorRole.BACKGROUND_GRADIENT_START: "#f4f4f4",
    ColorRole.BACKGROUND_GRADIENT_END: "#f4f4f4",
    ColorRole.BACKGROUND_TERTIARY: "#fefefe",
    ColorRole.BACKGROUND_TERTIARY_HOVER: "#e0e0e0",
    ColorRole.BACKGROUND_TERTIARY_PRESSED: "#c8c8c8",
    ColorRole.BACKGROUND_DIALOG: "#d8d8d8",
    ColorRole.LOGO_BACKGROUND: "#f8f8f8",

    # Text colours
    ColorRole.TEXT_PRIMARY: "#202020",
    ColorRole.TEXT_BRIGHT: "#000000",
    ColorRole.TEXT_HEADING: "#204080",
    ColorRole.TEXT_HEADING_BRIGHT: "#203880",
    ColorRole.TEXT_DISABLED: "#909090",
    ColorRole.TEXT_SELECTED: "#c8c8dc",
    ColorRole.TEXT_FOUND: "#e0b4b4",
    ColorRole.TEXT_FOUND_DIM: "#f4d8d8",
    ColorRole.TEXT_RECOMMENDED: "#ffffff",
    ColorRole.TEXT_LINK: "#0000ff",
    ColorRole.TEXT_INACTIVE: "#707070",
    ColorRole.TEXT_EPHEMERAL: "#a0785c",
    ColorRole.TEXT_EPHEMERAL_INACTIVE: "#c09070",
    ColorRole.TEXT_ERROR: "#f04030",
    ColorRole.TEXT_ERROR_INACTIVE: "#f07060",
    ColorRole.TEXT_SUCCESS: "#16a34a",

    # Edit box colours
    ColorRole.EDIT_BOX_BORDER: "#404080",
    ColorRole.EDIT_BOX_BACKGROUND: "#b8b8f8",
    ColorRole.EDIT_BOX_ERROR: "#d04030",

    # Mindspace colours
    ColorRole.MINDSPACE_BACKGROUND: "#fafafa",
    ColorRole.MINDSPACE_NAME_BACKGROUND: "#d0d0d0",
    ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER: "#b0b0b0",
    ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED: "#909090",
    ColorRole.MINDSPACE_HEADING: "#e0e0e0",
    ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND: "#f0f0f0",
    ColorRole.MINDSPACE_FOLDER: "#4B89DC",
    ColorRole.MINDSPACE_FOLDER_BREADCRUMB: "#C08090",

    # Tab colours
    ColorRole.TAB_BAR_BACKGROUND: "#ececec",
    ColorRole.TAB_BACKGROUND_ACTIVE: "#ffffff",
    ColorRole.TAB_BACKGROUND_INACTIVE: "#d4d4d4",
    ColorRole.TAB_BACKGROUND_HOVER: "#c8c8ff",
    ColorRole.TAB_BACKGROUND_UPDATED: "#f0d0f8",
    ColorRole.TAB_BORDER_ACTIVE: "#ff4030",

    # Button colours
    ColorRole.BUTTON_BACKGROUND: "#e8e8e8",
    ColorRole.BUTTON_BACKGROUND_PRESSED: "#b0b0b0",
    ColorRole.BUTTON_BACKGROUND_HOVER: "#c0c0c0",
    ColorRole.BUTTON_SECONDARY_BACKGROUND: "#d8d8d8",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED: "#b0b0b0",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER: "#c0c0c0",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED: "#6080e0",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED: "#4060c0",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER: "#5070d0",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE: "#e06048",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED: "#c04030",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER: "#d0503c",
    ColorRole.BUTTON_BACKGROUND_EDIT: "#a06008",
    ColorRole.BUTTON_BACKGROUND_EDIT_PRESSED: "#804800",
    ColorRole.BUTTON_BACKGROUND_EDIT_HOVER: "#b07018",
    ColorRole.BUTTON_BACKGROUND_DISABLED: "#e0e0e0",

    # Switch colours
    ColorRole.SWITCH_TRACK_ON: "#90a0e0",
    ColorRole.SWITCH_TRACK_OFF: "#e0e0e0",
    ColorRole.SWITCH_TRACK_DISABLED_ON: "#c0d2f0",
    ColorRole.SWITCH_TRACK_BORDER: "#b0b0b0",
    ColorRole.SWITCH_KNOB: "#202020",
    ColorRole.SWITCH_KNOB_DISABLED: "#707070",

    # Menu elements
    ColorRole.MENU_BACKGROUND: "#f0f0f0",
    ColorRole.MENU_HOVER: "#70a0f0",
    ColorRole.MENU_BORDER: "#b0b0b0",
    ColorRole.COMBO_ITEM_HOVER: "#d0d0d0",

    # Splitter bars
    ColorRole.SPLITTER: "#b0b0b0",
    ColorRole.TAB_SPLITTER: "#fcfcfc",

    # Scroll bar elements
    ColorRole.SCROLLBAR_BACKGROUND: "#f0f0f0",
    ColorRole.SCROLLBAR_HANDLE: "#c0c0c0",

    # Code block border
    ColorRole.CODE_BORDER: "#e0e0e0",

    # Blockquote elements
    ColorRole.BLOCKQUOTE_BORDER: "#b4b4cd",
    ColorRole.BLOCKQUOTE_BACKGROUND: "#e9e9f3",

    # Table elements
    ColorRole.TABLE_BORDER: "#a0a0a0",
    ColorRole.TABLE_HEADER_BACKGROUND: "#d0d0e0",

    # Message colours
    ColorRole.MESSAGE_BACKGROUND: "#f8f8f8",
    ColorRole.MESSAGE_BACKGROUND_HOVER: "#c4c4c4",
    ColorRole.MESSAGE_BACKGROUND_PRESSED: "#b0b0b0",
    ColorRole.MESSAGE_USER_BACKGROUND: "#e0e0e0",
    ColorRole.MESSAGE_USER_BACKGROUND_HOVER: "#c0c0c0",
    ColorRole.MESSAGE_USER_BACKGROUND_PRESSED: "#a0a0a0",
    # The input background has an alpha channel so it allows a small amount of what's underneath it to show through.
    ColorRole.MESSAGE_INPUT_BACKGROUND: "#ead0d0d0",
    ColorRole.MESSAGE_ATTACHMENT_BACKGROUND: "#f0f0f0",
    ColorRole.MESSAGE_BORDER: "#e4e4e4",
    ColorRole.MESSAGE_USER_BORDER: "#d0d0d0",
    ColorRole.MESSAGE_INPUT_BORDER: "#909090",
    ColorRole.MESSAGE_SPOTLIGHTED: "#607488",
    ColorRole.MESSAGE_USER: "#5068a0",
    ColorRole.MESSAGE_AI: "#208020",
    ColorRole.MESSAGE_REASONING: "#808080",
    ColorRole.MESSAGE_TOOL_CALL: "#808080",
    ColorRole.MESSAGE_TOOL_RESULT: "#808080",
    ColorRole.MESSAGE_USER_QUEUED: "#606060",
    ColorRole.MESSAGE_SYSTEM_ERROR: "#a04040",
    ColorRole.MESSAGE_SYSTEM_SUCCESS: "#40a040",
    ColorRole.MESSAGE_SYNTAX: "#806040",
    ColorRole.MESSAGE_STREAMING: "#a07050",
    ColorRole.MESSAGE_TRACE: "#606060",
    ColorRole.MESSAGE_INFORMATION: "#0060c0",
    ColorRole.MESSAGE_WARNING: "#c0a020",
    ColorRole.MESSAGE_ERROR: "#c03030",

    # Status bar elements
    ColorRole.STATUS_BAR_BACKGROUND: "#e8e8e8",
    ColorRole.CANARY_BACKGROUND: "#ff8080",

    # Close button states
    ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER: "#ff7070",

    # Drop target highlights
    ColorRole.DROP_TARGET_HIGHLIGHT: "#a8b8f8",
    ColorRole.DROP_TARGET_SEPARATOR_HIGHLIGHT: "#142454",

    # Line numbers
    ColorRole.LINE_NUMBER: "#c0c0c0",

    # Diff view colours
    ColorRole.DIFF_REMOVED_BACKGROUND: "#f0c8c8",
    ColorRole.DIFF_ADDED_BACKGROUND: "#c8f0c8",
    ColorRole.DIFF_CHANGED_BACKGROUND: "#c8c8f0",
    ColorRole.DIFF_HUNK_LINE_NUMBER: "#202020",

    # VCS status colours
    ColorRole.VCS_MODIFIED: "#c0a020",
    ColorRole.VCS_ADDED: "#207020",
    ColorRole.VCS_DELETED: "#c03030",
    ColorRole.VCS_RENAMED: "#4040a0",

    # Usage model colours
    ColorRole.USAGE_MODEL_COLOR_1: "#2878c8",
    ColorRole.USAGE_MODEL_COLOR_2: "#28a060",
    ColorRole.USAGE_MODEL_COLOR_3: "#c07800",
    ColorRole.USAGE_MODEL_COLOR_4: "#c03050",
    ColorRole.USAGE_MODEL_COLOR_5: "#6050c8",
    ColorRole.USAGE_MODEL_COLOR_6: "#c04820",
    ColorRole.USAGE_MODEL_COLOR_7: "#2090a0",
    ColorRole.USAGE_MODEL_COLOR_8: "#608020",

    # Syntax highlighting
    ColorRole.SYNTAX_ERROR: "#ff0000",
    ColorRole.SYNTAX_01: "#007070",
    ColorRole.SYNTAX_02: "#202020",
    ColorRole.SYNTAX_03: "#40a040",
    ColorRole.SYNTAX_04: "#c000a0",
    ColorRole.SYNTAX_05: "#606060",
    ColorRole.SYNTAX_06: "#0080a0",
    ColorRole.SYNTAX_07: "#a0a000",
    ColorRole.SYNTAX_08: "#5040c0",
    ColorRole.SYNTAX_09: "#0080a0",
    ColorRole.SYNTAX_10: "#a000a0",
    ColorRole.SYNTAX_11: "#0040c0",
    ColorRole.SYNTAX_12: "#c03030",
    ColorRole.SYNTAX_13: "#c080a0",
    ColorRole.SYNTAX_14: "#a09040",
    ColorRole.SYNTAX_15: "#2090a0",
    ColorRole.SYNTAX_16: "#508020",
    ColorRole.SYNTAX_17: "#404040",
    ColorRole.SYNTAX_18: "#609060",
    ColorRole.SYNTAX_19: "#a04020",
    ColorRole.SYNTAX_20: "#b03828",
    ColorRole.SYNTAX_21: "#24906c",

    # Terminal basic colors
    ColorRole.TERM_BLACK: "#000000",
    ColorRole.TERM_RED: "#cd0000",
    ColorRole.TERM_GREEN: "#00cd00",
    ColorRole.TERM_YELLOW: "#cdcd00",
    ColorRole.TERM_BLUE: "#0000ee",
    ColorRole.TERM_MAGENTA: "#cd00cd",
    ColorRole.TERM_CYAN: "#00cdcd",
    ColorRole.TERM_WHITE: "#e5e5e5",

    # Terminal bright colors
    ColorRole.TERM_BRIGHT_BLACK: "#7f7f7f",
    ColorRole.TERM_BRIGHT_RED: "#ff0000",
    ColorRole.TERM_BRIGHT_GREEN: "#00ff00",
    ColorRole.TERM_BRIGHT_YELLOW: "#ffff00",
    ColorRole.TERM_BRIGHT_BLUE: "#5c5cff",
    ColorRole.TERM_BRIGHT_MAGENTA: "#ff00ff",
    ColorRole.TERM_BRIGHT_CYAN: "#00ffff",
    ColorRole.TERM_BRIGHT_WHITE: "#ffffff",
}

_COLOR_BLIND_COLORS: Dict[ColorRole, str] = {
    # Brand colours
    ColorRole.BRAND_PRIMARY: "#56b4e9",
    ColorRole.BRAND_GRADIENT_START: "#56b4e9",
    ColorRole.BRAND_GRADIENT_END: "#cc79a7",
    ColorRole.BRAND_ICON_BG_START: "#1e0e50",
    ColorRole.BRAND_ICON_BG_END: "#060612",

    # Background colours
    ColorRole.BACKGROUND_PRIMARY: "#101214",
    ColorRole.BACKGROUND_SECONDARY: "#1b2026",
    ColorRole.BACKGROUND_GRADIENT_START: "#101214",
    ColorRole.BACKGROUND_GRADIENT_END: "#101214",
    ColorRole.BACKGROUND_TERTIARY: "#0d0f12",
    ColorRole.BACKGROUND_TERTIARY_HOVER: "#24303a",
    ColorRole.BACKGROUND_TERTIARY_PRESSED: "#344450",
    ColorRole.BACKGROUND_DIALOG: "#1f242a",
    ColorRole.LOGO_BACKGROUND: "#2f3640",

    # Text colours
    ColorRole.TEXT_PRIMARY: "#d7dde3",
    ColorRole.TEXT_BRIGHT: "#ffffff",
    ColorRole.TEXT_HEADING: "#56b4e9",
    ColorRole.TEXT_HEADING_BRIGHT: "#8bd3ff",
    ColorRole.TEXT_DISABLED: "#7a8590",
    ColorRole.TEXT_SELECTED: "#274b66",
    ColorRole.TEXT_FOUND: "#7a6500",
    ColorRole.TEXT_FOUND_DIM: "#433a12",
    ColorRole.TEXT_RECOMMENDED: "#ffffff",
    ColorRole.TEXT_LINK: "#56b4e9",
    ColorRole.TEXT_INACTIVE: "#9aa4ad",
    ColorRole.TEXT_EPHEMERAL: "#d7dde3",
    ColorRole.TEXT_EPHEMERAL_INACTIVE: "#8b949e",
    ColorRole.TEXT_ERROR: "#e69f00",
    ColorRole.TEXT_ERROR_INACTIVE: "#b9822c",
    ColorRole.TEXT_SUCCESS: "#009e73",

    # Edit box colours
    ColorRole.EDIT_BOX_BORDER: "#3a4856",
    ColorRole.EDIT_BOX_BACKGROUND: "#14181d",
    ColorRole.EDIT_BOX_ERROR: "#e69f00",

    # Mindspace colours
    ColorRole.MINDSPACE_BACKGROUND: "#0c0f12",
    ColorRole.MINDSPACE_NAME_BACKGROUND: "#161b20",
    ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER: "#202934",
    ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED: "#293848",
    ColorRole.MINDSPACE_HEADING: "#56b4e9",
    ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND: "#090b0d",
    ColorRole.MINDSPACE_FOLDER: "#56b4e9",
    ColorRole.MINDSPACE_FOLDER_BREADCRUMB: "#e69f00",

    # Tab colours
    ColorRole.TAB_BAR_BACKGROUND: "#11161c",
    ColorRole.TAB_BACKGROUND_ACTIVE: "#171d24",
    ColorRole.TAB_BACKGROUND_INACTIVE: "#202833",
    ColorRole.TAB_BACKGROUND_HOVER: "#263242",
    ColorRole.TAB_BACKGROUND_UPDATED: "#233f4a",
    ColorRole.TAB_BORDER_ACTIVE: "#56b4e9",

    # Button colours
    ColorRole.BUTTON_BACKGROUND: "#28323d",
    ColorRole.BUTTON_BACKGROUND_PRESSED: "#3a4652",
    ColorRole.BUTTON_BACKGROUND_HOVER: "#333f4d",
    ColorRole.BUTTON_SECONDARY_BACKGROUND: "#242c34",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED: "#36424d",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER: "#2f3944",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED: "#0072b2",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED: "#005986",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER: "#1689c7",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE: "#d55e00",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED: "#a94b00",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER: "#e7771a",
    ColorRole.BUTTON_BACKGROUND_EDIT: "#4b3d70",
    ColorRole.BUTTON_BACKGROUND_EDIT_PRESSED: "#3a3058",
    ColorRole.BUTTON_BACKGROUND_EDIT_HOVER: "#5b4a84",
    ColorRole.BUTTON_BACKGROUND_DISABLED: "#2a3036",

    # Switch colours
    ColorRole.SWITCH_TRACK_ON: "#0072b2",
    ColorRole.SWITCH_TRACK_OFF: "#28323d",
    ColorRole.SWITCH_TRACK_DISABLED_ON: "#004f80",
    ColorRole.SWITCH_TRACK_BORDER: "#56b4e9",
    ColorRole.SWITCH_KNOB: "#ffffff",
    ColorRole.SWITCH_KNOB_DISABLED: "#7a8590",

    # Menu elements
    ColorRole.MENU_BACKGROUND: "#1f242a",
    ColorRole.MENU_HOVER: "#263242",
    ColorRole.MENU_BORDER: "#3a4856",
    ColorRole.COMBO_ITEM_HOVER: "#2a333c",

    # Splitter bars
    ColorRole.SPLITTER: "#3a4856",
    ColorRole.TAB_SPLITTER: "#2a333c",

    # Scroll bar elements
    ColorRole.SCROLLBAR_BACKGROUND: "#151a20",
    ColorRole.SCROLLBAR_HANDLE: "#566573",

    # Code block border
    ColorRole.CODE_BORDER: "#3a4856",

    # Blockquote elements
    ColorRole.BLOCKQUOTE_BORDER: "#425365",
    ColorRole.BLOCKQUOTE_BACKGROUND: "#16202d",

    # Table elements
    ColorRole.TABLE_BORDER: "#3a4856",
    ColorRole.TABLE_HEADER_BACKGROUND: "#202833",

    # Message colours
    ColorRole.MESSAGE_BACKGROUND: "#171b20",
    ColorRole.MESSAGE_BACKGROUND_HOVER: "#202832",
    ColorRole.MESSAGE_BACKGROUND_PRESSED: "#293443",
    ColorRole.MESSAGE_USER_BACKGROUND: "#1a2430",
    ColorRole.MESSAGE_USER_BACKGROUND_HOVER: "#223246",
    ColorRole.MESSAGE_USER_BACKGROUND_PRESSED: "#2a3d55",
    # The input background has an alpha channel so it allows a small amount of what's underneath it to show through.
    ColorRole.MESSAGE_INPUT_BACKGROUND: "#ea1a2430",
    ColorRole.MESSAGE_ATTACHMENT_BACKGROUND: "#222a33",
    ColorRole.MESSAGE_BORDER: "#36424d",
    ColorRole.MESSAGE_USER_BORDER: "#3c5870",
    ColorRole.MESSAGE_INPUT_BORDER: "#566573",
    ColorRole.MESSAGE_SPOTLIGHTED: "#0072b2",
    ColorRole.MESSAGE_USER: "#56b4e9",
    ColorRole.MESSAGE_AI: "#009e73",
    ColorRole.MESSAGE_REASONING: "#cc79a7",
    ColorRole.MESSAGE_TOOL_CALL: "#f0e442",
    ColorRole.MESSAGE_TOOL_RESULT: "#f0e442",
    ColorRole.MESSAGE_USER_QUEUED: "#9aa4ad",
    ColorRole.MESSAGE_SYSTEM_ERROR: "#e69f00",
    ColorRole.MESSAGE_SYSTEM_SUCCESS: "#009e73",
    ColorRole.MESSAGE_SYNTAX: "#56b4e9",
    ColorRole.MESSAGE_STREAMING: "#56b4e9",
    ColorRole.MESSAGE_TRACE: "#b8c0c8",
    ColorRole.MESSAGE_INFORMATION: "#56b4e9",
    ColorRole.MESSAGE_WARNING: "#f0e442",
    ColorRole.MESSAGE_ERROR: "#e69f00",

    # Status bar elements
    ColorRole.STATUS_BAR_BACKGROUND: "#11161c",
    ColorRole.CANARY_BACKGROUND: "#d55e00",

    # Close button states
    ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER: "#384552",

    # Drop target highlights
    ColorRole.DROP_TARGET_HIGHLIGHT: "#274b66",
    ColorRole.DROP_TARGET_SEPARATOR_HIGHLIGHT: "#56b4e9",

    # Line numbers
    ColorRole.LINE_NUMBER: "#8b949e",

    # Diff view colours
    ColorRole.DIFF_REMOVED_BACKGROUND: "#4a2a12",
    ColorRole.DIFF_ADDED_BACKGROUND: "#143d36",
    ColorRole.DIFF_CHANGED_BACKGROUND: "#3d3a12",
    ColorRole.DIFF_HUNK_LINE_NUMBER: "#f0e442",

    # VCS status colours
    ColorRole.VCS_MODIFIED: "#f0e442",
    ColorRole.VCS_ADDED: "#009e73",
    ColorRole.VCS_DELETED: "#d55e00",
    ColorRole.VCS_RENAMED: "#56b4e9",

    # Usage model colours (Okabe-Ito colour-blind safe palette)
    ColorRole.USAGE_MODEL_COLOR_1: "#56b4e9",
    ColorRole.USAGE_MODEL_COLOR_2: "#009e73",
    ColorRole.USAGE_MODEL_COLOR_3: "#e69f00",
    ColorRole.USAGE_MODEL_COLOR_4: "#d55e00",
    ColorRole.USAGE_MODEL_COLOR_5: "#cc79a7",
    ColorRole.USAGE_MODEL_COLOR_6: "#0072b2",
    ColorRole.USAGE_MODEL_COLOR_7: "#f0e442",
    ColorRole.USAGE_MODEL_COLOR_8: "#000000",

    # Syntax highlighting
    ColorRole.SYNTAX_ERROR: "#e69f00",
    ColorRole.SYNTAX_01: "#56b4e9",
    ColorRole.SYNTAX_02: "#f2f2f2",
    ColorRole.SYNTAX_03: "#8bd3c7",
    ColorRole.SYNTAX_04: "#cc79a7",
    ColorRole.SYNTAX_05: "#9aa4ad",
    ColorRole.SYNTAX_06: "#56b4e9",
    ColorRole.SYNTAX_07: "#f0e442",
    ColorRole.SYNTAX_08: "#b596e6",
    ColorRole.SYNTAX_09: "#8bd3ff",
    ColorRole.SYNTAX_10: "#cc79a7",
    ColorRole.SYNTAX_11: "#0072b2",
    ColorRole.SYNTAX_12: "#d55e00",
    ColorRole.SYNTAX_13: "#b596e6",
    ColorRole.SYNTAX_14: "#e69f00",
    ColorRole.SYNTAX_15: "#009e73",
    ColorRole.SYNTAX_16: "#009e73",
    ColorRole.SYNTAX_17: "#d7dde3",
    ColorRole.SYNTAX_18: "#7aa6a0",
    ColorRole.SYNTAX_19: "#e69f00",
    ColorRole.SYNTAX_20: "#d55e00",
    ColorRole.SYNTAX_21: "#009e73",

    # Terminal basic colors
    ColorRole.TERM_BLACK: "#000000",
    ColorRole.TERM_RED: "#d55e00",
    ColorRole.TERM_GREEN: "#009e73",
    ColorRole.TERM_YELLOW: "#f0e442",
    ColorRole.TERM_BLUE: "#0072b2",
    ColorRole.TERM_MAGENTA: "#cc79a7",
    ColorRole.TERM_CYAN: "#56b4e9",
    ColorRole.TERM_WHITE: "#d7dde3",
    ColorRole.TERM_BRIGHT_BLACK: "#7a8590",
    ColorRole.TERM_BRIGHT_RED: "#e69f00",
    ColorRole.TERM_BRIGHT_GREEN: "#32c795",
    ColorRole.TERM_BRIGHT_YELLOW: "#fff16a",
    ColorRole.TERM_BRIGHT_BLUE: "#56b4e9",
    ColorRole.TERM_BRIGHT_MAGENTA: "#e59ac2",
    ColorRole.TERM_BRIGHT_CYAN: "#8bd3ff",
    ColorRole.TERM_BRIGHT_WHITE: "#ffffff",
}

_OCEAN_LIGHT_COLORS: Dict[ColorRole, str] = {
    # Brand colours
    ColorRole.BRAND_PRIMARY: "#6248e8",
    ColorRole.BRAND_GRADIENT_START: "#29c5ff",
    ColorRole.BRAND_GRADIENT_END: "#c050ff",
    ColorRole.BRAND_ICON_BG_START: "#1e0e50",
    ColorRole.BRAND_ICON_BG_END: "#060612",

    # Background colours
    ColorRole.BACKGROUND_PRIMARY: "#f6f8fb",
    ColorRole.BACKGROUND_SECONDARY: "#e8eef5",
    ColorRole.BACKGROUND_GRADIENT_START: "#f4f4f4",
    ColorRole.BACKGROUND_GRADIENT_END: "#f4f4f4",
    ColorRole.BACKGROUND_TERTIARY: "#ffffff",
    ColorRole.BACKGROUND_TERTIARY_HOVER: "#dfe8f2",
    ColorRole.BACKGROUND_TERTIARY_PRESSED: "#cfdbe8",
    ColorRole.BACKGROUND_DIALOG: "#ffffff",
    ColorRole.LOGO_BACKGROUND: "#e9f0f8",

    # Text colours
    ColorRole.TEXT_PRIMARY: "#1f2933",
    ColorRole.TEXT_BRIGHT: "#0f1720",
    ColorRole.TEXT_HEADING: "#204080",
    ColorRole.TEXT_HEADING_BRIGHT: "#203880",
    ColorRole.TEXT_DISABLED: "#65758a",
    ColorRole.TEXT_SELECTED: "#cfe4fb",
    ColorRole.TEXT_FOUND: "#e0b4b4",
    ColorRole.TEXT_FOUND_DIM: "#f4d8d8",
    ColorRole.TEXT_RECOMMENDED: "#ffffff",
    ColorRole.TEXT_LINK: "#185d96",
    ColorRole.TEXT_INACTIVE: "#707070",
    ColorRole.TEXT_EPHEMERAL: "#a0785c",
    ColorRole.TEXT_EPHEMERAL_INACTIVE: "#c09070",
    ColorRole.TEXT_ERROR: "#f04030",
    ColorRole.TEXT_ERROR_INACTIVE: "#f07060",
    ColorRole.TEXT_SUCCESS: "#16a34a",

    # Edit box colours
    ColorRole.EDIT_BOX_BORDER: "#aebfd1",
    ColorRole.EDIT_BOX_BACKGROUND: "#ffffff",
    ColorRole.EDIT_BOX_ERROR: "#d04030",

    # Mindspace colours
    ColorRole.MINDSPACE_BACKGROUND: "#edf3f9",
    ColorRole.MINDSPACE_NAME_BACKGROUND: "#d0d0d0",
    ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER: "#b0b0b0",
    ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED: "#909090",
    ColorRole.MINDSPACE_HEADING: "#22384f",
    ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND: "#d9e6f3",
    ColorRole.MINDSPACE_FOLDER: "#1f6fb2",
    ColorRole.MINDSPACE_FOLDER_BREADCRUMB: "#174f80",

    # Tab colours
    ColorRole.TAB_BAR_BACKGROUND: "#d9e3ef",
    ColorRole.TAB_BACKGROUND_ACTIVE: "#ffffff",
    ColorRole.TAB_BACKGROUND_INACTIVE: "#e7edf5",
    ColorRole.TAB_BACKGROUND_HOVER: "#edf3f9",
    ColorRole.TAB_BACKGROUND_UPDATED: "#f0d0f8",
    ColorRole.TAB_BORDER_ACTIVE: "#1f6fff",

    # Button colours
    ColorRole.BUTTON_BACKGROUND: "#e3ebf4",
    ColorRole.BUTTON_BACKGROUND_PRESSED: "#c6d5e5",
    ColorRole.BUTTON_BACKGROUND_HOVER: "#d6e2ef",
    ColorRole.BUTTON_SECONDARY_BACKGROUND: "#d8d8d8",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED: "#b0b0b0",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER: "#c0c0c0",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED: "#1f6fb2",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED: "#144c7a",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER: "#185d96",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE: "#c94a3a",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED: "#9f3328",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER: "#b83e31",
    ColorRole.BUTTON_BACKGROUND_EDIT: "#a06008",
    ColorRole.BUTTON_BACKGROUND_EDIT_PRESSED: "#804800",
    ColorRole.BUTTON_BACKGROUND_EDIT_HOVER: "#b07018",
    ColorRole.BUTTON_BACKGROUND_DISABLED: "#e0e0e0",

    # Switch colours
    ColorRole.SWITCH_TRACK_ON: "#1f6fb2",
    ColorRole.SWITCH_TRACK_OFF: "#d7e0eb",
    ColorRole.SWITCH_TRACK_DISABLED_ON: "#c0d2f0",
    ColorRole.SWITCH_TRACK_BORDER: "#92a8bf",
    ColorRole.SWITCH_KNOB: "#ffffff",
    ColorRole.SWITCH_KNOB_DISABLED: "#707070",

    # Menu elements
    ColorRole.MENU_BACKGROUND: "#f0f0f0",
    ColorRole.MENU_HOVER: "#70a0f0",
    ColorRole.MENU_BORDER: "#b0b0b0",
    ColorRole.COMBO_ITEM_HOVER: "#d0d0d0",

    # Splitter bars
    ColorRole.SPLITTER: "#b0b0b0",
    ColorRole.TAB_SPLITTER: "#fcfcfc",

    # Scroll bar elements
    ColorRole.SCROLLBAR_BACKGROUND: "#e7edf5",
    ColorRole.SCROLLBAR_HANDLE: "#9eb2c8",

    # Code block border
    ColorRole.CODE_BORDER: "#c5d3e1",

    # Blockquote elements
    ColorRole.BLOCKQUOTE_BORDER: "#b4b4cd",
    ColorRole.BLOCKQUOTE_BACKGROUND: "#e9e9f3",

    # Table elements
    ColorRole.TABLE_BORDER: "#c5d3e1",
    ColorRole.TABLE_HEADER_BACKGROUND: "#d0d0e0",

    # Message colours
    ColorRole.MESSAGE_BACKGROUND: "#f5f9fd",
    ColorRole.MESSAGE_BACKGROUND_HOVER: "#cfe0ef",
    ColorRole.MESSAGE_BACKGROUND_PRESSED: "#deeaf6",
    ColorRole.MESSAGE_USER_BACKGROUND: "#e9f3fa",
    ColorRole.MESSAGE_USER_BACKGROUND_HOVER: "#deedf8",
    ColorRole.MESSAGE_USER_BACKGROUND_PRESSED: "#d0e4f4",
    # The input background has an alpha channel so it allows a small amount of what's underneath it to show through.
    ColorRole.MESSAGE_INPUT_BACKGROUND: "#ead5e3f1",
    ColorRole.MESSAGE_ATTACHMENT_BACKGROUND: "#eef3f8",
    ColorRole.MESSAGE_BORDER: "#c5d3e1",
    ColorRole.MESSAGE_USER_BORDER: "#9fbede",
    ColorRole.MESSAGE_INPUT_BORDER: "#95a3b1",
    ColorRole.MESSAGE_SPOTLIGHTED: "#607488",
    ColorRole.MESSAGE_USER: "#185d96",
    ColorRole.MESSAGE_AI: "#1d7a4d",
    ColorRole.MESSAGE_REASONING: "#7a4fa3",
    ColorRole.MESSAGE_TOOL_CALL: "#526578",
    ColorRole.MESSAGE_TOOL_RESULT: "#526578",
    ColorRole.MESSAGE_USER_QUEUED: "#606060",
    ColorRole.MESSAGE_SYSTEM_ERROR: "#a04040",
    ColorRole.MESSAGE_SYSTEM_SUCCESS: "#40a040",
    ColorRole.MESSAGE_SYNTAX: "#185d96",
    ColorRole.MESSAGE_STREAMING: "#a07050",
    ColorRole.MESSAGE_TRACE: "#606060",
    ColorRole.MESSAGE_INFORMATION: "#185d96",
    ColorRole.MESSAGE_WARNING: "#7a5d00",
    ColorRole.MESSAGE_ERROR: "#9f3328",

    # Status bar elements
    ColorRole.STATUS_BAR_BACKGROUND: "#e8e8e8",
    ColorRole.CANARY_BACKGROUND: "#ff8080",

    # Close button states
    ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER: "#ff7070",

    # Drop target highlights
    ColorRole.DROP_TARGET_HIGHLIGHT: "#a8b8f8",
    ColorRole.DROP_TARGET_SEPARATOR_HIGHLIGHT: "#142454",

    # Line numbers
    ColorRole.LINE_NUMBER: "#c0c0c0",

    # Diff view colours
    ColorRole.DIFF_REMOVED_BACKGROUND: "#f0c8c8",
    ColorRole.DIFF_ADDED_BACKGROUND: "#c8f0c8",
    ColorRole.DIFF_CHANGED_BACKGROUND: "#c8c8f0",
    ColorRole.DIFF_HUNK_LINE_NUMBER: "#202020",

    # VCS status colours
    ColorRole.VCS_MODIFIED: "#c0a020",
    ColorRole.VCS_ADDED: "#207020",
    ColorRole.VCS_DELETED: "#c03030",
    ColorRole.VCS_RENAMED: "#4040a0",

    # Usage model colours
    ColorRole.USAGE_MODEL_COLOR_1: "#2878c8",
    ColorRole.USAGE_MODEL_COLOR_2: "#28a060",
    ColorRole.USAGE_MODEL_COLOR_3: "#c07800",
    ColorRole.USAGE_MODEL_COLOR_4: "#c03050",
    ColorRole.USAGE_MODEL_COLOR_5: "#6050c8",
    ColorRole.USAGE_MODEL_COLOR_6: "#c04820",
    ColorRole.USAGE_MODEL_COLOR_7: "#2090a0",
    ColorRole.USAGE_MODEL_COLOR_8: "#608020",

    # Syntax highlighting
    ColorRole.SYNTAX_ERROR: "#ff0000",
    ColorRole.SYNTAX_01: "#007070",
    ColorRole.SYNTAX_02: "#202020",
    ColorRole.SYNTAX_03: "#40a040",
    ColorRole.SYNTAX_04: "#c000a0",
    ColorRole.SYNTAX_05: "#606060",
    ColorRole.SYNTAX_06: "#0080a0",
    ColorRole.SYNTAX_07: "#a0a000",
    ColorRole.SYNTAX_08: "#5040c0",
    ColorRole.SYNTAX_09: "#0080a0",
    ColorRole.SYNTAX_10: "#a000a0",
    ColorRole.SYNTAX_11: "#0040c0",
    ColorRole.SYNTAX_12: "#c03030",
    ColorRole.SYNTAX_13: "#c080a0",
    ColorRole.SYNTAX_14: "#a09040",
    ColorRole.SYNTAX_15: "#2090a0",
    ColorRole.SYNTAX_16: "#508020",
    ColorRole.SYNTAX_17: "#404040",
    ColorRole.SYNTAX_18: "#609060",
    ColorRole.SYNTAX_19: "#a04020",
    ColorRole.SYNTAX_20: "#b03828",
    ColorRole.SYNTAX_21: "#24906c",

    # Terminal basic colors
    ColorRole.TERM_BLACK: "#000000",
    ColorRole.TERM_RED: "#cd0000",
    ColorRole.TERM_GREEN: "#00cd00",
    ColorRole.TERM_YELLOW: "#cdcd00",
    ColorRole.TERM_BLUE: "#0000ee",
    ColorRole.TERM_MAGENTA: "#cd00cd",
    ColorRole.TERM_CYAN: "#00cdcd",
    ColorRole.TERM_WHITE: "#e5e5e5",

    # Terminal bright colors
    ColorRole.TERM_BRIGHT_BLACK: "#7f7f7f",
    ColorRole.TERM_BRIGHT_RED: "#ff0000",
    ColorRole.TERM_BRIGHT_GREEN: "#00ff00",
    ColorRole.TERM_BRIGHT_YELLOW: "#ffff00",
    ColorRole.TERM_BRIGHT_BLUE: "#5c5cff",
    ColorRole.TERM_BRIGHT_MAGENTA: "#ff00ff",
    ColorRole.TERM_BRIGHT_CYAN: "#00ffff",
    ColorRole.TERM_BRIGHT_WHITE: "#ffffff",
}

_GLOSSY_LIGHT_COLORS: Dict[ColorRole, str] = {
    # Brand colours
    ColorRole.BRAND_PRIMARY: "#6248e8",
    ColorRole.BRAND_GRADIENT_START: "#29c5ff",
    ColorRole.BRAND_GRADIENT_END: "#c050ff",
    ColorRole.BRAND_ICON_BG_START: "#1e0e50",
    ColorRole.BRAND_ICON_BG_END: "#060612",

    # Background colours
    ColorRole.BACKGROUND_PRIMARY: "#fbfcfe",
    ColorRole.BACKGROUND_SECONDARY: "#eef1f6",
    ColorRole.BACKGROUND_GRADIENT_START: "#ffffff",
    ColorRole.BACKGROUND_GRADIENT_END: "#eef1f6",
    ColorRole.BACKGROUND_TERTIARY: "#ffffff",
    ColorRole.BACKGROUND_TERTIARY_HOVER: "#e8edf4",
    ColorRole.BACKGROUND_TERTIARY_PRESSED: "#d6dde8",
    ColorRole.BACKGROUND_DIALOG: "#ffffff",

    # Text colours
    ColorRole.TEXT_PRIMARY: "#1c2330",
    ColorRole.TEXT_BRIGHT: "#0b1018",
    ColorRole.TEXT_HEADING: "#2540a0",
    ColorRole.TEXT_HEADING_BRIGHT: "#1f389a",
    ColorRole.TEXT_DISABLED: "#9aa3b2",
    ColorRole.TEXT_SELECTED: "#d4e2fb",
    ColorRole.TEXT_FOUND: "#e0b4b4",
    ColorRole.TEXT_FOUND_DIM: "#f4d8d8",
    ColorRole.TEXT_RECOMMENDED: "#ffffff",
    ColorRole.TEXT_LINK: "#2563eb",
    ColorRole.TEXT_INACTIVE: "#707070",
    ColorRole.TEXT_EPHEMERAL: "#a0785c",
    ColorRole.TEXT_EPHEMERAL_INACTIVE: "#c09070",
    ColorRole.TEXT_ERROR: "#f04030",
    ColorRole.TEXT_ERROR_INACTIVE: "#f07060",
    ColorRole.TEXT_SUCCESS: "#16a34a",

    # Edit box colours
    ColorRole.EDIT_BOX_BORDER: "#c0cad8",
    ColorRole.EDIT_BOX_BACKGROUND: "#ffffff",
    ColorRole.EDIT_BOX_ERROR: "#d04030",

    # Mindspace colours
    ColorRole.MINDSPACE_BACKGROUND: "#f5f7fb",
    ColorRole.MINDSPACE_NAME_BACKGROUND: "#d0d0d0",
    ColorRole.MINDSPACE_NAME_BACKGROUND_HOVER: "#b0b0b0",
    ColorRole.MINDSPACE_NAME_BACKGROUND_PRESSED: "#909090",
    ColorRole.MINDSPACE_HEADING: "#2a3b55",
    ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND: "#eaeef6",
    ColorRole.MINDSPACE_FOLDER: "#3b82f6",
    ColorRole.MINDSPACE_FOLDER_BREADCRUMB: "#2563eb",

    # Tab colours
    ColorRole.TAB_BAR_BACKGROUND: "#e6ebf3",
    ColorRole.TAB_BACKGROUND_ACTIVE: "#ffffff",
    ColorRole.TAB_BACKGROUND_INACTIVE: "#eef1f6",
    ColorRole.TAB_BACKGROUND_HOVER: "#f5f7fb",
    ColorRole.TAB_BACKGROUND_UPDATED: "#f0d0f8",
    ColorRole.TAB_BORDER_ACTIVE: "#2563eb",

    # Button colours
    ColorRole.BUTTON_BACKGROUND: "#eef1f6",
    ColorRole.BUTTON_BACKGROUND_PRESSED: "#d2dae6",
    ColorRole.BUTTON_BACKGROUND_HOVER: "#e2e8f1",
    ColorRole.BUTTON_SECONDARY_BACKGROUND: "#d8d8d8",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED: "#b0b0b0",
    ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER: "#c0c0c0",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED: "#3b82f6",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED: "#1d4ed8",
    ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER: "#2563eb",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE: "#e0503c",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED: "#b53626",
    ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER: "#cc4433",
    ColorRole.BUTTON_BACKGROUND_EDIT: "#a06008",
    ColorRole.BUTTON_BACKGROUND_EDIT_PRESSED: "#804800",
    ColorRole.BUTTON_BACKGROUND_EDIT_HOVER: "#b07018",
    ColorRole.BUTTON_BACKGROUND_DISABLED: "#e0e0e0",

    # Switch colours
    ColorRole.SWITCH_TRACK_ON: "#3b82f6",
    ColorRole.SWITCH_TRACK_OFF: "#dde3ec",
    ColorRole.SWITCH_TRACK_DISABLED_ON: "#c0d2f0",
    ColorRole.SWITCH_TRACK_BORDER: "#aab4c2",
    ColorRole.SWITCH_KNOB: "#ffffff",
    ColorRole.SWITCH_KNOB_DISABLED: "#707070",

    # Menu elements
    ColorRole.MENU_BACKGROUND: "#ffffff",
    ColorRole.MENU_HOVER: "#70a0f0",
    ColorRole.MENU_BORDER: "#cdd5e0",
    ColorRole.COMBO_ITEM_HOVER: "#e2e8f1",

    # Splitter bars
    ColorRole.SPLITTER: "#cdd5e0",
    ColorRole.TAB_SPLITTER: "#fcfcfc",

    # Scroll bar elements
    ColorRole.SCROLLBAR_BACKGROUND: "#eef1f6",
    ColorRole.SCROLLBAR_HANDLE: "#c0cad8",

    # Code block border
    ColorRole.CODE_BORDER: "#dde3ec",

    # Blockquote elements
    ColorRole.BLOCKQUOTE_BORDER: "#b4b4cd",
    ColorRole.BLOCKQUOTE_BACKGROUND: "#eef1f8",

    # Table elements
    ColorRole.TABLE_BORDER: "#dde3ec",
    ColorRole.TABLE_HEADER_BACKGROUND: "#dfe5ef",

    # Message colours
    ColorRole.MESSAGE_BACKGROUND: "#fbfcfe",
    ColorRole.MESSAGE_BACKGROUND_HOVER: "#e8edf4",
    ColorRole.MESSAGE_BACKGROUND_PRESSED: "#dde3ec",
    ColorRole.MESSAGE_USER_BACKGROUND: "#eef3fb",
    ColorRole.MESSAGE_USER_BACKGROUND_HOVER: "#e2eaf7",
    ColorRole.MESSAGE_USER_BACKGROUND_PRESSED: "#d4e0f4",
    # The input background has an alpha channel so it allows a small amount of what's underneath it to show through.
    ColorRole.MESSAGE_INPUT_BACKGROUND: "#eadde3ec",
    ColorRole.MESSAGE_ATTACHMENT_BACKGROUND: "#eef1f6",
    ColorRole.MESSAGE_BORDER: "#dde3ec",
    ColorRole.MESSAGE_USER_BORDER: "#bcd0ef",
    ColorRole.MESSAGE_INPUT_BORDER: "#a0aab8",
    ColorRole.MESSAGE_SPOTLIGHTED: "#607488",
    ColorRole.MESSAGE_USER: "#2563eb",
    ColorRole.MESSAGE_AI: "#1d7a4d",
    ColorRole.MESSAGE_REASONING: "#7a4fa3",
    ColorRole.MESSAGE_TOOL_CALL: "#526578",
    ColorRole.MESSAGE_TOOL_RESULT: "#526578",
    ColorRole.MESSAGE_USER_QUEUED: "#606060",
    ColorRole.MESSAGE_SYSTEM_ERROR: "#a04040",
    ColorRole.MESSAGE_SYSTEM_SUCCESS: "#40a040",
    ColorRole.MESSAGE_SYNTAX: "#2563eb",
    ColorRole.MESSAGE_STREAMING: "#a07050",
    ColorRole.MESSAGE_TRACE: "#606060",
    ColorRole.MESSAGE_INFORMATION: "#2563eb",
    ColorRole.MESSAGE_WARNING: "#7a5d00",
    ColorRole.MESSAGE_ERROR: "#b53626",

    # Status bar elements
    ColorRole.STATUS_BAR_BACKGROUND: "#eef1f6",
    ColorRole.CANARY_BACKGROUND: "#ff8080",

    # Close button states
    ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER: "#ff7070",

    # Drop target highlights
    ColorRole.DROP_TARGET_HIGHLIGHT: "#a8b8f8",
    ColorRole.DROP_TARGET_SEPARATOR_HIGHLIGHT: "#142454",

    # Line numbers
    ColorRole.LINE_NUMBER: "#c0c0c0",

    # Diff view colours
    ColorRole.DIFF_REMOVED_BACKGROUND: "#f0c8c8",
    ColorRole.DIFF_ADDED_BACKGROUND: "#c8f0c8",
    ColorRole.DIFF_CHANGED_BACKGROUND: "#c8c8f0",
    ColorRole.DIFF_HUNK_LINE_NUMBER: "#202020",

    # VCS status colours
    ColorRole.VCS_MODIFIED: "#c0a020",
    ColorRole.VCS_ADDED: "#207020",
    ColorRole.VCS_DELETED: "#c03030",
    ColorRole.VCS_RENAMED: "#4040a0",

    # Usage model colours
    ColorRole.USAGE_MODEL_COLOR_1: "#2878c8",
    ColorRole.USAGE_MODEL_COLOR_2: "#28a060",
    ColorRole.USAGE_MODEL_COLOR_3: "#c07800",
    ColorRole.USAGE_MODEL_COLOR_4: "#c03050",
    ColorRole.USAGE_MODEL_COLOR_5: "#6050c8",
    ColorRole.USAGE_MODEL_COLOR_6: "#c04820",
    ColorRole.USAGE_MODEL_COLOR_7: "#2090a0",
    ColorRole.USAGE_MODEL_COLOR_8: "#608020",

    # Syntax highlighting
    ColorRole.SYNTAX_ERROR: "#ff0000",
    ColorRole.SYNTAX_01: "#007070",
    ColorRole.SYNTAX_02: "#202020",
    ColorRole.SYNTAX_03: "#40a040",
    ColorRole.SYNTAX_04: "#c000a0",
    ColorRole.SYNTAX_05: "#606060",
    ColorRole.SYNTAX_06: "#0080a0",
    ColorRole.SYNTAX_07: "#a0a000",
    ColorRole.SYNTAX_08: "#5040c0",
    ColorRole.SYNTAX_09: "#0080a0",
    ColorRole.SYNTAX_10: "#a000a0",
    ColorRole.SYNTAX_11: "#0040c0",
    ColorRole.SYNTAX_12: "#c03030",
    ColorRole.SYNTAX_13: "#c080a0",
    ColorRole.SYNTAX_14: "#a09040",
    ColorRole.SYNTAX_15: "#2090a0",
    ColorRole.SYNTAX_16: "#508020",
    ColorRole.SYNTAX_17: "#404040",
    ColorRole.SYNTAX_18: "#609060",
    ColorRole.SYNTAX_19: "#a04020",
    ColorRole.SYNTAX_20: "#b03828",
    ColorRole.SYNTAX_21: "#24906c",

    # Terminal basic colors
    ColorRole.TERM_BLACK: "#000000",
    ColorRole.TERM_RED: "#cd0000",
    ColorRole.TERM_GREEN: "#00cd00",
    ColorRole.TERM_YELLOW: "#cdcd00",
    ColorRole.TERM_BLUE: "#0000ee",
    ColorRole.TERM_MAGENTA: "#cd00cd",
    ColorRole.TERM_CYAN: "#00cdcd",
    ColorRole.TERM_WHITE: "#e5e5e5",

    # Terminal bright colors
    ColorRole.TERM_BRIGHT_BLACK: "#7f7f7f",
    ColorRole.TERM_BRIGHT_RED: "#ff0000",
    ColorRole.TERM_BRIGHT_GREEN: "#00ff00",
    ColorRole.TERM_BRIGHT_YELLOW: "#ffff00",
    ColorRole.TERM_BRIGHT_BLUE: "#5c5cff",
    ColorRole.TERM_BRIGHT_MAGENTA: "#ff00ff",
    ColorRole.TERM_BRIGHT_CYAN: "#00ffff",
    ColorRole.TERM_BRIGHT_WHITE: "#ffffff",
}

DARK_PALETTE = FixedPalette(_DARK_COLORS)
LIGHT_PALETTE = FixedPalette(_LIGHT_COLORS)
COLOR_BLIND_PALETTE = FixedPalette(_COLOR_BLIND_COLORS)
OCEAN_LIGHT_PALETTE = FixedPalette(_OCEAN_LIGHT_COLORS)
GLOSSY_LIGHT_PALETTE = FixedPalette(_GLOSSY_LIGHT_COLORS)
