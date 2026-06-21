"""Color picker dialog for customizing application theme colors."""

from typing import Callable, List, Tuple, Dict

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QLabel,
    QWidget, QFrame, QListWidget, QListWidgetItem, QStackedWidget,
    QSplitter, QScrollArea, QColorDialog, QSizePolicy, QStyledItemDelegate,
    QStyleOptionViewItem
)
from PySide6.QtCore import QEvent, QModelIndex, QPersistentModelIndex, QPointF, QRectF, QSize, Signal, Qt, QTimer
from PySide6.QtGui import QColor, QFont, QPainter, QPen

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.style_manager import StyleManager
from desktop.color_theme import ColorTheme


# Preset themes: (name, gradient_start, gradient_end, {role_name: {mode_name: hex}})
# gradient_start / gradient_end are purely visual hints for the preset button.
# An empty colors dict means "clear all overrides" (the Default preset).
_PRESETS: List[Tuple[str, str, str, Dict[str, Dict[str, str]]]] = [
    (
        "Default",
        "#1c1c1c", "#404040",
        {},   # empty = clear all custom colors, no mode switch
    ),
    (
        "Enterprise Dark",
        "#07111c", "#2f80c9",
        {
            "BACKGROUND_PRIMARY": {"DARK": "#07111c"},
            "BACKGROUND_SECONDARY": {"DARK": "#101b27"},
            "BACKGROUND_TERTIARY": {"DARK": "#0c1622"},
            "BACKGROUND_TERTIARY_HOVER": {"DARK": "#1a2b3d"},
            "BACKGROUND_TERTIARY_PRESSED": {"DARK": "#24384c"},
            "BACKGROUND_DIALOG": {"DARK": "#111d2a"},
            "TAB_BAR_BACKGROUND": {"DARK": "#09131f"},
            "TAB_BACKGROUND_ACTIVE": {"DARK": "#152536"},
            "TAB_BACKGROUND_INACTIVE": {"DARK": "#101b27"},
            "TAB_BORDER_ACTIVE": {"DARK": "#4ea1e8"},
            "MINDSPACE_BACKGROUND": {"DARK": "#08111b"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND": {"DARK": "#060d14"},
            "MINDSPACE_HEADING": {"DARK": "#7ab8f0"},
            "MINDSPACE_FOLDER": {"DARK": "#4ea1e8"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#7ab8f0"},
            "EDIT_BOX_BACKGROUND": {"DARK": "#0e1a26"},
            "EDIT_BOX_BORDER": {"DARK": "#2a4057"},
            "BUTTON_BACKGROUND": {"DARK": "#172839"},
            "BUTTON_BACKGROUND_HOVER": {"DARK": "#20354a"},
            "BUTTON_BACKGROUND_PRESSED": {"DARK": "#2a4057"},
            "BUTTON_BACKGROUND_RECOMMENDED": {"DARK": "#256ea8"},
            "BUTTON_BACKGROUND_RECOMMENDED_HOVER": {"DARK": "#2f80c9"},
            "BUTTON_BACKGROUND_RECOMMENDED_PRESSED": {"DARK": "#1d5d8f"},
            "BUTTON_BACKGROUND_DESTRUCTIVE": {"DARK": "#8b1c1c"},
            "SWITCH_TRACK_ON": {"DARK": "#256ea8"},
            "SWITCH_TRACK_OFF": {"DARK": "#172839"},
            "SWITCH_TRACK_BORDER": {"DARK": "#4ea1e8"},
            "SWITCH_KNOB": {"DARK": "#e4f8ff"},
            "TEXT_PRIMARY": {"DARK": "#d7e3ee"},
            "TEXT_BRIGHT": {"DARK": "#f3f8fc"},
            "TEXT_DISABLED": {"DARK": "#7d8fa1"},
            "TEXT_RECOMMENDED": {"DARK": "#ffffff"},
            "MESSAGE_BACKGROUND": {"DARK": "#101a25"},
            "MESSAGE_BACKGROUND_HOVER": {"DARK": "#172638"},
            "MESSAGE_BACKGROUND_PRESSED": {"DARK": "#1f3146"},
            "MESSAGE_USER_BACKGROUND": {"DARK": "#112235"},
            "MESSAGE_USER_BACKGROUND_HOVER": {"DARK": "#18304a"},
            "MESSAGE_USER_BACKGROUND_PRESSED": {"DARK": "#203d5c"},
            "MESSAGE_BORDER": {"DARK": "#2a4057"},
            "MESSAGE_USER_BORDER": {"DARK": "#356b9f"},
            "MESSAGE_USER": {"DARK": "#7ab8f0"},
            "MESSAGE_AI": {"DARK": "#64d28f"},
            "MESSAGE_TOOL_CALL": {"DARK": "#8fa8bd"},
            "MESSAGE_TOOL_RESULT": {"DARK": "#8fa8bd"},
        },
    ),
    (
        "Secure Green",
        "#05140d", "#25a46a",
        {
            "BACKGROUND_PRIMARY": {"DARK": "#05140d"},
            "BACKGROUND_SECONDARY": {"DARK": "#0b2116"},
            "BACKGROUND_TERTIARY": {"DARK": "#07180f"},
            "BACKGROUND_TERTIARY_HOVER": {"DARK": "#123021"},
            "BACKGROUND_TERTIARY_PRESSED": {"DARK": "#1a432f"},
            "BACKGROUND_DIALOG": {"DARK": "#0d2418"},
            "TAB_BAR_BACKGROUND": {"DARK": "#06140d"},
            "TAB_BACKGROUND_ACTIVE": {"DARK": "#123525"},
            "TAB_BACKGROUND_INACTIVE": {"DARK": "#0b2116"},
            "TAB_BORDER_ACTIVE": {"DARK": "#30c989"},
            "MINDSPACE_BACKGROUND": {"DARK": "#030f09"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND": {"DARK": "#020b07"},
            "MINDSPACE_HEADING": {"DARK": "#5bd99d"},
            "MINDSPACE_FOLDER": {"DARK": "#30c989"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#5bd99d"},
            "EDIT_BOX_BACKGROUND": {"DARK": "#0a1d13"},
            "EDIT_BOX_BORDER": {"DARK": "#224b36"},
            "BUTTON_BACKGROUND": {"DARK": "#123021"},
            "BUTTON_BACKGROUND_HOVER": {"DARK": "#1a432f"},
            "BUTTON_BACKGROUND_PRESSED": {"DARK": "#23583d"},
            "BUTTON_BACKGROUND_RECOMMENDED": {"DARK": "#17734a"},
            "BUTTON_BACKGROUND_RECOMMENDED_HOVER": {"DARK": "#219260"},
            "BUTTON_BACKGROUND_RECOMMENDED_PRESSED": {"DARK": "#125e3b"},
            "BUTTON_BACKGROUND_DESTRUCTIVE": {"DARK": "#8b1c1c"},
            "SWITCH_TRACK_ON": {"DARK": "#17734a"},
            "SWITCH_TRACK_OFF": {"DARK": "#123021"},
            "SWITCH_TRACK_BORDER": {"DARK": "#30c989"},
            "SWITCH_KNOB": {"DARK": "#e4ffe8"},
            "TEXT_PRIMARY": {"DARK": "#d8eadf"},
            "TEXT_BRIGHT": {"DARK": "#f1fff6"},
            "TEXT_DISABLED": {"DARK": "#7ca38d"},
            "TEXT_RECOMMENDED": {"DARK": "#ffffff"},
            "MESSAGE_BACKGROUND": {"DARK": "#0b1d13"},
            "MESSAGE_BACKGROUND_HOVER": {"DARK": "#112c1d"},
            "MESSAGE_BACKGROUND_PRESSED": {"DARK": "#173b28"},
            "MESSAGE_USER_BACKGROUND": {"DARK": "#0d2619"},
            "MESSAGE_USER_BACKGROUND_HOVER": {"DARK": "#123623"},
            "MESSAGE_USER_BACKGROUND_PRESSED": {"DARK": "#194832"},
            "MESSAGE_BORDER": {"DARK": "#224b36"},
            "MESSAGE_USER_BORDER": {"DARK": "#2a7a54"},
            "MESSAGE_USER": {"DARK": "#5bd99d"},
            "MESSAGE_AI": {"DARK": "#30c989"},
            "MESSAGE_TOOL_CALL": {"DARK": "#8fb5a0"},
            "MESSAGE_TOOL_RESULT": {"DARK": "#8fb5a0"},
        },
    ),
    (
        "Midnight Blue",
        "#061428", "#4b8fe8",
        {
            "BACKGROUND_PRIMARY": {"DARK": "#061428"},
            "BACKGROUND_SECONDARY": {"DARK": "#0c1f3a"},
            "BACKGROUND_TERTIARY": {"DARK": "#08172b"},
            "BACKGROUND_TERTIARY_HOVER": {"DARK": "#142d50"},
            "BACKGROUND_TERTIARY_PRESSED": {"DARK": "#1d3f68"},
            "BACKGROUND_DIALOG": {"DARK": "#102642"},
            "TAB_BAR_BACKGROUND": {"DARK": "#04101f"},
            "TAB_BACKGROUND_ACTIVE": {"DARK": "#113765"},
            "TAB_BACKGROUND_INACTIVE": {"DARK": "#0c1f3a"},
            "TAB_BORDER_ACTIVE": {"DARK": "#4b8fe8"},
            "MINDSPACE_BACKGROUND": {"DARK": "#030b16"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND": {"DARK": "#020810"},
            "MINDSPACE_HEADING": {"DARK": "#6aa6f8"},
            "MINDSPACE_FOLDER": {"DARK": "#58a6ff"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#8bbfff"},
            "EDIT_BOX_BACKGROUND": {"DARK": "#0c1f3a"},
            "EDIT_BOX_BORDER": {"DARK": "#113765"},
            "BUTTON_BACKGROUND": {"DARK": "#102b4a"},
            "BUTTON_BACKGROUND_HOVER": {"DARK": "#173a62"},
            "BUTTON_BACKGROUND_PRESSED": {"DARK": "#204c7e"},
            "BUTTON_BACKGROUND_RECOMMENDED": {"DARK": "#1b5790"},
            "BUTTON_BACKGROUND_RECOMMENDED_HOVER": {"DARK": "#2b6db8"},
            "BUTTON_BACKGROUND_RECOMMENDED_PRESSED": {"DARK": "#164a7a"},
            "BUTTON_BACKGROUND_DESTRUCTIVE": {"DARK": "#8b1c1c"},
            "SWITCH_TRACK_ON": {"DARK": "#2b6db8"},
            "SWITCH_TRACK_OFF": {"DARK": "#102b4a"},
            "SWITCH_TRACK_BORDER": {"DARK": "#6aa6f8"},
            "SWITCH_KNOB": {"DARK": "#eef6ff"},
            "TEXT_PRIMARY": {"DARK": "#c4dcf8"},
            "TEXT_BRIGHT": {"DARK": "#eef6ff"},
            "TEXT_DISABLED": {"DARK": "#45627f"},
            "TEXT_RECOMMENDED": {"DARK": "#ffffff"},
            "MESSAGE_BACKGROUND": {"DARK": "#0d1f36"},
            "MESSAGE_BACKGROUND_HOVER": {"DARK": "#142d4c"},
            "MESSAGE_BACKGROUND_PRESSED": {"DARK": "#1c3c64"},
            "MESSAGE_USER_BACKGROUND": {"DARK": "#0f2744"},
            "MESSAGE_USER_BACKGROUND_HOVER": {"DARK": "#16375d"},
            "MESSAGE_USER_BACKGROUND_PRESSED": {"DARK": "#1e4776"},
            "MESSAGE_BORDER": {"DARK": "#244d78"},
            "MESSAGE_USER_BORDER": {"DARK": "#2b6db8"},
            "MESSAGE_USER": {"DARK": "#8bbfff"},
            "MESSAGE_AI": {"DARK": "#64d28f"},
            "MESSAGE_TOOL_CALL": {"DARK": "#91a9c6"},
            "MESSAGE_TOOL_RESULT": {"DARK": "#91a9c6"},
        },
    ),
    (
        "Enterprise Light",
        "#f7f9fc", "#4b89dc",
        {
            "BACKGROUND_PRIMARY": {"LIGHT": "#f6f8fb"},
            "BACKGROUND_SECONDARY": {"LIGHT": "#e8eef5"},
            "BACKGROUND_TERTIARY": {"LIGHT": "#ffffff"},
            "BACKGROUND_TERTIARY_HOVER": {"LIGHT": "#dfe8f2"},
            "BACKGROUND_TERTIARY_PRESSED": {"LIGHT": "#cfdbe8"},
            "BACKGROUND_DIALOG": {"LIGHT": "#ffffff"},
            "TAB_BAR_BACKGROUND": {"LIGHT": "#d9e3ef"},
            "TAB_BACKGROUND_ACTIVE": {"LIGHT": "#ffffff"},
            "TAB_BACKGROUND_INACTIVE": {"LIGHT": "#e7edf5"},
            "TAB_BACKGROUND_HOVER": {"LIGHT": "#edf3f9"},
            "TAB_BORDER_ACTIVE": {"LIGHT": "#1f5f99"},
            "MINDSPACE_BACKGROUND": {"LIGHT": "#edf3f9"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND": {"LIGHT": "#d9e6f3"},
            "MINDSPACE_HEADING": {"LIGHT": "#22384f"},
            "MINDSPACE_FOLDER": {"LIGHT": "#1f6fb2"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"LIGHT": "#174f80"},
            "EDIT_BOX_BACKGROUND": {"LIGHT": "#ffffff"},
            "EDIT_BOX_BORDER": {"LIGHT": "#aebfd1"},
            "BUTTON_BACKGROUND": {"LIGHT": "#e3ebf4"},
            "BUTTON_BACKGROUND_HOVER": {"LIGHT": "#d6e2ef"},
            "BUTTON_BACKGROUND_PRESSED": {"LIGHT": "#c6d5e5"},
            "BUTTON_BACKGROUND_RECOMMENDED": {"LIGHT": "#1f6fb2"},
            "BUTTON_BACKGROUND_RECOMMENDED_HOVER": {"LIGHT": "#185d96"},
            "BUTTON_BACKGROUND_RECOMMENDED_PRESSED": {"LIGHT": "#144c7a"},
            "BUTTON_BACKGROUND_DESTRUCTIVE": {"LIGHT": "#c94a3a"},
            "BUTTON_BACKGROUND_DESTRUCTIVE_HOVER": {"LIGHT": "#b83e31"},
            "BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED": {"LIGHT": "#9f3328"},
            "SWITCH_TRACK_ON": {"LIGHT": "#1f6fb2"},
            "SWITCH_TRACK_OFF": {"LIGHT": "#d7e0eb"},
            "SWITCH_TRACK_BORDER": {"LIGHT": "#92a8bf"},
            "SWITCH_KNOB": {"LIGHT": "#ffffff"},
            "TEXT_PRIMARY": {"LIGHT": "#1f2933"},
            "TEXT_BRIGHT": {"LIGHT": "#0f1720"},
            "TEXT_DISABLED": {"LIGHT": "#65758a"},
            "TEXT_RECOMMENDED": {"LIGHT": "#ffffff"},
            "TEXT_LINK": {"LIGHT": "#185d96"},
            "TEXT_SELECTED": {"LIGHT": "#cfe4fb"},
            "MESSAGE_BACKGROUND": {"LIGHT": "#f5f9fd"},
            "MESSAGE_BACKGROUND_HOVER": {"LIGHT": "#eaf2fa"},
            "MESSAGE_BACKGROUND_PRESSED": {"LIGHT": "#deeaf6"},
            "MESSAGE_USER_BACKGROUND": {"LIGHT": "#e9f3fa"},
            "MESSAGE_USER_BACKGROUND_HOVER": {"LIGHT": "#deedf8"},
            "MESSAGE_USER_BACKGROUND_PRESSED": {"LIGHT": "#d0e4f4"},
            "MESSAGE_BORDER": {"LIGHT": "#c5d3e1"},
            "MESSAGE_USER_BORDER": {"LIGHT": "#9fbede"},
            "MESSAGE_ATTACHMENT_BACKGROUND": {"LIGHT": "#eef3f8"},
            "MESSAGE_USER": {"LIGHT": "#185d96"},
            "MESSAGE_AI": {"LIGHT": "#1d7a4d"},
            "MESSAGE_REASONING": {"LIGHT": "#7a4fa3"},
            "MESSAGE_TOOL_CALL": {"LIGHT": "#526578"},
            "MESSAGE_TOOL_RESULT": {"LIGHT": "#526578"},
            "MESSAGE_SYNTAX": {"LIGHT": "#185d96"},
            "MESSAGE_INFORMATION": {"LIGHT": "#185d96"},
            "MESSAGE_WARNING": {"LIGHT": "#7a5d00"},
            "MESSAGE_ERROR": {"LIGHT": "#9f3328"},
            "CODE_BORDER": {"LIGHT": "#c5d3e1"},
            "TABLE_BORDER": {"LIGHT": "#c5d3e1"},
            "SCROLLBAR_BACKGROUND": {"LIGHT": "#e7edf5"},
            "SCROLLBAR_HANDLE": {"LIGHT": "#9eb2c8"},
        },
    ),
    (
        "High Contrast",
        "#050505", "#f6d400",
        {
            "BACKGROUND_PRIMARY": {"DARK": "#050505"},
            "BACKGROUND_SECONDARY": {"DARK": "#101010"},
            "BACKGROUND_TERTIARY": {"DARK": "#050505"},
            "BACKGROUND_TERTIARY_HOVER": {"DARK": "#1a1a1a"},
            "BACKGROUND_TERTIARY_PRESSED": {"DARK": "#242424"},
            "BACKGROUND_DIALOG": {"DARK": "#000000"},
            "TAB_BAR_BACKGROUND": {"DARK": "#050505"},
            "TAB_BACKGROUND_ACTIVE": {"DARK": "#141414"},
            "TAB_BACKGROUND_INACTIVE": {"DARK": "#0a0a0a"},
            "TAB_BORDER_ACTIVE": {"DARK": "#f6d400"},
            "MINDSPACE_BACKGROUND": {"DARK": "#000000"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND": {"DARK": "#050505"},
            "MINDSPACE_HEADING": {"DARK": "#f6d400"},
            "MINDSPACE_FOLDER": {"DARK": "#f6d400"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#ffe866"},
            "EDIT_BOX_BACKGROUND": {"DARK": "#050505"},
            "EDIT_BOX_BORDER": {"DARK": "#f6d400"},
            "BUTTON_BACKGROUND": {"DARK": "#171717"},
            "BUTTON_BACKGROUND_HOVER": {"DARK": "#242424"},
            "BUTTON_BACKGROUND_PRESSED": {"DARK": "#303030"},
            "BUTTON_BACKGROUND_RECOMMENDED": {"DARK": "#f6d400"},
            "BUTTON_BACKGROUND_RECOMMENDED_HOVER": {"DARK": "#ffe866"},
            "BUTTON_BACKGROUND_RECOMMENDED_PRESSED": {"DARK": "#d8b900"},
            "BUTTON_BACKGROUND_DESTRUCTIVE": {"DARK": "#ff6b4a"},
            "BUTTON_BACKGROUND_DESTRUCTIVE_HOVER": {"DARK": "#ff8568"},
            "BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED": {"DARK": "#d95438"},
            "TEXT_RECOMMENDED": {"DARK": "#000000"},
            "SWITCH_TRACK_ON": {"DARK": "#f6d400"},
            "SWITCH_TRACK_OFF": {"DARK": "#171717"},
            "SWITCH_TRACK_BORDER": {"DARK": "#f6d400"},
            "SWITCH_KNOB": {"DARK": "#000000"},
            "TEXT_PRIMARY": {"DARK": "#f7f7f7"},
            "TEXT_BRIGHT": {"DARK": "#ffffff"},
            "TEXT_DISABLED": {"DARK": "#888888"},
            "MESSAGE_BACKGROUND": {"DARK": "#050505"},
            "MESSAGE_BACKGROUND_HOVER": {"DARK": "#111111"},
            "MESSAGE_BACKGROUND_PRESSED": {"DARK": "#1c1c1c"},
            "MESSAGE_USER_BACKGROUND": {"DARK": "#080800"},
            "MESSAGE_USER_BACKGROUND_HOVER": {"DARK": "#141000"},
            "MESSAGE_USER_BACKGROUND_PRESSED": {"DARK": "#201a00"},
            "MESSAGE_BORDER": {"DARK": "#777777"},
            "MESSAGE_USER_BORDER": {"DARK": "#f6d400"},
            "MESSAGE_USER": {"DARK": "#ffe866"},
            "MESSAGE_AI": {"DARK": "#ffffff"},
            "MESSAGE_TOOL_CALL": {"DARK": "#d0d0d0"},
            "MESSAGE_TOOL_RESULT": {"DARK": "#d0d0d0"},
        },
    ),
]

# Sections: (section_id, display_label, [(swatch_label, ColorRole), ...])
_SECTIONS: List[Tuple[str, str, List[Tuple[str, ColorRole]]]] = [
    ("background", "Background", [
        ("Primary background", ColorRole.BACKGROUND_PRIMARY),
        ("Secondary background", ColorRole.BACKGROUND_SECONDARY),
        ("Gradient start", ColorRole.BACKGROUND_GRADIENT_START),
        ("Gradient end", ColorRole.BACKGROUND_GRADIENT_END),
        ("Tertiary background", ColorRole.BACKGROUND_TERTIARY),
        ("Tertiary hover", ColorRole.BACKGROUND_TERTIARY_HOVER),
        ("Tertiary pressed", ColorRole.BACKGROUND_TERTIARY_PRESSED),
        ("Dialog background", ColorRole.BACKGROUND_DIALOG),
    ]),
    ("tabs", "Tabs", [
        ("Tab bar background", ColorRole.TAB_BAR_BACKGROUND),
        ("Active tab", ColorRole.TAB_BACKGROUND_ACTIVE),
        ("Inactive tab", ColorRole.TAB_BACKGROUND_INACTIVE),
        ("Active tab border", ColorRole.TAB_BORDER_ACTIVE),
    ]),
    ("side_panel", "Side Panel", [
        ("Panel background", ColorRole.MINDSPACE_BACKGROUND),
        ("Tool rail background", ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND),
        ("Heading text", ColorRole.MINDSPACE_HEADING),
    ]),
    ("input_box", "Input Box", [
        ("Input background", ColorRole.EDIT_BOX_BACKGROUND),
        ("Input border", ColorRole.EDIT_BOX_BORDER),
    ]),
    ("buttons", "Buttons", [
        ("Default button", ColorRole.BUTTON_BACKGROUND),
        ("Recommended button", ColorRole.BUTTON_BACKGROUND_RECOMMENDED),
        ("Destructive button", ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE),
    ]),
    ("switches", "Switches", [
        ("On track", ColorRole.SWITCH_TRACK_ON),
        ("Off track", ColorRole.SWITCH_TRACK_OFF),
        ("Track border", ColorRole.SWITCH_TRACK_BORDER),
        ("Knob", ColorRole.SWITCH_KNOB),
    ]),
    ("text", "Text", [
        ("Primary text", ColorRole.TEXT_PRIMARY),
        ("Bright text", ColorRole.TEXT_BRIGHT),
        ("Disabled text", ColorRole.TEXT_DISABLED),
    ]),
    ("code_highlighting", "Code Highlighting", [
        ("Error", ColorRole.SYNTAX_ERROR),
        ("Plain code text", ColorRole.SYNTAX_02),
        ("Comments", ColorRole.SYNTAX_03),
        ("Keywords", ColorRole.SYNTAX_13),
        ("Functions", ColorRole.SYNTAX_07),
        ("Types", ColorRole.SYNTAX_21),
        ("Strings", ColorRole.SYNTAX_20),
        ("Numbers and constants", ColorRole.SYNTAX_16),
        ("Operators and punctuation", ColorRole.SYNTAX_17),
        ("Attributes and annotations", ColorRole.SYNTAX_08),
        ("Links and references", ColorRole.SYNTAX_11),
        ("Headings and fences", ColorRole.SYNTAX_14),
    ]),
    ("icons", "Icons", [
        ("Folder icon", ColorRole.MINDSPACE_FOLDER),
        ("Folder breadcrumb", ColorRole.MINDSPACE_FOLDER_BREADCRUMB),
        ("VCS: modified", ColorRole.VCS_MODIFIED),
        ("VCS: added", ColorRole.VCS_ADDED),
        ("VCS: deleted", ColorRole.VCS_DELETED),
        ("VCS: renamed", ColorRole.VCS_RENAMED),
    ]),
    ("accessibility", "Accessibility", [
        ("Error", ColorRole.MESSAGE_ERROR),
        ("Warning", ColorRole.MESSAGE_WARNING),
        ("Information", ColorRole.MESSAGE_INFORMATION),
        ("Error text", ColorRole.TEXT_ERROR),
        ("Link text", ColorRole.TEXT_LINK),
    ]),
]

# Accessibility presets: (name, description, {role_name: {mode_name: hex}})
# Only override the signal/status colors — other custom colors are preserved.
_ACCESSIBILITY_PRESETS: List[Tuple[str, str, Dict[str, Dict[str, str]]]] = [
    (
        "Deuteranopia",
        "Red-green colorblindness (~8% of males). Uses blue/orange contrast.",
        {
            "MESSAGE_ERROR": {"DARK": "#e87820"},
            "MESSAGE_WARNING": {"DARK": "#2070c0"},
            "MESSAGE_INFORMATION": {"DARK": "#8840c0"},
            "TEXT_ERROR": {"DARK": "#ff9020"},
            "TEXT_LINK": {"DARK": "#4499ff"},
            "VCS_MODIFIED": {"DARK": "#0099bb"},
            "VCS_ADDED": {"DARK": "#4488ff"},
            "VCS_DELETED": {"DARK": "#e87820"},
            "VCS_RENAMED": {"DARK": "#9966ff"},
            "MINDSPACE_FOLDER": {"DARK": "#4488ff"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#4488ff"},
        },
    ),
    (
        "Protanopia",
        "Red-weakness (~1% of males). Uses blue/gold contrast.",
        {
            "MESSAGE_ERROR": {"DARK": "#c89900"},
            "MESSAGE_WARNING": {"DARK": "#0080cc"},
            "MESSAGE_INFORMATION": {"DARK": "#7733bb"},
            "TEXT_ERROR": {"DARK": "#ddaa00"},
            "TEXT_LINK": {"DARK": "#3388ff"},
            "VCS_MODIFIED": {"DARK": "#00aacc"},
            "VCS_ADDED": {"DARK": "#3388ff"},
            "VCS_DELETED": {"DARK": "#c89900"},
            "VCS_RENAMED": {"DARK": "#8855ee"},
            "MINDSPACE_FOLDER": {"DARK": "#3388ff"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#3388ff"},
        },
    ),
    (
        "Tritanopia",
        "Blue-yellow colorblindness (rare). Uses red/green/magenta contrast.",
        {
            "MESSAGE_ERROR": {"DARK": "#dd3333"},
            "MESSAGE_WARNING": {"DARK": "#ee8800"},
            "MESSAGE_INFORMATION": {"DARK": "#22aa55"},
            "TEXT_ERROR": {"DARK": "#ff4444"},
            "TEXT_LINK": {"DARK": "#cc44cc"},
            "VCS_MODIFIED": {"DARK": "#ee8800"},
            "VCS_ADDED": {"DARK": "#22cc66"},
            "VCS_DELETED": {"DARK": "#dd3333"},
            "VCS_RENAMED": {"DARK": "#cc44cc"},
            "MINDSPACE_FOLDER": {"DARK": "#ee8800"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#ee8800"},
        },
    ),
    (
        "High Contrast",
        "Maximum contrast for low-vision users.",
        {
            "BACKGROUND_PRIMARY": {"DARK": "#000000"},
            "BACKGROUND_SECONDARY": {"DARK": "#111111"},
            "BACKGROUND_DIALOG": {"DARK": "#0a0a0a"},
            "TEXT_PRIMARY": {"DARK": "#ffffff"},
            "TEXT_BRIGHT": {"DARK": "#ffff00"},
            "TEXT_DISABLED": {"DARK": "#aaaaaa"},
            "TEXT_ERROR": {"DARK": "#ff4422"},
            "TEXT_LINK": {"DARK": "#66ccff"},
            "MESSAGE_ERROR": {"DARK": "#ff2200"},
            "MESSAGE_WARNING": {"DARK": "#ffcc00"},
            "MESSAGE_INFORMATION": {"DARK": "#00ccff"},
            "VCS_MODIFIED": {"DARK": "#ffcc00"},
            "VCS_ADDED": {"DARK": "#00ff88"},
            "VCS_DELETED": {"DARK": "#ff2200"},
            "VCS_RENAMED": {"DARK": "#00ccff"},
            "MINDSPACE_FOLDER": {"DARK": "#ffcc00"},
            "MINDSPACE_FOLDER_BREADCRUMB": {"DARK": "#ffcc00"},
            "TAB_BORDER_ACTIVE": {"DARK": "#ffffff"},
            "EDIT_BOX_BORDER": {"DARK": "#ffffff"},
        },
    ),
]


class _SwatchButton(QPushButton):
    """Colored square button that opens a color picker on click."""

    color_chosen = Signal(str)  # emits hex color string

    def __init__(
        self,
        role: ColorRole,
        style_manager: StyleManager,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._role = role
        self._style_manager = style_manager
        self._current_color = style_manager.get_color_str(role)
        zoom = style_manager.zoom_factor()
        size = int(28 * zoom)
        self.setFixedSize(size, size)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.clicked.connect(self._on_click)
        self.refresh()

    def refresh(self) -> None:
        """Sync the swatch color to the current StyleManager value for this role."""
        self._current_color = self._style_manager.get_color_str(self._role)
        self._apply_color(self._current_color)

    def _apply_color(self, hex_color: str) -> None:
        zoom = self._style_manager.zoom_factor()
        radius = int(4 * zoom)
        self.setStyleSheet(f"""
            QPushButton {{
                background-color: {hex_color};
                border: 1px solid rgba(128,128,128,0.4);
                border-radius: {radius}px;
            }}
            QPushButton:hover {{
                border: 2px solid rgba(255,255,255,0.6);
            }}
        """)

    def _on_click(self) -> None:
        initial = QColor(self._current_color)
        color = QColorDialog.getColor(initial, self, "Choose Color")
        if color.isValid():
            self._current_color = color.name()
            self._apply_color(self._current_color)
            self.color_chosen.emit(self._current_color)


class _GradientButton(QPushButton):
    """Button for choosing a two-colour background gradient."""

    gradient_chosen = Signal(str, str)

    def __init__(self, style_manager: StyleManager, parent: QWidget | None = None) -> None:
        super().__init__("Choose gradient", parent)
        self._style_manager = style_manager
        self.clicked.connect(self._on_click)
        self.refresh()

    def refresh(self) -> None:
        """Refresh the preview background from current background colours."""
        start = self._style_manager.get_color_str(ColorRole.BACKGROUND_GRADIENT_START)
        end = self._style_manager.get_color_str(ColorRole.BACKGROUND_GRADIENT_END)
        text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        border = self._style_manager.get_color_str(ColorRole.EDIT_BOX_BORDER)
        zoom = self._style_manager.zoom_factor()
        self.setMinimumHeight(round(30 * zoom))
        self.setStyleSheet(f"""
            QPushButton {{
                background: qlineargradient(
                    x1:0, y1:0, x2:1, y2:1,
                    stop:0 {start},
                    stop:1 {end}
                );
                color: {text};
                border: 1px solid {border};
                border-radius: {round(5 * zoom)}px;
                padding: 6px 10px;
            }}
        """)

    def _on_click(self) -> None:
        start_initial = QColor(self._style_manager.get_color_str(ColorRole.BACKGROUND_GRADIENT_START))
        start = QColorDialog.getColor(start_initial, self, "Choose Gradient Start")
        if not start.isValid():
            return

        end_initial = QColor(self._style_manager.get_color_str(ColorRole.BACKGROUND_GRADIENT_END))
        end = QColorDialog.getColor(end_initial, self, "Choose Gradient End")
        if not end.isValid():
            return

        self.gradient_chosen.emit(start.name(), end.name())
        self.refresh()


class _NavItemDelegate(QStyledItemDelegate):
    """Controls row height for the section nav list."""

    def __init__(self, style_manager: StyleManager, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = style_manager

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        row_height = fm.height() + round(12 * zoom)
        return QSize(super().sizeHint(option, index).width(), row_height)


class _SectionPage(QWidget):
    """One stacked page showing swatches for a group of color roles."""

    def __init__(
        self,
        rows: List[Tuple[str, ColorRole]],
        style_manager: StyleManager,
        on_color_changed: Callable[[ColorRole, str], None],
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._style_manager = style_manager
        self._rows = rows
        self._on_color_changed = on_color_changed
        self._swatches: Dict[ColorRole, _SwatchButton] = {}
        self._gradient_button: _GradientButton | None = None

        layout = QVBoxLayout()
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(int(8 * style_manager.zoom_factor()))

        for label_text, role in rows:
            row = QHBoxLayout()
            row.setSpacing(int(10 * style_manager.zoom_factor()))

            swatch = _SwatchButton(role, style_manager, self)
            swatch.color_chosen.connect(lambda hex_c, r=role: on_color_changed(r, hex_c))
            self._swatches[role] = swatch

            lbl = QLabel(label_text)
            lbl.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Preferred)

            row.addWidget(swatch)
            row.addWidget(lbl)
            row.addStretch()
            layout.addLayout(row)

        roles = {role for _, role in rows}
        if ColorRole.BACKGROUND_GRADIENT_START in roles and ColorRole.BACKGROUND_GRADIENT_END in roles:
            gradient = _GradientButton(style_manager, self)
            gradient.gradient_chosen.connect(self._on_gradient_chosen)
            self._gradient_button = gradient
            layout.addWidget(gradient)

        layout.addStretch()
        self.setLayout(layout)

    def refresh_swatches(self) -> None:
        """Refresh all swatches to reflect current StyleManager colors."""
        for swatch in self._swatches.values():
            swatch.refresh()
        if self._gradient_button is not None:
            self._gradient_button.refresh()

    def roles(self) -> List[ColorRole]:
        """Return the color roles edited by this section."""
        return [role for _, role in self._rows]

    def _on_gradient_chosen(self, start: str, end: str) -> None:
        """Apply a gradient by updating the optional background gradient roles."""
        self._on_color_changed(ColorRole.BACKGROUND_GRADIENT_START, start)
        self._on_color_changed(ColorRole.BACKGROUND_GRADIENT_END, end)


class _PresetPreviewButton(QPushButton):
    """Theme preset button that paints a compact UI preview."""

    def __init__(
        self,
        name: str,
        grad_start: str,
        grad_end: str,
        colors: Dict[str, Dict[str, str]],
        style_manager: StyleManager,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._name = name
        self._grad_start = grad_start
        self._grad_end = grad_end
        self._colors = colors
        self._style_manager = style_manager
        self.setCheckable(True)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        self.setText("")
        self.setStyleSheet("border: none; background: transparent;")

    def _role_color(self, role: ColorRole, fallback: str) -> QColor:
        role_colors = self._colors.get(role.name, {})
        return QColor(role_colors.get("CUSTOM") or role_colors.get("DARK") or role_colors.get("LIGHT") or fallback)

    def paintEvent(self, event: QEvent) -> None:
        del event

        zoom = self._style_manager.zoom_factor()
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        outer = QRectF(self.rect()).adjusted(1, 1, -1, -1)
        bg = self._role_color(ColorRole.BACKGROUND_DIALOG, self._grad_start)
        panel = self._role_color(ColorRole.BACKGROUND_SECONDARY, self._grad_start)
        tab = self._role_color(ColorRole.TAB_BACKGROUND_ACTIVE, self._grad_start)
        border = self._role_color(ColorRole.TAB_BORDER_ACTIVE, self._grad_end)
        text = self._role_color(ColorRole.TEXT_PRIMARY, "#ffffff")
        muted = self._role_color(ColorRole.TEXT_DISABLED, "#7a8590")
        message = self._role_color(ColorRole.MESSAGE_BACKGROUND, self._grad_start)
        input_bg = self._role_color(ColorRole.EDIT_BOX_BACKGROUND, self._grad_start)
        switch = self._role_color(ColorRole.SWITCH_TRACK_ON, self._grad_end)
        folder = self._role_color(ColorRole.MINDSPACE_FOLDER, self._grad_end)

        painter.setPen(QPen(border if self.isChecked() else QColor(96, 112, 128, 90), 2 if self.isChecked() else 1))
        painter.setBrush(bg)
        painter.drawRoundedRect(outer, round(8 * zoom), round(8 * zoom))

        title_font = QFont(painter.font())
        title_font.setBold(True)
        title_font.setPointSizeF(max(7.0, title_font.pointSizeF() * 0.78))
        painter.setFont(title_font)
        painter.setPen(text)
        painter.drawText(outer.adjusted(8, 6, -8, 0), Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft, self._name)

        preview = outer.adjusted(8, 24, -8, -8)
        rail_w = preview.width() * 0.23
        rail = QRectF(preview.left(), preview.top(), rail_w, preview.height())
        content = QRectF(rail.right() + 5, preview.top(), preview.width() - rail_w - 5, preview.height())

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(panel)
        painter.drawRoundedRect(rail, 5, 5)

        painter.setBrush(folder)
        for i in range(3):
            y = rail.top() + 8 + (i * 13)
            folder_rect = QRectF(rail.left() + 6, y + 2, rail.width() - 12, 6)
            tab_rect = QRectF(folder_rect.left(), y, folder_rect.width() * 0.45, 3)
            painter.drawRoundedRect(tab_rect, 1.5, 1.5)
            painter.drawRoundedRect(folder_rect, 2, 2)
        painter.setBrush(muted)
        painter.drawRoundedRect(QRectF(rail.left() + 6, rail.bottom() - 13, rail.width() - 12, 4), 2, 2)

        tab_rect = QRectF(content.left(), content.top(), content.width() * 0.55, 12)
        painter.setBrush(tab)
        painter.drawRoundedRect(tab_rect, 4, 4)
        painter.setBrush(switch)
        painter.drawEllipse(QRectF(tab_rect.left() + 5, tab_rect.top() + 3, 6, 6))

        msg_rect = QRectF(content.left(), tab_rect.bottom() + 5, content.width(), 32)
        painter.setPen(QPen(border, 1))
        painter.setBrush(message)
        painter.drawRoundedRect(msg_rect, 5, 5)

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(text)
        painter.drawRoundedRect(QRectF(msg_rect.left() + 7, msg_rect.top() + 8, msg_rect.width() * 0.45, 4), 2, 2)
        painter.setBrush(muted)
        painter.drawRoundedRect(QRectF(msg_rect.left() + 7, msg_rect.top() + 18, msg_rect.width() * 0.68, 4), 2, 2)

        input_rect = QRectF(content.left(), content.bottom() - 16, content.width(), 14)
        painter.setPen(QPen(border, 1))
        painter.setBrush(input_bg)
        painter.drawRoundedRect(input_rect, 4, 4)

        if self.isChecked():
            check_rect = QRectF(outer.right() - 25, outer.bottom() - 25, 17, 17)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(border)
            painter.drawEllipse(check_rect)
            painter.setPen(QPen(QColor("#ffffff"), 1.5))
            painter.drawLine(
                QPointF(check_rect.left() + 5, check_rect.center().y()),
                QPointF(check_rect.left() + 8, check_rect.bottom() - 5)
            )
            painter.drawLine(
                QPointF(check_rect.left() + 8, check_rect.bottom() - 5),
                QPointF(check_rect.right() - 4, check_rect.top() + 5)
            )


class ThemeColorPickerDialog(QDialog):
    """
    Modal dialog for customizing application theme colors.

    Allows choosing the base color mode (System / Dark / Light) and
    overriding specific color roles per section.  Changes are applied
    live via StyleManager and can be reverted on Cancel.
    """

    theme_settings_changed = Signal(ColorTheme, dict)

    def __init__(self, initial_mode: ColorTheme | None = None, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        # Snapshot on open so Cancel can revert
        self._snapshot_mode = self._style_manager.user_color_theme()
        self._snapshot_colors = self._style_manager.get_custom_colors()
        self._snapshot_preset = self._style_manager.active_preset()

        # Apply the initial mode (e.g. passed from Settings dialog combo)
        if initial_mode is not None and initial_mode != self._snapshot_mode:
            self._style_manager.set_color_theme(initial_mode)

        self._section_pages: List[_SectionPage] = []
        self._committed = False  # True only after Apply / OK — revert on close otherwise
        # Read persisted preset name so the right chip is highlighted on reopen
        self._active_preset_name: str | None = self._style_manager.active_preset()
        self._preset_button_map: Dict[str, QPushButton] = {}

        self.setWindowTitle("Customize Colors")
        self.setMinimumWidth(980)
        self.setMinimumHeight(620)
        self.setModal(True)

        self._build_ui()
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self) -> None:
        zoom = self._style_manager.zoom_factor()
        main_layout = QVBoxLayout()
        main_layout.setSpacing(0)
        main_layout.setContentsMargins(0, 0, 0, 0)

        body_layout = QHBoxLayout()
        body_layout.setContentsMargins(0, 0, 0, 0)
        body_layout.setSpacing(0)

        left_widget = QWidget()
        left_widget.setObjectName("ThemeCustomizerMain")
        left_layout = QVBoxLayout()
        left_layout.setSpacing(0)
        left_layout.setContentsMargins(0, 0, 0, 0)
        left_widget.setLayout(left_layout)

        right_panel = QWidget()
        right_panel.setObjectName("ThemeSettingsPanel")
        right_panel.setFixedWidth(int(280 * zoom))
        right_layout = QVBoxLayout()
        right_layout.setContentsMargins(int(18 * zoom), int(18 * zoom), int(18 * zoom), int(18 * zoom))
        right_layout.setSpacing(int(12 * zoom))
        right_panel.setLayout(right_layout)

        panel_title = QLabel("Theme Customizer")
        panel_title.setObjectName("ThemePanelTitle")
        right_layout.addWidget(panel_title)

        self._active_theme_label = QLabel(self._active_preset_name or "Custom")
        self._active_theme_label.setObjectName("ActiveThemeLabel")
        right_layout.addWidget(self._active_theme_label)

        mode_title = QLabel("Mode")
        mode_title.setObjectName("ThemePanelSectionLabel")
        right_layout.addWidget(mode_title)

        mode_row = QVBoxLayout()
        mode_row.setContentsMargins(0, 0, 0, 0)
        mode_row.setSpacing(int(8 * zoom))

        self._mode_buttons: Dict[ColorTheme, QPushButton] = {}
        for mode, label in [
            (ColorTheme.LIGHT, "Light"),
            (ColorTheme.DARK, "Dark"),
            (ColorTheme.SYSTEM, "System"),
            (ColorTheme.COLOR_BLIND, "Color Blind"),
            (ColorTheme.CUSTOM, "Custom"),
        ]:
            btn = QPushButton(label)
            btn.setCheckable(True)
            btn.setProperty("colorMode", mode.name)
            btn.clicked.connect(lambda checked, m=mode: self._on_mode_selected(m))
            self._mode_buttons[mode] = btn
            mode_row.addWidget(btn)

        self._update_mode_buttons(self._style_manager.user_color_theme())

        mode_widget = QWidget()
        mode_widget.setObjectName("ModeBar")
        mode_widget.setLayout(mode_row)
        right_layout.addWidget(mode_widget)

        # Separator
        sep_top = QFrame()
        sep_top.setFrameShape(QFrame.Shape.HLine)
        sep_top.setObjectName("ColorPickerSep")
        right_layout.addWidget(sep_top)

        # Presets bar — always visible; clicking a preset switches to Custom + applies colors
        presets_scroll = QScrollArea()
        presets_scroll.setObjectName("PresetsBar")
        presets_scroll.setFixedHeight(int(132 * zoom))
        presets_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        presets_scroll.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        presets_scroll.setFrameShape(QFrame.Shape.NoFrame)
        presets_scroll.setWidgetResizable(True)

        presets_inner = QWidget()
        presets_layout = QHBoxLayout()
        presets_layout.setContentsMargins(12, 10, 12, 10)
        presets_layout.setSpacing(int(10 * zoom))

        btn_w = int(150 * zoom)
        btn_h = int(108 * zoom)
        for name, grad_start, grad_end, colors in _PRESETS:
            btn = _PresetPreviewButton(name, grad_start, grad_end, colors, self._style_manager)
            btn.setFixedSize(btn_w, btn_h)
            btn.setProperty("presetName", name)
            btn.setProperty("presetGradStart", grad_start)
            btn.setProperty("presetGradEnd", grad_end)
            btn.setChecked(name == self._active_preset_name)
            btn.clicked.connect(lambda _, n=name, c=colors: self._on_preset_clicked(n, c))
            presets_layout.addWidget(btn)
            self._preset_button_map[name] = btn

        presets_layout.addStretch()
        presets_inner.setLayout(presets_layout)
        presets_scroll.setWidget(presets_inner)
        left_layout.addWidget(presets_scroll)

        sep_presets = QFrame()
        sep_presets.setFrameShape(QFrame.Shape.HLine)
        sep_presets.setObjectName("ColorPickerSep")
        left_layout.addWidget(sep_presets)

        # Splitter: nav list + stacked section pages
        self._splitter = QSplitter(Qt.Orientation.Horizontal)
        self._splitter.setHandleWidth(1)
        self._splitter.setChildrenCollapsible(False)

        self._nav_list = QListWidget()
        self._nav_list.setObjectName("ColorPickerNav")
        self._nav_list.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self._nav_list.setFixedWidth(int(160 * zoom))
        self._nav_list.currentItemChanged.connect(self._on_nav_changed)
        self._nav_list.setItemDelegate(_NavItemDelegate(self._style_manager, self._nav_list))

        self._stack = QStackedWidget()

        for section_id, section_label, rows in _SECTIONS:
            item = QListWidgetItem("  " + section_label)
            item.setData(Qt.ItemDataRole.UserRole, section_id)
            self._nav_list.addItem(item)

            page = _SectionPage(rows, self._style_manager, self._on_swatch_color_changed)
            scroll = QScrollArea()
            scroll.setWidgetResizable(True)
            scroll.setFrameShape(QFrame.Shape.NoFrame)

            inner = QWidget()
            inner_layout = QVBoxLayout()
            spacing = int(self._style_manager.message_bubble_spacing())
            inner_layout.setContentsMargins(spacing, spacing, spacing, spacing)
            inner_layout.setSpacing(int(8 * zoom))

            heading = QLabel(section_label)
            heading.setObjectName("SectionHeading")
            inner_layout.addWidget(heading)
            inner_layout.addWidget(page)

            if section_id == "accessibility":
                inner_layout.addSpacing(int(16 * zoom))
                a11y_sub = QLabel("Colorblind-Friendly Presets")
                a11y_sub.setObjectName("SectionSubHeading")
                inner_layout.addWidget(a11y_sub)

                a11y_note = QLabel(
                    "These presets adjust status and alert colors to be distinguishable "
                    "for different types of color vision. They merge with your current "
                    "custom colors — other roles are unchanged."
                )
                a11y_note.setObjectName("A11yNote")
                a11y_note.setWordWrap(True)
                inner_layout.addWidget(a11y_note)

                a11y_row = QHBoxLayout()
                a11y_row.setSpacing(int(8 * zoom))
                for a11y_name, a11y_desc, a11y_colors in _ACCESSIBILITY_PRESETS:
                    a11y_btn = QPushButton(a11y_name)
                    a11y_btn.setToolTip(a11y_desc)
                    a11y_btn.setProperty("a11yPreset", True)
                    a11y_btn.clicked.connect(
                        lambda _, c=a11y_colors: self._on_accessibility_preset_clicked(c)
                    )
                    a11y_row.addWidget(a11y_btn)
                a11y_row.addStretch()
                inner_layout.addLayout(a11y_row)
                inner_layout.addSpacing(int(8 * zoom))

            reset_btn = QPushButton("Reset section to defaults")
            reset_btn.setProperty("section_idx", len(self._section_pages))
            reset_btn.clicked.connect(
                lambda checked, p=page: self._on_reset_section(p)
            )
            inner_layout.addWidget(reset_btn, alignment=Qt.AlignmentFlag.AlignLeft)
            inner.setLayout(inner_layout)
            scroll.setWidget(inner)

            self._stack.addWidget(scroll)
            self._section_pages.append(page)

        self._splitter.addWidget(self._nav_list)
        self._splitter.addWidget(self._stack)
        self._splitter.setStretchFactor(0, 0)
        self._splitter.setStretchFactor(1, 1)
        self._splitter.setSizes([int(160 * zoom), 520])
        left_layout.addWidget(self._splitter, 1)

        # Placeholder shown when mode is not CUSTOM
        self._placeholder = QLabel("Select \"Custom\" mode to customize individual colors.")
        self._placeholder.setObjectName("ColorPickerPlaceholder")
        self._placeholder.setAlignment(Qt.AlignmentFlag.AlignCenter)
        left_layout.addWidget(self._placeholder, 1)

        # Select first section
        if self._nav_list.count() > 0:
            self._nav_list.setCurrentRow(0)

        # Show sections only in Custom mode
        self._update_sections_visibility(self._style_manager.user_color_theme())

        # Separator above footer buttons
        sep_bot = QFrame()
        sep_bot.setFrameShape(QFrame.Shape.HLine)
        sep_bot.setObjectName("ColorPickerSep")
        right_layout.addWidget(sep_bot)

        # Footer buttons
        min_w = int(90 * zoom)
        min_h = 36

        self._reset_all_btn = QPushButton("Reset all to defaults")
        self._reset_all_btn.setMinimumHeight(min_h)
        self._reset_all_btn.setProperty("destructive", True)
        self._reset_all_btn.clicked.connect(self._on_reset_all)
        right_layout.addWidget(self._reset_all_btn)
        right_layout.addStretch()

        self._cancel_btn = QPushButton("Cancel")
        self._cancel_btn.setMinimumWidth(min_w)
        self._cancel_btn.setMinimumHeight(min_h)
        self._cancel_btn.clicked.connect(self._on_cancel)

        self._preview_btn = QPushButton("Preview")
        self._preview_btn.setMinimumWidth(min_w)
        self._preview_btn.setMinimumHeight(min_h)
        self._preview_btn.setToolTip(
            "See changes in the app. Close or Cancel to revert without saving."
        )
        self._preview_btn.clicked.connect(self._on_preview)

        self._apply_btn = QPushButton("Apply")
        self._apply_btn.setMinimumWidth(min_w)
        self._apply_btn.setMinimumHeight(min_h)
        self._apply_btn.clicked.connect(self._on_apply)

        self._ok_btn = QPushButton("OK")
        self._ok_btn.setProperty("recommended", True)
        self._ok_btn.setMinimumWidth(min_w)
        self._ok_btn.setMinimumHeight(min_h)
        self._ok_btn.clicked.connect(self._on_ok)

        for btn in [self._preview_btn, self._apply_btn, self._ok_btn, self._cancel_btn]:
            right_layout.addWidget(btn)

        body_layout.addWidget(left_widget, 1)
        body_layout.addWidget(right_panel)
        main_layout.addLayout(body_layout, 1)
        self.setLayout(main_layout)

    # ------------------------------------------------------------------
    # Event handlers
    # ------------------------------------------------------------------

    def _on_mode_selected(self, mode: ColorTheme) -> None:
        self._style_manager.set_color_theme(mode)
        self._update_mode_buttons(mode)
        self._update_sections_visibility(mode)
        self._refresh_all_swatches()

    def _on_nav_changed(self, current: QListWidgetItem | None, _prev: QListWidgetItem | None) -> None:
        if current is None:
            return
        idx = self._nav_list.row(current)
        self._stack.setCurrentIndex(idx)

    def _on_swatch_color_changed(self, role: ColorRole, hex_color: str) -> None:
        self._style_manager.set_custom_color(role, hex_color)
        # Deselect preset since colors no longer match exactly
        self._update_preset_highlight(None)

    def _on_reset_section(self, page: _SectionPage) -> None:
        self._style_manager.clear_section_custom_colors(page.roles())
        page.refresh_swatches()

    def _on_accessibility_preset_clicked(self, colors: Dict[str, Dict[str, str]]) -> None:
        """Merge accessibility color overrides into the current custom palette."""
        current = self._style_manager.get_custom_colors()
        current.update(colors)
        if self._style_manager.user_color_theme() != ColorTheme.CUSTOM:
            self._style_manager.set_color_theme(ColorTheme.CUSTOM)
            self._update_mode_buttons(ColorTheme.CUSTOM)
            self._update_sections_visibility(ColorTheme.CUSTOM)
        self._style_manager.apply_custom_colors(current)
        self._refresh_all_swatches()

    def _on_preset_clicked(self, name: str, colors: Dict[str, Dict[str, str]]) -> None:
        """Apply a preset palette. 'Default' clears overrides; others switch to Custom mode."""
        if name != "Default" and self._style_manager.user_color_theme() != ColorTheme.CUSTOM:
            self._style_manager.set_color_theme(ColorTheme.CUSTOM)
            self._update_mode_buttons(ColorTheme.CUSTOM)
            self._update_sections_visibility(ColorTheme.CUSTOM)
        self._style_manager.apply_custom_colors(colors)
        self._update_preset_highlight(name)
        self._refresh_all_swatches()

    def _on_reset_all(self) -> None:
        self._style_manager.apply_custom_colors({})
        self._update_preset_highlight("Default")
        self._refresh_all_swatches()

    def _on_preview(self) -> None:
        """Make the dialog transparent so the user can see the live changes in the app."""
        self.setWindowOpacity(0.12)
        self._preview_btn.setText("Previewing…")
        self._preview_btn.setEnabled(False)
        QTimer.singleShot(3000, self._restore_from_preview)

    def _restore_from_preview(self) -> None:
        self.setWindowOpacity(1.0)
        self._preview_btn.setText("Preview")
        self._preview_btn.setEnabled(True)

    def _on_apply(self) -> None:
        self._committed = True
        mode = self._style_manager.user_color_theme()
        colors = self._style_manager.get_custom_colors()
        self.theme_settings_changed.emit(mode, colors)

    def _on_ok(self) -> None:
        self._on_apply()
        self.accept()

    def _on_cancel(self) -> None:
        self.reject()

    def reject(self) -> None:
        """Revert all changes unless Apply/OK was already clicked."""
        self._restore_from_preview()  # ensure opacity is restored if previewing
        if not self._committed:
            self._style_manager.set_color_theme(self._snapshot_mode)
            self._style_manager.apply_custom_colors(self._snapshot_colors)
            self._style_manager.set_active_preset(self._snapshot_preset)
        super().reject()

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _update_preset_highlight(self, name: str | None) -> None:
        """Highlight the named preset button; deselect all others. Persists to StyleManager."""
        self._active_preset_name = name
        self._style_manager.set_active_preset(name)
        self._active_theme_label.setText(name or "Custom")
        for btn_name, btn in self._preset_button_map.items():
            btn.setChecked(btn_name == name)
            btn.update()

    def _update_mode_buttons(self, active_mode: ColorTheme) -> None:
        for mode, btn in self._mode_buttons.items():
            btn.setChecked(mode == active_mode)

    def _update_sections_visibility(self, mode: ColorTheme) -> None:
        is_custom = mode == ColorTheme.CUSTOM
        self._splitter.setVisible(is_custom)
        self._placeholder.setVisible(not is_custom)

    def _refresh_all_swatches(self) -> None:
        for page in self._section_pages:
            page.refresh_swatches()

    def _on_style_changed(self) -> None:
        zoom = self._style_manager.zoom_factor()
        base_fs = self._style_manager.base_font_size()
        font_pt = base_fs * zoom

        bg_dialog = self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)
        bg_secondary = self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        text_primary = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        text_disabled = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        nav_selected = self._style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)
        nav_hover = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        splitter_col = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)
        btn_bg = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)
        btn_hover = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        btn_pressed = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)
        btn_rec = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)
        btn_rec_hover = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER)
        btn_rec_pressed = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED)
        text_rec = self._style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED)
        btn_dest = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE)
        btn_dest_hover = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER)
        btn_dest_pressed = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED)

        self.setStyleSheet(self._style_manager.get_dialog_stylesheet() + f"""
            #ThemeCustomizerMain {{
                background-color: {bg_dialog};
            }}
            #ThemeSettingsPanel {{
                background-color: {bg_secondary};
                border-left: 1px solid {splitter_col};
            }}
            QLabel#ThemePanelTitle {{
                color: {text_primary};
                font-size: {font_pt * 1.1}pt;
                font-weight: bold;
                text-transform: uppercase;
            }}
            QLabel#ActiveThemeLabel {{
                color: {text_primary};
                background-color: {bg_dialog};
                border: 1px solid {splitter_col};
                border-radius: 6px;
                padding: 8px 10px;
                font-size: {font_pt}pt;
                font-weight: bold;
            }}
            QLabel#ThemePanelSectionLabel {{
                color: {text_disabled};
                font-size: {font_pt * 0.88}pt;
                font-weight: bold;
            }}
            #ModeBar {{
                background-color: transparent;
            }}
            QPushButton[colorMode] {{
                background-color: {btn_bg};
                color: {text_primary};
                border: 1px solid {splitter_col};
                border-radius: 5px;
                padding: 6px 8px;
                min-height: {round(28 * zoom)}px;
            }}
            QPushButton[colorMode]:hover {{
                background-color: {btn_hover};
            }}
            #PresetsBar, #PresetsBar > QWidget > QWidget {{
                background-color: {bg_dialog};
            }}
            QListWidget#ColorPickerNav {{
                background-color: {bg_secondary};
                border: none;
                border-radius: 0px;
                padding: 8px 0px;
                font-size: {font_pt}pt;
                outline: none;
            }}
            QListWidget#ColorPickerNav::item {{
                color: {text_primary};
                padding: 6px 12px;
                border: none;
            }}
            QListWidget#ColorPickerNav::item:selected {{
                background-color: {nav_selected};
                color: {text_primary};
            }}
            QListWidget#ColorPickerNav::item:hover:!selected {{
                background-color: {nav_hover};
            }}
            QListWidget#ColorPickerNav::item:disabled {{
                color: {text_disabled};
            }}
            QSplitter::handle {{
                background-color: {splitter_col};
                width: 1px;
            }}
            QLabel#SectionHeading {{
                font-size: {font_pt * 1.1}pt;
                font-weight: bold;
            }}
            QPushButton[recommended="true"] {{
                background-color: {btn_rec};
                color: {text_rec};
                border: none;
                border-radius: 4px;
                padding: 6px;
            }}
            QPushButton[recommended="true"]:hover {{
                background-color: {btn_rec_hover};
            }}
            QPushButton[recommended="true"]:pressed {{
                background-color: {btn_rec_pressed};
            }}
            QPushButton[colorMode]:checked {{
                background-color: {nav_selected};
                color: {text_primary};
                border-color: {btn_rec};
            }}
            QPushButton[destructive="true"] {{
                background-color: {btn_dest};
                color: {text_rec};
                border: none;
                border-radius: 4px;
                padding: 6px;
            }}
            QPushButton[destructive="true"]:hover {{
                background-color: {btn_dest_hover};
            }}
            QPushButton[destructive="true"]:pressed {{
                background-color: {btn_dest_pressed};
            }}
            QLabel#ColorPickerPlaceholder {{
                color: {text_disabled};
                font-size: {font_pt}pt;
            }}
            QLabel#SectionSubHeading {{
                font-size: {font_pt}pt;
                font-weight: bold;
                color: {text_primary};
            }}
            QLabel#A11yNote {{
                color: {text_disabled};
                font-size: {font_pt * 0.9}pt;
            }}
            QPushButton[a11yPreset="true"] {{
                background-color: {btn_bg};
                color: {text_primary};
                border: 1px solid rgba(128,128,128,0.3);
                border-radius: 4px;
                padding: 6px 12px;
            }}
            QPushButton[a11yPreset="true"]:hover {{
                background-color: {btn_hover};
                border-color: rgba(128,128,128,0.6);
            }}
            QPushButton[a11yPreset="true"]:pressed {{
                background-color: {btn_pressed};
            }}
        """)

        # Refresh swatches in case colors changed
        current_mode = self._style_manager.user_color_theme()
        self._refresh_all_swatches()
        self._update_mode_buttons(current_mode)
        self._update_sections_visibility(current_mode)

        nav_section_font = QFont()
        nav_section_font.setPointSizeF(base_fs * zoom)
        for i in range(self._nav_list.count()):
            item = self._nav_list.item(i)
            if item:
                item.setFont(nav_section_font)
