"""Color picker dialog for customizing application theme colors."""

from typing import Callable, List, Tuple, Dict

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QLabel,
    QWidget, QFrame, QListWidget, QListWidgetItem, QStackedWidget,
    QSplitter, QScrollArea, QColorDialog, QSizePolicy, QStyledItemDelegate,
    QStyleOptionViewItem
)
from PySide6.QtCore import QModelIndex, QPersistentModelIndex, QSize, Signal, Qt, QTimer
from PySide6.QtGui import QColor, QFont

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager, ColorMode


# Preset themes: (name, gradient_start, gradient_end, {role_name: {mode_name: hex}})
# gradient_start / gradient_end are purely visual hints for the preset button.
# An empty colors dict means "clear all overrides" (the None/default preset).
_PRESETS: List[Tuple[str, str, str, Dict[str, Dict[str, str]]]] = [
    (
        "None",
        "#1c1c1c", "#404040",
        {},   # empty = clear all custom colors, no mode switch
    ),
    (
        "Ocean Breeze",
        "#041428", "#00bcd4",
        {
            "BACKGROUND_PRIMARY":              {"DARK": "#041428"},
            "BACKGROUND_SECONDARY":            {"DARK": "#081e3c"},
            "BACKGROUND_DIALOG":               {"DARK": "#0a2444"},
            "TAB_BAR_BACKGROUND":              {"DARK": "#030e1c"},
            "TAB_BACKGROUND_ACTIVE":           {"DARK": "#0e3060"},
            "TAB_BACKGROUND_INACTIVE":         {"DARK": "#081e3c"},
            "TAB_BORDER_ACTIVE":               {"DARK": "#00bcd4"},
            "MINDSPACE_BACKGROUND":            {"DARK": "#030c18"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND":  {"DARK": "#020a14"},
            "MINDSPACE_HEADING":               {"DARK": "#00bcd4"},
            "EDIT_BOX_BACKGROUND":             {"DARK": "#081e3c"},
            "EDIT_BOX_BORDER":                 {"DARK": "#0e3060"},
            "BUTTON_BACKGROUND":               {"DARK": "#0e2a4a"},
            "BUTTON_BACKGROUND_RECOMMENDED":   {"DARK": "#006080"},
            "BUTTON_BACKGROUND_DESTRUCTIVE":   {"DARK": "#8b1c1c"},
            "TEXT_PRIMARY":                    {"DARK": "#b8e4f4"},
            "TEXT_BRIGHT":                     {"DARK": "#e4f8ff"},
            "TEXT_DISABLED":                   {"DARK": "#3a6080"},
        },
    ),
    (
        "Sunset Glow",
        "#1a0828", "#ff6d3a",
        {
            "BACKGROUND_PRIMARY":              {"DARK": "#1a0828"},
            "BACKGROUND_SECONDARY":            {"DARK": "#240c38"},
            "BACKGROUND_DIALOG":               {"DARK": "#2c1040"},
            "TAB_BAR_BACKGROUND":              {"DARK": "#12061c"},
            "TAB_BACKGROUND_ACTIVE":           {"DARK": "#5c1a38"},
            "TAB_BACKGROUND_INACTIVE":         {"DARK": "#240c38"},
            "TAB_BORDER_ACTIVE":               {"DARK": "#ff6d3a"},
            "MINDSPACE_BACKGROUND":            {"DARK": "#0e0418"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND":  {"DARK": "#0a0312"},
            "MINDSPACE_HEADING":               {"DARK": "#ff8c5a"},
            "EDIT_BOX_BACKGROUND":             {"DARK": "#240c38"},
            "EDIT_BOX_BORDER":                 {"DARK": "#5c1a38"},
            "BUTTON_BACKGROUND":               {"DARK": "#3c1048"},
            "BUTTON_BACKGROUND_RECOMMENDED":   {"DARK": "#7c2820"},
            "BUTTON_BACKGROUND_DESTRUCTIVE":   {"DARK": "#9e1c1c"},
            "TEXT_PRIMARY":                    {"DARK": "#ffd4b8"},
            "TEXT_BRIGHT":                     {"DARK": "#fff0e4"},
            "TEXT_DISABLED":                   {"DARK": "#7a4840"},
        },
    ),
    (
        "Emerald Forest",
        "#041408", "#4caf50",
        {
            "BACKGROUND_PRIMARY":              {"DARK": "#041408"},
            "BACKGROUND_SECONDARY":            {"DARK": "#081e10"},
            "BACKGROUND_DIALOG":               {"DARK": "#0a2414"},
            "TAB_BAR_BACKGROUND":              {"DARK": "#030e06"},
            "TAB_BACKGROUND_ACTIVE":           {"DARK": "#0e4020"},
            "TAB_BACKGROUND_INACTIVE":         {"DARK": "#081e10"},
            "TAB_BORDER_ACTIVE":               {"DARK": "#4caf50"},
            "MINDSPACE_BACKGROUND":            {"DARK": "#020c05"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND":  {"DARK": "#020a04"},
            "MINDSPACE_HEADING":               {"DARK": "#4caf50"},
            "EDIT_BOX_BACKGROUND":             {"DARK": "#081e10"},
            "EDIT_BOX_BORDER":                 {"DARK": "#0e4020"},
            "BUTTON_BACKGROUND":               {"DARK": "#0e2e14"},
            "BUTTON_BACKGROUND_RECOMMENDED":   {"DARK": "#1a6030"},
            "BUTTON_BACKGROUND_DESTRUCTIVE":   {"DARK": "#8b1c1c"},
            "TEXT_PRIMARY":                    {"DARK": "#b8f4c4"},
            "TEXT_BRIGHT":                     {"DARK": "#e4ffe8"},
            "TEXT_DISABLED":                   {"DARK": "#3a6044"},
        },
    ),
    (
        "Midnight Violet",
        "#08061a", "#7c4dff",
        {
            "BACKGROUND_PRIMARY":              {"DARK": "#08061a"},
            "BACKGROUND_SECONDARY":            {"DARK": "#0e0a28"},
            "BACKGROUND_DIALOG":               {"DARK": "#120e30"},
            "TAB_BAR_BACKGROUND":              {"DARK": "#060414"},
            "TAB_BACKGROUND_ACTIVE":           {"DARK": "#241848"},
            "TAB_BACKGROUND_INACTIVE":         {"DARK": "#0e0a28"},
            "TAB_BORDER_ACTIVE":               {"DARK": "#7c4dff"},
            "MINDSPACE_BACKGROUND":            {"DARK": "#060412"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND":  {"DARK": "#04030e"},
            "MINDSPACE_HEADING":               {"DARK": "#9c6dff"},
            "EDIT_BOX_BACKGROUND":             {"DARK": "#0e0a28"},
            "EDIT_BOX_BORDER":                 {"DARK": "#241848"},
            "BUTTON_BACKGROUND":               {"DARK": "#180e40"},
            "BUTTON_BACKGROUND_RECOMMENDED":   {"DARK": "#3828a0"},
            "BUTTON_BACKGROUND_DESTRUCTIVE":   {"DARK": "#8b1c1c"},
            "TEXT_PRIMARY":                    {"DARK": "#d8c8ff"},
            "TEXT_BRIGHT":                     {"DARK": "#f0eaff"},
            "TEXT_DISABLED":                   {"DARK": "#504880"},
        },
    ),
    (
        "Cherry Blossom",
        "#18060e", "#f06292",
        {
            "BACKGROUND_PRIMARY":              {"DARK": "#18060e"},
            "BACKGROUND_SECONDARY":            {"DARK": "#240c16"},
            "BACKGROUND_DIALOG":               {"DARK": "#2c101c"},
            "TAB_BAR_BACKGROUND":              {"DARK": "#10040a"},
            "TAB_BACKGROUND_ACTIVE":           {"DARK": "#5c1830"},
            "TAB_BACKGROUND_INACTIVE":         {"DARK": "#240c16"},
            "TAB_BORDER_ACTIVE":               {"DARK": "#f06292"},
            "MINDSPACE_BACKGROUND":            {"DARK": "#0c0408"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND":  {"DARK": "#0a0306"},
            "MINDSPACE_HEADING":               {"DARK": "#f48fb1"},
            "EDIT_BOX_BACKGROUND":             {"DARK": "#240c16"},
            "EDIT_BOX_BORDER":                 {"DARK": "#5c1830"},
            "BUTTON_BACKGROUND":               {"DARK": "#3c1024"},
            "BUTTON_BACKGROUND_RECOMMENDED":   {"DARK": "#7c2048"},
            "BUTTON_BACKGROUND_DESTRUCTIVE":   {"DARK": "#9e1c1c"},
            "TEXT_PRIMARY":                    {"DARK": "#ffd8e8"},
            "TEXT_BRIGHT":                     {"DARK": "#fff0f5"},
            "TEXT_DISABLED":                   {"DARK": "#7a4060"},
        },
    ),
    (
        "Arctic Dawn",
        "#060a12", "#64b5f6",
        {
            "BACKGROUND_PRIMARY":              {"DARK": "#060a12"},
            "BACKGROUND_SECONDARY":            {"DARK": "#0c1420"},
            "BACKGROUND_DIALOG":               {"DARK": "#101a28"},
            "TAB_BAR_BACKGROUND":              {"DARK": "#04080e"},
            "TAB_BACKGROUND_ACTIVE":           {"DARK": "#183048"},
            "TAB_BACKGROUND_INACTIVE":         {"DARK": "#0c1420"},
            "TAB_BORDER_ACTIVE":               {"DARK": "#64b5f6"},
            "MINDSPACE_BACKGROUND":            {"DARK": "#04060c"},
            "MINDSPACE_TOOL_RAIL_BACKGROUND":  {"DARK": "#030509"},
            "MINDSPACE_HEADING":               {"DARK": "#82c8f8"},
            "EDIT_BOX_BACKGROUND":             {"DARK": "#0c1420"},
            "EDIT_BOX_BORDER":                 {"DARK": "#183048"},
            "BUTTON_BACKGROUND":               {"DARK": "#102030"},
            "BUTTON_BACKGROUND_RECOMMENDED":   {"DARK": "#1c4a6c"},
            "BUTTON_BACKGROUND_DESTRUCTIVE":   {"DARK": "#8b1c1c"},
            "TEXT_PRIMARY":                    {"DARK": "#c0d8f0"},
            "TEXT_BRIGHT":                     {"DARK": "#e4f2ff"},
            "TEXT_DISABLED":                   {"DARK": "#3a5068"},
        },
    ),
]

# Sections: (section_id, display_label, [(swatch_label, ColorRole), ...])
_SECTIONS: List[Tuple[str, str, List[Tuple[str, ColorRole]]]] = [
    ("background", "Background", [
        ("Primary background", ColorRole.BACKGROUND_PRIMARY),
        ("Secondary background", ColorRole.BACKGROUND_SECONDARY),
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
            "MESSAGE_ERROR":                    {"DARK": "#e87820"},
            "MESSAGE_WARNING":                  {"DARK": "#2070c0"},
            "MESSAGE_INFORMATION":              {"DARK": "#8840c0"},
            "TEXT_ERROR":                       {"DARK": "#ff9020"},
            "TEXT_LINK":                        {"DARK": "#4499ff"},
            "VCS_MODIFIED":                     {"DARK": "#0099bb"},
            "VCS_ADDED":                        {"DARK": "#4488ff"},
            "VCS_DELETED":                      {"DARK": "#e87820"},
            "VCS_RENAMED":                      {"DARK": "#9966ff"},
            "MINDSPACE_FOLDER":                 {"DARK": "#4488ff"},
            "MINDSPACE_FOLDER_BREADCRUMB":      {"DARK": "#4488ff"},
        },
    ),
    (
        "Protanopia",
        "Red-weakness (~1% of males). Uses blue/gold contrast.",
        {
            "MESSAGE_ERROR":                    {"DARK": "#c89900"},
            "MESSAGE_WARNING":                  {"DARK": "#0080cc"},
            "MESSAGE_INFORMATION":              {"DARK": "#7733bb"},
            "TEXT_ERROR":                       {"DARK": "#ddaa00"},
            "TEXT_LINK":                        {"DARK": "#3388ff"},
            "VCS_MODIFIED":                     {"DARK": "#00aacc"},
            "VCS_ADDED":                        {"DARK": "#3388ff"},
            "VCS_DELETED":                      {"DARK": "#c89900"},
            "VCS_RENAMED":                      {"DARK": "#8855ee"},
            "MINDSPACE_FOLDER":                 {"DARK": "#3388ff"},
            "MINDSPACE_FOLDER_BREADCRUMB":      {"DARK": "#3388ff"},
        },
    ),
    (
        "Tritanopia",
        "Blue-yellow colorblindness (rare). Uses red/green/magenta contrast.",
        {
            "MESSAGE_ERROR":                    {"DARK": "#dd3333"},
            "MESSAGE_WARNING":                  {"DARK": "#ee8800"},
            "MESSAGE_INFORMATION":              {"DARK": "#22aa55"},
            "TEXT_ERROR":                       {"DARK": "#ff4444"},
            "TEXT_LINK":                        {"DARK": "#cc44cc"},
            "VCS_MODIFIED":                     {"DARK": "#ee8800"},
            "VCS_ADDED":                        {"DARK": "#22cc66"},
            "VCS_DELETED":                      {"DARK": "#dd3333"},
            "VCS_RENAMED":                      {"DARK": "#cc44cc"},
            "MINDSPACE_FOLDER":                 {"DARK": "#ee8800"},
            "MINDSPACE_FOLDER_BREADCRUMB":      {"DARK": "#ee8800"},
        },
    ),
    (
        "High Contrast",
        "Maximum contrast for low-vision users.",
        {
            "BACKGROUND_PRIMARY":               {"DARK": "#000000"},
            "BACKGROUND_SECONDARY":             {"DARK": "#111111"},
            "BACKGROUND_DIALOG":                {"DARK": "#0a0a0a"},
            "TEXT_PRIMARY":                     {"DARK": "#ffffff"},
            "TEXT_BRIGHT":                      {"DARK": "#ffff00"},
            "TEXT_DISABLED":                    {"DARK": "#aaaaaa"},
            "TEXT_ERROR":                       {"DARK": "#ff4422"},
            "TEXT_LINK":                        {"DARK": "#66ccff"},
            "MESSAGE_ERROR":                    {"DARK": "#ff2200"},
            "MESSAGE_WARNING":                  {"DARK": "#ffcc00"},
            "MESSAGE_INFORMATION":              {"DARK": "#00ccff"},
            "VCS_MODIFIED":                     {"DARK": "#ffcc00"},
            "VCS_ADDED":                        {"DARK": "#00ff88"},
            "VCS_DELETED":                      {"DARK": "#ff2200"},
            "VCS_RENAMED":                      {"DARK": "#00ccff"},
            "MINDSPACE_FOLDER":                 {"DARK": "#ffcc00"},
            "MINDSPACE_FOLDER_BREADCRUMB":      {"DARK": "#ffcc00"},
            "TAB_BORDER_ACTIVE":                {"DARK": "#ffffff"},
            "EDIT_BOX_BORDER":                  {"DARK": "#ffffff"},
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

        layout.addStretch()
        self.setLayout(layout)

    def refresh_swatches(self) -> None:
        """Refresh all swatches to reflect current StyleManager colors."""
        for swatch in self._swatches.values():
            swatch.refresh()

    def roles(self) -> List[ColorRole]:
        return [role for _, role in self._rows]


class ThemeColorPickerDialog(QDialog):
    """
    Modal dialog for customizing application theme colors.

    Allows choosing the base color mode (System / Dark / Light) and
    overriding specific color roles per section.  Changes are applied
    live via StyleManager and can be reverted on Cancel.
    """

    theme_settings_changed = Signal(ColorMode, dict)

    def __init__(self, initial_mode: ColorMode | None = None, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        # Snapshot on open so Cancel can revert
        self._snapshot_mode = self._style_manager.user_color_mode()
        self._snapshot_colors = self._style_manager.get_custom_colors()
        self._snapshot_preset = self._style_manager.active_preset()

        # Apply the initial mode (e.g. passed from Settings dialog combo)
        if initial_mode is not None and initial_mode != self._snapshot_mode:
            self._style_manager.set_color_mode(initial_mode)

        self._section_pages: List[_SectionPage] = []
        self._committed = False  # True only after Apply / OK — revert on close otherwise
        # Read persisted preset name so the right chip is highlighted on reopen
        self._active_preset_name: str | None = self._style_manager.active_preset()
        self._preset_button_map: Dict[str, QPushButton] = {}

        self.setWindowTitle("Customize Colors")
        self.setMinimumWidth(680)
        self.setMinimumHeight(520)
        self.setModal(True)

        self._build_ui()
        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self) -> None:
        zoom = self._style_manager.zoom_factor()
        base_fs = self._style_manager.base_font_size()
        main_layout = QVBoxLayout()
        main_layout.setSpacing(0)
        main_layout.setContentsMargins(0, 0, 0, 0)

        # Mode selector row at top
        mode_row = QHBoxLayout()
        mode_row.setContentsMargins(16, 12, 16, 12)
        mode_row.setSpacing(int(10 * zoom))

        mode_label = QLabel("Color Mode:")
        mode_label.setObjectName("ModeLabel")

        self._mode_buttons: Dict[ColorMode, QPushButton] = {}
        for mode, label in [
            (ColorMode.SYSTEM, "System (auto)"),
            (ColorMode.LIGHT, "Light"),
            (ColorMode.DARK, "Dark"),
            (ColorMode.COLOR_BLIND, "Color Blind"),
            (ColorMode.CUSTOM, "Custom"),
        ]:
            btn = QPushButton(label)
            btn.setCheckable(True)
            btn.setProperty("colorMode", mode.name)
            btn.clicked.connect(lambda checked, m=mode: self._on_mode_selected(m))
            self._mode_buttons[mode] = btn
            mode_row.addWidget(btn)

        mode_row.addStretch()
        self._update_mode_buttons(self._style_manager.user_color_mode())

        mode_widget = QWidget()
        mode_widget.setObjectName("ModeBar")
        mode_widget.setLayout(mode_row)
        main_layout.addWidget(mode_widget)

        # Separator
        sep_top = QFrame()
        sep_top.setFrameShape(QFrame.Shape.HLine)
        sep_top.setObjectName("ColorPickerSep")
        main_layout.addWidget(sep_top)

        # Presets bar — always visible; clicking a preset switches to Custom + applies colors
        presets_scroll = QScrollArea()
        presets_scroll.setObjectName("PresetsBar")
        presets_scroll.setFixedHeight(int(72 * zoom))
        presets_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        presets_scroll.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        presets_scroll.setFrameShape(QFrame.Shape.NoFrame)
        presets_scroll.setWidgetResizable(True)

        presets_inner = QWidget()
        presets_layout = QHBoxLayout()
        presets_layout.setContentsMargins(12, 8, 12, 8)
        presets_layout.setSpacing(int(8 * zoom))

        btn_w = int(130 * zoom)
        btn_h = int(52 * zoom)
        for name, grad_start, grad_end, colors in _PRESETS:
            btn = QPushButton(name)
            btn.setFixedSize(btn_w, btn_h)
            btn.setProperty("presetName", name)
            btn.setProperty("presetGradStart", grad_start)
            btn.setProperty("presetGradEnd", grad_end)
            is_active = name == self._active_preset_name
            btn.setStyleSheet(
                self._build_preset_btn_stylesheet(grad_start, grad_end, is_active, zoom, base_fs)
            )
            btn.clicked.connect(lambda _, n=name, c=colors: self._on_preset_clicked(n, c))
            presets_layout.addWidget(btn)
            self._preset_button_map[name] = btn

        presets_layout.addStretch()
        presets_inner.setLayout(presets_layout)
        presets_scroll.setWidget(presets_inner)
        main_layout.addWidget(presets_scroll)

        sep_presets = QFrame()
        sep_presets.setFrameShape(QFrame.Shape.HLine)
        sep_presets.setObjectName("ColorPickerSep")
        main_layout.addWidget(sep_presets)

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
        main_layout.addWidget(self._splitter, 1)

        # Placeholder shown when mode is not CUSTOM
        self._placeholder = QLabel("Select \"Custom\" mode to customize individual colors.")
        self._placeholder.setObjectName("ColorPickerPlaceholder")
        self._placeholder.setAlignment(Qt.AlignmentFlag.AlignCenter)
        main_layout.addWidget(self._placeholder, 1)

        # Select first section
        if self._nav_list.count() > 0:
            self._nav_list.setCurrentRow(0)

        # Show sections only in Custom mode
        self._update_sections_visibility(self._style_manager.user_color_mode())

        # Separator above footer buttons
        sep_bot = QFrame()
        sep_bot.setFrameShape(QFrame.Shape.HLine)
        sep_bot.setObjectName("ColorPickerSep")
        main_layout.addWidget(sep_bot)

        # Footer buttons
        footer = QHBoxLayout()
        footer.setContentsMargins(16, 10, 16, 10)
        footer.setSpacing(int(8 * zoom))

        min_w = int(90 * zoom)
        min_h = 36

        self._reset_all_btn = QPushButton("Reset all to defaults")
        self._reset_all_btn.setMinimumHeight(min_h)
        self._reset_all_btn.setProperty("destructive", True)
        self._reset_all_btn.clicked.connect(self._on_reset_all)
        footer.addWidget(self._reset_all_btn)

        footer.addStretch()

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

        for btn in [self._cancel_btn, self._preview_btn, self._apply_btn, self._ok_btn]:
            footer.addWidget(btn)

        main_layout.addLayout(footer)
        self.setLayout(main_layout)

    # ------------------------------------------------------------------
    # Event handlers
    # ------------------------------------------------------------------

    def _on_mode_selected(self, mode: ColorMode) -> None:
        self._style_manager.set_color_mode(mode)
        self._update_mode_buttons(mode)
        self._update_sections_visibility(mode)
        self._refresh_all_swatches()

    def _on_nav_changed(self, current: QListWidgetItem | None, _prev: QListWidgetItem | None) -> None:
        if current is None:
            return
        idx = self._nav_list.row(current)
        self._stack.setCurrentIndex(idx)

    def _on_swatch_color_changed(self, role: ColorRole, hex_color: str) -> None:
        # Always store against the resolved (DARK or LIGHT) mode
        mode = self._style_manager.color_mode()
        self._style_manager.set_custom_color(role, mode, hex_color)
        # Deselect preset since colors no longer match exactly
        self._update_preset_highlight(None)

    def _on_reset_section(self, page: _SectionPage) -> None:
        self._style_manager.clear_section_custom_colors(page.roles())
        page.refresh_swatches()

    def _on_accessibility_preset_clicked(self, colors: Dict[str, Dict[str, str]]) -> None:
        """Merge accessibility color overrides into the current custom palette."""
        current = self._style_manager.get_custom_colors()
        current.update(colors)
        if self._style_manager.user_color_mode() != ColorMode.CUSTOM:
            self._style_manager.set_color_mode(ColorMode.CUSTOM)
            self._update_mode_buttons(ColorMode.CUSTOM)
            self._update_sections_visibility(ColorMode.CUSTOM)
        self._style_manager.apply_custom_colors(current)
        self._refresh_all_swatches()

    def _on_preset_clicked(self, name: str, colors: Dict[str, Dict[str, str]]) -> None:
        """Apply a preset palette. 'None' clears overrides; others switch to Custom mode."""
        if name != "None" and self._style_manager.user_color_mode() != ColorMode.CUSTOM:
            self._style_manager.set_color_mode(ColorMode.CUSTOM)
            self._update_mode_buttons(ColorMode.CUSTOM)
            self._update_sections_visibility(ColorMode.CUSTOM)
        self._style_manager.apply_custom_colors(colors)
        self._update_preset_highlight(name)
        self._refresh_all_swatches()

    def _on_reset_all(self) -> None:
        self._style_manager.apply_custom_colors({})
        self._update_preset_highlight("None")
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
        mode = self._style_manager.user_color_mode()
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
            self._style_manager.set_color_mode(self._snapshot_mode)
            self._style_manager.apply_custom_colors(self._snapshot_colors)
            self._style_manager.set_active_preset(self._snapshot_preset)
        super().reject()

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _build_preset_btn_stylesheet(
        self, grad_start: str, grad_end: str, is_active: bool, zoom: float, base_fs: float
    ) -> str:
        """Generate a smooth 3-stop diagonal gradient stylesheet for a preset button."""
        # Compute midpoint colour
        s = grad_start.lstrip("#")
        e = grad_end.lstrip("#")
        r = (int(s[0:2], 16) + int(e[0:2], 16)) // 2
        g = (int(s[2:4], 16) + int(e[2:4], 16)) // 2
        b = (int(s[4:6], 16) + int(e[4:6], 16)) // 2
        grad_mid = f"#{r:02x}{g:02x}{b:02x}"

        radius = int(10 * zoom)
        font_pt = int(base_fs * zoom)
        if is_active:
            border = "2.5px solid rgba(255,255,255,0.95)"
        else:
            border = "1px solid rgba(255,255,255,0.18)"

        return f"""
            QPushButton {{
                background: qlineargradient(
                    x1:0.1, y1:0, x2:0.9, y2:1,
                    stop:0.0 {grad_start},
                    stop:0.45 {grad_mid},
                    stop:1.0 {grad_end}
                );
                color: #ffffff;
                border: {border};
                border-radius: {radius}px;
                font-weight: bold;
                font-size: {font_pt}pt;
                padding: 4px 6px;
            }}
            QPushButton:hover {{
                border: 2px solid rgba(255,255,255,0.65);
            }}
            QPushButton:pressed {{
                border: 2px solid rgba(255,255,255,0.9);
            }}
        """

    def _update_preset_highlight(self, name: str | None) -> None:
        """Highlight the named preset button; deselect all others. Persists to StyleManager."""
        self._active_preset_name = name
        self._style_manager.set_active_preset(name)
        zoom = self._style_manager.zoom_factor()
        base_fs = self._style_manager.base_font_size()
        for btn_name, btn in self._preset_button_map.items():
            grad_start = btn.property("presetGradStart")
            grad_end = btn.property("presetGradEnd")
            btn.setStyleSheet(
                self._build_preset_btn_stylesheet(
                    grad_start, grad_end, btn_name == name, zoom, base_fs
                )
            )

    def _update_mode_buttons(self, active_mode: ColorMode) -> None:
        for mode, btn in self._mode_buttons.items():
            btn.setChecked(mode == active_mode)

    def _update_sections_visibility(self, mode: ColorMode) -> None:
        is_custom = mode == ColorMode.CUSTOM
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
            #ModeBar {{
                background-color: {bg_secondary};
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
        current_mode = self._style_manager.user_color_mode()
        self._refresh_all_swatches()
        self._update_mode_buttons(current_mode)
        self._update_sections_visibility(current_mode)

        nav_section_font = QFont()
        nav_section_font.setPointSizeF(base_fs * zoom)
        for i in range(self._nav_list.count()):
            item = self._nav_list.item(i)
            if item:
                item.setFont(nav_section_font)
