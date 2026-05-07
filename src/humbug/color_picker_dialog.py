"""Color picker dialog for customizing application theme colors."""

from typing import Callable, List, Tuple, Dict

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QLabel,
    QWidget, QFrame, QListWidget, QListWidgetItem, QStackedWidget,
    QSplitter, QScrollArea, QColorDialog, QSizePolicy, QStyledItemDelegate,
    QStyleOptionViewItem
)
from PySide6.QtCore import QModelIndex, QPersistentModelIndex, QSize, Signal, Qt
from PySide6.QtGui import QColor, QFont

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.style_manager import StyleManager, ColorMode


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

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()

        # Snapshot on open so Cancel can revert
        self._snapshot_mode = self._style_manager.user_color_mode()
        self._snapshot_colors = self._style_manager.get_custom_colors()

        self._section_pages: List[_SectionPage] = []

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

        # Select first section
        if self._nav_list.count() > 0:
            self._nav_list.setCurrentRow(0)

        # Separator above footer buttons
        sep_bot = QFrame()
        sep_bot.setFrameShape(QFrame.Shape.HLine)
        sep_bot.setObjectName("ColorPickerSep")
        main_layout.addWidget(sep_bot)

        # Footer buttons
        footer = QHBoxLayout()
        footer.setContentsMargins(16, 10, 16, 10)
        footer.setSpacing(int(8 * zoom))
        footer.addStretch()

        min_w = int(90 * zoom)
        min_h = 36

        self._cancel_btn = QPushButton("Cancel")
        self._cancel_btn.setMinimumWidth(min_w)
        self._cancel_btn.setMinimumHeight(min_h)
        self._cancel_btn.clicked.connect(self._on_cancel)

        self._apply_btn = QPushButton("Apply")
        self._apply_btn.setMinimumWidth(min_w)
        self._apply_btn.setMinimumHeight(min_h)
        self._apply_btn.clicked.connect(self._on_apply)

        self._ok_btn = QPushButton("OK")
        self._ok_btn.setProperty("recommended", True)
        self._ok_btn.setMinimumWidth(min_w)
        self._ok_btn.setMinimumHeight(min_h)
        self._ok_btn.clicked.connect(self._on_ok)

        for btn in [self._cancel_btn, self._apply_btn, self._ok_btn]:
            footer.addWidget(btn)

        main_layout.addLayout(footer)
        self.setLayout(main_layout)

    # ------------------------------------------------------------------
    # Event handlers
    # ------------------------------------------------------------------

    def _on_mode_selected(self, mode: ColorMode) -> None:
        self._style_manager.set_color_mode(mode)
        self._update_mode_buttons(mode)
        self._refresh_all_swatches()

    def _on_nav_changed(self, current: QListWidgetItem | None, _prev: QListWidgetItem | None) -> None:
        if current is None:
            return
        idx = self._nav_list.row(current)
        self._stack.setCurrentIndex(idx)

    def _on_swatch_color_changed(self, role: ColorRole, hex_color: str) -> None:
        mode = self._style_manager.color_mode()
        self._style_manager.set_custom_color(role, mode, hex_color)

    def _on_reset_section(self, page: _SectionPage) -> None:
        self._style_manager.clear_section_custom_colors(page.roles())
        page.refresh_swatches()

    def _on_apply(self) -> None:
        mode = self._style_manager.user_color_mode()
        colors = self._style_manager.get_custom_colors()
        self.theme_settings_changed.emit(mode, colors)

    def _on_ok(self) -> None:
        self._on_apply()
        self.accept()

    def _on_cancel(self) -> None:
        self._style_manager.set_color_mode(self._snapshot_mode)
        self._style_manager.apply_custom_colors(self._snapshot_colors)
        self.reject()

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _update_mode_buttons(self, active_mode: ColorMode) -> None:
        for mode, btn in self._mode_buttons.items():
            btn.setChecked(mode == active_mode)

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

        self.setStyleSheet(self._style_manager.get_dialog_stylesheet() + f"""
            #ModeBar {{
                background-color: {bg_secondary};
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
        """)

        # Refresh swatches in case colors changed
        self._refresh_all_swatches()
        self._update_mode_buttons(self._style_manager.user_color_mode())

        nav_section_font = QFont()
        nav_section_font.setPointSizeF(base_fs * zoom)
        for i in range(self._nav_list.count()):
            item = self._nav_list.item(i)
            if item:
                item.setFont(nav_section_font)
