"""Collapsible accordion section with animated slide expand/collapse."""

from typing import List

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QLabel, QSizePolicy
from PySide6.QtGui import QIcon
from PySide6.QtCore import QEvent, QPropertyAnimation, QEasingCurve, Qt

from humbug.color_role import ColorRole
from humbug.settings.settings_item import SettingsItem
from humbug.ui_constants import ACCORDION_ANIM_DURATION_MS, QWIDGETSIZE_MAX


class SettingsAccordion(SettingsItem):
    """
    Collapsible accordion that groups settings under a clickable header.

    Expand/collapse is animated by sliding the content area's maximumHeight
    from 0 to its natural height (or back).  No setVisible() is needed —
    a maximumHeight of 0 with minimumHeight of 0 makes the widget invisible.
    """

    def __init__(self, title: str, expanded: bool = False, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self._expanded = expanded
        self._content_items: List[SettingsItem] = []
        self._animation: QPropertyAnimation | None = None

        outer = QVBoxLayout()
        outer.setSpacing(0)
        outer.setContentsMargins(0, 5, 0, 5)

        self._header_btn = QPushButton()
        self._header_btn.setObjectName("AccordionHeader")
        self._header_btn.setCursor(Qt.CursorShape.PointingHandCursor)
        self._header_btn.setFlat(True)
        self._header_btn.setSizePolicy(
            QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed
        )
        self._header_btn.clicked.connect(self._on_toggle)

        header_layout = QHBoxLayout(self._header_btn)
        header_layout.setContentsMargins(12, 10, 14, 10)
        header_layout.setSpacing(10)

        self._title_label = QLabel(title)
        self._title_label.setObjectName("AccordionTitle")

        self._chevron = QLabel()
        self._chevron.setObjectName("AccordionChevron")
        self._chevron.setAlignment(Qt.AlignmentFlag.AlignCenter)

        header_layout.addWidget(self._title_label, 1)
        header_layout.addWidget(self._chevron)

        self._content_widget = QWidget()
        self._content_widget.setObjectName("AccordionContent")

        # Clipping ensures content never overflows the animated boundary.
        self._content_widget.setProperty("AccordionContent", True)
        self._content_layout = QVBoxLayout(self._content_widget)
        self._content_layout.setSpacing(0)
        self._content_layout.setContentsMargins(14, 8, 14, 14)

        # Start collapsed: zero max-height acts as invisible without setVisible.
        self._content_widget.setMinimumHeight(0)
        self._content_widget.setMaximumHeight(0 if not expanded else QWIDGETSIZE_MAX)

        outer.addWidget(self._header_btn)
        outer.addWidget(self._content_widget)
        self.setLayout(outer)

        self._on_style_changed()

    def add_content(self, item: SettingsItem) -> None:
        """Add a settings item into the collapsible content area."""
        self._content_layout.addWidget(item)
        self._content_items.append(item)
        item.value_changed.connect(self.value_changed)

    def set_label(self, text: str) -> None:
        """Set the accordion header title text."""
        self._title_label.setText(text)

    def is_modified(self) -> bool:
        return any(item.is_modified() for item in self._content_items)

    def reset_modified_state(self) -> None:
        for item in self._content_items:
            item.reset_modified_state()

    def _on_toggle(self) -> None:
        self._expanded = not self._expanded
        self._on_style_changed()   # update header border radius + chevron

        # Stop any in-progress animation so direction changes feel responsive.
        if self._animation and self._animation.state() == QPropertyAnimation.State.Running:
            self._animation.stop()

        if self._expanded:
            # Measure the natural height before constraining it.
            self._content_widget.setMaximumHeight(QWIDGETSIZE_MAX)
            target_h = self._content_widget.sizeHint().height()
            start_h = 0
            self._content_widget.setMaximumHeight(0)   # will be driven by animation

        else:
            start_h = self._content_widget.height()
            target_h = 0

        anim = QPropertyAnimation(self._content_widget, b"maximumHeight", self)
        anim.setDuration(ACCORDION_ANIM_DURATION_MS)
        anim.setStartValue(start_h)
        anim.setEndValue(target_h)
        anim.setEasingCurve(QEasingCurve.Type.OutCubic if self._expanded else QEasingCurve.Type.InCubic)
        anim.start()
        self._animation = anim

    def _update_chevron(self) -> None:
        if self._expanded:
            icon_name = "arrow-down"

        else:
            is_rtl = self.layoutDirection() == Qt.LayoutDirection.RightToLeft
            icon_name = "arrow-left" if is_rtl else "arrow-right"

        path = self._style_manager.get_icon_path(icon_name)
        pixmap = QIcon(path).pixmap(self._chevron.size())
        self._chevron.setPixmap(pixmap)

    def changeEvent(self, event: QEvent) -> None:
        super().changeEvent(event)
        if event.type() == QEvent.Type.LayoutDirectionChange:
            self._update_chevron()

    def _on_style_changed(self) -> None:
        super()._on_style_changed()

        font_size = self._style_manager.base_font_size()
        zoom = self._style_manager.zoom_factor()
        font_pt = int(font_size * zoom)
        chevron_size = max(14, int(16 * zoom))
        header_min_height = max(42, int(44 * zoom))

        text_color = self._style_manager.get_color_str(ColorRole.TEXT_BRIGHT)
        disabled_text = self._style_manager.get_color_str(ColorRole.TEXT_DISABLED)
        bg = self._style_manager.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        content_bg = self._style_manager.get_color_str(ColorRole.BACKGROUND_DIALOG)
        bg_hover = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        bg_pressed = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_PRESSED)
        border_color = self._style_manager.get_color_str(ColorRole.MENU_BORDER)
        accent = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED)
        radius = 8

        self._chevron.setFixedSize(chevron_size, chevron_size)
        self._header_btn.setMinimumHeight(header_min_height)

        header_bottom_border = "0px" if self._expanded else "1px"
        header_bottom_radius = 0 if self._expanded else radius

        self._header_btn.setStyleSheet(f"""
            QPushButton#AccordionHeader {{
                background-color: {bg};
                border: 1px solid {border_color};
                border-bottom-width: {header_bottom_border};
                border-top-left-radius: {radius}px;
                border-top-right-radius: {radius}px;
                border-bottom-left-radius: {header_bottom_radius}px;
                border-bottom-right-radius: {header_bottom_radius}px;
                text-align: left;
                padding: 0px;
            }}
            QPushButton#AccordionHeader:hover {{
                background-color: {bg_hover};
            }}
            QPushButton#AccordionHeader:pressed {{
                background-color: {bg_pressed};
            }}
            QPushButton#AccordionHeader:disabled {{
                color: {disabled_text};
            }}
        """)

        self._title_label.setStyleSheet(f"""
            QLabel#AccordionTitle {{
                color: {text_color};
                font-size: {font_pt}pt;
                font-weight: bold;
                background: transparent;
                border: none;
                padding: 0px;
                margin: 0px;
            }}
        """)

        self._chevron.setStyleSheet("""
            QLabel#AccordionChevron {
                background: transparent;
                border: none;
                padding: 0px;
                margin: 0px;
            }
        """)

        self._content_widget.setStyleSheet(f"""
            QWidget#AccordionContent {{
                background-color: {content_bg};
                border: 1px solid {border_color};
                border-top-left-radius: 0px;
                border-top-right-radius: 0px;
                border-bottom-left-radius: {radius}px;
                border-bottom-right-radius: {radius}px;
            }}
        """)

        self._update_chevron()
