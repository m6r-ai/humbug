"""Reusable collapsible accordion with a smooth, animated expand/collapse."""

from PySide6.QtCore import (
    Qt,
    Signal,
    Property,
    QPointF,
    QPropertyAnimation,
    QParallelAnimationGroup,
    QEasingCurve,
)
from PySide6.QtGui import QColor, QMouseEvent, QPainter, QPaintEvent, QPen
from PySide6.QtWidgets import QFrame, QHBoxLayout, QVBoxLayout, QWidget

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager
from desktop.ui_constants import ACCORDION_ANIM_DURATION_MS, QWIDGETSIZE_MAX


class _Chevron(QWidget):
    """A small chevron that smoothly rotates between pointing right and down."""

    def __init__(self, color: str, size: int, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._color = color
        self._angle = 0.0
        self.setFixedSize(size, size)
        self.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, True)

    def angle(self) -> float:
        """Return the current chevron rotation in degrees."""
        return self._angle

    def set_angle(self, value: float) -> None:
        """Set the chevron rotation in degrees and repaint."""
        self._angle = value
        self.update()

    rotation = Property(float, angle, set_angle)

    def set_color(self, color: str) -> None:
        """Set the chevron stroke colour."""
        self._color = color
        self.update()

    def paintEvent(self, event: QPaintEvent) -> None:
        del event
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)

        pen = QPen(QColor(self._color))
        pen.setWidthF(max(1.6, self.width() / 9.0))
        pen.setCapStyle(Qt.PenCapStyle.RoundCap)
        pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
        painter.setPen(pen)

        painter.translate(self.width() / 2.0, self.height() / 2.0)
        painter.rotate(self._angle)
        r = min(self.width(), self.height()) / 2.0 - pen.widthF()
        painter.drawPolyline([
            QPointF(-r * 0.45, -r * 0.8),
            QPointF(r * 0.55, 0.0),
            QPointF(-r * 0.45, r * 0.8),
        ])
        painter.end()


class _ClickableFrame(QFrame):
    """A frame that emits ``clicked`` on a left-button release within its bounds."""

    clicked = Signal()

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.MouseButton.LeftButton and self.rect().contains(event.position().toPoint()):
            self.clicked.emit()

        super().mouseReleaseEvent(event)


class Accordion(QFrame):
    """
    A collapsible section with a clickable header and an animated body.

    The header hosts caller-supplied widgets (added via ``header_layout()``)
    followed by a chevron that rotates as the section opens.  Body widgets are
    added via ``body_layout()``.

    Expand/collapse animates the body's maximumHeight between 0 and its natural
    height — the exact mechanism used by the (stable) SettingsAccordion.  The
    widget is purely presentational and emits ``toggled(bool)`` on each change.
    """

    toggled = Signal(bool)

    def __init__(self, expanded: bool = False, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setObjectName("Accordion")
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)

        self._style_manager = StyleManager()
        self._expanded = expanded
        self._content_visible = expanded
        self._animation: QParallelAnimationGroup | None = None

        zoom = self._style_manager.zoom_factor()
        pad = max(8, int(10 * zoom))

        outer = QVBoxLayout(self)
        outer.setContentsMargins(0, 0, 0, 0)
        outer.setSpacing(0)

        self._header = _ClickableFrame()
        self._header.setObjectName("AccordionHeaderArea")
        self._header.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)
        self._header.setAttribute(Qt.WidgetAttribute.WA_Hover, True)
        self._header.setCursor(Qt.CursorShape.PointingHandCursor)
        self._header.clicked.connect(self.toggle)

        header_outer = QHBoxLayout(self._header)
        header_outer.setContentsMargins(pad, pad, pad, pad)
        header_outer.setSpacing(int(pad * 0.75))

        self._header_layout = QHBoxLayout()
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(int(pad * 0.75))
        header_outer.addLayout(self._header_layout, 1)

        self._chevron = _Chevron(
            self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE),
            max(12, int(13 * zoom)),
        )
        self._chevron.set_angle(90.0 if expanded else 0.0)
        header_outer.addWidget(self._chevron)

        outer.addWidget(self._header)

        self._content = QWidget()
        self._content.setObjectName("AccordionBody")
        self._body_layout = QVBoxLayout(self._content)
        self._body_layout.setContentsMargins(0, 0, 0, 0)
        self._body_layout.setSpacing(0)

        # A maximumHeight of 0 with a minimumHeight of 0 hides the body without
        # setVisible(); the slide animates maximumHeight between the two.
        self._content.setMinimumHeight(0)
        self._content.setMaximumHeight(QWIDGETSIZE_MAX if expanded else 0)
        outer.addWidget(self._content)

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_style_changed()

    def header_layout(self) -> QHBoxLayout:
        """Return the layout for caller-supplied header widgets (left of the chevron)."""
        return self._header_layout

    def body_layout(self) -> QVBoxLayout:
        """Return the layout that collapsible body widgets are added into."""
        return self._body_layout

    def is_expanded(self) -> bool:
        """Return whether the section is currently expanded."""
        return self._expanded

    def toggle(self) -> None:
        """Toggle the expanded state with animation."""
        self.set_expanded(not self._expanded)

    def set_expanded(self, expanded: bool, animate: bool = True) -> None:
        """Expand or collapse the section, optionally without animation."""
        if expanded == self._expanded and self._animation is None:
            return

        if self._animation and self._animation.state() == QParallelAnimationGroup.State.Running:
            active = self._animation
            self._animation = None
            active.stop()

        start_h = self._content.height()
        self._expanded = expanded

        if expanded:
            # Show the expanded chrome immediately, then measure the natural height.
            self._content_visible = True
            self._on_style_changed()
            self._content.setMaximumHeight(QWIDGETSIZE_MAX)
            target_h = self._content.sizeHint().height()
            self._content.setMaximumHeight(start_h)

        else:
            # Keep the expanded chrome until the body has fully collapsed.
            target_h = 0

        if not animate:
            self._content_visible = expanded
            self._content.setMaximumHeight(QWIDGETSIZE_MAX if expanded else 0)
            self._chevron.set_angle(90.0 if expanded else 0.0)
            self._on_style_changed()
            self.toggled.emit(expanded)
            return

        height_anim = QPropertyAnimation(self._content, b"maximumHeight")
        height_anim.setDuration(ACCORDION_ANIM_DURATION_MS)
        height_anim.setStartValue(start_h)
        height_anim.setEndValue(target_h)
        height_anim.setEasingCurve(QEasingCurve.Type.InOutCubic)

        chevron_anim = QPropertyAnimation(self._chevron, b"rotation")
        chevron_anim.setDuration(ACCORDION_ANIM_DURATION_MS)
        chevron_anim.setStartValue(self._chevron.angle())
        chevron_anim.setEndValue(90.0 if expanded else 0.0)
        chevron_anim.setEasingCurve(QEasingCurve.Type.InOutCubic)

        group = QParallelAnimationGroup(self)
        group.addAnimation(height_anim)
        group.addAnimation(chevron_anim)
        group.finished.connect(lambda: self._on_animation_finished(group, expanded))
        group.start()
        self._animation = group

        self.toggled.emit(expanded)

    def _on_animation_finished(self, group: QParallelAnimationGroup, expanded: bool) -> None:
        """Clean up after an expand/collapse animation and finalize content visibility."""
        if group is not self._animation:
            return

        self._animation = None
        self._content_visible = expanded
        self._content.setMaximumHeight(QWIDGETSIZE_MAX if expanded else 0)
        self._on_style_changed()

    def _on_style_changed(self) -> None:
        """Rebuild the accordion's stylesheet from current style manager colors."""
        sm = self._style_manager
        radius = max(6, int(8 * sm.zoom_factor()))

        bg = sm.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        body_bg = sm.get_color_str(ColorRole.BACKGROUND_DIALOG)
        hover = sm.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        border = sm.get_color_str(ColorRole.MENU_BORDER)

        self._chevron.set_color(sm.get_color_str(ColorRole.TEXT_INACTIVE))

        # Square the header's bottom corners only while the body is visible.
        header_bottom_radius = 0 if self._content_visible else radius

        self.setStyleSheet(f"""
            QFrame#Accordion {{
                background-color: {body_bg};
                border: 1px solid {border};
                border-radius: {radius}px;
            }}
            #Accordion QWidget#AccordionBody {{
                background: transparent;
            }}
            #Accordion QLabel {{
                background: transparent;
            }}
            #Accordion QFrame#AccordionHeaderArea {{
                background-color: {bg};
                border: none;
                border-top-left-radius: {radius}px;
                border-top-right-radius: {radius}px;
                border-bottom-left-radius: {header_bottom_radius}px;
                border-bottom-right-radius: {header_bottom_radius}px;
            }}
            #Accordion QFrame#AccordionHeaderArea:hover {{
                background-color: {hover};
            }}
        """)
