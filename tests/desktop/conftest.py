"""Shared fixtures for desktop (Qt) widget tests.

These tests exercise the presentational overlay widgets offscreen.  The
QT_QPA_PLATFORM variable must be set before the QApplication is created.
"""
# pylint: disable=redefined-outer-name

import os
import time

import pytest

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

# pylint: disable=wrong-import-position
from PySide6.QtCore import QEvent, QPoint, QPointF, Qt
from PySide6.QtGui import QColor, QMouseEvent, QPixmap
from PySide6.QtWidgets import QApplication, QWidget

from desktop.tab_manager.tab_overview import TabOverviewEntry


@pytest.fixture(scope="session")
def qapp():
    """Session-wide QApplication running on the offscreen platform."""
    app = QApplication.instance() or QApplication([])
    yield app


@pytest.fixture
def host(qapp):
    """A parent widget sized like a tab area."""
    widget = QWidget()
    widget.resize(1200, 800)
    yield widget
    widget.deleteLater()
    qapp.processEvents()


@pytest.fixture
def make_entries():
    """Factory for lists of TabOverviewEntry display records."""
    def _make(count: int = 6, current: int = 2, width: int = 800, height: int = 600, dpr: float = 1.0):
        pixmap = QPixmap(width, height)
        pixmap.setDevicePixelRatio(dpr)
        pixmap.fill(QColor("red"))
        return [
            TabOverviewEntry(
                tab_id=f"t{i}",
                icon_name="editor",
                title=f"Tab {i}",
                thumbnail=pixmap,
                is_current=(i == current),
            )
            for i in range(count)
        ]

    return _make


@pytest.fixture
def mouse(qapp):
    """Send a synthetic mouse event to a widget."""
    def _send(widget, event_type: QEvent.Type, pos: QPoint, button=Qt.MouseButton.LeftButton):
        buttons = button if event_type != QEvent.Type.MouseButtonRelease else Qt.MouseButton.NoButton
        event = QMouseEvent(
            event_type,
            QPointF(pos),
            QPointF(pos),
            QPointF(pos),
            button,
            buttons,
            Qt.KeyboardModifier.NoModifier,
        )
        qapp.sendEvent(widget, event)

    return _send


@pytest.fixture
def settle(qapp):
    """Process events for long enough that in-flight animations finish."""
    def _settle(ms: int = 450):
        deadline = time.time() + ms / 1000
        while time.time() < deadline:
            qapp.processEvents()
            time.sleep(0.01)

    return _settle
