"""Shared fixtures for editor tests requiring Qt.

The QT_QPA_PLATFORM variable must be set before the QApplication is created.
"""
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

# pylint: disable=wrong-import-position
import pytest

from PySide6.QtWidgets import QApplication


@pytest.fixture(scope="session")
def qapp():
    """Session-wide QApplication running on the offscreen platform."""
    app = QApplication.instance() or QApplication([])
    return app
