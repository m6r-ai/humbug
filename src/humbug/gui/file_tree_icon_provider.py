"""Workspace file tree icon management."""

import os
from typing import Dict

from PySide6.QtCore import Qt, QFileInfo
from PySide6.QtGui import QIcon, QPainter, QPainterPath, QPixmap, QColor
from PySide6.QtWidgets import QFileIconProvider

from humbug.gui.color_role import ColorRole
from humbug.gui.style_manager import StyleManager


class FileTreeIconProvider(QFileIconProvider):
    """Custom file icon provider with scalable icons."""

    def __init__(self):
        """Initialize the icon provider."""
        super().__init__()
        self._style_manager = StyleManager()
        self._cached_icons: Dict[str, QIcon] = {}
        self._current_size = 16

        # Initialize standard icons
        self._initialize_standard_icons()

    def _initialize_standard_icons(self):
        """Create standard icons for different file types."""
        # Base folder icon (closed)
        self._folder_icon = self._create_folder_icon(False)
        self._folder_open_icon = self._create_folder_icon(True)

        # Base file icon
        self._file_icon = self._create_file_icon()

        # Special file type icons
        self._icons = {
            '.conv': self._create_conversation_icon(),
            '.m6r': self._create_metaphor_icon(),
            '.py': self._create_code_icon('#3572A5'),  # Python blue
            '.js': self._create_code_icon('#f1e05a'),  # JavaScript yellow
            '.ts': self._create_code_icon('#3178c6'),  # TypeScript blue
            '.html': self._create_code_icon('#e34c26'),  # HTML orange
            '.css': self._create_code_icon('#563d7c'),  # CSS purple
            '.md': self._create_text_icon(),
            '.txt': self._create_text_icon()
        }

    def _create_folder_icon(self, open_state: bool) -> QIcon:
        """Create a folder icon."""
        icon = QIcon()
        sizes = [16, 24, 32, 48, 64]

        for size in sizes:
            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.transparent)

            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.Antialiasing)

            # Draw folder shape
            path = QPainterPath()
            if open_state:
                # Open folder shape
                path.moveTo(2, size * 0.3)
                path.lineTo(size * 0.4, size * 0.3)
                path.lineTo(size * 0.5, size * 0.15)
                path.lineTo(size - 2, size * 0.15)
                path.lineTo(size * 0.8, size - 2)
                path.lineTo(2, size - 2)
            else:
                # Closed folder shape
                path.moveTo(2, size * 0.2)
                path.lineTo(size * 0.4, size * 0.2)
                path.lineTo(size * 0.5, size * 0.1)
                path.lineTo(size - 2, size * 0.1)
                path.lineTo(size - 2, size - 2)
                path.lineTo(2, size - 2)

            path.closeSubpath()

            color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
            painter.setPen(color)
            painter.setBrush(color)
            painter.drawPath(path)

            painter.end()
            icon.addPixmap(pixmap)

        return icon

    def _create_file_icon(self) -> QIcon:
        """Create a generic file icon."""
        icon = QIcon()
        sizes = [16, 24, 32, 48, 64]

        for size in sizes:
            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.transparent)

            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.Antialiasing)

            # Draw file shape
            path = QPainterPath()
            path.moveTo(2, 2)
            path.lineTo(size * 0.7, 2)
            path.lineTo(size - 2, size * 0.3)
            path.lineTo(size - 2, size - 2)
            path.lineTo(2, size - 2)
            path.closeSubpath()

            # Draw dog-ear
            fold = QPainterPath()
            fold.moveTo(size * 0.7, 2)
            fold.lineTo(size * 0.7, size * 0.3)
            fold.lineTo(size - 2, size * 0.3)

            color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
            painter.setPen(color)
            painter.setBrush(color)
            painter.drawPath(path)
            painter.drawPath(fold)

            painter.end()
            icon.addPixmap(pixmap)

        return icon

    def _create_conversation_icon(self) -> QIcon:
        """Create conversation file icon."""
        icon = QIcon()
        sizes = [16, 24, 32, 48, 64]

        for size in sizes:
            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.transparent)

            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.Antialiasing)

            # Draw speech bubble shape
            path = QPainterPath()
            path.addRoundedRect(2, 2, size - 4, size * 0.7, size * 0.2, size * 0.2)

            # Add pointer
            point_path = QPainterPath()
            point_path.moveTo(size * 0.2, size * 0.7)
            point_path.lineTo(size * 0.3, size - 2)
            point_path.lineTo(size * 0.4, size * 0.7)

            color = self._style_manager.get_color(ColorRole.MESSAGE_AI)
            painter.setPen(color)
            painter.setBrush(color)
            painter.drawPath(path)
            painter.drawPath(point_path)

            painter.end()
            icon.addPixmap(pixmap)

        return icon

    def _create_metaphor_icon(self) -> QIcon:
        """Create Metaphor file icon."""
        icon = QIcon()
        sizes = [16, 24, 32, 48, 64]

        for size in sizes:
            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.transparent)

            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.Antialiasing)

            # Draw stylized 'M' shape
            path = QPainterPath()
            path.moveTo(2, size - 2)
            path.lineTo(2, 2)
            path.lineTo(size/2, size * 0.6)
            path.lineTo(size - 2, 2)
            path.lineTo(size - 2, size - 2)

            color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
            painter.setPen(color)
            painter.setBrush(color)
            painter.drawPath(path)

            painter.end()
            icon.addPixmap(pixmap)

        return icon

    def _create_code_icon(self, lang_color: str) -> QIcon:
        """Create programming language file icon."""
        icon = QIcon()
        sizes = [16, 24, 32, 48, 64]

        for size in sizes:
            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.transparent)

            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.Antialiasing)

            # Draw base file shape first
            path = QPainterPath()
            path.moveTo(2, 2)
            path.lineTo(size * 0.7, 2)
            path.lineTo(size - 2, size * 0.3)
            path.lineTo(size - 2, size - 2)
            path.lineTo(2, size - 2)
            path.closeSubpath()

            color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
            painter.setPen(color)
            painter.setBrush(color)
            painter.drawPath(path)

            # Draw colored dot for language
            dot_path = QPainterPath()
            dot_path.addEllipse(size/2 - size*0.15, size/2 - size*0.15, size*0.3, size*0.3)

            painter.setPen(Qt.NoPen)
            painter.setBrush(QColor(lang_color))
            painter.drawPath(dot_path)

            painter.end()
            icon.addPixmap(pixmap)

        return icon

    def _create_text_icon(self) -> QIcon:
        """Create text file icon."""
        icon = QIcon()
        sizes = [16, 24, 32, 48, 64]

        for size in sizes:
            pixmap = QPixmap(size, size)
            pixmap.fill(Qt.transparent)

            painter = QPainter(pixmap)
            painter.setRenderHint(QPainter.Antialiasing)

            # Draw file shape
            path = QPainterPath()
            path.moveTo(2, 2)
            path.lineTo(size - 2, 2)
            path.lineTo(size - 2, size - 2)
            path.lineTo(2, size - 2)
            path.closeSubpath()

            color = self._style_manager.get_color(ColorRole.TEXT_PRIMARY)
            painter.setPen(color)
            painter.setBrush(color)
            painter.drawPath(path)

            # Draw text lines
            line_color = self._style_manager.get_color(ColorRole.BACKGROUND_PRIMARY)
            painter.setPen(line_color)
            line_spacing = size * 0.2
            for y in range(int(size * 0.2), int(size * 0.8), int(line_spacing)):
                painter.drawLine(size * 0.2, y, size * 0.8, y)

            painter.end()
            icon.addPixmap(pixmap)

        return icon

    def icon(self, info: QFileInfo):
        """Get the appropriate icon for a file type."""
        print(f"icon called for: {info}")
        if info.isDir():
            return self._folder_icon

        ext = os.path.splitext(info.fileName())[1].lower()
        return self._icons.get(ext, self._file_icon)

    def update_icons(self):
        """Update icons when style changes."""
        self._initialize_standard_icons()
