"""Transition widget that animates a departing latched folder out of the breadcrumb bar."""

import os

from PySide6.QtCore import Qt, QFileInfo, QModelIndex, QSize, Signal
from PySide6.QtGui import QIcon, QStandardItem, QStandardItemModel
from PySide6.QtWidgets import QFrame, QTreeView, QWidget

from humbug.mindspace.mindspace_tree_icon_provider import MindspaceTreeIconProvider
from humbug.mindspace.mindspace_tree_style import MindspaceTreeStyle
from humbug.style_manager import StyleManager
from humbug.color_role import ColorRole


_PATH_ROLE = Qt.ItemDataRole.UserRole


class MindspaceBreadcrumbTransition(QTreeView):
    """
    A single-row tree view that animates a departing latched folder out of view.

    When two sibling expanded folders are adjacent in the main tree, transitioning
    the breadcrumb latch from one to the other requires a buffer zone.  This widget
    holds the departing folder and shrinks its height from (row_height - 1) down to
    zero as the main tree scrolls, giving the appearance of the item scrolling out
    from between the breadcrumb bar and the main tree.

    The widget is hidden when inactive and shows no scrollbars.  It is purely
    decorative — no interaction is supported.
    """

    height_changed = Signal(int)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the transition widget."""
        super().__init__(parent)
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.setHeaderHidden(True)
        self.setAnimated(False)
        self.setSortingEnabled(False)
        self.setEditTriggers(QTreeView.EditTrigger.NoEditTriggers)
        self.setSelectionMode(QTreeView.SelectionMode.NoSelection)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setContextMenuPolicy(Qt.ContextMenuPolicy.NoContextMenu)
        self.setAcceptDrops(False)
        self.setFocusPolicy(Qt.FocusPolicy.NoFocus)

        self._style_manager = StyleManager()
        self._icon_provider = MindspaceTreeIconProvider()
        self._tree_style = MindspaceTreeStyle()
        self.setStyle(self._tree_style)

        self._model = QStandardItemModel(self)
        self.setModel(self._model)

        self._active_path: str = ""
        self._required_height: int = 0

        self.hide()

    # ------------------------------------------------------------------ #
    # Public API                                                           #
    # ------------------------------------------------------------------ #

    def populate(self, path: str, root_path: str) -> None:
        """
        Load the departing folder into the widget and show it at maximum height.

        The caller must call update_height() immediately after to set the correct
        initial height based on the current fractional scroll offset.

        Ancestor items from root_path down to path's parent are inserted above the
        departing item so that it appears at the correct indentation depth, matching
        its position in the main tree.  These ancestor items are non-interactive and
        invisible to the user — they exist only to drive Qt's indentation.

        Args:
            path: Absolute path of the departing latched folder.
            root_path: Absolute path of the tree root (mindspace root).
        """
        self._active_path = path
        self._model.clear()

        icon = self._folder_icon(path)

        # Build the ancestor chain from root down to path's parent.
        ancestors: list[str] = []
        current = os.path.dirname(os.path.normpath(path))
        root_norm = os.path.normpath(root_path)
        while current != root_norm:
            ancestors.append(current)
            parent = os.path.dirname(current)
            if parent == current:
                break
            current = parent

        ancestors.reverse()

        # Insert ancestor items as invisible, non-interactive placeholders.
        parent_item = self._model.invisibleRootItem()
        for ancestor in ancestors:
            stub = QStandardItem()
            stub.setData(ancestor, _PATH_ROLE)
            stub.setEditable(False)
            stub.setFlags(Qt.ItemFlag.NoItemFlags)
            parent_item.appendRow(stub)
            parent_item = stub
            super(MindspaceBreadcrumbTransition, self).setExpanded(
                self._model.indexFromItem(stub), True
            )

        # Insert the actual departing item under the deepest ancestor.
        item = QStandardItem(os.path.basename(path))
        item.setData(path, _PATH_ROLE)
        item.setEditable(False)
        item.setIcon(icon)
        parent_item.appendRow(item)

    def clear_item(self) -> None:
        """Remove the current item and notify the container to hide the widget."""
        self._active_path = ""
        self._model.clear()
        self._required_height = 0
        self.updateGeometry()
        self.height_changed.emit(0)

    def update_height(self, fractional_offset: int, row_height: int) -> None:
        """
        Adjust the widget height based on how far the main tree has scrolled.

        Args:
            fractional_offset: Pixels the topmost main-tree item has scrolled
                above the viewport top (0 = item fully visible, positive = scrolled up).
            row_height: Height in pixels of one tree row.
        """
        if not self._active_path:
            return

        new_height = max(0, row_height - 1 - fractional_offset)
        self._required_height = new_height
        self.updateGeometry()
        self.height_changed.emit(new_height)

        if new_height == 0:
            self.clear_item()

    def sizeHint(self) -> QSize:
        """Report the current required height to the layout."""
        return QSize(self.width(), self._required_height)

    def minimumSizeHint(self) -> QSize:
        """Report the current required height to the layout."""
        return QSize(0, self._required_height)

    def is_active(self) -> bool:
        """Return True if this widget is currently showing a departing folder."""
        return bool(self._active_path)

    def active_path(self) -> str:
        """Return the path of the departing folder, or empty string if inactive."""
        return self._active_path

    # ------------------------------------------------------------------ #
    # Collapse prevention                                                  #
    # ------------------------------------------------------------------ #

    def collapse(self, index: QModelIndex) -> None:  # type: ignore[override]
        """Suppress collapse."""

    def setExpanded(self, index: QModelIndex, expanded: bool) -> None:  # type: ignore[override]
        """Only allow expansion, never collapse."""
        if expanded:
            super().setExpanded(index, True)

    # ------------------------------------------------------------------ #
    # Styling                                                              #
    # ------------------------------------------------------------------ #

    def apply_style(self, font_size: float, zoom_factor: float) -> None:
        """
        Apply current style settings to match the breadcrumb bar and main tree.

        Args:
            font_size: Base font size in points.
            zoom_factor: Current zoom factor.
        """
        self._icon_provider.update_icons()

        icon_size = round(16 * zoom_factor)
        self.setIconSize(QSize(icon_size, icon_size))
        self.setIndentation(icon_size)

        font = self.font()
        font.setPointSizeF(font_size * zoom_factor)
        self.setFont(font)

        tree_bg = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        tree_hover = self._style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
        text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        tree_margin = round(6 * zoom_factor)

        self.setStyleSheet(f"""
            MindspaceBreadcrumbTransition {{
                background-color: darkgreen;
                color: {text};
                outline: none;
                margin-left: {tree_margin}px;
            }}
            MindspaceBreadcrumbTransition::item {{
                color: {text};
                padding: 3px 0px;
                margin: 0px;
            }}
            MindspaceBreadcrumbTransition::item:hover {{
                background-color: {tree_hover};
            }}
            MindspaceBreadcrumbTransition::branch {{
                background-color: darkgreen;
            }}
        """)

        self._refresh_icons()

    # ------------------------------------------------------------------ #
    # Internal helpers                                                     #
    # ------------------------------------------------------------------ #

    def _folder_icon(self, path: str) -> QIcon:
        """Return the folder icon for the given path."""
        info = QFileInfo(path if path else ".")
        return self._icon_provider.icon(info)

    def _refresh_icons(self) -> None:
        """Refresh folder icons after an icon provider update."""
        def refresh_recursive(parent: QModelIndex) -> None:
            for row in range(self._model.rowCount(parent)):
                index = self._model.index(row, 0, parent)
                path = index.data(_PATH_ROLE)
                if path:
                    icon = self._folder_icon(path)
                    self._model.setData(index, icon, Qt.ItemDataRole.DecorationRole)
                refresh_recursive(index)

        refresh_recursive(QModelIndex())
