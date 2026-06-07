"""
Combo box setting for selecting from a list of options.
"""

from typing import Any, List, Tuple, cast

from PySide6.QtCore import QEvent, QPoint, QRect, QSize, Qt, QRectF, QTimer
from PySide6.QtGui import QHideEvent, QIcon, QKeyEvent, QPainter, QPaintEvent, QPainterPath
from PySide6.QtWidgets import (
    QAbstractItemView,
    QFrame,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from desktop.settings.settings_field import SettingsField
from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


class _SettingsComboButton(QPushButton):
    """Push button trigger that paints the app dropdown asset on the right."""

    def __init__(self) -> None:
        super().__init__()
        self._dropdown_icon = QIcon()
        self._show_dropdown_icon = True

    def set_dropdown_icon(self, icon_path: str) -> None:
        """Set the app-provided dropdown icon."""
        self._dropdown_icon = QIcon(icon_path)
        self.update()

    def set_dropdown_icon_visible(self, visible: bool) -> None:
        """Show or hide the dropdown icon."""
        self._show_dropdown_icon = visible
        self.update()

    def paintEvent(self, event: QPaintEvent) -> None:
        super().paintEvent(event)
        if not self._show_dropdown_icon or self._dropdown_icon.isNull():
            return

        icon_size = max(12, min(16, round(self.fontMetrics().height() * 0.65)))
        edge_margin = max(4, round(self.fontMetrics().height() * 0.2))
        icon_x = (
            edge_margin
            if self.layoutDirection() == Qt.LayoutDirection.RightToLeft else
            self.width() - edge_margin - icon_size
        )
        rect = QRect(
            icon_x,
            (self.height() - icon_size) // 2,
            icon_size,
            icon_size
        )

        painter = QPainter(self)
        mode = QIcon.Mode.Normal if self.isEnabled() else QIcon.Mode.Disabled
        self._dropdown_icon.paint(painter, rect, Qt.AlignmentFlag.AlignCenter, mode)
        painter.end()


class _SettingsComboPopup(QFrame):
    """App-owned combo popup without native combo-box chrome."""

    def __init__(self, owner: "SettingsCombo") -> None:
        super().__init__(owner, Qt.WindowType.Popup | Qt.WindowType.NoDropShadowWindowHint)

        self._owner = owner
        self.setObjectName("SettingsComboPopupWindow")
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground, True)
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.setLineWidth(0)
        self.setMidLineWidth(0)
        self._searchable = False

        self._search = QLineEdit()
        self._search.setObjectName("SettingsComboPopupSearch")
        self._search.setPlaceholderText("Search")
        self._search.setClearButtonEnabled(True)
        self._search.textChanged.connect(self._filter_items)
        self._search.returnPressed.connect(self._activate_current_item)

        self._list = QListWidget()
        self._list.setObjectName("SettingsComboPopupList")
        self._list.setFrameShape(QFrame.Shape.NoFrame)
        self._list.setLineWidth(0)
        self._list.setMidLineWidth(0)
        self._list.setContentsMargins(0, 0, 0, 0)
        self._list.setViewportMargins(0, 0, 0, 0)
        self._list.setSpacing(0)
        self._list.setMouseTracking(True)
        self._list.setUniformItemSizes(True)
        self._list.setVerticalScrollMode(QAbstractItemView.ScrollMode.ScrollPerPixel)
        self._list.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._list.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self._list.itemClicked.connect(self._on_item_clicked)

        layout = QVBoxLayout()
        layout.setContentsMargins(6, 6, 6, 6)
        layout.setSpacing(6)
        layout.addWidget(self._search)
        layout.addWidget(self._list)
        self.setLayout(layout)

    def set_searchable(self, searchable: bool) -> None:
        """Show or hide the search field and clear it when hiding."""
        self._searchable = searchable
        self._search.setVisible(searchable)
        if not searchable:
            self._search.clear()

    def set_items(self, rows: List[Tuple[str, Any, bool]]) -> None:
        """Repopulate the list from rows of (text, data, enabled) tuples."""
        current_data = self._owner.get_value()
        self._list.clear()

        for text, data, enabled in rows:
            item = QListWidgetItem(text)
            item.setData(Qt.ItemDataRole.UserRole, data)
            item.setData(Qt.ItemDataRole.UserRole + 1, enabled)
            item.setSizeHint(self._item_size_hint(enabled))
            if not enabled:
                item.setFlags(Qt.ItemFlag.NoItemFlags)

            self._list.addItem(item)

        self.select_data(current_data)
        self._filter_items(self._search.text())

    def select_data(self, data: Any) -> None:
        """Highlight the first enabled row whose data matches, or fall back to the first enabled row."""
        for row in range(self._list.count()):
            item = self._list.item(row)
            if item.data(Qt.ItemDataRole.UserRole + 1) and item.data(Qt.ItemDataRole.UserRole) == data:
                self._list.setCurrentRow(row)
                return

        self._select_first_visible_enabled_item()

    def popup(self, global_pos: QPoint, width: int) -> None:
        """
        Show the popup at global_pos with the given width.

        Flips the popup above the trigger button if it would otherwise extend
        below the available screen area.
        """
        if self._searchable:
            self._search.clear()

        self._filter_items(self._search.text())
        self.setFixedWidth(width)
        self._resize_to_contents()

        screen = self._owner.window().screen()
        if screen:
            available = screen.availableGeometry()
            if global_pos.y() + self.height() > available.bottom():
                global_pos.setY(self._owner.button_top_global_y() - self.height())

        # Move into position then show on the next event loop iteration.  This
        # lets Qt create the native handle naturally via show(), and the deferred
        # move ensures the position is applied after the native window exists.
        # Showing only after the move avoids a visible flash at position (0, 0)
        # on first open.  On Linux/XCB this also avoids forcing early native
        # handle creation via winId(), which causes the compositor to disturb
        # the parent dialog's position.
        def _show_at_pos() -> None:
            self.move(global_pos)
            self.show()
            self.raise_()

        self.hide()
        QTimer.singleShot(0, _show_at_pos)

        if self._searchable:
            self._search.setFocus(Qt.FocusReason.PopupFocusReason)
            self._search.selectAll()

        else:
            self._list.setFocus(Qt.FocusReason.PopupFocusReason)

    def paintEvent(self, _event: QPaintEvent) -> None:
        """Paint the rounded background manually to support translucent corners."""
        radius = self._owner.style_manager().radius("surface")
        bg_color = self._owner.style_manager().get_color(ColorRole.MENU_BACKGROUND)
        border_color = self._owner.style_manager().get_color(ColorRole.TEXT_INACTIVE)

        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Fill the full rounded rect with the background colour.
        path = QPainterPath()
        path.addRoundedRect(QRectF(self.rect()), radius, radius)
        painter.fillPath(path, bg_color)

        # Stroke an inset path so the 1px border sits fully inside the widget
        # bounds and does not bleed into the transparent area outside.
        border_path = QPainterPath()
        border_path.addRoundedRect(QRectF(self.rect()).adjusted(0.5, 0.5, -0.5, -0.5), radius - 0.5, radius - 0.5)
        painter.setPen(border_color)
        painter.drawPath(border_path)

        painter.end()

    def apply_style(self, stylesheet: str) -> None:
        """Apply a stylesheet to the popup frame, list, and search field."""
        self.setStyleSheet(stylesheet)
        self._list.setStyleSheet(stylesheet)
        self._search.setStyleSheet(stylesheet)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        """Handle common popup keyboard actions."""
        if event.key() == Qt.Key.Key_Escape:
            self.hide()
            event.accept()
            return

        if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            self._activate_current_item()
            event.accept()
            return

        super().keyPressEvent(event)

    def hideEvent(self, event: QHideEvent) -> None:
        """Notify the owner when the popup is hidden so it can reset the chevron."""
        super().hideEvent(event)
        self._owner.on_popup_hidden()

    def _item_size_hint(self, enabled: bool) -> QSize:
        zoom = self._owner.style_manager().zoom_factor()
        base_height = 34 if enabled else 28
        height = max(int(base_height * zoom), self.fontMetrics().height() + int(10 * zoom))
        return QSize(1, height)

    def _resize_to_contents(self) -> None:
        visible_count = 0
        content_height = 0
        max_visible_rows = 12
        for row in range(self._list.count()):
            item = self._list.item(row)
            if item.isHidden():
                continue

            visible_count += 1
            if visible_count <= max_visible_rows:
                content_height += item.sizeHint().height()

        list_height = max(1, content_height)
        scrollbar_policy = (
            Qt.ScrollBarPolicy.ScrollBarAsNeeded
            if visible_count > max_visible_rows else
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff
        )
        self._list.setVerticalScrollBarPolicy(scrollbar_policy)

        search_height = self._search.sizeHint().height() if self._searchable else 0
        self._list.setFixedHeight(list_height)
        popup_layout = cast(QVBoxLayout, self.layout())
        margins = popup_layout.contentsMargins()
        spacing = popup_layout.spacing() if self._searchable else 0
        self.setFixedHeight(
            margins.top() + search_height + spacing + list_height + margins.bottom()
        )

    def _filter_items(self, text: str) -> None:
        query = text.casefold()
        group_rows: List[int] = []
        group_has_match = False

        for row in range(self._list.count()):
            item = self._list.item(row)
            enabled = bool(item.data(Qt.ItemDataRole.UserRole + 1))
            if not enabled:
                if group_rows:
                    for group_row in group_rows:
                        self._list.item(group_row).setHidden(not group_has_match)

                group_rows = [row]
                group_has_match = False
                item.setHidden(False)
                continue

            matched = query in item.text().casefold()
            item.setHidden(not matched)
            group_has_match = group_has_match or matched

        if group_rows:
            for group_row in group_rows:
                self._list.item(group_row).setHidden(not group_has_match)

        self._select_first_visible_enabled_item()
        if self.isVisible():
            self._resize_to_contents()

    def _select_first_visible_enabled_item(self) -> None:
        for row in range(self._list.count()):
            item = self._list.item(row)
            if not item.isHidden() and item.data(Qt.ItemDataRole.UserRole + 1):
                self._list.setCurrentRow(row)
                return

        self._list.setCurrentRow(-1)

    def _activate_current_item(self) -> None:
        item = self._list.currentItem()
        if item:
            self._on_item_clicked(item)

    def _on_item_clicked(self, item: QListWidgetItem) -> None:
        if not item.data(Qt.ItemDataRole.UserRole + 1):
            return

        self._owner.set_value(item.data(Qt.ItemDataRole.UserRole), reset_initial=False)
        self.hide()


class SettingsCombo(SettingsField):
    """
    Combo box setting for selecting from a list of options.

    Attributes:
        _initial_index (int): The initial selected index
        _items (List[tuple]): The items and their data values
    """

    def __init__(
        self,
        label_text: str,
        items: List[Tuple[str, Any]] | None = None,
        parent: QWidget | None = None,
        searchable: bool = False
    ) -> None:
        """
        Initialize a combo box setting.

        Args:
            label_text: Text for the setting label
            items: List of (display_text, data_value) tuples for combo items
            searchable: Whether to show search in the popup
            parent: Parent widget
        """
        super().__init__(label_text, parent)

        self._items: List[Tuple[str, Any, bool]] = []
        self._current_index = -1
        self._initial_index = -1
        self._searchable = False

        self._button = _SettingsComboButton()
        self._button.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self._button.setCursor(Qt.CursorShape.PointingHandCursor)
        self._button.clicked.connect(self._show_popup)

        self._popup = _SettingsComboPopup(self)
        self._popup.set_searchable(False)

        self._layout.addWidget(self._button)
        self.set_searchable(searchable)
        self._on_style_changed()

        if items:
            self.set_items(items)

        self._initial_index = self._current_index

    def style_manager(self) -> StyleManager:
        """Return the shared style manager."""
        return self._style_manager

    def button_top_global_y(self) -> int:
        """Return the combo button top edge in global coordinates."""
        return self._button.mapToGlobal(QPoint(0, 0)).y()

    def _show_popup(self) -> None:
        if not self._items:
            return

        self._popup.set_items(self._items)
        self._popup.select_data(self.get_value())
        global_pos = self._button.mapToGlobal(QPoint(0, self._button.height() - 1))
        self._popup.popup(global_pos, self._button.width())
        self._button.set_dropdown_icon(self._style_manager.get_icon_path("arrow-up"))

    def on_popup_hidden(self) -> None:
        """Reset the chevron to the down arrow when the popup is dismissed."""
        self._button.set_dropdown_icon(self._style_manager.get_icon_path("arrow-down"))

    def _emit_if_changed(self, previous_index: int) -> None:
        if self._current_index != previous_index:
            self.value_changed.emit()

    def is_modified(self) -> bool:
        """Check if combo selection has changed."""
        return self._current_index != self._initial_index

    def reset_modified_state(self) -> None:
        """Reset the initial index to current index."""
        self._initial_index = self._current_index

    def get_value(self) -> Any:
        """Get the current selected item's data."""
        if 0 <= self._current_index < len(self._items):
            return self._items[self._current_index][1]

        return None

    def get_text(self) -> str:
        """Get the current selected item's text."""
        if 0 <= self._current_index < len(self._items):
            return self._items[self._current_index][0]

        return ""

    def set_value(self, value: Any, reset_initial: bool = True) -> None:
        """Set the combo box selection by data value."""
        previous_index = self._current_index
        for index, (_text, data_value, enabled) in enumerate(self._items):
            if enabled and data_value == value:
                self._current_index = index
                self._sync_button_text()
                if reset_initial:
                    self._initial_index = index

                self._emit_if_changed(previous_index)
                return

    def set_items(self, items: List[Tuple[str, Any]]) -> None:
        """
        Update the items in the combo box.

        Args:
            items: List of (display_text, data_value) tuples
        """
        current_data = self.get_value()
        self._items = [(display_text, data_value, True) for display_text, data_value in items]
        self._restore_selection(current_data)
        self._initial_index = self._current_index

    def set_grouped_items(self, groups: List[Tuple[str, List[Tuple[str, Any]]]]) -> None:
        """
        Populate the combo with named groups acting as non-selectable headers.

        Args:
            groups: List of (group_name, [(display_text, data_value), ...]).
        """
        current_data = self.get_value()
        rows: List[Tuple[str, Any, bool]] = []
        for group_name, items in groups:
            rows.append((f"── {group_name} ──", None, False))
            rows.extend((display_text, data_value, True) for display_text, data_value in items)

        self._items = rows
        self._restore_selection(current_data)
        self._initial_index = self._current_index

    def set_searchable(self, enabled: bool = True) -> None:
        """
        Enable or disable popup search.
        """
        self._searchable = enabled
        self._button.set_dropdown_icon_visible(not enabled)
        self._popup.set_searchable(enabled)
        self._apply_button_style()

    def changeEvent(self, event: QEvent) -> None:
        """Update trigger styling when layout direction changes."""
        super().changeEvent(event)
        if event.type() == QEvent.Type.LayoutDirectionChange:
            self._button.setLayoutDirection(self.layoutDirection())
            self._apply_button_style()

    def _restore_selection(self, current_data: Any) -> None:
        self._current_index = -1
        for index, (_text, data_value, enabled) in enumerate(self._items):
            if enabled and data_value == current_data:
                self._current_index = index
                break

        if self._current_index < 0:
            for index, (_text, _data_value, enabled) in enumerate(self._items):
                if enabled:
                    self._current_index = index
                    break

        self._sync_button_text()
        self._popup.set_items(self._items)

    def _sync_button_text(self) -> None:
        self._button.setText(self.get_text())

    def _on_style_changed(self) -> None:
        """Update combo box styling."""
        super()._on_style_changed()

        self._button.set_dropdown_icon(self._style_manager.get_icon_path("arrow-down"))
        self._apply_button_style()
        self._popup.apply_style(self._style_manager.get_combo_popup_stylesheet())

    def _apply_button_style(self) -> None:
        """Apply trigger styling for normal and searchable modes."""
        zoom_factor = self._style_manager.zoom_factor()
        self._button.setMinimumHeight(int(36 * zoom_factor))
        if self._searchable:
            object_name = "SettingsComboSearchButton"

        elif self.layoutDirection() == Qt.LayoutDirection.RightToLeft:
            object_name = "SettingsComboButtonRtl"

        else:
            object_name = "SettingsComboButton"

        self._button.setObjectName(object_name)
        self._button.setStyleSheet(
            self._style_manager.get_combo_box_stylesheet(f"QPushButton#{object_name}")
        )

    def setEnabled(self, arg__1: bool) -> None:
        """Enable or disable the combo box."""
        self._button.setEnabled(arg__1)
