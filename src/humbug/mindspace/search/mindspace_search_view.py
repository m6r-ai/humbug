from __future__ import annotations

from collections import OrderedDict

from PySide6.QtCore import Qt, QTimer, QSize, Signal
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QLabel, QLineEdit, QTreeWidget, QTreeWidgetItem, QVBoxLayout, QWidget

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.mindspace.mindspace_pane_style import build_tree_pane_stylesheet
from humbug.mindspace.mindspace_section_header import MindspaceSectionHeader
from humbug.mindspace.mindspace_view_type import MindspaceViewType
from humbug.mindspace.search.mindspace_search_engine import MindspaceSearchEngine, MindspaceSearchMatch
from humbug.style_manager import StyleManager


class MindspaceSearchView(QWidget):
    """Global search pane for searching across the current mindspace."""

    file_clicked = Signal(MindspaceViewType, str, bool)

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._search_engine = MindspaceSearchEngine()
        self._mindspace_path = ""

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._header = MindspaceSectionHeader(self._language_manager.strings().global_search, self)
        layout.addWidget(self._header)

        self._search_input = QLineEdit(self)
        self._search_input.setObjectName("_search_input")
        self._search_input.textChanged.connect(self._schedule_search)
        layout.addWidget(self._search_input)

        self._status_label = QLabel(self)
        self._status_label.setObjectName("_status_label")
        layout.addWidget(self._status_label)

        self._results_tree = QTreeWidget(self)
        self._results_tree.setObjectName("MindspaceSearchResultsTree")
        self._results_tree.setHeaderHidden(True)
        self._results_tree.setRootIsDecorated(True)
        self._results_tree.setUniformRowHeights(False)
        self._results_tree.itemClicked.connect(lambda item, _column: self._open_item(item, True))
        self._results_tree.itemDoubleClicked.connect(lambda item, _column: self._open_item(item, False))
        layout.addWidget(self._results_tree)

        self._debounce_timer = QTimer(self)
        self._debounce_timer.setSingleShot(True)
        self._debounce_timer.setInterval(200)
        self._debounce_timer.timeout.connect(self._perform_search)

        self._on_language_changed()

    def set_mindspace(self, path: str) -> None:
        """Set the current mindspace root."""
        self._mindspace_path = path
        self._perform_search()

    def focus_search(self) -> None:
        """Focus the search input."""
        self._search_input.setFocus()
        self._search_input.selectAll()

    def _schedule_search(self) -> None:
        """Debounce search runs while the user types."""
        self._debounce_timer.start()

    def _perform_search(self) -> None:
        """Run global search against the current mindspace."""
        query = self._search_input.text().strip()
        self._results_tree.clear()

        if not self._mindspace_path:
            self._status_label.setText("")
            return

        if not query:
            self._status_label.setText(self._language_manager.strings().global_search_empty_state)
            return

        matches = self._search_engine.search(self._mindspace_path, query)
        if not matches:
            self._status_label.setText(self._language_manager.strings().global_search_no_results)
            return

        grouped_matches: OrderedDict[str, list[MindspaceSearchMatch]] = OrderedDict()
        for match in matches:
            grouped_matches.setdefault(match.path, []).append(match)

        for path, path_matches in grouped_matches.items():
            first_match = path_matches[0]
            top_level = QTreeWidgetItem([first_match.relative_path])
            top_level.setData(0, Qt.ItemDataRole.UserRole, first_match.path)
            top_level.setData(0, Qt.ItemDataRole.UserRole + 1, first_match.view_type)
            top_level.setToolTip(0, first_match.relative_path)
            top_level.setIcon(0, self._icon_for_view_type(first_match.view_type))

            for match in path_matches:
                child_text = self._describe_match(match)
                child = QTreeWidgetItem([child_text])
                child.setData(0, Qt.ItemDataRole.UserRole, match.path)
                child.setData(0, Qt.ItemDataRole.UserRole + 1, match.view_type)
                child.setToolTip(0, child_text)
                top_level.addChild(child)

            top_level.setExpanded(True)
            self._results_tree.addTopLevelItem(top_level)

        strings = self._language_manager.strings()
        self._status_label.setText(strings.global_search_results.format(
            len(matches),
            len(grouped_matches),
        ))

    def _describe_match(self, match: MindspaceSearchMatch) -> str:
        if match.is_path_match:
            return self._language_manager.strings().global_search_path_match

        if match.line_number is None:
            return match.relative_path

        return f"L{match.line_number}: {match.line_text}"

    def _icon_for_view_type(self, view_type: MindspaceViewType) -> QIcon:
        icon_name = "conversation" if view_type == MindspaceViewType.CONVERSATIONS else "files"
        return QIcon(self._style_manager.scale_icon(icon_name, 16))

    def _open_item(self, item: QTreeWidgetItem, ephemeral: bool) -> None:
        path = item.data(0, Qt.ItemDataRole.UserRole)
        view_type = item.data(0, Qt.ItemDataRole.UserRole + 1)
        if not isinstance(path, str) or not isinstance(view_type, MindspaceViewType):
            return

        self.file_clicked.emit(view_type, path, ephemeral)

    def _on_language_changed(self) -> None:
        strings = self._language_manager.strings()
        self._header.set_title(strings.global_search)
        self._search_input.setPlaceholderText(strings.global_search_placeholder)
        self._perform_search()
        self.apply_style()

    def apply_style(self) -> None:
        """Apply search pane styling."""
        zoom_factor = self._style_manager.zoom_factor()
        base_font_size = self._style_manager.base_font_size()

        self._header.apply_style()

        font = self.font()
        font.setPointSizeF(base_font_size * zoom_factor)
        self.setFont(font)
        self._search_input.setFont(font)
        self._status_label.setFont(font)
        self._results_tree.setFont(font)
        self._results_tree.setIconSize(QSize(round(16 * zoom_factor), round(16 * zoom_factor)))
        self._results_tree.setIndentation(round(16 * zoom_factor))

        input_bg = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        input_border = self._style_manager.get_color_str(ColorRole.MENU_BORDER)
        text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        subtle_text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        status_padding = round(8 * zoom_factor)
        input_margin = round(8 * zoom_factor)
        input_padding = round(8 * zoom_factor)
        radius = round(6 * zoom_factor)

        self.setStyleSheet(
            build_tree_pane_stylesheet(
                self._style_manager,
                "MindspaceSearchView",
                "QTreeWidget#MindspaceSearchResultsTree",
                self.layoutDirection(),
                zoom_factor,
            )
            + f"""
            QLineEdit#_search_input {{
                background-color: {input_bg};
                color: {text};
                border: 1px solid {input_border};
                border-radius: {radius}px;
                margin: {input_margin}px {input_margin}px 0px {input_margin}px;
                padding: {input_padding}px;
            }}
            QLabel#_status_label {{
                color: {subtle_text};
                padding: {status_padding}px;
            }}
            """
        )
