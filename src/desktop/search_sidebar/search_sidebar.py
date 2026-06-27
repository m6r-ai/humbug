from collections import OrderedDict
import re

from PySide6.QtCore import Qt, QTimer, QSize, QModelIndex, QPersistentModelIndex, Signal
from PySide6.QtGui import QIcon, QPainter
from PySide6.QtWidgets import (
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QStyle,
    QStyledItemDelegate,
    QStyleOptionViewItem,
    QToolButton,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)

from mindspace.mindspace_content_type import MindspaceContentType
from mindspace.mindspace_search_engine import MindspaceSearchEngine, MindspaceSearchMatch

from desktop.color_role import ColorRole
from desktop.language.language_manager import LanguageManager
from desktop.sidebar.sidebar_base import SidebarBase
from desktop.sidebar.sidebar_pane_style import build_tree_pane_stylesheet
from desktop.sidebar.sidebar_section_header import SidebarSectionHeader
from desktop.sidebar.sidebar_tree_style import SidebarTreeStyle
from desktop.style_manager import StyleManager

_LINE_NUMBER_ROLE = Qt.ItemDataRole.UserRole + 11
_MESSAGE_ID_ROLE = Qt.ItemDataRole.UserRole + 12
_HIGHLIGHT_RANGES_ROLE = Qt.ItemDataRole.UserRole + 10


class _SearchResultDelegate(QStyledItemDelegate):
    """Paint tree items with inline search-term highlights."""

    def __init__(self, style_manager: StyleManager, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._style_manager = style_manager

    def sizeHint(
        self,
        option: QStyleOptionViewItem,
        index: QModelIndex | QPersistentModelIndex,
    ) -> QSize:
        """Return a zoom-scaled row height matching the other mindspace tree views."""
        zoom = self._style_manager.zoom_factor()
        fm = option.fontMetrics  # type: ignore
        line_height = fm.height()
        row_height = max(line_height + round(10 * zoom), round(24 * zoom))
        return QSize(super().sizeHint(option, index).width(), row_height)

    def paint(self, painter: QPainter, option: QStyleOptionViewItem, index: QModelIndex | QPersistentModelIndex) -> None:
        opt = QStyleOptionViewItem(option)
        self.initStyleOption(opt, index)
        text = opt.text
        ranges = index.data(_HIGHLIGHT_RANGES_ROLE) or []

        opt.text = ""
        widget = opt.widget
        if widget is None:
            super().paint(painter, option, index)
            return

        style = widget.style()

        style.drawControl(QStyle.ControlElement.CE_ItemViewItem, opt, painter, opt.widget)
        if not text:
            return

        text_rect = style.subElementRect(QStyle.SubElement.SE_ItemViewItemText, opt, opt.widget)
        if not text_rect.isValid():
            return

        metrics = opt.fontMetrics
        display_text = metrics.elidedText(text, opt.textElideMode, text_rect.width())
        if not display_text:
            return

        clipped_ranges: list[tuple[int, int]] = []
        for start, length in ranges:
            if start >= len(display_text) or length <= 0:
                continue

            clipped_ranges.append((start, min(length, len(display_text) - start)))

        text_color = (opt.palette.highlightedText().color()
                      if opt.state & QStyle.StateFlag.State_Selected
                      else opt.palette.text().color())
        highlight_color = self._style_manager.get_color(ColorRole.TEXT_FOUND_DIM)

        painter.save()
        painter.setClipRect(text_rect)
        x = text_rect.x()
        baseline = text_rect.y() + (text_rect.height() + metrics.ascent() - metrics.descent()) // 2
        cursor = 0

        for start, length in clipped_ranges:
            if start > cursor:
                segment = display_text[cursor:start]
                painter.setPen(text_color)
                painter.drawText(x, baseline, segment)
                x += metrics.horizontalAdvance(segment)

            segment = display_text[start:start + length]
            segment_width = metrics.horizontalAdvance(segment)
            painter.fillRect(x, text_rect.y() + 2, segment_width, text_rect.height() - 4, highlight_color)
            painter.setPen(text_color)
            painter.drawText(x, baseline, segment)
            x += segment_width
            cursor = start + length

        if cursor < len(display_text):
            segment = display_text[cursor:]
            painter.setPen(text_color)
            painter.drawText(x, baseline, segment)

        painter.restore()


class SearchSidebar(SidebarBase):
    """Global search pane for searching across the current mindspace."""

    file_clicked = Signal(str, str, bool)               # panel_id, path, ephemeral

    # panel_id, path, ephemeral, query, case_sensitive, regexp, line_number, message_id
    result_activated = Signal(str, str, bool, str, bool, bool, object, object)
    highlights_cleared = Signal()

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)

        self._style_manager = StyleManager()
        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._search_engine = MindspaceSearchEngine()
        self._mindspace_path = ""
        self._regexp_invalid = False

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._header = SidebarSectionHeader(self._language_manager.strings().mindspace_search, self)
        layout.addWidget(self._header)

        search_bar = QWidget(self)
        search_bar.setObjectName("_search_bar")
        search_layout = QHBoxLayout(search_bar)
        search_layout.setContentsMargins(10, 5, 10, 3)
        search_layout.setSpacing(0)

        self._search_input = QLineEdit(search_bar)
        self._search_input.setObjectName("_search_input")
        self._search_input.textChanged.connect(self._schedule_search)
        search_layout.addWidget(self._search_input, 1)

        layout.addWidget(search_bar)

        self._options_panel = QWidget(self)
        self._options_panel.setObjectName("_options_panel")
        self._options_layout = QHBoxLayout(self._options_panel)
        self._options_layout.setContentsMargins(10, 3, 10, 5)
        self._options_layout.addStretch()

        self._match_case_button = QToolButton(self._options_panel)
        self._match_case_button.setObjectName("toggleButton")
        self._match_case_button.setCheckable(True)
        self._match_case_button.toggled.connect(self._on_option_toggled)
        self._options_layout.addWidget(self._match_case_button)

        self._whole_word_button = QToolButton(self._options_panel)
        self._whole_word_button.setObjectName("toggleButton")
        self._whole_word_button.setCheckable(True)
        self._whole_word_button.toggled.connect(self._on_option_toggled)
        self._options_layout.addWidget(self._whole_word_button)

        self._regexp_button = QToolButton(self._options_panel)
        self._regexp_button.setObjectName("toggleButton")
        self._regexp_button.setCheckable(True)
        self._regexp_button.toggled.connect(self._on_option_toggled)
        self._options_layout.addWidget(self._regexp_button)

        self._hidden_button = QToolButton(self._options_panel)
        self._hidden_button.setObjectName("toggleButton")
        self._hidden_button.setCheckable(True)
        self._hidden_button.toggled.connect(self._on_option_toggled)
        self._options_layout.addWidget(self._hidden_button)

        layout.addWidget(self._options_panel)

        self._status_label = QLabel(self)
        self._status_label.setObjectName("_status_label")
        layout.addWidget(self._status_label)

        self._truncated_label = QLabel(self)
        self._truncated_label.setObjectName("_truncated_label")
        self._truncated_label.hide()
        layout.addWidget(self._truncated_label)

        self._results_tree = QTreeWidget(self)
        self._results_tree.setObjectName("SearchSidebarResultsTree")
        self._tree_style = SidebarTreeStyle()
        self._results_tree.setStyle(self._tree_style)
        self._results_tree.setHeaderHidden(True)
        self._results_tree.setRootIsDecorated(True)
        self._results_tree.setUniformRowHeights(False)
        self._results_tree.setItemDelegate(_SearchResultDelegate(self._style_manager, self._results_tree))
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

    def _on_option_toggled(self, _checked: bool) -> None:
        """Update results immediately when a search option is toggled."""
        self._debounce_timer.stop()
        self._perform_search()

    def _perform_search(self) -> None:
        """Run global search against the current mindspace."""
        query = self._search_input.text().strip()
        self._results_tree.clear()
        self._truncated_label.hide()

        if not self._mindspace_path:
            self._regexp_invalid = False
            self._status_label.setText("")
            self.highlights_cleared.emit()
            self.apply_style()
            return

        if not query:
            self._regexp_invalid = False
            self._status_label.setText(self._language_manager.strings().mindspace_search_empty_state)
            self.highlights_cleared.emit()
            self.apply_style()
            return

        case_sensitive = self._match_case_button.isChecked()
        whole_word = self._whole_word_button.isChecked()
        regexp_enabled = self._regexp_button.isChecked()
        include_hidden = self._hidden_button.isChecked()

        if regexp_enabled:
            try:
                re.compile(query, 0 if case_sensitive else re.IGNORECASE)

            except re.error:
                self._regexp_invalid = True
                self._status_label.setText(self._language_manager.strings().find_invalid_regexp)
                self.apply_style()
                return

        self._regexp_invalid = False
        matches = self._search_engine.search(
            self._mindspace_path,
            query,
            case_sensitive=case_sensitive,
            whole_word=whole_word,
            regexp=regexp_enabled,
            include_hidden=include_hidden,
        )
        matches = self._deduplicate_matches(matches)
        if not matches:
            self._status_label.setText(self._language_manager.strings().mindspace_search_no_results)
            self._truncated_label.hide()
            self.apply_style()
            return

        grouped_matches: OrderedDict[str, list[MindspaceSearchMatch]] = OrderedDict()
        for match in matches:
            grouped_matches.setdefault(match.path, []).append(match)

        for _path, path_matches in grouped_matches.items():
            first_match = path_matches[0]
            top_level = QTreeWidgetItem([first_match.relative_path])
            top_level.setData(0, Qt.ItemDataRole.UserRole, first_match.path)
            first_panel_id = self._content_type_to_panel_id(first_match.content_type)
            top_level.setData(0, Qt.ItemDataRole.UserRole + 1, first_panel_id)
            top_level.setData(0, _HIGHLIGHT_RANGES_ROLE, self._highlight_ranges_for_text(first_match.relative_path))
            top_level.setToolTip(0, first_match.relative_path)
            top_level.setIcon(0, self._icon_for_panel_id(first_panel_id))

            for match in path_matches:
                child_text = self._describe_match(match)
                child = QTreeWidgetItem([child_text])
                child.setData(0, Qt.ItemDataRole.UserRole, match.path)
                child.setData(0, Qt.ItemDataRole.UserRole + 1, self._content_type_to_panel_id(match.content_type))
                child.setData(0, _LINE_NUMBER_ROLE, match.line_number)
                child.setData(0, _MESSAGE_ID_ROLE, match.message_id)
                child.setData(0, _HIGHLIGHT_RANGES_ROLE, self._highlight_ranges_for_match(match, child_text))
                child.setToolTip(0, child_text)
                top_level.addChild(child)

            top_level.setExpanded(True)
            self._results_tree.addTopLevelItem(top_level)

        strings = self._language_manager.strings()
        self._status_label.setText(strings.mindspace_search_results.format(
            len(matches),
            len(grouped_matches),
        ))
        truncated = len(matches) >= MindspaceSearchEngine.MAX_MATCHES
        self._truncated_label.setText(
            strings.mindspace_search_results_limited.format(MindspaceSearchEngine.MAX_MATCHES)
        )
        self._truncated_label.setVisible(truncated)
        self.apply_style()

    def _describe_match(self, match: MindspaceSearchMatch) -> str:
        if match.is_path_match:
            return self._language_manager.strings().mindspace_search_path_match

        if match.line_number is None:
            if match.line_text:
                return match.line_text

            return match.relative_path

        return f"L{match.line_number}: {match.line_text}"

    def _deduplicate_matches(self, matches: list[MindspaceSearchMatch]) -> list[MindspaceSearchMatch]:
        """Remove duplicate search results while preserving order."""
        unique_matches: list[MindspaceSearchMatch] = []
        seen: set[tuple[str, int | None, str, bool]] = set()
        for match in matches:
            key = (match.path, match.line_number, match.line_text, match.is_path_match)
            if key in seen:
                continue

            seen.add(key)
            unique_matches.append(match)

        return unique_matches

    def _highlight_ranges_for_match(self, match: MindspaceSearchMatch, display_text: str) -> list[tuple[int, int]]:
        if match.is_path_match:
            return []

        if match.line_number is None:
            return self._highlight_ranges_for_text(display_text)

        prefix = f"L{match.line_number}: "
        ranges = self._highlight_ranges_for_text(match.line_text)
        return [(start + len(prefix), length) for start, length in ranges]

    def _highlight_ranges_for_text(self, text: str) -> list[tuple[int, int]]:
        query, case_sensitive, regexp = self.current_find_request()
        if not query:
            return []

        if regexp:
            try:
                pattern = re.compile(query, 0 if case_sensitive else re.IGNORECASE)

            except re.error:
                return []

            return [(match.start(), match.end() - match.start()) for match in pattern.finditer(text)]

        haystack = text if case_sensitive else text.casefold()
        needle = query if case_sensitive else query.casefold()
        ranges: list[tuple[int, int]] = []
        pos = 0
        while True:
            pos = haystack.find(needle, pos)
            if pos == -1:
                return ranges

            ranges.append((pos, len(query)))
            pos += max(1, len(query))

    def _content_type_to_panel_id(self, content_type: MindspaceContentType) -> str:
        """Map a MindspaceContentType to the corresponding panel ID for display."""
        if content_type == MindspaceContentType.CONVERSATIONS:
            return "conversations"

        return "files"

    def _icon_for_panel_id(self, panel_id: str) -> QIcon:
        icon_name = "conversation" if panel_id == "conversations" else "files"
        return QIcon(self._style_manager.scale_icon(icon_name, 16))

    def _open_item(self, item: QTreeWidgetItem, ephemeral: bool) -> None:
        path = item.data(0, Qt.ItemDataRole.UserRole)
        panel_id = item.data(0, Qt.ItemDataRole.UserRole + 1)
        if not isinstance(path, str) or not isinstance(panel_id, str):
            return

        line_number = item.data(0, _LINE_NUMBER_ROLE)
        message_id = item.data(0, _MESSAGE_ID_ROLE)
        query, case_sensitive, regexp = self.current_find_request()
        if query:
            self.result_activated.emit(panel_id, path, ephemeral, query, case_sensitive, regexp, line_number, message_id)

        else:
            self.file_clicked.emit(panel_id, path, ephemeral)

    def current_find_request(self) -> tuple[str, bool, bool]:
        """Return the query and effective find options for reuse in opened tabs."""
        query = self._search_input.text().strip()
        case_sensitive = self._match_case_button.isChecked()
        regexp_enabled = self._regexp_button.isChecked()
        if self._whole_word_button.isChecked() and query and not regexp_enabled:
            return rf"\b{re.escape(query)}\b", case_sensitive, True

        return query, case_sensitive, regexp_enabled

    def _on_language_changed(self) -> None:
        strings = self._language_manager.strings()
        self._header.set_title(strings.mindspace_search)
        self._search_input.setPlaceholderText(strings.mindspace_search_placeholder)
        self._match_case_button.setToolTip(strings.find_match_case)
        self._whole_word_button.setToolTip(strings.mindspace_search_whole_word)
        self._regexp_button.setToolTip(strings.find_use_regexp)
        self._hidden_button.setToolTip(strings.mindspace_search_include_hidden)
        self._match_case_button.setAccessibleName(strings.find_match_case)
        self._whole_word_button.setAccessibleName(strings.mindspace_search_whole_word)
        self._regexp_button.setAccessibleName(strings.find_use_regexp)
        self._hidden_button.setAccessibleName(strings.mindspace_search_include_hidden)
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
        self._truncated_label.setFont(font)
        self._results_tree.setFont(font)
        icon_size = round(16 * zoom_factor)
        self._results_tree.setIconSize(QSize(icon_size, icon_size))
        self._results_tree.setIndentation(icon_size)
        input_bg = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        subtle_text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        button_hover = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        button_checked = self._style_manager.get_color_str(ColorRole.TEXT_FOUND)
        button_checked_hover = self._style_manager.get_color_str(ColorRole.TEXT_FOUND_DIM)
        status_gap = round(6 * zoom_factor)
        input_field_bg = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)
        input_field_bg_error = self._style_manager.get_color_str(ColorRole.EDIT_BOX_ERROR)
        self._options_layout.setSpacing(round(6 * zoom_factor))
        icon_px = round(base_font_size * zoom_factor * 96 / 72)
        option_icon_size = QSize(icon_px, icon_px)
        self._match_case_button.setIcon(QIcon(self._style_manager.scale_icon("find-match-case", icon_px)))
        self._match_case_button.setIconSize(option_icon_size)
        self._whole_word_button.setIcon(QIcon(self._style_manager.scale_icon("find-whole-word", icon_px)))
        self._whole_word_button.setIconSize(option_icon_size)
        self._regexp_button.setIcon(QIcon(self._style_manager.scale_icon("find-regexp", icon_px)))
        self._regexp_button.setIconSize(option_icon_size)
        self._hidden_button.setIcon(QIcon(self._style_manager.scale_icon("find-hidden", icon_px)))
        self._hidden_button.setIconSize(option_icon_size)

        self.setStyleSheet(
            build_tree_pane_stylesheet(
                self._style_manager,
                "SearchSidebar",
                "QTreeWidget#SearchSidebarResultsTree",
                self.layoutDirection(),
                zoom_factor,
            )
            + f"""
            QWidget#_search_bar {{
                background: transparent;
                margin: {round(6 * zoom_factor)}px {round(6 * zoom_factor)}px 0px {round(6 * zoom_factor)}px;
            }}
            QLineEdit#_search_input {{
                background-color: {input_field_bg_error if self._regexp_invalid else input_field_bg};
                color: {text};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {int(base_font_size * zoom_factor)}pt;
            }}
            QWidget#_options_panel {{
                background-color: transparent;
                border: none;
                margin: 0px {round(6 * zoom_factor)}px 0px {round(6 * zoom_factor)}px;
            }}
            QWidget#_options_panel QToolButton#toggleButton {{
                color: {text};
                background-color: {input_field_bg};
                border: none;
                border-radius: 4px;
                padding: 4px;
                font-size: {int(base_font_size * zoom_factor)}pt;
            }}
            QWidget#_options_panel QToolButton#toggleButton:hover {{
                background-color: {button_checked_hover};
            }}
            QWidget#_options_panel QToolButton#toggleButton:pressed {{
                background-color: {button_checked};
            }}
            QWidget#_options_panel QToolButton#toggleButton:checked {{
                background-color: {button_checked};
                color: {text};
            }}
            QWidget#_options_panel QToolButton#toggleButton:checked:hover {{
                background-color: {button_checked_hover};
            }}
            QWidget#_options_panel QToolButton#toggleButton:checked:pressed {{
                background-color: {input_field_bg};
            }}
            QLabel#_status_label {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_ERROR) if self._regexp_invalid else subtle_text};
                padding: {round(status_gap * 2)}px {round(6 * zoom_factor)}px {status_gap}px {round(6 * zoom_factor)}px;
                min-height: {round(16 * zoom_factor)}px;
            }}
            QLabel#_truncated_label {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_ERROR)};
                padding: 0px {round(6 * zoom_factor)}px {status_gap}px {round(6 * zoom_factor)}px;
                min-height: {round(16 * zoom_factor)}px;
            }}
            QTreeWidget#SearchSidebarResultsTree {{
                background-color: {input_bg};
                color: {text};
                outline: none;
                border: none;
            }}
            QTreeWidget#SearchSidebarResultsTree::item {{
                padding: 0px;
                margin: 0px;
            }}
            QTreeWidget#SearchSidebarResultsTree::item:hover {{
                background-color: {button_hover};
            }}
            QTreeWidget#SearchSidebarResultsTree::item:selected {{
                background-color: {button_checked};
                color: {text};
            }}
            """
        )
