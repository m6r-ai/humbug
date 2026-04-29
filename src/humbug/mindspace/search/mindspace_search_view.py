from __future__ import annotations

from collections import OrderedDict
import re

from PySide6.QtCore import Qt, QTimer, QSize, Signal
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import (
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QToolButton,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)

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
        self._options_expanded = False
        self._regexp_invalid = False

        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        self._header = MindspaceSectionHeader(self._language_manager.strings().global_search, self)
        layout.addWidget(self._header)

        search_bar = QWidget(self)
        search_bar.setObjectName("_search_bar")
        search_layout = QHBoxLayout(search_bar)
        search_layout.setSpacing(0)

        self._search_input = QLineEdit(search_bar)
        self._search_input.setObjectName("_search_input")
        self._search_input.textChanged.connect(self._schedule_search)
        search_layout.addWidget(self._search_input, 1)

        self._options_button = QToolButton(search_bar)
        self._options_button.setObjectName("_options_button")
        self._options_button.clicked.connect(self._toggle_options_panel)
        search_layout.addWidget(self._options_button)
        layout.addWidget(search_bar)

        self._options_panel = QWidget(self)
        self._options_panel.setObjectName("_options_panel")
        options_layout = QHBoxLayout(self._options_panel)
        options_layout.setContentsMargins(8, 8, 8, 8)
        options_layout.addStretch()
        # options_layout.setSpacing(6)

        self._match_case_button = QToolButton(self._options_panel)
        self._match_case_button.setObjectName("toggleButton")
        self._match_case_button.setCheckable(True)
        self._match_case_button.toggled.connect(self._schedule_search)
        options_layout.addWidget(self._match_case_button)

        self._whole_word_button = QToolButton(self._options_panel)
        self._whole_word_button.setObjectName("toggleButton")
        self._whole_word_button.setCheckable(True)
        self._whole_word_button.toggled.connect(self._schedule_search)
        options_layout.addWidget(self._whole_word_button)

        self._regexp_button = QToolButton(self._options_panel)
        self._regexp_button.setObjectName("toggleButton")
        self._regexp_button.setCheckable(True)
        self._regexp_button.toggled.connect(self._schedule_search)
        options_layout.addWidget(self._regexp_button)

        self._options_panel.hide()
        layout.addWidget(self._options_panel)

        self._status_label = QLabel(self)
        self._status_label.setObjectName("_status_label")
        layout.addWidget(self._status_label)

        self._results_tree = QTreeWidget(self)
        self._results_tree.setObjectName("MindspaceSearchResultsTree")
        self._results_tree.setHeaderHidden(True)
        self._results_tree.setRootIsDecorated(True)
        self._results_tree.setUniformRowHeights(False)
        self._results_tree.setIndentation(0)
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
            self._regexp_invalid = False
            self._status_label.setText("")
            self.apply_style()
            return

        if not query:
            self._regexp_invalid = False
            self._status_label.setText(self._language_manager.strings().global_search_empty_state)
            self.apply_style()
            return

        case_sensitive = self._match_case_button.isChecked()
        whole_word = self._whole_word_button.isChecked()
        regexp_enabled = self._regexp_button.isChecked()

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
        )
        if not matches:
            self._status_label.setText(self._language_manager.strings().global_search_no_results)
            self.apply_style()
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
        self.apply_style()

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
        self._options_button.setToolTip(strings.global_search_options)
        self._match_case_button.setToolTip(strings.find_match_case)
        self._whole_word_button.setToolTip(strings.global_search_whole_word)
        self._regexp_button.setToolTip(strings.find_use_regexp)
        self._match_case_button.setAccessibleName(strings.find_match_case)
        self._whole_word_button.setAccessibleName(strings.global_search_whole_word)
        self._regexp_button.setAccessibleName(strings.find_use_regexp)
        self._perform_search()
        self.apply_style()

    def _toggle_options_panel(self) -> None:
        """Expand or collapse the search options panel."""
        self._options_expanded = not self._options_expanded
        self._options_panel.setVisible(self._options_expanded)
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
        icon_size = round(16 * zoom_factor)
        self._results_tree.setIconSize(QSize(icon_size, icon_size))
        self._results_tree.setIndentation(round(14 * zoom_factor))
        self._options_button.setAutoRaise(True)
        self._match_case_button.setAutoRaise(True)
        self._whole_word_button.setAutoRaise(True)
        self._regexp_button.setAutoRaise(True)

        input_bg = self._style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
        input_border = self._style_manager.get_color_str(ColorRole.MENU_BORDER)
        input_error = self._style_manager.get_color_str(ColorRole.EDIT_BOX_ERROR)
        text = self._style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        subtle_text = self._style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
        button_hover = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)
        button_pressed = self._style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)
        button_checked = self._style_manager.get_color_str(ColorRole.TEXT_FOUND)
        m = round(10 * zoom_factor)
        m_sm = round(6 * zoom_factor)
        input_padding_y = round(7 * zoom_factor)
        input_padding_x = round(10 * zoom_factor)
        radius = round(8 * zoom_factor)
        btn_radius = round(5 * zoom_factor)
        btn_size = round(22 * zoom_factor)
        btn_padding = round(3 * zoom_factor)
        search_border = input_error if self._regexp_invalid else input_border

        options_icon_name = (
            "arrow-down"
            if self._options_expanded
            else ("arrow-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "arrow-left")
        )
        self._options_button.setIcon(QIcon(self._style_manager.scale_icon(options_icon_name, 14)))
        self._options_button.setIconSize(QSize(round(14 * zoom_factor), round(14 * zoom_factor)))
        option_icon_size = QSize(round(14 * zoom_factor), round(14 * zoom_factor))
        self._match_case_button.setIcon(QIcon(self._style_manager.scale_icon("find-match-case", 14)))
        self._match_case_button.setIconSize(option_icon_size)
        self._whole_word_button.setIcon(QIcon(self._style_manager.scale_icon("find-whole-word", 14)))
        self._whole_word_button.setIconSize(option_icon_size)
        self._regexp_button.setIcon(QIcon(self._style_manager.scale_icon("find-regexp", 14)))
        self._regexp_button.setIconSize(option_icon_size)

        self.setStyleSheet(
            build_tree_pane_stylesheet(
                self._style_manager,
                "MindspaceSearchView",
                "QTreeWidget#MindspaceSearchResultsTree",
                self.layoutDirection(),
                zoom_factor,
            )
            + f"""
            QWidget#_search_bar {{
                background: transparent;
                margin: {m}px {m}px {m_sm}px {m}px;
            }}
            QLineEdit#_search_input {{
                background-color: {input_bg};
                color: {text};
                border: 1px solid {search_border};
                border-right: none;
                border-top-left-radius: {radius}px;
                border-bottom-left-radius: {radius}px;
                border-top-right-radius: 0px;
                border-bottom-right-radius: 0px;
                padding: {input_padding_y}px {input_padding_x}px;
                min-height: {round(22 * zoom_factor)}px;
            }}
            QToolButton#_options_button {{
                background-color: {input_bg};
                color: {text};
                border-top: 1px solid {search_border};
                border-right: 1px solid {search_border};
                border-bottom: 1px solid {search_border};
                border-left: 1px solid {input_border};
                border-top-right-radius: {radius}px;
                border-bottom-right-radius: {radius}px;
                padding: {input_padding_y}px {round(7 * zoom_factor)}px;
                min-width: {round(24 * zoom_factor)}px;
                min-height: {round(22 * zoom_factor)}px;
                max-height: {round(22 * zoom_factor)}px;
            }}
            QToolButton#_options_button:hover {{
                background-color: {button_hover};
            }}
            QToolButton#_options_button:pressed {{
                background-color: {button_pressed};
            }}
            QWidget#_options_panel {{
                background-color: transparent;
                border: none;
                margin: {round(2 * zoom_factor)}px {m}px {m_sm}px {m}px;
            }}
            QWidget#_options_panel QToolButton#toggleButton {{
                color: {text};
                background-color: transparent;
                border: 1px solid {input_border};
                border-radius: {btn_radius}px;
                padding: {btn_padding}px;
                min-width: {btn_size}px;
                min-height: {btn_size}px;
                max-width: {btn_size}px;
                max-height: {btn_size}px;
            }}
            QWidget#_options_panel QToolButton#toggleButton:hover {{
                background-color: {button_hover};
            }}
            QWidget#_options_panel QToolButton#toggleButton:pressed {{
                background-color: {button_pressed};
            }}
            QWidget#_options_panel QToolButton#toggleButton:checked {{
                background-color: {button_checked};
                border-color: {button_checked};
                color: {text};
            }}
            QWidget#_options_panel QToolButton#toggleButton:checked:hover {{
                background-color: {button_checked};
            }}
            QLabel#_status_label {{
                color: {self._style_manager.get_color_str(ColorRole.TEXT_ERROR) if self._regexp_invalid else subtle_text};
                padding: {m_sm}px {m}px {m_sm}px {m}px;
                min-height: {round(16 * zoom_factor)}px;
            }}
            QTreeWidget#MindspaceSearchResultsTree {{
                background-color: {input_bg};
                color: {text};
                outline: none;
                border: 1px solid {input_border};
                border-radius: {radius}px;
                margin: 0px {m}px {m}px {m}px;
                padding: {m_sm}px 0px;
            }}
            QTreeWidget#MindspaceSearchResultsTree::item {{
                min-height: {round(22 * zoom_factor)}px;
                padding: {round(5 * zoom_factor)}px {round(8 * zoom_factor)}px;
                margin: 0px {m_sm}px;
                border-radius: {btn_radius}px;
            }}
            QTreeWidget#MindspaceSearchResultsTree::item:hover {{
                background-color: {button_hover};
            }}
            QTreeWidget#MindspaceSearchResultsTree::item:selected {{
                background-color: {button_checked};
                color: {text};
            }}
            """
        )
