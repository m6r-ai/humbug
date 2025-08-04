from datetime import datetime
import logging
from typing import Dict, List, Tuple

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QToolButton, QFileDialog, QPushButton
)
from PySide6.QtCore import Signal, QPoint, QSize, Qt, QRect, QObject
from PySide6.QtGui import QIcon, QGuiApplication, QPaintEvent, QResizeEvent, QColor, QPainter, QPen

from ai import AIMessageSource
from ai_tool import AIToolCall
from dmarkdown import MarkdownASTDocumentNode, MarkdownASTTextNode, MarkdownConverter
from syntax import ProgrammingLanguage

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType, MessageBoxButton
from humbug.min_height_text_edit import MinHeightTextEdit
from humbug.style_manager import StyleManager
from humbug.tabs.conversation.conversation_message_section import ConversationMessageSection


class ConversationMessage(QFrame):
    """Widget for displaying a single message in the conversation history with header."""

    selection_changed = Signal(bool)
    scroll_requested = Signal(QPoint)
    mouse_released = Signal()
    fork_requested = Signal()
    delete_requested = Signal()
    expand_requested = Signal(bool)
    tool_call_approved = Signal(AIToolCall)
    tool_call_rejected = Signal(str)

    def __init__(
        self,
        style: AIMessageSource,
        timestamp: datetime | None = None,
        model: str | None = None,
        message_id: str | None = None,
        user_name: str | None = None,
        content: str | None = None,
        parent: QWidget | None = None,
        is_input: bool = False
    ) -> None:
        """
        Initialize the message widget.

        Args:
            style: The style type ('user', 'ai', 'system', or 'error')
            timestamp: datetime object for the message timestamp
            model: Model name for the message
            message_id: Optional message ID for tracking
            user_name: Optional user name for the message
            content: Optional initial content for the message
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        self.setObjectName("ConversationMessage")

        self._is_input = is_input

        self._logger = logging.getLogger("ConversationMessage")

        self._language_manager = LanguageManager()
        self._language_manager.language_changed.connect(self._on_language_changed)
        self._message_source = style
        self._message_content = ""
        self._message_timestamp = timestamp
        self._message_model = model
        self._message_id = message_id
        self._message_user_name = user_name

        self._style_manager = StyleManager()

        self._message_rendered = True
        if not is_input and not content:
            self._message_rendered = False
            self.hide()

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        spacing = int(self._style_manager.message_bubble_spacing())
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header.setObjectName("_header")
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(4)

        # Add expand/collapse button for all messages (input and non-input)
        self._expand_button: QToolButton | None = None

        if not is_input:
            self._expand_button = QToolButton(self._header)
            self._expand_button.setObjectName("_expand_button")
            self._expand_button.clicked.connect(self._toggle_expanded)
            self._header_layout.addWidget(self._expand_button)

        # Create role and timestamp labels
        self._role_label = QLabel(self._header)
        self._role_label.setObjectName("_role_label")
        self._role_label.setIndent(0)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addStretch()

        role_sources = {
            AIMessageSource.USER: "user",
            AIMessageSource.AI: "ai",
            AIMessageSource.REASONING: "reasoning",
            AIMessageSource.TOOL_CALL: "tool_call",
            AIMessageSource.TOOL_RESULT: "tool_result",
            AIMessageSource.SYSTEM: "system"
        }

        current_style = self._message_source or AIMessageSource.USER
        role = role_sources.get(current_style, "user")
        self._role_label.setProperty("message_source", role)
        self.setProperty("message_source", role)

        self._copy_message_button: QToolButton | None = None
        self._save_message_button: QToolButton | None = None
        self._fork_message_button: QToolButton | None = None
        self._delete_message_button: QToolButton | None = None

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Container for message sections
        self._sections_container = QWidget(self)
        self._sections_container.setObjectName("_sections_container")
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)
        self._sections_layout.setSpacing(15)
        self._layout.addWidget(self._sections_container)

        # Tool approval widgets
        self._approval_widget: QWidget | None = None
        self._approval_text_edit: MinHeightTextEdit | None = None
        self._approval_approve_button: QPushButton | None = None
        self._approval_reject_button: QPushButton | None = None

        # Track sections
        self._sections: List[ConversationMessageSection] = []
        self._section_with_selection: ConversationMessageSection | None = None

        # Expanded state - default to True, will be updated in set_content based on message type
        self._is_expanded = True

        # If this is an input widget then create the input section
        if is_input:
            section = self._create_section_widget()
            self._sections.append(section)
            self._sections_layout.addWidget(section)

        # Initialize markdown converter
        self._markdown_converter = MarkdownConverter()

        self._is_focused = False
        self._is_bookmarked = False

        self._style_manager.style_changed.connect(self._on_style_changed)
        self._on_language_changed()

        self._needs_lazy_update = True

        # Set default expanded state based on message type
        # Tool calls and tool results should be collapsed by default
        default_expanded = style not in (AIMessageSource.TOOL_CALL, AIMessageSource.TOOL_RESULT)
        self.set_expanded(default_expanded)

        self._on_style_changed()

        if content:
            self.set_content(content)

    def paintEvent(self, arg__1: QPaintEvent) -> None:
        """Override paint event to paint custom borders."""
        super().paintEvent(arg__1)

        painter = QPainter(self)
        border_width = 2
        border_radius = int(self._style_manager.message_bubble_spacing() / 2)
        border_color = self._get_border_color()

        # Enable antialiasing for smooth curves
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)

        # Set up the pen for the border
        pen = QPen(QColor(border_color), border_width)
        pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
        painter.setPen(pen)

        # Don't fill the rectangle, just draw the border
        painter.setBrush(Qt.BrushStyle.NoBrush)

        # Adjust rectangle to center the border line
        # (QPainter draws the border centered on the rectangle edge)
        adjustment = border_width // 2
        adjusted_rect = self.rect().adjusted(adjustment, adjustment, -adjustment, -adjustment)

        # Draw the rounded rectangle border
        painter.drawRoundedRect(adjusted_rect, border_radius, border_radius)

    def is_rendered(self) -> bool:
        """Check if the message will be rendered."""
        return self._message_rendered

    def is_focused(self) -> bool:
        """Check if this message is focused."""
        return self._is_focused

    def set_focused(self, focused: bool) -> None:
        """Set the focused state of this message."""
        assert not self.isHidden(), "Focused message must not be hidden"

        if self._is_focused == focused:
            return

        self._is_focused = focused
        if focused:
            self.setFocus()

        self.style().unpolish(self)
        self.style().polish(self)

    def is_bookmarked(self) -> bool:
        """Check if this message is bookmarked."""
        return self._is_bookmarked

    def set_bookmarked(self, bookmarked: bool) -> None:
        """Set the bookmarked state."""
        self._is_bookmarked = bookmarked
        self.style().unpolish(self)
        self.style().polish(self)

    def is_expanded(self) -> bool:
        """Check if this message is expanded."""
        return self._is_expanded

    def set_expanded(self, expanded: bool) -> None:
        """
        Set the expanded state of this message.

        Args:
            expanded: Whether the message should be expanded
        """
        if self._is_expanded == expanded:
            return

        self._is_expanded = expanded

        if expanded:
            self._sections_container.show()

        else:
            self._sections_container.hide()

        self._update_expand_button()

    def _toggle_expanded(self) -> None:
        """Toggle the expanded state of this message."""
        self.set_expanded(not self._is_expanded)
        self.expand_requested.emit(self._is_expanded)

    def _update_expand_button(self) -> None:
        """Update the expand button icon and tooltip based on current state."""
        if not self._expand_button:
            return

        strings = self._language_manager.strings()

        if self._is_expanded:
            # Show down expand when expanded
            icon_name = "expand-down"
            tooltip = strings.tooltip_collapse_message

        else:
            # Show right expand when collapsed
            icon_name = "expand-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "expand-left"
            tooltip = strings.tooltip_expand_message

        # Update icon
        icon_base_size = 14
        self._expand_button.setIcon(QIcon(self._style_manager.scale_icon(
            self._style_manager.get_icon_path(icon_name), icon_base_size
        )))

        # Update tooltip
        self._expand_button.setToolTip(tooltip)

    def _on_language_changed(self) -> None:
        """Update text when language changes."""
        if self._is_input:
            return

        self._update_role_text()

        strings = self._language_manager.strings()
        if self._copy_message_button:
            self._copy_message_button.setToolTip(strings.tooltip_copy_message)

        if self._save_message_button:
            self._save_message_button.setToolTip(strings.tooltip_save_message)

        if self._fork_message_button:
            self._fork_message_button.setToolTip(strings.tooltip_fork_message)

        if self._delete_message_button:
            self._delete_message_button.setToolTip(strings.tooltip_delete_from_message)

        if self._approval_approve_button:
            self._approval_approve_button.setText(strings.approve_tool_call)

        if self._approval_reject_button:
            self._approval_reject_button.setText(strings.reject_tool_call)

        # Update expand button tooltip
        self._update_expand_button()

    def _update_role_text(self) -> None:
        """Update the role text based on current language."""
        if not self._message_source:
            return

        strings = self._language_manager.strings()
        match self._message_source:
            case AIMessageSource.USER:
                if self._message_user_name:
                    role_text = self._message_user_name

                else:
                    role_text = strings.role_you

            case AIMessageSource.AI:
                role_text = strings.role_assistant.format(model=self._message_model)

            case AIMessageSource.REASONING:
                role_text = strings.role_reasoning.format(model=self._message_model)

            case AIMessageSource.SYSTEM:
                role_text = strings.role_system

            case AIMessageSource.TOOL_CALL:
                role_text = strings.role_tool_call

            case AIMessageSource.TOOL_RESULT:
                role_text = strings.role_tool_result

        # Format with timestamp
        if self._message_timestamp is not None:
            timestamp_str = self._message_timestamp.strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]
            self._role_label.setText(f"{role_text} @ {timestamp_str}")

        else:
            self._role_label.setText(role_text)

    def _create_section_widget(self, language: ProgrammingLanguage | None = None) -> ConversationMessageSection:
        """
        Create a new section widget.

        Args:
            language: Optional programming language for the section

        Returns:
            A new ConversationMessageSection instance
        """
        section = ConversationMessageSection(self._is_input, language, self._sections_container)
        section.setObjectName("ConversationMessageSection")

        section.selection_changed.connect(
            lambda has_selection: self._handle_section_selection_changed(section, has_selection)
        )
        section.scroll_requested.connect(self.scroll_requested)
        section.mouse_released.connect(self.mouse_released)

        # Determine style class
        is_user_message = self._message_source == AIMessageSource.USER
        if language is not None:
            # Code block
            style_class = "code-user" if is_user_message else "code-system"

        else:
            # Text section
            style_class = "text-user" if is_user_message else "text-system"

        # Set property that QSS will match against
        section.setProperty("section_style", style_class)
        section.apply_style()

        return section

    def lazy_update(self, viewport_rect: QRect, scroll_container: QWidget, event_filter: QObject) -> None:
        """
        Handle lazy updates for sections based on viewport visibility.

        Args:
            viewport_rect: The visible viewport rectangle in scroll container coordinates
            scroll_container: The scroll container widget for coordinate mapping
        """
        # Cascade lazy updates to all sections
        for section in self._sections:
            if not section.isVisible():
                continue

            # Get section position relative to scroll container
            section_pos = section.mapTo(scroll_container, QPoint(0, 0))
            section_rect = QRect(section_pos, section.size())

            # Only create highlighter if section intersects with viewport
            if section_rect.intersects(viewport_rect):
                section.lazy_update(event_filter)

        if not self._needs_lazy_update:
            return

        self._needs_lazy_update = False

        # Add fork button only for AI messages
        style = self._message_source
        if style == AIMessageSource.AI:
            strings = self._language_manager.strings()
            self._fork_message_button = QToolButton(self)
            self._fork_message_button.setObjectName("_fork_button")
            self._fork_message_button.clicked.connect(self._fork_message)
            self._fork_message_button.setToolTip(strings.tooltip_fork_message)
            self._fork_message_button.installEventFilter(event_filter)
            self._header_layout.addWidget(self._fork_message_button)

        # Add delete button only for user messages
        elif style == AIMessageSource.USER and not self._is_input:
            strings = self._language_manager.strings()
            self._delete_message_button = QToolButton(self)
            self._delete_message_button.setObjectName("_delete_button")
            self._delete_message_button.clicked.connect(self._delete_message)
            self._delete_message_button.setToolTip(strings.tooltip_delete_from_message)
            self._delete_message_button.installEventFilter(event_filter)
            self._header_layout.addWidget(self._delete_message_button)

        # We have copy and save buttons for several message sources
        if style in (AIMessageSource.USER, AIMessageSource.AI, AIMessageSource.REASONING) and not self._is_input:
            self._copy_message_button = QToolButton(self)
            self._copy_message_button.setObjectName("_copy_button")
            self._copy_message_button.clicked.connect(self._copy_message)
            self._copy_message_button.installEventFilter(event_filter)
            self._header_layout.addWidget(self._copy_message_button)

            self._save_message_button = QToolButton(self)
            self._save_message_button.setObjectName("_save_button")
            self._save_message_button.clicked.connect(self._save_message)
            self._save_message_button.installEventFilter(event_filter)
            self._header_layout.addWidget(self._save_message_button)

        self._apply_button_style()

    def _handle_section_selection_changed(self, section: ConversationMessageSection, has_selection: bool) -> None:
        """
        Handle selection changes in a section widget.

        Args:
            section: The section widget where selection changed
            has_selection: Whether there is a selection
        """
        if not has_selection:
            if self._section_with_selection == section:
                self._section_with_selection = None

            return

        # Clear selection in other sections
        if self._section_with_selection and self._section_with_selection != section:
            self._section_with_selection.clear_selection()

        self._section_with_selection = section
        self.selection_changed.emit(has_selection)

    def show_tool_approval_ui(self, tool_call: AIToolCall, reason: str, destructive: bool) -> None:
        """
        Show tool approval UI for the given tool calls.

        Args:
            tool_call: Tool call that needs approval
            reason: Reason for the tool call
            destructive: Whether the tool calls are destructive
        """
        assert self._approval_widget is None, "Approval widget already exists"

        self._approval_widget = QWidget()
        self._approval_widget.setObjectName("_approval_widget")
        layout = QVBoxLayout(self._approval_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(8)
        strings = self._language_manager.strings()

        self._approval_text_edit = MinHeightTextEdit()
        self._approval_text_edit.setObjectName("_approval_text_edit")
        self._approval_text_edit.set_text(reason)
        self._approval_text_edit.setReadOnly(True)
        layout.addWidget(self._approval_text_edit)

        # Approval buttons
        button_layout = QHBoxLayout()
        button_layout.setSpacing(10)
        button_layout.addStretch()

        min_button_height = 40
        zoom_factor = self._style_manager.zoom_factor()
        min_button_width = int(180 * zoom_factor)

        self._approval_approve_button = QPushButton(strings.approve_tool_call)
        self._approval_approve_button.setObjectName("_approval_approve_button")
        self._approval_approve_button.clicked.connect(lambda: self._approve_tool_call(tool_call))
        self._approval_approve_button.setMinimumWidth(min_button_width)
        self._approval_approve_button.setMinimumHeight(min_button_height)
        self._approval_approve_button.setProperty("recommended", not destructive)
        self._approval_approve_button.setContentsMargins(8, 8, 8, 8)

        self._approval_reject_button = QPushButton(strings.reject_tool_call)
        self._approval_reject_button.setObjectName("_approval_reject_button")
        self._approval_reject_button.clicked.connect(self._reject_tool_call)
        self._approval_reject_button.setMinimumWidth(min_button_width)
        self._approval_reject_button.setMinimumHeight(min_button_height)
        self._approval_reject_button.setContentsMargins(8, 8, 8, 8)

        button_layout.addWidget(self._approval_approve_button)
        button_layout.addWidget(self._approval_reject_button)
        button_layout.addStretch()

        layout.addSpacing(2)
        layout.addLayout(button_layout)

        self._layout.addWidget(self._approval_widget)

    def _approve_tool_call(self, tool_call: AIToolCall) -> None:
        """Handle tool call approval."""
        self.tool_call_approved.emit(tool_call)
        self._remove_approval_widget()

    def _reject_tool_call(self) -> None:
        """Handle tool call rejection."""
        self.tool_call_rejected.emit("Tool call was rejected by the user")
        self._remove_approval_widget()

    def _remove_approval_widget(self) -> None:
        """Remove the approval widget."""
        if self._approval_widget:
            self._layout.removeWidget(self._approval_widget)
            self._approval_widget.deleteLater()
            self._approval_widget = None
            self._approval_approve_button = None
            self._approval_reject_button = None

    def _get_border_color(self) -> str:
        """Get the border color based on current state."""
        if self._is_focused and self.hasFocus():
            return self._style_manager.get_color_str(ColorRole.MESSAGE_FOCUSED)

        if self._is_bookmarked:
            return self._style_manager.get_color_str(ColorRole.MESSAGE_BOOKMARK)

        current_style = self._message_source or AIMessageSource.USER
        return self._style_manager.get_color_str(
            ColorRole.MESSAGE_USER_BACKGROUND if current_style == AIMessageSource.USER else ColorRole.MESSAGE_BACKGROUND
        )

    def set_content(self, text: str) -> None:
        """
        Set content with style, handling incremental updates for AI responses.

        Args:
            text: The message text content
        """
        self._message_content = text

        # Extract sections directly using the markdown converter
        if not self._is_input:
            # Process the content and extract sections in one step
            sections_data = self._markdown_converter.extract_sections(text, None)

        else:
            # Input widgets don't use markdown processing
            text_node = MarkdownASTDocumentNode()
            text_node.add_child(MarkdownASTTextNode(text))
            sections_data = [(text_node, None)]

        # Create or update sections
        for i, (node, language) in enumerate(sections_data):
            # Create new section if needed
            if i >= len(self._sections):
                section = self._create_section_widget(language)
                self._sections.append(section)
                self._sections_layout.addWidget(section)

                section.set_content(node)
                continue

            if i == len(self._sections) - 1:
                # Update the last section with new content
                section = self._sections[-1]
                if language != section.language():
                    section.set_language(language)

                section.set_content(node)

        # Remove any extra sections
        while len(self._sections) > len(sections_data):
            section = self._sections.pop()
            self._sections_layout.removeWidget(section)
            section.deleteLater()

        # Show the message if it has text
        if text:
            self._message_rendered = True
            self.show()

    def message_id(self) -> str | None:
        """Get the unique message ID."""
        return self._message_id

    def _copy_message(self) -> None:
        """Copy the entire message content to clipboard."""
        content = self._message_content
        QGuiApplication.clipboard().setText(content)

    def _save_message(self) -> bool:
        """Show save as dialog and save message as a markdown file."""
        strings = self._language_manager.strings()

        # Show file dialog
        export_dialog = QFileDialog()
        export_dialog.setWindowTitle(strings.file_dialog_save_file)
        export_dialog.setAcceptMode(QFileDialog.AcceptMode.AcceptSave)
        export_dialog.selectFile("message.md")

        if export_dialog.exec_() != QFileDialog.DialogCode.Accepted:
            return False

        filename = export_dialog.selectedFiles()[0]

        # Save the file
        try:
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(self._message_content)

            return True

        except Exception as e:
            self._logger.error("Failed to save message: %s", str(e))
            MessageBox.show_message(
                self,
                MessageBoxType.CRITICAL,
                strings.error_saving_file_title,
                strings.could_not_save.format(filename, str(e))
            )
            return False

    def _fork_message(self) -> None:
        """Fork the conversation at this message."""
        self.fork_requested.emit()

    def _delete_message(self) -> None:
        """Delete this message from the conversation."""
        strings = self._language_manager.strings()

        # Show confirmation dialog using the application's MessageBox class
        result = MessageBox.show_message(
            self,  # parent widget
            MessageBoxType.QUESTION,
            strings.delete_from_here_title,
            strings.delete_from_here_message,
            [MessageBoxButton.YES, MessageBoxButton.NO],
            destructive=True
        )

        if result != MessageBoxButton.YES:
            return

        self.delete_requested.emit()

    def has_selection(self) -> bool:
        """Check if any section has selected text."""
        return self._section_with_selection is not None and self._section_with_selection.has_selection()

    def get_selected_text(self) -> str:
        """
        Get any selected text in this message.

        Returns:
            Currently selected text or empty string
        """
        if self._section_with_selection:
            return self._section_with_selection.get_selected_text()

        return ""

    def copy_selection(self) -> None:
        """Copy selected text to clipboard."""
        if self._section_with_selection:
            self._section_with_selection.copy_selection()

    def clear_selection(self) -> None:
        """Clear any text selection in this message."""
        if self._section_with_selection:
            self._section_with_selection.clear_selection()
            self._section_with_selection = None

    def resizeEvent(self, event: QResizeEvent) -> None:
        """Handle resize events."""
        super().resizeEvent(event)

    def _apply_button_style(self) -> None:
        """Apply the current style to all buttons."""
        style_manager = self._style_manager

        # Set icons and sizes for buttons
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * style_manager.zoom_factor())
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        if self._copy_message_button:
            self._copy_message_button.setIcon(QIcon(style_manager.scale_icon(
                style_manager.get_icon_path("copy"), icon_base_size
            )))
            self._copy_message_button.setIconSize(icon_size)

        if self._save_message_button:
            self._save_message_button.setIcon(QIcon(style_manager.scale_icon(
                style_manager.get_icon_path("save"), icon_base_size
            )))
            self._save_message_button.setIconSize(icon_size)

        if self._fork_message_button:
            self._fork_message_button.setIcon(QIcon(style_manager.scale_icon(
                style_manager.get_icon_path("fork"), icon_base_size
            )))
            self._fork_message_button.setIconSize(icon_size)

        if self._delete_message_button:
            self._delete_message_button.setIcon(QIcon(style_manager.scale_icon(
                style_manager.get_icon_path("delete"), icon_base_size
            )))
            self._delete_message_button.setIconSize(icon_size)

        if self._expand_button:
            self._expand_button.setIconSize(icon_size)
            # Update the expand button icon and tooltip
            self._update_expand_button()


    def _on_style_changed(self) -> None:
        """Handle the style changing."""
        style_manager = self._style_manager

        factor = style_manager.zoom_factor()
        font = self.font()
        base_font_size = style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

        self._role_label.setFont(font)

        self._apply_button_style()

        # Apply fonts to approval buttons if present
        if self._approval_approve_button:
            self._approval_approve_button.setFont(font)

        if self._approval_reject_button:
            self._approval_reject_button.setFont(font)

        if self._approval_text_edit:
            self._approval_text_edit.setFont(font)

        # Apply styling to all sections
        for section in self._sections:
            section.apply_style()

    def find_text(self, text: str) -> List[Tuple[int, int, int]]:
        """
        Find all instances of text in this message.

        Args:
            text: Text to search for

        Returns:
            List of (section, start_position, end_position) tuples for each match
        """
        all_matches: List[Tuple[int, int, int]] = []
        for i, section in enumerate(self._sections):
            section_matches = section.find_text(text)
            if section_matches:
                # Include the section with each match
                for match in section_matches:
                    all_matches.append((i, match[0], match[1]))

        return all_matches

    def highlight_matches(
        self,
        matches: List[Tuple[int, int, int]],
        current_match_index: int = -1,
        highlight_color: QColor | None = None,
        dim_highlight_color: QColor | None = None
    ) -> None:
        """
        Highlight matches in this message.

        Args:
            matches: List of (section, start_position, end_position) tuples to highlight
            current_match_index: Index of current match to highlight differently, or -1 for none
            highlight_color: QColor for current match, defaults to system highlight color
            dim_highlight_color: QColor for other matches, defaults to dimmer highlight color
        """
        # First clear all highlights
        self.clear_highlights()

        if not matches:
            return

        # Group matches by section
        section_matches: Dict[ConversationMessageSection, List[Tuple[int, int, int]]] = {}
        for section in self._sections:
            section_matches[section] = []

        # Distribute matches to their respective sections
        for i, match in enumerate(matches):
            section_num, start, end = match
            section = self._sections[section_num]
            if section in section_matches:
                section_matches[section].append((start, end, i))

        # Highlight matches in each section
        for section, section_matches_list in section_matches.items():
            if not section_matches_list:
                continue

            # Extract position tuples (without the index)
            positions = [(start, end) for start, end, _ in section_matches_list]

            # Find if current match is in this section
            section_current_idx = -1
            for i, (_, _, idx) in enumerate(section_matches_list):
                if idx == current_match_index:
                    section_current_idx = i
                    break

            # Highlight this section's matches
            section.highlight_matches(
                positions,
                section_current_idx,
                highlight_color,
                dim_highlight_color
            )

    def clear_highlights(self) -> None:
        """Clear all highlights from the message."""
        for section in self._sections:
            section.clear_highlights()

    def select_and_scroll_to_position(self, section_num: int, position: int) -> QPoint:
        """
        Select text and get position for scrolling.

        Args:
            section_num: Section number to scroll to
            position: Text position to scroll to

        Returns:
            QPoint: Position to scroll to, relative to this widget
        """
        if 0 <= section_num < len(self._sections):
            section = self._sections[section_num]
            # Get position relative to the section
            pos_in_section = section.select_and_scroll_to_position(position)

            # Map from section to this widget's coordinates
            return section.mapTo(self, pos_in_section)

        return QPoint(0, 0)
