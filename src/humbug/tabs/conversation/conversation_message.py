from datetime import datetime
import logging
from typing import Dict, List, Tuple

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QToolButton, QFileDialog, QPushButton
)
from PySide6.QtCore import Signal, QPoint, QSize, Qt
from PySide6.QtGui import QIcon, QGuiApplication, QResizeEvent, QColor

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
            parent: Optional parent widget
            is_input: Whether this is an input widget (affects styling)
        """
        super().__init__(parent)
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Plain)

        # Set object name for QSS targeting
        self.setObjectName("conversationMessage")

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

        if not is_input:
            self.setVisible(False)

        # Create layout
        self._layout = QVBoxLayout(self)
        self.setLayout(self._layout)
        spacing = int(self._style_manager.message_bubble_spacing())
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(spacing, spacing, spacing, spacing)

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header.setObjectName("messageHeader")
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(4)

        # Add expand/collapse button for all messages (input and non-input)
        self._expand_button: QToolButton | None = None

        if not is_input:
            self._expand_button = QToolButton(self)
            self._expand_button.setObjectName("expandButton")
            self._expand_button.clicked.connect(self._toggle_expanded)
            self._header_layout.addWidget(self._expand_button)

        # Create role and timestamp labels
        self._role_label = QLabel(self)
        self._role_label.setObjectName("roleLabel")
        self._role_label.setIndent(0)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addStretch()

        self._copy_message_button: QToolButton | None = None
        self._save_message_button: QToolButton | None = None
        self._fork_message_button: QToolButton | None = None
        self._delete_message_button: QToolButton | None = None

        # Add header widget to main layout
        self._layout.addWidget(self._header)

        # Container for message sections
        self._sections_container = QWidget(self)
        self._sections_container.setObjectName("sectionsContainer")
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)
        self._sections_layout.setSpacing(15)
        self._layout.addWidget(self._sections_container)

        # Tool approval widgets
        self._approval_widget: QWidget | None = None
        self._text_edit: MinHeightTextEdit | None = None
        self._approve_button: QPushButton | None = None
        self._reject_button: QPushButton | None = None

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

        # Add fork button only for AI messages
        if style == AIMessageSource.AI:
            strings = self._language_manager.strings()
            self._fork_message_button = QToolButton(self)
            self._fork_message_button.setObjectName("forkButton")
            self._fork_message_button.clicked.connect(self._fork_message)
            self._fork_message_button.setToolTip(strings.tooltip_fork_message)
            self._header_layout.addWidget(self._fork_message_button)

        # Add delete button only for user messages
        elif style == AIMessageSource.USER and not self._is_input:
            strings = self._language_manager.strings()
            self._delete_message_button = QToolButton(self)
            self._delete_message_button.setObjectName("deleteButton")
            self._delete_message_button.clicked.connect(self._delete_message)
            self._delete_message_button.setToolTip(strings.tooltip_delete_from_message)
            self._header_layout.addWidget(self._delete_message_button)

        # We have copy and save buttons for several message sources
        if style in (AIMessageSource.USER, AIMessageSource.AI, AIMessageSource.REASONING) and not self._is_input:
            self._copy_message_button = QToolButton(self)
            self._copy_message_button.setObjectName("copyButton")
            self._copy_message_button.clicked.connect(self._copy_message)
            self._header_layout.addWidget(self._copy_message_button)

            self._save_message_button = QToolButton(self)
            self._save_message_button.setObjectName("saveButton")
            self._save_message_button.clicked.connect(self._save_message)
            self._header_layout.addWidget(self._save_message_button)

        # Set default expanded state based on message type
        # Tool calls and tool results should be collapsed by default
        default_expanded = style not in (AIMessageSource.TOOL_CALL, AIMessageSource.TOOL_RESULT)
        self.set_expanded(default_expanded)

        self._on_style_changed()

    def is_focused(self) -> bool:
        """Check if this message is focused."""
        return self._is_focused

    def set_focused(self, focused: bool) -> None:
        """Set the focused state of this message."""
        assert self.isVisible(), "Focused message must be visible"

        if self._is_focused == focused:
            return

        self._is_focused = focused
        if focused:
            self.setFocus()

        self._apply_shared_stylesheet()

    def is_bookmarked(self) -> bool:
        """Check if this message is bookmarked."""
        return self._is_bookmarked

    def set_bookmarked(self, bookmarked: bool) -> None:
        """Set the bookmarked state."""
        self._is_bookmarked = bookmarked
        self._apply_shared_stylesheet()

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
        self._sections_container.setVisible(expanded)
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

        if self._approve_button:
            self._approve_button.setText(strings.approve_tool_call)

        if self._reject_button:
            self._reject_button.setText(strings.reject_tool_call)

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
        section.setObjectName("messageSection")

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
        section.setProperty("sectionStyle", style_class)

        factor = self._style_manager.zoom_factor()
        font = self.font()
        base_font_size = self._style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)

        section.apply_style(font)

        return section

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

        self._approval_widget = self._create_tool_approval_widget(tool_call, reason, destructive)
        self._layout.addWidget(self._approval_widget)
#        self._apply_shared_stylesheet

    def _create_tool_approval_widget(self, tool_call: AIToolCall, reason: str, destructive: bool) -> QWidget:
        """Create widget for tool call approval."""
        approval_widget = QWidget()
        approval_widget.setObjectName("approvalWidget")
        layout = QVBoxLayout(approval_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(8)
        strings = self._language_manager.strings()

        self._text_edit = MinHeightTextEdit()
        self._text_edit.setObjectName("approvalTextEdit")
        self._text_edit.set_text(reason)
        self._text_edit.setReadOnly(True)
        layout.addWidget(self._text_edit)

        # Approval buttons
        button_layout = QHBoxLayout()
        button_layout.setSpacing(10)
        button_layout.addStretch()

        min_button_height = 40
        zoom_factor = self._style_manager.zoom_factor()
        min_button_width = int(180 * zoom_factor)

        self._approve_button = QPushButton(strings.approve_tool_call)
        self._approve_button.setObjectName("approveButton")
        self._approve_button.clicked.connect(lambda: self._approve_tool_call(tool_call))
        self._approve_button.setMinimumWidth(min_button_width)
        self._approve_button.setMinimumHeight(min_button_height)
        self._approve_button.setProperty("recommended", not destructive)
        self._approve_button.setContentsMargins(8, 8, 8, 8)

        self._reject_button = QPushButton(strings.reject_tool_call)
        self._reject_button.setObjectName("rejectButton")
        self._reject_button.clicked.connect(self._reject_tool_call)
        self._reject_button.setMinimumWidth(min_button_width)
        self._reject_button.setMinimumHeight(min_button_height)
        self._reject_button.setContentsMargins(8, 8, 8, 8)

        button_layout.addWidget(self._approve_button)
        button_layout.addWidget(self._reject_button)
        button_layout.addStretch()

        layout.addSpacing(2)
        layout.addLayout(button_layout)

        return approval_widget

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
            self._approve_button = None
            self._reject_button = None

    def _get_color_palette(self) -> Dict[str, str]:
        """Get all colors needed for styling in one place."""
        style_manager = self._style_manager
        return {
            'text_primary': style_manager.get_color_str(ColorRole.TEXT_PRIMARY),
            'text_selected': style_manager.get_color_str(ColorRole.TEXT_SELECTED),
            'text_recommended': style_manager.get_color_str(ColorRole.TEXT_RECOMMENDED),
            'language_color': style_manager.get_color_str(ColorRole.MESSAGE_LANGUAGE),
            'light_bg': style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND),
            'dark_bg': style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND),
            'code_bg': style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY),
            'button_hover': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER),
            'button_pressed': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED),
            'button_secondary_bg': style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND),
            'button_secondary_hover': style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_HOVER),
            'button_secondary_pressed': style_manager.get_color_str(ColorRole.BUTTON_SECONDARY_BACKGROUND_PRESSED),
            'button_recommended_bg': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED),
            'button_recommended_hover': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_HOVER),
            'button_recommended_pressed': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_RECOMMENDED_PRESSED),
            'button_destructive_bg': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE),
            'button_destructive_hover': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_HOVER),
            'button_destructive_pressed': style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED),
            'scrollbar_bg': style_manager.get_color_str(ColorRole.SCROLLBAR_BACKGROUND),
            'scrollbar_handle': style_manager.get_color_str(ColorRole.SCROLLBAR_HANDLE),
            'message_focused': style_manager.get_color_str(ColorRole.MESSAGE_FOCUSED),
            'message_bookmark': style_manager.get_color_str(ColorRole.MESSAGE_BOOKMARK),
        }

    def _get_role_color(self) -> str:
        """Get the role color for the current message type."""
        role_colours = {
            AIMessageSource.USER: ColorRole.MESSAGE_USER,
            AIMessageSource.AI: ColorRole.MESSAGE_AI,
            AIMessageSource.REASONING: ColorRole.MESSAGE_REASONING,
            AIMessageSource.TOOL_CALL: ColorRole.MESSAGE_TOOL_CALL,
            AIMessageSource.TOOL_RESULT: ColorRole.MESSAGE_TOOL_RESULT,
            AIMessageSource.SYSTEM: ColorRole.MESSAGE_SYSTEM_ERROR
        }

        current_style = self._message_source or AIMessageSource.USER
        role = role_colours.get(current_style, ColorRole.MESSAGE_USER)
        return self._style_manager.get_color_str(role)

    def _get_background_color(self) -> str:
        """Get the background color for the current message type."""
        current_style = self._message_source or AIMessageSource.USER
        return self._style_manager.get_color_str(
            ColorRole.MESSAGE_USER_BACKGROUND if current_style == AIMessageSource.USER else ColorRole.MESSAGE_BACKGROUND
        )

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

    def _build_message_frame_styles(self, colors: Dict[str, str]) -> str:
        """Build styles for the main message frame."""
        background_color = self._get_background_color()
        border_color = self._get_border_color()
        border_radius = int(self._style_manager.message_bubble_spacing())

        return f"""
            QFrame#conversationMessage {{
                background-color: {background_color};
                margin: 0;
                border-radius: {border_radius}px;
                border: 2px solid {border_color};
            }}

            QWidget#messageHeader,
            QWidget#sectionsContainer {{
                background-color: {background_color};
                border: none;
                border-radius: 0;
                padding: 0;
                margin: 0;
            }}
        """

    def _build_header_styles(self, colors: Dict[str, str]) -> str:
        """Build styles for the header area."""
        role_color = self._get_role_color()
        background_color = self._get_background_color()

        return f"""
            QLabel#roleLabel {{
                color: {role_color};
                margin: 0;
                padding: 0;
                background-color: {background_color};
            }}
        """

    def _build_button_styles(self, colors: Dict[str, str]) -> str:
        """Build styles for all buttons in the message."""
        background_color = self._get_background_color()

        return f"""
            QToolButton#expandButton,
            QToolButton#copyButton,
            QToolButton#saveButton,
            QToolButton#forkButton,
            QToolButton#deleteButton {{
                background-color: {background_color};
                color: {colors['text_primary']};
                border: none;
                padding: 0px;
                margin: 0px;
            }}

            QToolButton#copyButton:hover,
            QToolButton#saveButton:hover,
            QToolButton#forkButton:hover,
            QToolButton#deleteButton:hover {{
                background-color: {colors['button_hover']};
            }}

            QToolButton#copyButton:pressed,
            QToolButton#saveButton:pressed,
            QToolButton#forkButton:pressed,
            QToolButton#deleteButton:pressed {{
                background-color: {colors['button_pressed']};
            }}
        """

    def _build_approval_styles(self, colors: Dict[str, str]) -> str:
        """Build styles for tool approval widgets."""
        background_color = self._get_background_color()

        return f"""
            QWidget#approvalWidget {{
                background-color: {background_color};
            }}

            QTextEdit#approvalTextEdit {{
                color: {colors['text_primary']};
                background-color: {background_color};
                border: none;
                border-radius: 0px;
                padding: 0;
                margin: 0;
            }}

            QPushButton#approveButton,
            QPushButton#rejectButton {{
                background-color: {colors['button_secondary_bg']};
                color: {colors['text_primary']};
                border-radius: 4px;
            }}

            QPushButton#approveButton:hover,
            QPushButton#rejectButton:hover {{
                background-color: {colors['button_secondary_hover']};
            }}

            QPushButton#approveButton:pressed,
            QPushButton#rejectButton:pressed {{
                background-color: {colors['button_secondary_pressed']};
            }}

            QPushButton#approveButton[recommended="true"] {{
                background-color: {colors['button_recommended_bg']};
                color: {colors['text_recommended']};
            }}

            QPushButton#approveButton[recommended="true"]:hover {{
                background-color: {colors['button_recommended_hover']};
            }}

            QPushButton#approveButton[recommended="true"]:pressed {{
                background-color: {colors['button_recommended_pressed']};
            }}

            QPushButton#approveButton[recommended="false"] {{
                background-color: {colors['button_destructive_bg']};
                color: {colors['text_recommended']};
            }}

            QPushButton#approveButton[recommended="false"]:hover {{
                background-color: {colors['button_destructive_hover']};
            }}

            QPushButton#approveButton[recommended="false"]:pressed {{
                background-color: {colors['button_destructive_pressed']};
            }}
        """

    def _build_section_styles(self, colors: Dict[str, str]) -> str:
        """Build styles for message sections."""
        border_radius = int(self._style_manager.message_bubble_spacing() / 2)
        return f"""
            QFrame#messageSection[sectionStyle="text-system"] {{
                background-color: {colors['light_bg']};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}
            QFrame#messageSection[sectionStyle="text-user"] {{
                background-color: {colors['dark_bg']};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}
            QFrame#messageSection[sectionStyle="code-system"] {{
                background-color: {colors['code_bg']};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}
            QFrame#messageSection[sectionStyle="code-user"] {{
                background-color: {colors['code_bg']};
                margin: 0;
                border-radius: {border_radius}px;
                border: 0;
            }}

            /* Text areas within message sections */
            QFrame#messageSection QTextEdit {{
                color: {colors['text_primary']};
                background-color: transparent;
                border: none;
                padding: 0;
                margin: 0;
                selection-background-color: {colors['text_selected']};
            }}

            /* Labels (language headers) within message sections */
            QFrame#messageSection QLabel {{
                color: {colors['language_color']};
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            /* Header containers within message sections */
            QFrame#messageSection QWidget {{
                background-color: transparent;
                margin: 0;
                padding: 0;
            }}

            /* Buttons within message sections */
            QFrame#messageSection QToolButton {{
                background-color: transparent;
                color: {colors['text_primary']};
                border: none;
                padding: 0px;
            }}
            QFrame#messageSection QToolButton:hover {{
                background-color: {colors['button_hover']};
            }}
            QFrame#messageSection QToolButton:pressed {{
                background-color: {colors['button_pressed']};
            }}

            /* Scrollbars within message sections */
            QFrame#messageSection QScrollBar:horizontal {{
                height: 12px;
                background: {colors['scrollbar_bg']};
            }}
            QFrame#messageSection QScrollBar::handle:horizontal {{
                background: {colors['scrollbar_handle']};
                min-width: 20px;
            }}
            QFrame#messageSection QScrollBar::add-page:horizontal, QFrame#messageSection QScrollBar::sub-page:horizontal {{
                background: none;
            }}
            QFrame#messageSection QScrollBar::add-line:horizontal, QFrame#messageSection QScrollBar::sub-line:horizontal {{
                width: 0px;
            }}
        """

    def _apply_shared_stylesheet(self) -> None:
        """Apply the shared stylesheet to this message."""
        # Calculate all colors once
        colors = self._get_color_palette()

        # Build sections: message frame, header, buttons, sections, approval UI
        stylesheet_parts = [
            self._build_message_frame_styles(colors),
            self._build_header_styles(colors),
            self._build_button_styles(colors),
            self._build_approval_styles(colors),
            self._build_section_styles(colors)
        ]

        shared_stylesheet = "\n".join(stylesheet_parts)
        self.setStyleSheet(shared_stylesheet)

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
            self.setVisible(True)

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

    def _on_style_changed(self) -> None:
        """Handle the style changing."""
        style_manager = self._style_manager

        factor = style_manager.zoom_factor()
        font = self.font()
        base_font_size = style_manager.base_font_size()
        font.setPointSizeF(base_font_size * factor)
        self.setFont(font)

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

        # Apply fonts to approval buttons if present
        if self._approve_button:
            self._approve_button.setFont(font)

        if self._reject_button:
            self._reject_button.setFont(font)

        if self._text_edit:
            self._text_edit.setFont(font)

        # Apply styling to all sections
        for section in self._sections:
            section.apply_style(font)

        self._apply_shared_stylesheet()

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
