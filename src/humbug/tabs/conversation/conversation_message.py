from datetime import datetime
import logging
from typing import Dict, List, Tuple
import colorsys

from PySide6.QtWidgets import (
    QFrame, QVBoxLayout, QLabel, QHBoxLayout, QWidget, QToolButton, QFileDialog, QPushButton, QApplication
)
from PySide6.QtCore import Signal, QPoint, QSize, Qt
from PySide6.QtGui import QIcon, QGuiApplication, QPaintEvent, QColor, QPainter, QPen

from ai import AIMessageSource
from ai_tool import AIToolCall
from dmarkdown import MarkdownASTTextNode, MarkdownConverter
from syntax import ProgrammingLanguage

from humbug.color_role import ColorRole
from humbug.language.language_manager import LanguageManager
from humbug.message_box import MessageBox, MessageBoxType, MessageBoxButton
from humbug.tabs.code_block_highlighter import CodeBlockHighlighter
from humbug.tabs.markdown_text_edit import MarkdownTextEdit
from humbug.style_manager import StyleManager, ColorMode
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
    tool_call_i_am_unsure = Signal()
    tool_call_rejected = Signal(str)

    def __init__(
        self,
        style: AIMessageSource,
        timestamp: datetime | None = None,
        model: str | None = None,
        message_id: str | None = None,
        user_name: str | None = None,
        content: str | None = None,
        context: str | None = None,
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
            context: Optional context for the message
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

        style_manager = StyleManager()
        self._style_manager = style_manager

        # Border animation state
        self._is_border_animated = False
        self._animation_frame = 0
        self._animation_steps = 64

        self._message_rendered = True
        if not is_input and not content:
            self._message_rendered = False
            self.hide()

        # Create header area with horizontal layout
        self._header = QWidget(self)
        self._header.setObjectName("_header")
        self._header_layout = QHBoxLayout(self._header)
        self._header_layout.setContentsMargins(0, 0, 0, 0)
        self._header_layout.setSpacing(4)

        # Add expand/collapse button for all messages (input and non-input)
        self._expand_button: QToolButton | None = None

        if not is_input:
            self._expand_button = QToolButton()
            self._expand_button.setObjectName("_expand_button")
            self._expand_button.clicked.connect(self._toggle_expanded)
            self._header_layout.addWidget(self._expand_button)

        # Expanded state - default to True, will be updated in set_content based on message type
        self._is_expanded = True

        # Pending content for deferred rendering (when collapsed)
        self._pending_content: str | None = None
        self._pending_context: str | None = None

        # Create role and timestamp labels
        self._role_label = QLabel(self._header)
        self._role_label.setObjectName("_role_label")
        self._role_label.setIndent(0)
        self._header_layout.addWidget(self._role_label)
        self._header_layout.addStretch()

        role_sources = {
            AIMessageSource.USER: "user",
            AIMessageSource.AI_CONNECTED: "ai_connected",
            AIMessageSource.AI: "ai",
            AIMessageSource.REASONING: "reasoning",
            AIMessageSource.TOOL_CALL: "tool_call",
            AIMessageSource.TOOL_RESULT: "tool_result",
            AIMessageSource.SYSTEM: "system",
            AIMessageSource.USER_QUEUED: "user_queued"
        }

        current_style = self._message_source or AIMessageSource.USER
        role = role_sources.get(current_style, "user")
        self.setProperty("message_source", role)

        self._copy_message_button: QToolButton | None = None
        self._save_message_button: QToolButton | None = None
        self._fork_message_button: QToolButton | None = None
        self._delete_message_button: QToolButton | None = None

        # Add fork button only for AI messages
        style = self._message_source
        if style == AIMessageSource.AI:
            self._fork_message_button = QToolButton()
            self._fork_message_button.setObjectName("_fork_button")
            self._fork_message_button.clicked.connect(self._fork_message)
            self._header_layout.addWidget(self._fork_message_button)

        # Add delete button only for user messages
        elif style == AIMessageSource.USER and not self._is_input:
            self._delete_message_button = QToolButton()
            self._delete_message_button.setObjectName("_delete_button")
            self._delete_message_button.clicked.connect(self._delete_message)
            self._header_layout.addWidget(self._delete_message_button)

        # We have copy and save buttons for several message sources
        if style in (AIMessageSource.USER, AIMessageSource.AI, AIMessageSource.REASONING) and not self._is_input:
            self._copy_message_button = QToolButton()
            self._copy_message_button.setObjectName("_copy_button")
            self._copy_message_button.clicked.connect(self._copy_message)
            self._header_layout.addWidget(self._copy_message_button)

            self._save_message_button = QToolButton()
            self._save_message_button.setObjectName("_save_button")
            self._save_message_button.clicked.connect(self._save_message)
            self._header_layout.addWidget(self._save_message_button)

        # Container for message sections
        self._sections_container = QWidget(self)
        self._sections_container.setObjectName("_sections_container")
        self._sections_layout = QVBoxLayout(self._sections_container)
        self._sections_layout.setContentsMargins(0, 0, 0, 0)

        # Create layout
        self._layout = QVBoxLayout(self)
        self._layout.addWidget(self._header)
        self._layout.addWidget(self._sections_container)

        # Tool approval widgets
        self._approval_widget: QWidget | None = None
        self._approval_context_widget: QWidget | None = None
        self._approval_text_edit: MarkdownTextEdit | None = None
        self._approval_context_text_edit: MarkdownTextEdit | None = None
        self._approval_approve_button: QPushButton | None = None
        self._approval_i_am_unsure_button: QPushButton | None = None
        self._approval_reject_button: QPushButton | None = None

        # Track sections
        self._sections: List[ConversationMessageSection] = []
        self._section_with_selection: ConversationMessageSection | None = None

        # If this is an input widget then create the input section
        if is_input:
            section = self._create_section_widget()
            self._sections.append(section)
            self._sections_layout.addWidget(section)

        # Initialize markdown converter
        self._markdown_converter = MarkdownConverter()

        self._is_spotlighted = False

        self._on_language_changed()

        # Set default expanded state based on message type
        # Tool calls and tool results should be collapsed by default
        default_expanded = style not in (AIMessageSource.TOOL_CALL, AIMessageSource.TOOL_RESULT)
        self.set_expanded(default_expanded)

        self._context = context
        if content:
            self.set_content(content)

        self.setLayout(self._layout)

    def set_border_animation(self, active: bool, frame: int = 0, step: int = 64) -> None:
        """
        Enable/disable border animation with specific frame.

        Args:
            active: Whether border animation should be active
            frame: Animation frame (0 to animation_steps-1)
        """
        self._is_border_animated = active
        self._animation_frame = frame
        self._animation_steps = step
        self._update_border_style()

    def _get_fade_color(self) -> str:
        """
        Calculate the current fade color based on animation frame using color palette.

        Returns:
            str: Hex color string for the current animation frame
        """
        # Animation parameters
        hue = self._animation_frame / self._animation_steps
        saturation = 0.7
        if self._style_manager.color_mode() == ColorMode.DARK:
            value = 0.5

        else:
            value = 1.0

        # Convert HSV to RGB
        r, g, b = colorsys.hsv_to_rgb(hue, saturation, value)

        # Convert to 0-255 range and format as hex
        r_int = int(r * 255)
        g_int = int(g * 255)
        b_int = int(b * 255)

        return f"#{r_int:02x}{g_int:02x}{b_int:02x}"

    def _update_border_style(self) -> None:
        """Update the border style with the current animation color."""
        self.style().unpolish(self)
        self.style().polish(self)

    def _has_focus_in_hierarchy(self) -> bool:
        """Check if this widget or any of its descendants has focus."""
        focus_widget = QApplication.focusWidget()
        if focus_widget is None:
            return False

        return focus_widget == self or self.isAncestorOf(focus_widget)

    def paintEvent(self, arg__1: QPaintEvent) -> None:
        """Override paint event to paint custom borders."""
        super().paintEvent(arg__1)

        painter = QPainter(self)
        zoom_factor = self._style_manager.zoom_factor()
        border_radius = int(self._style_manager.message_bubble_spacing() * zoom_factor)

        if self._is_border_animated:
            border_color = self._get_fade_color()
            border_width = 2

        elif self._is_spotlighted and self._has_focus_in_hierarchy():
            border_color = self._style_manager.get_color_str(ColorRole.MESSAGE_SPOTLIGHTED)
            border_width = 2

        else:
            current_style = self._message_source or AIMessageSource.USER
            border_color = self._style_manager.get_color_str(
                ColorRole.MESSAGE_USER_BORDER if current_style == AIMessageSource.USER else ColorRole.MESSAGE_BORDER
            )
            border_width = 1

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
        adjustment = (border_width + 1) // 2
        adjusted_rect = self.rect().adjusted(adjustment, adjustment, -adjustment, -adjustment)

        # Draw the rounded rectangle border
        painter.drawRoundedRect(adjusted_rect, border_radius, border_radius)

    def is_rendered(self) -> bool:
        """Check if the message will be rendered."""
        return self._message_rendered

    def set_rendered(self, rendered: bool) -> None:
        """Set the rendered state of this message."""
        self._message_rendered = rendered
        if not rendered:
            self.hide()
            return

        self.show()

    def is_spotlighted(self) -> bool:
        """Check if this message is spotlighted."""
        return self._is_spotlighted

    def set_spotlighted(self, spotlighted: bool) -> None:
        """Set the spotlighted state of this message."""
        if self.isHidden():
            return

        if self._is_spotlighted == spotlighted:
            return

        self._is_spotlighted = spotlighted
        self._update_border_style()

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

        assert self._expand_button is not None, "Expand button should exist"
        self._is_expanded = expanded

        strings = self._language_manager.strings()

        if expanded:
            # If we have pending content, render it now
            if self._pending_content is not None:
                self._render_content(self._pending_content, self._pending_context)
                self._pending_content = None
                self._pending_context = None

            self._sections_container.show()
            icon_name = "expand-down"
            tooltip = strings.tooltip_collapse_message

        else:
            self._sections_container.hide()
            icon_name = "expand-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "expand-left"
            tooltip = strings.tooltip_expand_message

        # Update icon
        icon_base_size = 14
        self._expand_button.setIcon(QIcon(self._style_manager.scale_icon(icon_name, icon_base_size)))

        # Update tooltip
        self._expand_button.setToolTip(tooltip)

    def _toggle_expanded(self) -> None:
        """Toggle the expanded state of this message."""
        self.set_expanded(not self._is_expanded)
        self.expand_requested.emit(self._is_expanded)

    def _on_language_changed(self) -> None:
        """Update text when language changes."""
        if self._is_input:
            return

        strings = self._language_manager.strings()
        if self._message_source:
            match self._message_source:
                case AIMessageSource.USER:
                    if self._message_user_name:
                        role_text = self._message_user_name

                    else:
                        role_text = strings.role_you

                case AIMessageSource.USER_QUEUED:
                    role_text = strings.role_you_queued

                case AIMessageSource.AI_CONNECTED:
                    role_text = strings.role_connected.format(model=self._message_model)

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

        if self._approval_i_am_unsure_button:
            self._approval_i_am_unsure_button.setText(strings.i_am_unsure_about_tool_call)

        if self._approval_reject_button:
            self._approval_reject_button.setText(strings.reject_tool_call)

        if self._expand_button:
            tooltip = strings.tooltip_collapse_message if self._is_expanded else strings.tooltip_expand_message
            self._expand_button.setToolTip(tooltip)

    def _create_section_widget(self, language: ProgrammingLanguage | None = None) -> ConversationMessageSection:
        """
        Create a new section widget.

        Args:
            language: Optional programming language for the section

        Returns:
            A new ConversationMessageSection instance
        """
        section = ConversationMessageSection(self._is_input, language, self._sections_container)
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

    def show_tool_approval_ui(self, tool_call: AIToolCall, reason: str, context: str | None, destructive: bool) -> None:
        """
        Show tool approval UI for the given tool calls.

        Args:
            tool_call: Tool call that needs approval
            reason: Reason for the tool call
            context: Additional context for the tool call
            destructive: Whether the tool calls are destructive
        """
        assert self._approval_widget is None, "Approval widget already exists"

        style_manager = self._style_manager
        zoom_factor = style_manager.zoom_factor()
        spacing = int(style_manager.message_bubble_spacing() * zoom_factor)

        self._approval_widget = QWidget()
        self._approval_widget.setObjectName("_approval_widget")
        layout = QVBoxLayout(self._approval_widget)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(spacing)
        strings = self._language_manager.strings()

        self._approval_text_edit = MarkdownTextEdit(False)
        self._approval_text_edit.setObjectName("_approval_text_edit")
        self._approval_text_edit.set_text(reason)
        self._approval_text_edit.setReadOnly(True)
        layout.addWidget(self._approval_text_edit)

        if context is not None:
            self._approval_context_widget = QWidget()
            self._approval_context_widget.setObjectName("_approval_context_widget")
            layout2 = QVBoxLayout(self._approval_context_widget)
            layout2.setContentsMargins(spacing, spacing, spacing, spacing)
            layout2.setSpacing(spacing)
            self._approval_context_text_edit = MarkdownTextEdit(False)
            self._approval_context_text_edit.setObjectName("_approval_context_text_edit")
            self._approval_context_text_edit.set_text(context)
            self._approval_context_text_edit.setReadOnly(True)
            highlighter = CodeBlockHighlighter(self._approval_context_text_edit.document())
            highlighter.set_language(ProgrammingLanguage.DIFF)
            layout2.addWidget(self._approval_context_text_edit)
            layout.addWidget(self._approval_context_widget)

        # Approval buttons
        button_layout = QHBoxLayout()
        button_layout.setSpacing(spacing)
        button_layout.addStretch()

        min_button_height = 40
        min_button_width = int(180 * zoom_factor)

        self._approval_approve_button = QPushButton(strings.approve_tool_call)
        self._approval_approve_button.setObjectName("_approval_approve_button")
        self._approval_approve_button.clicked.connect(lambda: self._approve_tool_call(tool_call))
        self._approval_approve_button.setMinimumWidth(min_button_width)
        self._approval_approve_button.setMinimumHeight(min_button_height)
        self._approval_approve_button.setProperty("recommended", not destructive)
        self._approval_approve_button.setContentsMargins(8, 8, 8, 8)

        self._approval_i_am_unsure_button = QPushButton(strings.i_am_unsure_about_tool_call)
        self._approval_i_am_unsure_button.setObjectName("_approval_i_am_unsure_button")
        self._approval_i_am_unsure_button.clicked.connect(self._i_am_unsure_about_tool_call)
        self._approval_i_am_unsure_button.setMinimumWidth(min_button_width)
        self._approval_i_am_unsure_button.setMinimumHeight(min_button_height)
        self._approval_i_am_unsure_button.setContentsMargins(8, 8, 8, 8)

        self._approval_reject_button = QPushButton(strings.reject_tool_call)
        self._approval_reject_button.setObjectName("_approval_reject_button")
        self._approval_reject_button.clicked.connect(self._reject_tool_call)
        self._approval_reject_button.setMinimumWidth(min_button_width)
        self._approval_reject_button.setMinimumHeight(min_button_height)
        self._approval_reject_button.setContentsMargins(8, 8, 8, 8)

        button_layout.addWidget(self._approval_approve_button)
        button_layout.addWidget(self._approval_i_am_unsure_button)
        button_layout.addWidget(self._approval_reject_button)
        button_layout.addStretch()

        layout.addSpacing(2)
        layout.addLayout(button_layout)

        self._layout.addWidget(self._approval_widget)

        # Expand the message for context when showing the approval UI
        self.set_expanded(True)

    def _approve_tool_call(self, tool_call: AIToolCall) -> None:
        """Handle tool call approval."""
        self.tool_call_approved.emit(tool_call)
        self.remove_tool_approval_ui()

    def _i_am_unsure_about_tool_call(self) -> None:
        """Handle unsure about tool call."""
        self.tool_call_i_am_unsure.emit()
        self.remove_tool_approval_ui()

    def _reject_tool_call(self) -> None:
        """Handle tool call rejection."""
        self.tool_call_rejected.emit("Tool call was rejected by the user")
        self.remove_tool_approval_ui()

    def remove_tool_approval_ui(self) -> None:
        """Remove the tool approval UI."""
        # Collapse the message when removing approval UI
        self.set_expanded(False)

        if self._approval_widget:
            self._layout.removeWidget(self._approval_widget)
            self._approval_widget.deleteLater()
            self._approval_widget = None
            self._approval_text_edit = None
            self._approval_context_text_edit = None
            self._approval_context_widget = None
            self._approval_approve_button = None
            self._approval_i_am_unsure_button = None
            self._approval_reject_button = None

    def _get_border_color(self) -> str:
        """Get the border color based on current state."""
        if self._is_border_animated:
            return self._get_fade_color()

        if self._is_spotlighted and self.hasFocus():
            return self._style_manager.get_color_str(ColorRole.MESSAGE_SPOTLIGHTED)

        current_style = self._message_source or AIMessageSource.USER
        return self._style_manager.get_color_str(
            ColorRole.MESSAGE_USER_BORDER if current_style == AIMessageSource.USER else ColorRole.MESSAGE_BORDER
        )

    def set_content(self, text: str) -> None:
        """
        Set content with style, handling incremental updates for AI responses.

        For collapsed messages, content rendering is deferred until expansion.

        Args:
            text: The message text content
        """
        self._message_content = text

        # Input widgets don't have multiple sections so handle them as a special case.
        if self._is_input:
            self._sections[0].set_content(MarkdownASTTextNode(text))
            if text:
                if not self._message_rendered:
                    self._message_rendered = True
                    self.show()

            return

        # Check if we should defer rendering
        if not self._is_expanded:
            # Store content for later rendering
            self._pending_content = text
            self._pending_context = self._context

            # Show the message widget (even if collapsed)
            if text and not self._message_rendered:
                self._message_rendered = True
                self.show()

            return

        # Expanded - render immediately
        self._render_content(text, self._context)

    def _render_content(self, text: str, context: str | None) -> None:
        """
        Actually render the content into sections.

        Args:
            text: The message text content
            context: Optional context to append to the message
        """
        # If we were given context, append it to the message for section extraction
        if context:
            text += f"\n{context}"

        # Extract sections directly using the markdown converter
        sections_data = self._markdown_converter.extract_sections(text, None)

        # Create or update sections
        for i, (node, language) in enumerate(sections_data):
            # Create new section if needed
            if i >= len(self._sections):
                section = self._create_section_widget(language)
                section.set_content(node)
                section.apply_style()
                self._sections.append(section)
                self._sections_layout.addWidget(section)
                continue

            if i == len(self._sections) - 1:
                # Update the last section with new content
                section = self._sections[-1]
                section.set_language(language)
                section.set_content(node)

        # Remove any extra sections
        while len(self._sections) > len(sections_data):
            section = self._sections.pop()
            self._sections_layout.removeWidget(section)
            section.deleteLater()

        # Show the message if it has text
        if text:
            if not self._message_rendered:
                self._message_rendered = True
                self.show()

    def message_id(self) -> str | None:
        """Get the unique message ID."""
        return self._message_id

    def message_source(self) -> AIMessageSource | None:
        """Get the source of the message."""
        return self._message_source

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

    def apply_style(self) -> None:
        """Apply style changes."""
        style_manager = self._style_manager

        zoom_factor = style_manager.zoom_factor()
        spacing = int(style_manager.message_bubble_spacing() * zoom_factor)
        self._layout.setSpacing(spacing)
        self._layout.setContentsMargins(spacing, spacing, spacing, spacing)
        self._sections_layout.setSpacing(spacing)

        font = self.font()
        base_font_size = style_manager.base_font_size()
        font.setPointSizeF(base_font_size * zoom_factor)
        self._role_label.setFont(font)

        # Set icons and sizes for buttons
        icon_base_size = 14
        icon_scaled_size = int(icon_base_size * zoom_factor)
        icon_size = QSize(icon_scaled_size, icon_scaled_size)

        if self._copy_message_button:
            self._copy_message_button.setIcon(QIcon(style_manager.scale_icon("copy", icon_base_size)))
            self._copy_message_button.setIconSize(icon_size)

        if self._save_message_button:
            self._save_message_button.setIcon(QIcon(style_manager.scale_icon("save", icon_base_size)))
            self._save_message_button.setIconSize(icon_size)

        if self._fork_message_button:
            self._fork_message_button.setIcon(QIcon(style_manager.scale_icon("fork", icon_base_size)))
            self._fork_message_button.setIconSize(icon_size)

        if self._delete_message_button:
            self._delete_message_button.setIcon(QIcon(style_manager.scale_icon("delete", icon_base_size)))
            self._delete_message_button.setIconSize(icon_size)

        if self._expand_button:
            if self._is_expanded:
                icon_name = "expand-down"

            else:
                icon_name = "expand-right" if self.layoutDirection() == Qt.LayoutDirection.LeftToRight else "expand-left"

            self._expand_button.setIcon(QIcon(self._style_manager.scale_icon(icon_name, icon_base_size)))
            self._expand_button.setIconSize(icon_size)

        # Apply fonts to approval buttons if present
        if self._approval_approve_button:
            self._approval_approve_button.setFont(font)

        if self._approval_i_am_unsure_button:
            self._approval_i_am_unsure_button.setFont(font)

        if self._approval_reject_button:
            self._approval_reject_button.setFont(font)

        if self._approval_text_edit:
            self._approval_text_edit.setFont(font)

        if self._approval_context_text_edit:
            self._approval_context_text_edit.setFont(font)

        # Apply styling to all sections
        for section in self._sections:
            section.apply_style()

    def find_text(self, text: str) -> List[Tuple[int, int, int]]:
        """
        Find all instances of text in this message.

        For collapsed messages with pending content, searches the raw text.
        For expanded messages, searches rendered sections.

        Note: Search is case-insensitive.

        Args:
            text: Text to search for

        Returns:
            List of (section, start_position, end_position) tuples for each match
        """
        # If collapsed with pending content, search raw text
        if not self._is_expanded and self._pending_content:
            return self._find_text_in_raw_content(text)

        # Otherwise search rendered sections normally
        all_matches: List[Tuple[int, int, int]] = []
        for i, section in enumerate(self._sections):
            section_matches = section.find_text(text)
            if section_matches:
                # Include the section with each match
                for match in section_matches:
                    all_matches.append((i, match[0], match[1]))

        return all_matches

    def _find_text_in_raw_content(self, text: str) -> List[Tuple[int, int, int]]:
        """
        Search in raw unrendered content (case-insensitive).

        Returns matches as if they're all in section 0, since sections
        don't exist yet. Positions are character offsets in the raw text.

        Args:
            text: Text to search for

        Returns:
            List of (section, start_position, end_position) tuples for each match
        """
        if not self._pending_content:
            return []

        matches = []
        content_lower = self._pending_content.lower()
        search_lower = text.lower()
        pos = 0

        while True:
            pos = content_lower.find(search_lower, pos)
            if pos == -1:
                break

            # All matches reported as section 0 (placeholder)
            matches.append((0, pos, pos + len(text)))
            pos += 1

        return matches

    def highlight_matches(
        self,
        matches: List[Tuple[int, int, int]],
        current_match_index: int = -1,
        highlight_color: QColor | None = None,
        dim_highlight_color: QColor | None = None
    ) -> None:
        """
        Highlight matches in this message.

        Note: Cannot highlight matches in collapsed messages with pending content.
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

        # Can't highlight if we have pending content (no sections yet)
        if self._pending_content is not None:
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
