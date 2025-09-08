"""
Conversation widget event filter implementation.

This module provides an event filter for tracking widget activation and deactivation events
in the conversation widget.
"""

from PySide6.QtCore import QEvent, QObject, Signal
from PySide6.QtWidgets import QWidget


class ConversationWidgetEventFilter(QObject):
    """Event filter to track activation events from child widgets."""

    widget_activated = Signal(object)
    widget_deactivated = Signal(object)

    def __init__(self, parent: QWidget | None = None) -> None:
        """Initialize the event filter."""
        super().__init__(parent)

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        """
        Filter events to detect widget activation.

        Args:
            watched: The object that received the event
            event: The event that was received

        Returns:
            True if event was handled, False to pass to the target object
        """
        if event.type() in (QEvent.Type.MouseButtonPress, QEvent.Type.FocusIn):
            # Simply emit the signal with the object that received the event
            self.widget_activated.emit(watched)
            return False  # Don't consume the event

        if event.type() == QEvent.Type.FocusOut:
            # Emit a widget deactivated signal
            self.widget_deactivated.emit(watched)
            return False  # Don't consume the event

        return super().eventFilter(watched, event)
