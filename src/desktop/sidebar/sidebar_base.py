from PySide6.QtWidgets import QApplication, QWidget


class SidebarBase(QWidget):
    """Abstract base class for sidebar widgets."""

    def has_focus(self) -> bool:
        """Return True if this panel or one of its descendants currently has keyboard focus."""
        focus_widget = QApplication.focusWidget()
        return focus_widget is not None and self.isAncestorOf(focus_widget)

    def set_mindspace(self, path: str) -> None:
        """
        Set the active mindspace root path.

        Args:
            path: Absolute path to the mindspace directory, or empty string to clear.
        """

    def reveal_and_select_file(self, file_path: str) -> None:
        """
        Reveal and select a file within this panel.

        The default implementation is a no-op; panels that support navigation
        should override this.

        Args:
            file_path: Absolute path to the file to reveal.
        """

    def apply_style(self) -> None:
        """Apply current application style. Panels should override to update styling."""
