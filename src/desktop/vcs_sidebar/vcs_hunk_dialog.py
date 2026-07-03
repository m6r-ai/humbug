"""Dialog for selecting which hunks of a file to stage or unstage."""

from PySide6.QtWidgets import (
    QCheckBox, QDialog, QDialogButtonBox, QFrame, QPlainTextEdit, QScrollArea,
    QVBoxLayout, QWidget
)

from desktop.color_role import ColorRole
from desktop.git_ui_helpers import DiffHighlighter
from desktop.style_manager import StyleManager


class HunkSelectionDialog(QDialog):
    """Lists a file's hunks with checkboxes; returns the chosen hunk indices."""

    def __init__(self, title: str, hunks: list[str], style_manager: StyleManager,
                 parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setWindowTitle(title)
        self._style_manager = style_manager
        self._checks: list[QCheckBox] = []

        zoom = style_manager.zoom_factor()
        self.resize(round(560 * zoom), round(520 * zoom))

        layout = QVBoxLayout(self)

        scroll = QScrollArea(self)
        scroll.setWidgetResizable(True)
        container = QWidget()
        col = QVBoxLayout(container)
        col.setSpacing(round(8 * zoom))

        mono = style_manager.make_monospace_font()
        mono.setPointSizeF(style_manager.base_font_size() * zoom)

        for hunk in hunks:
            first_line = hunk.splitlines()[0] if hunk else "@@"
            check = QCheckBox(first_line, container)
            check.setChecked(True)
            self._checks.append(check)
            col.addWidget(check)

            preview = QPlainTextEdit(container)
            preview.setReadOnly(True)
            preview.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
            preview.setFont(mono)
            preview.setPlainText(hunk)
            preview.setFrameShape(QFrame.Shape.NoFrame)
            lines = hunk.count("\n") + 1
            preview.setFixedHeight(round((min(lines, 12) * mono.pointSizeF() * 1.7) + 12))
            DiffHighlighter(preview.document(), style_manager)
            col.addWidget(preview)

        col.addStretch()
        scroll.setWidget(container)
        layout.addWidget(scroll)

        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel, self
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

        self._apply_style()

    def selected_indices(self) -> list[int]:
        """Return the indices of the checked hunks."""
        return [i for i, check in enumerate(self._checks) if check.isChecked()]

    def _apply_style(self) -> None:
        """Apply themed styling to the dialog."""
        sm = self._style_manager
        bg = sm.get_color_str(ColorRole.BACKGROUND_PRIMARY)
        panel = sm.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        text = sm.get_color_str(ColorRole.TEXT_PRIMARY)
        self.setStyleSheet(f"""
            QDialog {{ background-color: {bg}; }}
            QWidget {{ color: {text}; }}
            QScrollArea {{ border: none; background: {bg}; }}
            QPlainTextEdit {{ background-color: {panel}; color: {text}; border-radius: 4px; }}
        """)

    @classmethod
    def choose(cls, title: str, hunks: list[str], style_manager: StyleManager,
               parent: QWidget | None = None) -> list[int] | None:
        """Show the dialog; return chosen hunk indices, or None if cancelled."""
        dialog = cls(title, hunks, style_manager, parent)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            return dialog.selected_indices()

        return None
