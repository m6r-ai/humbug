"""Dialogs for single-file history and blame."""

import os

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QDialog, QListWidget, QListWidgetItem, QPlainTextEdit, QSplitter, QVBoxLayout, QWidget
)

from git import GitError, get_blame, get_commit_file_diff, get_log

from desktop.color_role import ColorRole
from desktop.git_ui_helpers import DiffHighlighter, relative_time
from desktop.language.language_manager import LanguageManager
from desktop.style_manager import StyleManager


_HASH_ROLE = Qt.ItemDataRole.UserRole


class FileHistoryDialog(QDialog):
    """Shows the commits that touched a file and the diff each applied."""

    def __init__(self, repo_root: str, file_path: str, style_manager: StyleManager,
                 parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._repo_root = repo_root
        self._file_path = file_path
        self._sm = style_manager

        rel = os.path.relpath(file_path, repo_root)
        self.setWindowTitle(LanguageManager().strings().git_file_history_title.format(rel))

        zoom = style_manager.zoom_factor()
        self.resize(round(640 * zoom), round(560 * zoom))

        layout = QVBoxLayout(self)
        split = QSplitter(Qt.Orientation.Vertical, self)

        self._commits = QListWidget(split)
        self._commits.currentItemChanged.connect(self._on_commit_selected)
        split.addWidget(self._commits)

        self._diff = QPlainTextEdit(split)
        self._diff.setReadOnly(True)
        self._diff.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        mono = style_manager.make_monospace_font()
        mono.setPointSizeF(style_manager.base_font_size() * zoom)
        self._diff.setFont(mono)
        DiffHighlighter(self._diff.document(), style_manager)
        split.addWidget(self._diff)
        split.setStretchFactor(0, 1)
        split.setStretchFactor(1, 2)

        layout.addWidget(split)
        self._apply_style()
        self._load()

    def _load(self) -> None:
        """Load the file's commit history into the list."""
        try:
            commits = get_log(self._repo_root, path=self._file_path)

        except GitError:
            return

        for commit in commits:
            item = QListWidgetItem(
                f"{commit.subject}\n{commit.short_hash} · {commit.author_name} · "
                f"{relative_time(commit.timestamp)}"
            )
            item.setData(_HASH_ROLE, commit.commit_hash)
            self._commits.addItem(item)

        if self._commits.count():
            self._commits.setCurrentRow(0)

    def _on_commit_selected(
        self, current: QListWidgetItem | None, _prev: QListWidgetItem | None = None
    ) -> None:
        """Show the selected commit's diff for the file."""
        self._diff.clear()
        if current is None:
            return

        try:
            self._diff.setPlainText(
                get_commit_file_diff(self._repo_root, current.data(_HASH_ROLE), self._file_path)
            )

        except GitError:
            pass

    def _apply_style(self) -> None:
        """Apply themed styling to the dialog."""
        bg = self._sm.get_color_str(ColorRole.BACKGROUND_PRIMARY)
        panel = self._sm.get_color_str(ColorRole.BACKGROUND_SECONDARY)
        text = self._sm.get_color_str(ColorRole.TEXT_PRIMARY)
        self.setStyleSheet(f"""
            QDialog {{ background-color: {bg}; }}
            QListWidget {{ background-color: {bg}; color: {text}; border: none; }}
            QPlainTextEdit {{ background-color: {panel}; color: {text}; border: none; }}
        """)


class BlameDialog(QDialog):
    """Shows per-line authorship for a file."""

    def __init__(self, repo_root: str, file_path: str, style_manager: StyleManager,
                 parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._sm = style_manager

        rel = os.path.relpath(file_path, repo_root)
        self.setWindowTitle(LanguageManager().strings().git_blame_title.format(rel))

        zoom = style_manager.zoom_factor()
        self.resize(round(760 * zoom), round(560 * zoom))

        layout = QVBoxLayout(self)
        view = QPlainTextEdit(self)
        view.setReadOnly(True)
        view.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        mono = style_manager.make_monospace_font()
        mono.setPointSizeF(style_manager.base_font_size() * zoom)
        view.setFont(mono)
        layout.addWidget(view)

        try:
            blame = get_blame(repo_root, file_path)
            author_w = max((len(b.author) for b in blame), default=0)
            lines = [
                f"{b.short_hash}  {b.author:<{author_w}}  {b.line_number:>4}  {b.content}"
                for b in blame
            ]
            view.setPlainText("\n".join(lines))

        except GitError as e:
            view.setPlainText(str(e))

        bg = style_manager.get_color_str(ColorRole.BACKGROUND_PRIMARY)
        text = style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
        self.setStyleSheet(f"QDialog {{ background-color: {bg}; }} "
                           f"QPlainTextEdit {{ background-color: {bg}; color: {text}; border: none; }}")
