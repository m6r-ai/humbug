"""Pinned artifacts project overview tab."""

import os

from PySide6.QtCore import QPoint, Qt
from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QFileDialog,
    QFormLayout,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QMenu,
    QPushButton,
    QTextEdit,
    QVBoxLayout,
    QWidget,
)

from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.status_message import StatusMessage
from desktop.tab import TabBase, TabState
from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager
from mindspace.pinned_artifact import PinnedArtifact


class PinnedArtifactsTab(TabBase):
    """Tab providing a persistent project overview of pinned artifacts."""

    def __init__(self, tab_id: str, parent: QWidget | None = None) -> None:
        super().__init__(tab_id, parent)
        self._mindspace_manager = MindspaceManager()
        self._style_manager = StyleManager()

        layout = QVBoxLayout(self)
        layout.setObjectName("PinnedArtifactsLayout")
        self._hero_title = QLabel("Project Overview")
        self._hero_title.setObjectName("PinnedArtifactsTitle")
        self._hero_description = QLabel(
            "Keep the plans, decisions, prompts, research, and files that define this project close at hand."
        )
        self._hero_description.setObjectName("PinnedArtifactsDescription")
        self._artifact_count = QLabel()
        self._artifact_count.setObjectName("PinnedArtifactsCount")
        self._empty_label = QLabel("Nothing pinned yet. Add a note or pin a file to build this project's working memory.")
        self._empty_label.setObjectName("PinnedArtifactsEmpty")
        self._artifact_list = QListWidget()
        self._artifact_list.setObjectName("PinnedArtifactsList")
        self._artifact_list.setWordWrap(True)
        self._artifact_list.setUniformItemSizes(False)
        self._artifact_list.setTextElideMode(Qt.TextElideMode.ElideNone)
        self._artifact_list.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self._artifact_list.itemSelectionChanged.connect(self._update_remove_button)
        self._artifact_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._artifact_list.customContextMenuRequested.connect(self._show_context_menu)
        self._artifact_list.itemDoubleClicked.connect(lambda _item: self._edit_selected())
        layout.addWidget(self._hero_title)
        layout.addWidget(self._hero_description)
        layout.addWidget(self._artifact_count)
        layout.addWidget(self._empty_label)
        layout.addWidget(self._artifact_list)

        actions = QHBoxLayout()
        add_note_button = QPushButton("Add Note")
        add_note_button.clicked.connect(self._add_note)
        pin_file_button = QPushButton("Pin File")
        pin_file_button.clicked.connect(self._pin_file)
        self._remove_button = QPushButton("Remove")
        self._remove_button.clicked.connect(self._remove_selected)
        actions.addWidget(add_note_button)
        actions.addWidget(pin_file_button)
        actions.addStretch()
        actions.addWidget(self._remove_button)
        layout.addLayout(actions)
        self._style_manager.style_changed.connect(self.apply_style)
        self.apply_style()
        self.refresh()

    def tool_name(self) -> str:
        return "pinned_artifacts"

    def tab_title_from_path(self) -> str:
        return "Project Overview"

    def set_active(self, widget: QWidget, active: bool) -> None:
        if active:
            self.activated.emit()

    def activate(self) -> None:
        self.refresh()

    def set_path(self, path: str) -> None:
        pass

    def on_path_renamed(self, new_path: str) -> None:
        pass

    def can_close_tab(self) -> bool:
        return True

    def close_tab(self) -> None:
        pass

    def can_save(self) -> bool:
        return False

    def save(self) -> bool:
        return True

    def can_save_as(self) -> bool:
        return False

    def save_as(self) -> bool:
        return True

    def can_undo(self) -> bool:
        return False

    def undo(self) -> None:
        pass

    def can_redo(self) -> bool:
        return False

    def redo(self) -> None:
        pass

    def can_cut(self) -> bool:
        return False

    def cut(self) -> None:
        pass

    def can_copy(self) -> bool:
        return False

    def copy(self) -> None:
        pass

    def can_paste(self) -> bool:
        return False

    def paste(self) -> None:
        pass

    def can_submit(self) -> bool:
        return False

    def submit(self) -> None:
        pass

    def show_find(self) -> None:
        pass

    def update_status(self) -> None:
        self.status_message.emit(StatusMessage("Project Overview"))

    def get_state(self, temp_state: bool = False) -> TabState:
        return TabState(type=self.tool_name(), tab_id=self.tab_id(), path="")

    @classmethod
    def restore_from_state(cls, state: TabState, parent: QWidget) -> "PinnedArtifactsTab":
        return cls(state.tab_id, parent)

    def refresh(self) -> None:
        """Refresh the list from the active mindspace."""
        self._artifact_list.clear()
        if not self._mindspace_manager.has_mindspace():
            self._artifact_count.setText("No mindspace open")
            self._empty_label.setVisible(True)
            self._remove_button.setEnabled(False)
            return

        artifacts = self._mindspace_manager.mindspace().pinned_artifacts()
        item_word = "artifact" if len(artifacts) == 1 else "artifacts"
        self._artifact_count.setText(f"{len(artifacts)} pinned {item_word}")
        self._empty_label.setVisible(not artifacts)
        for artifact in artifacts:
            detail = artifact.content or artifact.path
            item = QListWidgetItem(f"{artifact.kind.title()}: {artifact.title}\n{detail}")
            item.setData(256, artifact.artifact_id)
            self._artifact_list.addItem(item)

        self._update_remove_button()

    def _add_note(self) -> None:
        """Create a new manually written artifact."""
        values = self._show_artifact_dialog()
        if values is None:
            return

        title, kind, content = values
        self._mindspace_manager.mindspace().add_pinned_artifact(title, kind, content)
        self.refresh()

    def _show_artifact_dialog(
        self, artifact: PinnedArtifact | None = None
    ) -> tuple[str, str, str] | None:
        """Show the editable artifact details dialog."""
        dialog = QDialog(self)
        dialog.setWindowTitle("Edit Pinned Artifact" if artifact else "Add Pinned Note")
        layout = QVBoxLayout(dialog)
        form = QFormLayout()
        title = QLineEdit(artifact.title if artifact else "")
        kind = QLineEdit(artifact.kind if artifact else "note")
        content = QTextEdit(artifact.content if artifact else "")
        form.addRow("Title", title)
        form.addRow("Type", kind)
        form.addRow("Content", content)
        layout.addLayout(form)
        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Cancel | QDialogButtonBox.StandardButton.Ok)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)
        if dialog.exec() != QDialog.DialogCode.Accepted or not title.text().strip():
            return None

        return title.text().strip(), kind.text().strip() or "note", content.toPlainText()

    def _pin_file(self) -> None:
        """Prompt for a file and add it as a pinned artifact."""
        file_path, _ = QFileDialog.getOpenFileName(self, "Pin File", self._mindspace_manager.file_dialog_directory())
        if not file_path:
            return

        relative_path = self._mindspace_manager.mindspace().get_relative_path(file_path)
        self._mindspace_manager.mindspace().add_pinned_artifact(
            os.path.basename(file_path), "file", path=relative_path
        )
        self.refresh()

    def _remove_selected(self) -> None:
        """Remove the currently selected pinned artifact."""
        item = self._artifact_list.currentItem()
        if item is None:
            return

        artifact_id = item.data(256)
        if isinstance(artifact_id, str):
            self._mindspace_manager.mindspace().remove_pinned_artifact(artifact_id)
            self.refresh()

    def _edit_selected(self) -> None:
        """Edit the selected artifact's title, type, and content."""
        artifact = self._selected_artifact()
        if artifact is None:
            return

        values = self._show_artifact_dialog(artifact)
        if values is None:
            return

        title, kind, content = values
        self._mindspace_manager.mindspace().update_pinned_artifact(
            artifact.artifact_id, title, kind, content
        )
        self.refresh()

    def _selected_artifact(self) -> PinnedArtifact | None:
        """Return the artifact represented by the selected list item."""
        item = self._artifact_list.currentItem()
        if item is None:
            return None

        artifact_id = item.data(256)
        if not isinstance(artifact_id, str):
            return None

        return next(
            (
                artifact
                for artifact in self._mindspace_manager.mindspace().pinned_artifacts()
                if artifact.artifact_id == artifact_id
            ),
            None,
        )

    def _show_context_menu(self, pos: QPoint) -> None:
        """Show artifact editing actions for the item under the pointer."""
        item = self._artifact_list.itemAt(pos)
        if item is None:
            return

        self._artifact_list.setCurrentItem(item)
        menu = QMenu(self)
        menu.addAction("Edit", self._edit_selected)
        menu.addAction("Delete", self._remove_selected)
        menu.exec(self._artifact_list.mapToGlobal(pos))

    def _update_remove_button(self) -> None:
        """Enable the remove button only when an artifact is selected."""
        self._remove_button.setEnabled(self._artifact_list.currentItem() is not None)

    def apply_style(self) -> None:
        """Apply the active visual theme to the project overview."""
        style = self._style_manager
        spacing = int(style.message_bubble_spacing())
        layout = self.layout()
        assert layout is not None
        layout.setContentsMargins(spacing * 2, spacing * 2, spacing * 2, spacing * 2)
        layout.setSpacing(spacing)
        self.setStyleSheet(f"""
            #PinnedArtifactsTitle {{
                color: {style.get_color_str(ColorRole.TEXT_HEADING)};
                font-size: {round(style.base_font_size() * style.zoom_factor() * 1.8)}pt;
                font-weight: bold;
            }}
            #PinnedArtifactsDescription, #PinnedArtifactsEmpty {{
                color: {style.get_color_str(ColorRole.TEXT_INACTIVE)};
            }}
            #PinnedArtifactsCount {{
                color: {style.get_color_str(ColorRole.TEXT_PRIMARY)};
                font-weight: bold;
            }}
            #PinnedArtifactsList {{
                background-color: {style.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
                border: 1px solid {style.get_color_str(ColorRole.EDIT_BOX_BORDER)};
                border-radius: {spacing}px;
                padding: {spacing}px;
                selection-background-color: {style.get_color_str(ColorRole.TEXT_SELECTED)};
                selection-color: {style.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
            #PinnedArtifactsList::item {{
                border-bottom: 1px solid {style.get_color_str(ColorRole.EDIT_BOX_BORDER)};
                padding: {spacing}px;
            }}
            #PinnedArtifactsList::item:selected {{
                color: {style.get_color_str(ColorRole.TEXT_PRIMARY)};
            }}
        """)
