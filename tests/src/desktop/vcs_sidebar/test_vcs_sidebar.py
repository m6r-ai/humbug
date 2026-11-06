"""Tests for the VCSSidebar panel."""

import os

from PySide6.QtCore import QRect, Qt
from PySide6.QtGui import QPainter, QPixmap
from PySide6.QtWidgets import QStyleOptionViewItem
import pytest

# pylint: disable=wrong-import-position
from desktop.language.language_manager import LanguageManager
from desktop.vcs_sidebar.vcs_sidebar import VCSSidebar
from desktop.vcs_sidebar.vcs_sidebar_delegate import _HEADER_ROLE
from git import GitFileStatus, GitStatusCode


@pytest.fixture
def panel(qapp, tmp_path):  # pylint: disable=unused-argument
    """A VCSSidebar whose mindspace root is an empty temporary directory."""
    widget = VCSSidebar()
    widget._mindspace_path = str(tmp_path)  # pylint: disable=protected-access
    yield widget
    widget.deleteLater()


def _entry(code: GitStatusCode, path: str, original_path: str | None = None) -> GitFileStatus:
    """Build a status entry for an absolute path."""
    return GitFileStatus(code=code, path=path, original_path=original_path)


def _row_texts(panel: VCSSidebar) -> list[str]:
    """Return the display text of every row in the panel, in order."""
    widget = panel._list_widget  # pylint: disable=protected-access
    return [widget.item(i).text() for i in range(widget.count())]


class TestDirectoryLabels:
    """Each directory the changed files sit in is introduced by one label row."""

    def test_files_are_listed_under_the_label_of_their_directory(self, panel, tmp_path):
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(GitStatusCode.MODIFIED, str(tmp_path / "src" / "a.py")),
            _entry(GitStatusCode.MODIFIED, str(tmp_path / "src" / "b.py")),
            _entry(GitStatusCode.UNTRACKED, str(tmp_path / "docs" / "c.md")),
        ])

        assert _row_texts(panel) == ["src", "M  a.py", "M  b.py", "docs", "?  c.md"]

    def test_a_file_at_the_mindspace_root_is_listed_under_the_root_label(self, panel, tmp_path):
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(GitStatusCode.ADDED, str(tmp_path / "README.md")),
        ])

        assert _row_texts(panel) == [LanguageManager().strings().mindspace_vcs_root, "A  README.md"]

    def test_a_rename_is_listed_under_the_directory_it_moved_into(self, panel, tmp_path):
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(
                GitStatusCode.RENAMED,
                str(tmp_path / "new" / "b.py"),
                str(tmp_path / "old" / "a.py"),
            ),
        ])

        assert _row_texts(panel) == ["new", "R  a.py \u2192 b.py"]

    def test_a_row_tooltip_carries_the_mindspace_relative_path(self, panel, tmp_path):
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(GitStatusCode.MODIFIED, str(tmp_path / "src" / "a.py")),
        ])

        item = panel._list_widget.item(1)  # pylint: disable=protected-access

        assert item.toolTip() == os.path.join("src", "a.py")


class TestLabelRows:
    """A label introduces its group but is not an entry that can be acted on."""

    def test_a_label_cannot_be_selected(self, panel, tmp_path):
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(GitStatusCode.MODIFIED, str(tmp_path / "src" / "a.py")),
        ])

        label = panel._list_widget.item(0)  # pylint: disable=protected-access

        assert label.data(_HEADER_ROLE) is True
        assert label.flags() == Qt.ItemFlag.NoItemFlags

    def test_a_label_carries_no_file_path(self, panel, tmp_path):
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(GitStatusCode.MODIFIED, str(tmp_path / "src" / "a.py")),
        ])

        label = panel._list_widget.item(0)  # pylint: disable=protected-access

        assert label.data(Qt.ItemDataRole.UserRole) is None


class TestRowPainting:
    """Both kinds of row render, whichever delegate path they take."""

    def test_every_row_paints(self, panel, tmp_path):
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(GitStatusCode.MODIFIED, str(tmp_path / "src" / "a.py")),
            _entry(GitStatusCode.ADDED, str(tmp_path / "README.md")),
        ])
        view = panel._list_widget  # pylint: disable=protected-access
        delegate = view.itemDelegate()
        model = view.model()

        pixmap = QPixmap(300, 200)
        painter = QPainter(pixmap)
        option = QStyleOptionViewItem()
        option.font = view.font()
        option.rect = QRect(0, 0, 300, 24)

        for row in range(model.rowCount()):
            delegate.paint(painter, option, model.index(row, 0))

        painter.end()


class TestRevealAndSelectFile:
    """Revealing a file selects its row, leaving the labels alone."""

    def test_the_row_of_the_revealed_file_is_selected(self, panel, tmp_path):
        target = str(tmp_path / "src" / "b.py")
        panel._on_status_changed([  # pylint: disable=protected-access
            _entry(GitStatusCode.MODIFIED, str(tmp_path / "src" / "a.py")),
            _entry(GitStatusCode.MODIFIED, target),
        ])

        panel.reveal_and_select_file(target)

        current = panel._list_widget.currentItem()  # pylint: disable=protected-access
        assert current.data(Qt.ItemDataRole.UserRole) == target

    def test_a_file_that_is_not_listed_leaves_the_selection_alone(self, panel, tmp_path):
        listed = str(tmp_path / "src" / "a.py")
        panel._on_status_changed([_entry(GitStatusCode.MODIFIED, listed)])  # pylint: disable=protected-access
        panel.reveal_and_select_file(listed)

        panel.reveal_and_select_file(str(tmp_path / "src" / "missing.py"))

        current = panel._list_widget.currentItem()  # pylint: disable=protected-access
        assert current.data(Qt.ItemDataRole.UserRole) == listed
