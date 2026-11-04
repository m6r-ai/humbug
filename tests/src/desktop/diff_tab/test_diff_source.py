"""Tests for the sources a diff view gets its two versions from."""

import os
import subprocess

import pytest

from desktop.diff_tab.diff_source import DiffSourceError, GitWorkingTreeDiffSource, JournalDiffSource
from mindspace.journal_entry import JournalActor
from mindspace.mindspace import Mindspace
from mindspace.mindspace_settings import MindspaceSettings


def _run(args: list[str], cwd: str) -> None:
    """Run a command, raising on failure."""
    subprocess.run(args, cwd=cwd, capture_output=True, text=True, check=True)


@pytest.fixture
def repo(tmp_path) -> str:
    """Create a git repository holding one committed file."""
    root = str(tmp_path / "repo")
    os.makedirs(root)
    _run(["git", "init"], cwd=root)
    _run(["git", "config", "user.email", "test@test.com"], cwd=root)
    _run(["git", "config", "user.name", "Test User"], cwd=root)

    with open(os.path.join(root, "notes.txt"), "w", encoding="utf-8") as f:
        f.write("committed line\n")

    _run(["git", "add", "notes.txt"], cwd=root)
    _run(["git", "commit", "-m", "Initial commit"], cwd=root)
    return root


@pytest.fixture
def mindspace(tmp_path) -> Mindspace:
    """Open a mindspace whose root is a temp directory."""
    root = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(root, ".humbug"))
    MindspaceSettings(enabled_tools={}).save(os.path.join(root, ".humbug", "settings.json"))
    opened = Mindspace(
        on_settings_changed=lambda: None,
        on_interactions_updated=lambda: None,
    )
    opened.open_mindspace(root)
    yield opened
    opened.close_mindspace()


class TestGitWorkingTreeDiffSource:
    """A git source compares the working tree with HEAD."""

    def test_compares_the_committed_version_with_the_working_tree(self, repo):
        path = os.path.join(repo, "notes.txt")
        with open(path, "a", encoding="utf-8") as f:
            f.write("uncommitted line\n")

        old_lines, new_lines, diff_text = GitWorkingTreeDiffSource(path, repo).fetch()

        assert old_lines == ["committed line"]
        assert new_lines == ["committed line", "uncommitted line"]
        assert "+uncommitted line" in diff_text

    def test_an_untracked_file_has_no_old_side(self, repo):
        path = os.path.join(repo, "new.txt")
        with open(path, "w", encoding="utf-8") as f:
            f.write("brand new\n")

        old_lines, new_lines, _diff_text = GitWorkingTreeDiffSource(path, repo).fetch()

        assert old_lines == []
        assert new_lines == ["brand new"]

    def test_a_file_outside_any_repository_is_reported(self, tmp_path):
        path = tmp_path / "loose.txt"
        path.write_text("content", encoding="utf-8")

        with pytest.raises(DiffSourceError):
            GitWorkingTreeDiffSource(str(path), str(tmp_path)).fetch()


class TestJournalDiffSource:
    """A journal source compares the replaced version with the file as it is now."""

    def _replaced(self, mindspace: Mindspace, path: str, name: str, old: bytes, new: bytes) -> int:
        """Record a write of new over old and leave the file holding the new content."""
        with open(path, "wb") as f:
            f.write(old)

        entry = mindspace.file_journal().record_write(path, name, new, JournalActor.AI, "conv")
        assert entry is not None

        with open(path, "wb") as f:
            f.write(new)

        return entry.sequence

    def test_compares_the_replaced_version_with_the_file_now(self, mindspace, tmp_path):
        path = os.path.join(mindspace.mindspace_path(), "notes.txt")
        sequence = self._replaced(mindspace, path, "notes.txt", b"before\n", b"after\n")

        old_lines, new_lines, diff_text = JournalDiffSource(path, sequence, mindspace).fetch()

        assert old_lines == ["before"]
        assert new_lines == ["after"]
        assert "-before" in diff_text
        assert "+after" in diff_text

    def test_a_deleted_file_has_no_current_side(self, mindspace):
        path = os.path.join(mindspace.mindspace_path(), "gone.txt")
        sequence = self._replaced(mindspace, path, "gone.txt", b"removed content\n", b"")
        os.remove(path)

        old_lines, new_lines, _diff_text = JournalDiffSource(path, sequence, mindspace).fetch()

        assert old_lines == ["removed content"]
        assert new_lines == []

    def test_binary_content_is_shown_with_replacement_characters_rather_than_failing(self, mindspace):
        path = os.path.join(mindspace.mindspace_path(), "image.bin")
        sequence = self._replaced(mindspace, path, "image.bin", b"\xff\xfe\x00binary", b"\xff\xfe\x00changed")

        old_lines, _new_lines, _diff_text = JournalDiffSource(path, sequence, mindspace).fetch()

        assert old_lines

    def test_an_unknown_sequence_is_reported(self, mindspace):
        path = os.path.join(mindspace.mindspace_path(), "notes.txt")
        with open(path, "w", encoding="utf-8") as f:
            f.write("content\n")

        with pytest.raises(DiffSourceError):
            JournalDiffSource(path, 999, mindspace).fetch()

    def test_a_creation_has_no_replaced_version(self, mindspace):
        path = os.path.join(mindspace.mindspace_path(), "new.txt")
        entry = mindspace.file_journal().record_write(path, "new.txt", b"new\n", JournalActor.AI, "conv")
        assert entry is not None
        with open(path, "wb") as f:
            f.write(b"new\n")

        with pytest.raises(DiffSourceError):
            JournalDiffSource(path, entry.sequence, mindspace).fetch()

    def test_a_file_outside_the_mindspace_is_reported(self, mindspace, tmp_path):
        outside = tmp_path / "elsewhere.txt"
        outside.write_text("content", encoding="utf-8")

        with pytest.raises(DiffSourceError):
            JournalDiffSource(str(outside), 1, mindspace).fetch()
