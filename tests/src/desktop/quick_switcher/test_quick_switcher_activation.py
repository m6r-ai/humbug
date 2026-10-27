"""Tests for what MainWindow does with a chosen Quick Switcher entry."""
# pylint: disable=protected-access

import logging

import pytest
from PySide6.QtGui import QAction, QKeySequence

from desktop.editor_tab.editor_tab import EditorTab
from desktop.main_window import MainWindow
from desktop.tab.tab_base import TabBase


class FakeContexts:
    """Records which context was focused."""

    def __init__(self) -> None:
        self.focused: list[str] = []

    def focus(self, context_id: str) -> None:
        self.focused.append(context_id)


class FakeMindspace:
    """Serves a single context registry."""

    def __init__(self, contexts: FakeContexts) -> None:
        self._contexts = contexts

    def contexts(self) -> FakeContexts:
        return self._contexts


class FakeMindspaceManager:
    """Stands in for MindspaceManager."""

    def __init__(self, contexts: FakeContexts) -> None:
        self._mindspace = FakeMindspace(contexts)

    def mindspace(self) -> FakeMindspace:
        return self._mindspace


class FakeEditorTab(EditorTab):
    """A tab that records the lines it is asked to go to."""

    def __init__(self) -> None:
        super().__init__("fake-tab", "")
        self.goto_lines: list[int] = []

    def goto_line_clamped(self, line: int) -> None:
        self.goto_lines.append(line)


class FakeTerminalTab(TabBase):
    """A tab that does not support going to a line."""

    def tool_name(self) -> str:
        return "terminal"


class FakeTabManager:
    """Serves tabs by id and reports the current tab."""

    def __init__(self, tabs: dict, current=None) -> None:
        self._tabs = tabs
        self._current = current

    def get_tab_by_id(self, tab_id: str):
        return self._tabs.get(tab_id)

    def get_current_tab(self):
        return self._current


class FakeHost:
    """A stand-in for MainWindow carrying only what the activation handler touches."""

    def __init__(self, tabs: dict) -> None:
        self.contexts = FakeContexts()
        self._mindspace_manager = FakeMindspaceManager(self.contexts)
        self._tab_manager = FakeTabManager(tabs)
        self._logger = logging.getLogger("test_quick_switcher_activation")
        self.opened_paths: list[str] = []
        self.open_result: str | None = None

    def _hide_quick_switcher(self) -> None:
        pass

    def _goto_line_in_tab(self, tab, line: int) -> None:
        MainWindow._goto_line_in_tab(self, tab, line)

    def _line_from_entry_id(self, line_text: str) -> int | None:
        return MainWindow._line_from_entry_id(self, line_text)

    def _open_by_panel_id(self, panel_id: str, path: str, ephemeral: bool) -> str | None:
        self.opened_paths.append(path)
        return self.open_result


@pytest.fixture
def editor_tab(qapp):
    """A fake editor tab, torn down with the Qt event loop."""
    tab = FakeEditorTab()
    yield tab
    tab.deleteLater()
    qapp.processEvents()


class TestJumpingToALineInAFile:
    """Tests for activating a ``file:line`` result."""

    def test_the_file_is_opened_by_path_and_scrolled(self, qapp, editor_tab):
        """The entry carries a path, which is opened (or focused) and then scrolled."""
        host = FakeHost({"opened-id": editor_tab})
        host.open_result = "opened-id"

        MainWindow._on_quick_switcher_entry_activated(host, "file-line:18:/mindspace/src/utils.py")

        assert host.opened_paths == ["/mindspace/src/utils.py"]
        assert editor_tab.goto_lines == [18]

    def test_a_path_containing_colons_survives(self, qapp, editor_tab):
        """Only the first two colons delimit the entry, so a Windows path stays intact."""
        host = FakeHost({"opened-id": editor_tab})
        host.open_result = "opened-id"

        MainWindow._on_quick_switcher_entry_activated(host, "file-line:7:C:/src/utils.py")

        assert host.opened_paths == ["C:/src/utils.py"]
        assert editor_tab.goto_lines == [7]

    def test_a_file_that_fails_to_open_jumps_nowhere(self, qapp, editor_tab):
        """Nothing is scrolled when the file could not be opened."""
        host = FakeHost({"opened-id": editor_tab})
        host.open_result = None

        MainWindow._on_quick_switcher_entry_activated(host, "file-line:18:/mindspace/missing.py")

        assert editor_tab.goto_lines == []

    def test_the_line_is_passed_to_the_tab_to_clamp(self, qapp, editor_tab):
        """The tab knows its own length, so the raw line reaches it unaltered."""
        host = FakeHost({"opened-id": editor_tab})
        host.open_result = "opened-id"

        MainWindow._on_quick_switcher_entry_activated(host, "file-line:9999:/mindspace/src/utils.py")

        assert editor_tab.goto_lines == [9999]

    def test_the_bare_line_form_reaches_the_current_tab(self, qapp, editor_tab):
        """``:line`` goes to whatever tab is in front."""
        host = FakeHost({})
        host._tab_manager = FakeTabManager({}, current=editor_tab)

        MainWindow._on_quick_switcher_entry_activated(host, "goto:42")

        assert editor_tab.goto_lines == [42]

    def test_a_tab_that_cannot_go_to_a_line_is_left_alone(self, qapp):
        """A terminal or conversation has no lines to go to, and must not raise."""
        tab = FakeTerminalTab("terminal-tab")
        host = FakeHost({})
        host._tab_manager = FakeTabManager({}, current=tab)

        MainWindow._on_quick_switcher_entry_activated(host, "goto:5")

        assert tab.can_show_goto_line() is False

    def test_no_current_tab_is_left_alone(self, qapp):
        """With nothing open there is nothing to scroll."""
        host = FakeHost({})
        host._tab_manager = FakeTabManager({}, current=None)

        MainWindow._on_quick_switcher_entry_activated(host, "goto:5")


class TestRunningACommand:
    """Tests for the commands offered behind the ``>`` prefix."""

    def test_only_enabled_commands_are_listed(self, qapp):
        """A command that cannot run now is left out rather than listed and ignored."""
        enabled = QAction("New File")
        disabled = QAction("Save")
        disabled.setEnabled(False)

        host = FakeHost({})
        host._quick_switcher_commands = lambda: {  # type: ignore[attr-defined]
            "new-file": (enabled, "editor"),
            "save": (disabled, "editor"),
        }

        entries = MainWindow._quick_switcher_command_entries(host)

        assert [entry.entry_id for entry in entries] == ["command:new-file"]

    def test_a_listed_command_shows_its_shortcut(self, qapp):
        """The palette teaches the shortcut rather than hiding it."""
        action = QAction("New File")
        action.setShortcut(QKeySequence("Ctrl+N"))

        host = FakeHost({})
        host._quick_switcher_commands = lambda: {"new-file": (action, "editor")}  # type: ignore[attr-defined]

        entries = MainWindow._quick_switcher_command_entries(host)

        assert entries[0].subtitle == QKeySequence("Ctrl+N").toString(QKeySequence.SequenceFormat.NativeText)

    def test_activating_a_command_triggers_its_action(self, qapp):
        """The listed entry and the action run are drawn from the same mapping."""
        action = QAction("New File")
        fired = []
        action.triggered.connect(lambda: fired.append(True))

        host = FakeHost({})
        host._quick_switcher_commands = lambda: {"new-file": (action, "editor")}  # type: ignore[attr-defined]

        MainWindow._run_quick_switcher_command(host, "new-file")

        assert fired == [True]

    def test_an_unknown_command_triggers_nothing(self, qapp):
        """A stale entry id must not fall through to some other command."""
        action = QAction("New File")
        fired = []
        action.triggered.connect(lambda: fired.append(True))

        host = FakeHost({})
        host._quick_switcher_commands = lambda: {"new-file": (action, "editor")}  # type: ignore[attr-defined]

        MainWindow._run_quick_switcher_command(host, "no-such-command")

        assert fired == []

    def test_a_disabled_command_is_not_triggered(self, qapp):
        """The enabled re-check guards against state that changed since the list was built."""
        action = QAction("Save")
        action.setEnabled(False)
        fired = []
        action.triggered.connect(lambda: fired.append(True))

        host = FakeHost({})
        host._quick_switcher_commands = lambda: {"save": (action, "editor")}  # type: ignore[attr-defined]

        MainWindow._run_quick_switcher_command(host, "save")

        assert fired == []


class TestMalformedEntryIds:
    """Tests for IDs that could not have come from the widget."""

    def test_a_non_numeric_line_is_ignored(self, qapp, editor_tab):
        """A malformed signal must not reach int() and raise."""
        host = FakeHost({"opened-id": editor_tab})
        host.open_result = "opened-id"

        MainWindow._on_quick_switcher_entry_activated(host, "file-line:abc:/mindspace/src/utils.py")

        assert host.opened_paths == []
        assert editor_tab.goto_lines == []

    def test_a_non_numeric_goto_is_ignored(self, qapp, editor_tab):
        """The bare line form is guarded the same way."""
        host = FakeHost({})
        host._tab_manager = FakeTabManager({}, current=editor_tab)

        MainWindow._on_quick_switcher_entry_activated(host, "goto:abc")

        assert editor_tab.goto_lines == []
