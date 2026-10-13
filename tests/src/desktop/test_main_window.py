"""
Tests for MainWindow mindspace opening and the instance registry.

These drive the real MainWindow methods against a lightweight host that supplies
only the collaborators the methods touch, so the ordering of the registry update
relative to the fallible UI restoration steps can be asserted without building
the whole application window.
"""

import logging
import os
import tempfile

import pytest

from desktop.language.en.en_strings import get_english_strings
from desktop.language.language_strings import LanguageStrings
from desktop.main_window import MainWindow
from desktop.message_box import MessageBoxButton, MessageBoxType
from desktop.user.instance_registry import InstanceRegistry
from mindspace.mindspace_error import MindspaceError, MindspaceExistsError


class FakeMindspaceManager:
    """Stands in for MindspaceManager, recording calls and raising on demand."""

    def __init__(self, mindspace_path: str = "", last_mindspace: str | None = None) -> None:
        self._mindspace_path = mindspace_path
        self._last_mindspace = last_mindspace
        self.open_error: Exception | None = None
        self.is_mindspace = True
        self.create_error: Exception | None = None
        self.created: list[tuple[str, list[str]]] = []

    def get_last_mindspace(self) -> str | None:
        return self._last_mindspace

    def mindspace_path(self) -> str:
        return self._mindspace_path

    def has_mindspace(self) -> bool:
        return bool(self._mindspace_path)

    def check_mindspace(self, path: str) -> bool:
        return self.is_mindspace

    def create_mindspace(self, path: str, folders: list[str]) -> None:
        if self.create_error is not None:
            raise self.create_error

        self.created.append((path, folders))
        self.is_mindspace = True

    def open_mindspace(self, path: str) -> None:
        if self.open_error is not None:
            raise self.open_error

        self._mindspace_path = path

    def close_mindspace(self) -> None:
        self._mindspace_path = ""


class FakeLanguageManager:
    """Stands in for LanguageManager, serving the real English strings."""

    def strings(self) -> LanguageStrings:
        return get_english_strings()


class FakeSidebarManager:
    """Stands in for SidebarManager, recording the mindspace paths it is given."""

    def __init__(self) -> None:
        self.paths: list[str] = []

    def set_mindspace(self, path: str) -> None:
        self.paths.append(path)


class FakeHost:
    """A minimal stand-in for MainWindow carrying just the collaborators used."""

    def __init__(
        self,
        mindspace_manager: FakeMindspaceManager,
        sidebar_manager: FakeSidebarManager,
        instance_registry: InstanceRegistry,
    ) -> None:
        self._mindspace_manager = mindspace_manager
        self._sidebar_manager = sidebar_manager
        self._instance_registry = instance_registry
        self._language_manager = FakeLanguageManager()
        self._logger = logging.getLogger("test_main_window")
        self._initial_mindspace_path: str | None = None
        self._restore_state_error: Exception | None = None
        self.restored_state = False
        self.tabs_can_close = True

    def _confirm_mindspace_initialization(self, path: str) -> bool:
        return MainWindow._confirm_mindspace_initialization(self, path)  # pylint: disable=protected-access

    def _create_mindspace_at(self, path: str) -> bool:
        return MainWindow._create_mindspace_at(self, path)  # pylint: disable=protected-access

    def _update_instance_registry(self) -> None:
        MainWindow._update_instance_registry(self)  # pylint: disable=protected-access

    def _restore_mindspace_state(self) -> None:
        if self._restore_state_error is not None:
            raise self._restore_state_error

        self.restored_state = True

    def _restore_prompt_marker_setting(self) -> None:
        pass

    def _save_mindspace_state(self) -> None:
        pass

    def _close_all_tabs(self) -> bool:
        return self.tabs_can_close


@pytest.fixture
def registry(tmp_path):
    """An instance registry backed by a temporary humbug directory."""
    return InstanceRegistry(str(tmp_path))


@pytest.fixture
def host(registry):
    """A fake host wired to a fresh mindspace manager, sidebar manager and registry."""
    mindspace_manager = FakeMindspaceManager()
    sidebar_manager = FakeSidebarManager()
    return FakeHost(mindspace_manager, sidebar_manager, registry)


def registered_path(registry: InstanceRegistry) -> str:
    """Return the mindspace path this process has registered, or empty if none."""
    path = registry._record_path(os.getpid())  # pylint: disable=protected-access
    if not os.path.exists(path):
        return ""

    record = registry._read_record(path)  # pylint: disable=protected-access
    assert record is not None
    return record.mindspace_path


class TestRestoreLastMindspaceRegistersTheOpenMindspace:
    def test_registers_the_mindspace_it_opens(self, host):
        with tempfile.TemporaryDirectory() as mindspace_path:
            host._initial_mindspace_path = mindspace_path  # pylint: disable=protected-access

            MainWindow._restore_last_mindspace(host)  # pylint: disable=protected-access

            assert registered_path(host._instance_registry) == mindspace_path  # pylint: disable=protected-access

    def test_registers_even_when_state_restoration_fails(self, host):
        with tempfile.TemporaryDirectory() as mindspace_path:
            host._initial_mindspace_path = mindspace_path  # pylint: disable=protected-access
            host._restore_state_error = MindspaceError("state restoration failed")  # pylint: disable=protected-access

            MainWindow._restore_last_mindspace(host)  # pylint: disable=protected-access

            assert registered_path(host._instance_registry) == mindspace_path  # pylint: disable=protected-access

    def test_registers_nothing_when_the_mindspace_fails_to_open(self, host):
        with tempfile.TemporaryDirectory() as mindspace_path:
            host._initial_mindspace_path = mindspace_path  # pylint: disable=protected-access
            host._mindspace_manager.open_error = MindspaceError("cannot open")  # pylint: disable=protected-access

            MainWindow._restore_last_mindspace(host)  # pylint: disable=protected-access

            assert registered_path(host._instance_registry) == ""  # pylint: disable=protected-access


class TestOpenMindspacePathRegistersTheOpenMindspace:
    def test_registers_the_mindspace_it_opens(self, host):
        with tempfile.TemporaryDirectory() as mindspace_path:
            MainWindow._open_mindspace_path(host, mindspace_path)  # pylint: disable=protected-access

            assert registered_path(host._instance_registry) == mindspace_path  # pylint: disable=protected-access

    def test_registers_even_when_state_restoration_fails(self, host):
        with tempfile.TemporaryDirectory() as mindspace_path:
            host._restore_state_error = MindspaceError("state restoration failed")  # pylint: disable=protected-access

            with pytest.raises(MindspaceError):
                MainWindow._open_mindspace_path(host, mindspace_path)  # pylint: disable=protected-access

            assert registered_path(host._instance_registry) == mindspace_path  # pylint: disable=protected-access


class FakeMessageBox:
    """Records the message boxes shown and answers them with a canned button."""

    def __init__(self, answer: MessageBoxButton = MessageBoxButton.OK) -> None:
        self.answer = answer
        self.shown: list[tuple[MessageBoxType, str]] = []

    def show_message(self, parent, msg_type, title, text, buttons=None, destructive=False):
        """Match MessageBox.show_message, recording the call instead of showing a dialog."""
        self.shown.append((msg_type, text))
        return self.answer


@pytest.fixture
def message_box(monkeypatch):
    """Replace MessageBox.show_message so dialogs are recorded rather than shown."""
    fake = FakeMessageBox()
    monkeypatch.setattr("desktop.main_window.MessageBox.show_message", fake.show_message)
    return fake


class TestInitializingAFolderThatIsNotAMindspace:
    def test_the_user_is_asked_before_anything_is_created(self, host, message_box):
        message_box.answer = MessageBoxButton.CANCEL
        host._mindspace_manager.is_mindspace = False  # pylint: disable=protected-access

        with tempfile.TemporaryDirectory() as path:
            assert MainWindow._open_mindspace_path(host, path) is None  # pylint: disable=protected-access

            assert host._mindspace_manager.created == []  # pylint: disable=protected-access
            assert registered_path(host._instance_registry) == ""  # pylint: disable=protected-access

        assert [msg_type for msg_type, _ in message_box.shown] == [MessageBoxType.QUESTION]

    def test_accepting_creates_the_mindspace_and_opens_it(self, host, message_box):
        host._mindspace_manager.is_mindspace = False  # pylint: disable=protected-access

        with tempfile.TemporaryDirectory() as path:
            MainWindow._open_mindspace_path(host, path)  # pylint: disable=protected-access

            assert host._mindspace_manager.created == [(path, [])]  # pylint: disable=protected-access
            assert registered_path(host._instance_registry) == path  # pylint: disable=protected-access

    def test_nothing_is_created_when_a_tab_refuses_to_close(self, host, message_box):
        """A mindspace left on disk that was never opened would be a stray directory."""
        host.tabs_can_close = False
        host._mindspace_manager = FakeMindspaceManager("/existing/mindspace")  # pylint: disable=protected-access
        host._mindspace_manager.is_mindspace = False  # pylint: disable=protected-access

        with tempfile.TemporaryDirectory() as path:
            MainWindow._open_mindspace_path(host, path)  # pylint: disable=protected-access

            assert host._mindspace_manager.created == []  # pylint: disable=protected-access
            assert registered_path(host._instance_registry) == ""  # pylint: disable=protected-access

    def test_a_creation_failure_is_reported_and_stops_the_open(self, host, message_box):
        host._mindspace_manager.is_mindspace = False  # pylint: disable=protected-access
        host._mindspace_manager.create_error = MindspaceError("disk full")  # pylint: disable=protected-access

        with tempfile.TemporaryDirectory() as path:
            MainWindow._open_mindspace_path(host, path)  # pylint: disable=protected-access

            assert registered_path(host._instance_registry) == ""  # pylint: disable=protected-access

        assert [msg_type for msg_type, _ in message_box.shown] == [
            MessageBoxType.QUESTION, MessageBoxType.CRITICAL
        ]
        assert "disk full" in message_box.shown[-1][1]

    def test_a_racing_creation_is_reported_and_stops_the_open(self, host, message_box):
        """Another process can win the race between the check and the create."""
        host._mindspace_manager.is_mindspace = False  # pylint: disable=protected-access
        host._mindspace_manager.create_error = MindspaceExistsError("already there")  # pylint: disable=protected-access

        with tempfile.TemporaryDirectory() as path:
            assert MainWindow._create_mindspace_at(host, path) is False  # pylint: disable=protected-access

        assert [msg_type for msg_type, _ in message_box.shown] == [MessageBoxType.CRITICAL]

    def test_an_existing_mindspace_is_opened_without_asking(self, host, message_box):
        with tempfile.TemporaryDirectory() as path:
            MainWindow._open_mindspace_path(host, path)  # pylint: disable=protected-access

            assert host._mindspace_manager.created == []  # pylint: disable=protected-access
            assert registered_path(host._instance_registry) == path  # pylint: disable=protected-access

        assert message_box.shown == []
