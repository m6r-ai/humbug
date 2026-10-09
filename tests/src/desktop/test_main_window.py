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

from desktop.main_window import MainWindow
from desktop.user.instance_registry import InstanceRegistry
from mindspace.mindspace_error import MindspaceError


class FakeMindspaceManager:
    """Stands in for MindspaceManager, recording calls and raising on demand."""

    def __init__(self, mindspace_path: str = "", last_mindspace: str | None = None) -> None:
        self._mindspace_path = mindspace_path
        self._last_mindspace = last_mindspace
        self.open_error: Exception | None = None

    def get_last_mindspace(self) -> str | None:
        return self._last_mindspace

    def mindspace_path(self) -> str:
        return self._mindspace_path

    def has_mindspace(self) -> bool:
        return bool(self._mindspace_path)

    def check_mindspace(self, path: str) -> bool:
        return True

    def open_mindspace(self, path: str) -> None:
        if self.open_error is not None:
            raise self.open_error

        self._mindspace_path = path

    def close_mindspace(self) -> None:
        self._mindspace_path = ""


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
        self._logger = logging.getLogger("test_main_window")
        self._initial_mindspace_path: str | None = None
        self._restore_state_error: Exception | None = None
        self.restored_state = False

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
        return True


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
