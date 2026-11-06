"""Tests for the background checkpoint poller."""

import asyncio

import pytest

from desktop.mindspace import mindspace_change_poller as poller_module
from desktop.mindspace.mindspace_change_poller import MindspaceChangePoller
from mindspace.mindspace_tree_scanner import TreeSnapshot


class _FakeWatcher:
    """Stands in for the change watcher, recording the interval it is asked for."""

    def __init__(self, interval: float = 5.0) -> None:
        self._interval = interval

    def interval_seconds(self) -> float:
        return self._interval


class _FakeMindspace:
    """Stands in for the mindspace, counting the walks and checkpoints it is asked for."""

    def __init__(self) -> None:
        self.walks = 0
        self.checkpoints = 0
        self.fail_walk = False

    def has_mindspace(self) -> bool:
        return True

    def change_watcher(self) -> _FakeWatcher:
        return _FakeWatcher()

    def scan_tree(self) -> TreeSnapshot:
        self.walks += 1
        if self.fail_walk:
            raise OSError("unreadable tree")

        return TreeSnapshot()

    def checkpoint_change_snapshot(self, snapshot: TreeSnapshot) -> None:
        assert snapshot is not None
        self.checkpoints += 1


class _FakeMindspaceManager:
    """Stands in for the manager, returning one fake mindspace."""

    def __init__(self, mindspace: _FakeMindspace) -> None:
        self._mindspace = mindspace

    def mindspace(self) -> _FakeMindspace:
        return self._mindspace


@pytest.fixture(autouse=True)
def _reset_poller(monkeypatch):
    """Give each test a poller that is not the process-wide singleton."""
    monkeypatch.setattr(MindspaceChangePoller, "_instance", None)
    yield
    MindspaceChangePoller._instance = None


@pytest.fixture(autouse=True)
def _event_loop():
    """Give each test a current event loop, which the poller dispatches onto."""
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)

    try:
        yield loop

    finally:
        pending = asyncio.all_tasks(loop)
        for task in pending:
            task.cancel()

        if pending:
            loop.run_until_complete(asyncio.gather(*pending, return_exceptions=True))

        loop.close()
        asyncio.set_event_loop(None)


def _poller(mindspace: _FakeMindspace) -> MindspaceChangePoller:
    poller = MindspaceChangePoller()
    poller._mindspace_manager = _FakeMindspaceManager(mindspace)  # pylint: disable=protected-access
    return poller


def _run(coro) -> None:
    """Run one coroutine to completion on the test's event loop."""
    asyncio.get_event_loop().run_until_complete(coro)


def test_a_trigger_takes_one_checkpoint():
    mindspace = _FakeMindspace()
    poller = _poller(mindspace)
    poller.start()
    _run(asyncio.sleep(0))
    before = mindspace.checkpoints

    poller.trigger()
    _run(asyncio.sleep(0))

    assert mindspace.checkpoints == before + 1


def test_a_trigger_while_a_walk_is_running_does_not_start_another():
    mindspace = _FakeMindspace()
    poller = _poller(mindspace)
    poller.start()
    _run(asyncio.sleep(0))
    before = mindspace.walks
    poller._in_flight = True  # pylint: disable=protected-access

    poller.trigger()

    assert mindspace.walks == before


def test_a_finished_walk_releases_the_in_flight_flag():
    mindspace = _FakeMindspace()
    poller = _poller(mindspace)
    poller.start()

    _run(poller._run_checkpoint())  # pylint: disable=protected-access

    assert poller._in_flight is False  # pylint: disable=protected-access


def test_a_failed_walk_releases_the_in_flight_flag():
    mindspace = _FakeMindspace()
    mindspace.fail_walk = True
    poller = _poller(mindspace)
    poller.start()

    _run(poller._run_checkpoint())  # pylint: disable=protected-access

    assert poller._in_flight is False  # pylint: disable=protected-access


def test_stopping_during_a_walk_releases_the_in_flight_flag():
    mindspace = _FakeMindspace()
    poller = _poller(mindspace)
    poller.start()

    poller.stop()
    _run(poller._run_checkpoint())  # pylint: disable=protected-access

    assert poller._in_flight is False  # pylint: disable=protected-access


def test_a_stopped_poller_does_not_checkpoint():
    mindspace = _FakeMindspace()
    poller = _poller(mindspace)

    poller.trigger()

    assert mindspace.checkpoints == 0


def test_a_walk_is_taken_before_the_checkpoint_is_recorded():
    mindspace = _FakeMindspace()
    poller = _poller(mindspace)
    poller.start()
    _run(asyncio.sleep(0))
    walks_before = mindspace.walks

    _run(poller._run_checkpoint())  # pylint: disable=protected-access

    assert mindspace.walks == walks_before + 1
    assert mindspace.checkpoints == mindspace.walks


def test_the_poller_does_not_import_time_for_a_throttle():
    """The cadence guards against concurrent walks, not a rate."""
    assert not hasattr(poller_module, "time")
