"""Tests for moving tabs between and within columns without dragging them."""

import os
import uuid

import pytest

# pylint: disable=wrong-import-position
from desktop.conversation_tab.conversation_tab import ConversationTab
from desktop.mindspace.mindspace_manager import MindspaceManager
from desktop.tab_manager.tab_manager import TabManager
from mindspace.mindspace_settings import MindspaceSettings


@pytest.fixture
def manager(qapp, tmp_path):
    """Create a MindspaceManager with an isolated home config file."""
    MindspaceManager._instance = None

    mgr = MindspaceManager()
    mgr._home_config = str(tmp_path / "mindspace.json")

    yield mgr

    MindspaceManager._instance = None


@pytest.fixture
def tab_manager(qapp, manager, tmp_path):
    """Create a TabManager subscribed to an open mindspace's registry."""
    path = os.path.join(str(tmp_path), "ms")
    os.makedirs(os.path.join(path, ".humbug"))
    manager._mindspace._path = path  # pylint: disable=protected-access
    manager._mindspace._settings = MindspaceSettings(enabled_tools={})  # pylint: disable=protected-access

    tm = TabManager(lambda source_type, path: None, None)

    def _create_conversation_tab(info, _registry, parent):
        return ConversationTab(info.context_id, info.path, parent)

    tm.register_context_factory("conversation", _create_conversation_tab)
    tm._subscribe_to_registry()  # pylint: disable=protected-access
    return tm


def open_tabs(tab_manager, manager, tmp_path, count: int) -> list[str]:
    """Open conversation tabs in the active column and return their IDs in order."""
    registry = manager.mindspace().contexts()
    opened = [
        registry.open(
            context_type="conversation",
            path=str(tmp_path / f"conv-{uuid.uuid4().hex}.json"),
        )
        for _ in range(count)
    ]
    return [cid for cid in qt_order(tab_manager, 0) if cid in set(opened)]


def split_into_two_columns(tab_manager, manager, tab_id: str) -> None:
    """Send a tab into a new column to the right, the way the user's split does."""
    manager.mindspace().contexts().split_column(tab_id, split_left=False)


def make_current(tab_manager, tab_id: str) -> None:
    """Make a tab the current one in its column, and that column the active one."""
    tab = tab_manager._tabs[tab_id]  # pylint: disable=protected-access
    column = tab_manager._find_column_for_tab(tab)  # pylint: disable=protected-access
    column.setCurrentWidget(tab)
    tab_manager._active_column = column  # pylint: disable=protected-access


def qt_order(tab_manager, column_index: int = 0) -> list[str]:
    """Return the context IDs of a column's tabs in tab-bar order."""
    column = tab_manager._tab_columns[column_index]  # pylint: disable=protected-access
    return [column.widget(index).tab_id() for index in range(column.count())]


def registry_order(manager, column_index: int = 0) -> list[str]:
    """Return the context IDs of a column in the registry's position order."""
    members = manager.mindspace().contexts().column_members(column_index)
    return [info.context_id for info in members]


class TestMovingATabWithinItsColumn:
    """Tests for reordering a tab without dragging it."""

    def test_moving_towards_the_end_swaps_with_the_next_tab(self, tab_manager, manager, tmp_path):
        """Moving a tab one place along puts it after the tab it passed."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        tab_manager.move_tab_within_column(first, towards_start=False)

        assert qt_order(tab_manager) == [second, first, third]

    def test_moving_towards_the_start_swaps_with_the_previous_tab(self, tab_manager, manager, tmp_path):
        """Moving a tab one place back puts it before the tab it passed."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        tab_manager.move_tab_within_column(third, towards_start=True)

        assert qt_order(tab_manager) == [first, third, second]

    def test_moving_to_the_start_puts_the_tab_first(self, tab_manager, manager, tmp_path):
        """A tab sent to the start leads the column."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        tab_manager.move_tab_to_column_end(third, towards_start=True)

        assert qt_order(tab_manager) == [third, first, second]

    def test_moving_to_the_end_puts_the_tab_last(self, tab_manager, manager, tmp_path):
        """A tab sent to the end trails the column."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        tab_manager.move_tab_to_column_end(first, towards_start=False)

        assert qt_order(tab_manager) == [second, third, first]

    def test_the_registry_matches_the_tab_bar_after_a_move(self, tab_manager, manager, tmp_path):
        """The saved session is ordered by registry position, so it must agree with Qt."""
        first, _second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        tab_manager.move_tab_to_column_end(third, towards_start=True)
        tab_manager.move_tab_within_column(first, towards_start=False)

        assert registry_order(manager) == qt_order(tab_manager)

    def test_moving_past_the_end_is_refused(self, tab_manager, manager, tmp_path):
        """The last tab cannot move further along, and nothing is disturbed."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        tab_manager.move_tab_within_column(third, towards_start=False)

        assert qt_order(tab_manager) == [first, second, third]

    def test_moving_past_the_start_is_refused(self, tab_manager, manager, tmp_path):
        """The first tab cannot move further back, and nothing is disturbed."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        tab_manager.move_tab_within_column(first, towards_start=True)

        assert qt_order(tab_manager) == [first, second, third]


class TestWhetherATabCanMoveWithinItsColumn:
    """Tests for the predicate that drives menu enablement."""

    def test_the_first_tab_cannot_move_towards_the_start(self, tab_manager, manager, tmp_path):
        """A tab already at the start has nowhere to go."""
        first, _second = open_tabs(tab_manager, manager, tmp_path, 2)
        make_current(tab_manager, first)

        assert tab_manager.can_move_tab_within_column(towards_start=True) is False
        assert tab_manager.can_move_tab_within_column(towards_start=False) is True

    def test_the_last_tab_cannot_move_towards_the_end(self, tab_manager, manager, tmp_path):
        """A tab already at the end has nowhere to go."""
        _first, second = open_tabs(tab_manager, manager, tmp_path, 2)
        make_current(tab_manager, second)

        assert tab_manager.can_move_tab_within_column(towards_start=False) is False
        assert tab_manager.can_move_tab_within_column(towards_start=True) is True

    def test_a_lone_tab_cannot_move_either_way(self, tab_manager, manager, tmp_path):
        """A column of one offers no reordering."""
        only, = open_tabs(tab_manager, manager, tmp_path, 1)
        make_current(tab_manager, only)

        assert tab_manager.can_move_tab_within_column(towards_start=True) is False
        assert tab_manager.can_move_tab_within_column(towards_start=False) is False


class TestMovingATabBetweenColumns:
    """Tests for sending a tab to the column beside it."""

    def _two_columns(self, tab_manager, manager, tmp_path):
        """Build a left column of two tabs and a right column of one."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)
        split_into_two_columns(tab_manager, manager, third)
        return first, second, third

    def test_moving_right_lands_the_tab_in_the_next_column(self, tab_manager, manager, tmp_path):
        """The tab leaves its column and joins the one to its right."""
        first, second, third = self._two_columns(tab_manager, manager, tmp_path)

        tab_manager.move_tab_to_adjacent_column(second, move_left=False)

        assert qt_order(tab_manager, 0) == [first]
        assert qt_order(tab_manager, 1) == [third, second]

    def test_moving_left_lands_the_tab_in_the_previous_column(self, tab_manager, manager, tmp_path):
        """The tab leaves its column and joins the one to its left."""
        first, second, third = self._two_columns(tab_manager, manager, tmp_path)

        tab_manager.move_tab_to_adjacent_column(third, move_left=True)

        assert qt_order(tab_manager, 0) == [first, second, third]

    def test_the_registry_column_follows_the_move(self, tab_manager, manager, tmp_path):
        """A moved tab's registry column must match where it actually is."""
        _first, second, _third = self._two_columns(tab_manager, manager, tmp_path)

        tab_manager.move_tab_to_adjacent_column(second, move_left=False)

        assert manager.mindspace().contexts().get(second).column == 1
        assert registry_order(manager, 1) == qt_order(tab_manager, 1)

    def test_moving_beyond_the_last_column_creates_a_new_column(self, tab_manager, manager, tmp_path):
        """Moving right at the edge creates a column and moves the tab into it."""
        first, second = open_tabs(tab_manager, manager, tmp_path, 2)

        tab_manager.move_tab_to_adjacent_column(second, move_left=False)

        assert qt_order(tab_manager, 0) == [first]
        assert qt_order(tab_manager, 1) == [second]

    def test_moving_beyond_the_first_column_creates_a_new_column(self, tab_manager, manager, tmp_path):
        """Moving left at the edge creates a column and moves the tab into it."""
        first, second = open_tabs(tab_manager, manager, tmp_path, 2)

        tab_manager.move_tab_to_adjacent_column(first, move_left=True)

        assert qt_order(tab_manager, 0) == [first]
        assert qt_order(tab_manager, 1) == [second]

    def test_moving_a_lone_tab_right_keeps_an_empty_left_column(self, tab_manager, manager, tmp_path):
        """Moving a lone tab right creates the requested empty column on its left."""
        only, = open_tabs(tab_manager, manager, tmp_path, 1)

        tab_manager.move_tab_to_adjacent_column(only, move_left=False)

        assert qt_order(tab_manager, 0) == []
        assert qt_order(tab_manager, 1) == [only]
        assert manager.mindspace().contexts().get(only).column == 1

    def test_whether_a_tab_can_move_to_an_adjacent_column(self, tab_manager, manager, tmp_path):
        """The predicate permits an edge move when a new column can be created."""
        first, _second, third = self._two_columns(tab_manager, manager, tmp_path)

        make_current(tab_manager, first)
        assert tab_manager.can_move_tab_to_column(move_left=True) is True
        assert tab_manager.can_move_tab_to_column(move_left=False) is True

        make_current(tab_manager, third)
        assert tab_manager.can_move_tab_to_column(move_left=True) is True
        assert tab_manager.can_move_tab_to_column(move_left=False) is True

    def test_a_lone_tab_can_move_right_into_a_new_column(self, tab_manager, manager, tmp_path):
        """The right-edge shortcut remains available for a one-tab column."""
        only, = open_tabs(tab_manager, manager, tmp_path, 1)
        make_current(tab_manager, only)

        assert tab_manager.can_move_tab_to_column(move_left=True) is False
        assert tab_manager.can_move_tab_to_column(move_left=False) is True


class TestDraggingKeepsTheRegistryInStep:
    """A drag reorders the tab bar directly, so the registry must be re-read."""

    def test_dropping_a_tab_earlier_updates_the_registry_order(self, tab_manager, manager, tmp_path):
        """Keyboard moves read position from the registry, so a drag must update it."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)
        column = tab_manager._tab_columns[0]  # pylint: disable=protected-access

        tab_manager._on_column_widget_tab_dropped(third, column, 0)  # pylint: disable=protected-access

        assert qt_order(tab_manager) == [third, first, second]
        assert registry_order(manager) == [third, first, second]

    def test_a_keyboard_move_after_a_drag_leaves_the_order_saveable(self, tab_manager, manager, tmp_path):
        """A stale position would reorder the registry differently from the tab bar."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)
        column = tab_manager._tab_columns[0]  # pylint: disable=protected-access

        tab_manager._on_column_widget_tab_dropped(third, column, 0)  # pylint: disable=protected-access
        tab_manager.move_tab_within_column(first, towards_start=False)

        assert qt_order(tab_manager) == [third, second, first]
        assert registry_order(manager) == qt_order(tab_manager)
