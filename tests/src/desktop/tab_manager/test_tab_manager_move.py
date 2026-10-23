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


def choose_context_menu_item(tab_manager, tab_id: str, label_attr: str, monkeypatch) -> None:
    """
    Open a tab's context menu and choose the item whose label is label_attr.

    The menu runs modally in production, so exec_ is replaced with a stub that
    returns the matching action.  Everything else - which items exist, their
    enablement and the handler they dispatch to - is the real code path.  The
    label is looked up through the language manager so the test works in any
    language.
    """
    from PySide6.QtCore import QPoint  # pylint: disable=import-outside-toplevel

    label = getattr(tab_manager._language_manager.strings(), label_attr)  # pylint: disable=protected-access

    def _exec(menu, _pos):
        for action in menu.actions():
            if action.text() == label:
                return action

        raise AssertionError(f"No menu item labelled {label!r}")

    monkeypatch.setattr("PySide6.QtWidgets.QMenu.exec_", _exec)
    tab_manager._on_tab_label_context_menu(tab_id, QPoint(0, 0))  # pylint: disable=protected-access


def menu_item_enabled(tab_manager, tab_id: str, label_attr: str, monkeypatch) -> bool:
    """Return whether the context menu item whose label is label_attr is enabled."""
    from PySide6.QtCore import QPoint  # pylint: disable=import-outside-toplevel

    label = getattr(tab_manager._language_manager.strings(), label_attr)  # pylint: disable=protected-access
    captured: dict[str, bool] = {}

    def _exec(menu, _pos):
        for action in menu.actions():
            captured[action.text()] = action.isEnabled()

        return None

    monkeypatch.setattr("PySide6.QtWidgets.QMenu.exec_", _exec)
    tab_manager._on_tab_label_context_menu(tab_id, QPoint(0, 0))  # pylint: disable=protected-access
    return captured[label]


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

    def test_whether_a_tab_can_move_to_an_adjacent_column(self, tab_manager, manager, tmp_path):
        """The predicate permits an edge move when a new column can be created."""
        first, _second, third = self._two_columns(tab_manager, manager, tmp_path)

        make_current(tab_manager, first)
        assert tab_manager.can_move_tab_to_column(move_left=True) is True
        assert tab_manager.can_move_tab_to_column(move_left=False) is True

        make_current(tab_manager, third)
        assert tab_manager.can_move_tab_to_column(move_left=True) is True
        assert tab_manager.can_move_tab_to_column(move_left=False) is False

    def test_a_lone_tab_cannot_create_an_empty_column(self, tab_manager, manager, tmp_path):
        """A lone tab cannot move beyond either edge because empty columns are not persisted."""
        only, = open_tabs(tab_manager, manager, tmp_path, 1)
        make_current(tab_manager, only)

        assert tab_manager.can_move_tab_to_column(move_left=True) is False
        assert tab_manager.can_move_tab_to_column(move_left=False) is False


class TestMovingATabKeepsItsContext:
    """A move relocates a tab within the workspace, so its context must survive."""

    def _two_columns(self, tab_manager, manager, tmp_path):
        """Build a left column of two tabs and a right column of one."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)
        split_into_two_columns(tab_manager, manager, third)
        return first, second, third

    def test_the_context_still_exists_after_the_move(self, tab_manager, manager, tmp_path):
        """The moved tab is still an open context, now in its new column."""
        _first, second, _third = self._two_columns(tab_manager, manager, tmp_path)

        tab_manager.move_tab_to_adjacent_column(second, move_left=False)

        info = manager.mindspace().contexts().get(second)
        assert info is not None
        assert info.column == 1

    def test_the_content_state_survives_the_move(self, tab_manager, manager, tmp_path):
        """Content state belongs to the context, so relocating it must not clear it."""
        _first, second, _third = self._two_columns(tab_manager, manager, tmp_path)
        registry = manager.mindspace().contexts()
        registry._content_state[second] = {"cursor": 12}  # pylint: disable=protected-access

        tab_manager.move_tab_to_adjacent_column(second, move_left=False)

        assert registry.get_content_state(second) == {"cursor": 12}

    def test_the_view_state_survives_the_move(self, tab_manager, manager, tmp_path):
        """View state belongs to the context, so relocating it must not clear it."""
        _first, second, _third = self._two_columns(tab_manager, manager, tmp_path)
        registry = manager.mindspace().contexts()
        registry.set_frontend_state(second, {"scroll": 40})

        tab_manager.move_tab_to_adjacent_column(second, move_left=False)

        assert registry.get_frontend_state(second) == {"scroll": 40}

    def test_the_model_survives_the_move(self, tab_manager, manager, tmp_path):
        """The rebuilt tab registers a model for the same context id."""
        _first, second, _third = self._two_columns(tab_manager, manager, tmp_path)
        registry = manager.mindspace().contexts()
        model = registry.get_model(second, object)
        assert model is not None

        tab_manager.move_tab_to_adjacent_column(second, move_left=False)

        moved_model = registry.get_model(second, object)
        assert moved_model is not None
        assert moved_model.context_id() == second


@pytest.fixture
def arabic(tab_manager):
    """Switch to a right-to-left language for the duration of a test."""
    from desktop.language.language_code import LanguageCode  # pylint: disable=import-outside-toplevel

    manager = tab_manager._language_manager  # pylint: disable=protected-access
    original = manager.current_language()
    manager.set_language(LanguageCode.AR)

    yield

    manager.set_language(original)


class TestContextMenuDirection:
    """Menu items must act in the direction their label names, in either layout."""

    def _two_columns(self, tab_manager, manager, tmp_path):
        """Build a left column of two tabs and a right column of one."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)
        split_into_two_columns(tab_manager, manager, third)
        return first, second, third

    def test_move_to_left_column_moves_to_the_lower_column_index(
        self, tab_manager, manager, tmp_path, monkeypatch
    ):
        """In a left-to-right layout, left is the lower column index."""
        _first, _second, third = self._two_columns(tab_manager, manager, tmp_path)

        choose_context_menu_item(tab_manager, third, "move_tab_to_left_column", monkeypatch)

        assert manager.mindspace().contexts().get(third).column == 0

    def test_move_to_left_column_moves_to_the_higher_column_index_in_rtl(
        self, tab_manager, manager, tmp_path, monkeypatch, arabic
    ):
        """In a right-to-left layout the visual order is mirrored, so left is higher."""
        _first, second, _third = self._two_columns(tab_manager, manager, tmp_path)

        choose_context_menu_item(tab_manager, second, "move_tab_to_left_column", monkeypatch)

        assert manager.mindspace().contexts().get(second).column == 1

    def test_move_tab_left_moves_one_place_towards_the_start(
        self, tab_manager, manager, tmp_path, monkeypatch
    ):
        """In a left-to-right layout, left is one place towards the first tab."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        choose_context_menu_item(tab_manager, third, "move_tab_left", monkeypatch)

        assert qt_order(tab_manager) == [first, third, second]

    def test_move_tab_left_moves_one_place_towards_the_end_in_rtl(
        self, tab_manager, manager, tmp_path, monkeypatch, arabic
    ):
        """In a right-to-left layout, left is one place towards the last tab."""
        first, second, third = open_tabs(tab_manager, manager, tmp_path, 3)

        choose_context_menu_item(tab_manager, first, "move_tab_left", monkeypatch)

        assert qt_order(tab_manager) == [second, first, third]

    def test_move_to_new_column_opens_the_column_on_the_right(
        self, tab_manager, manager, tmp_path, monkeypatch
    ):
        """In a left-to-right layout the new column opens to the right."""
        first, second = open_tabs(tab_manager, manager, tmp_path, 2)

        choose_context_menu_item(tab_manager, second, "move_tab_to_new_column", monkeypatch)

        assert qt_order(tab_manager, 0) == [first]
        assert qt_order(tab_manager, 1) == [second]

    def test_move_to_new_column_opens_the_column_on_the_right_in_rtl(
        self, tab_manager, manager, tmp_path, monkeypatch, arabic
    ):
        """In a right-to-left layout the visual order is mirrored, so right is the lower index."""
        first, second = open_tabs(tab_manager, manager, tmp_path, 2)

        choose_context_menu_item(tab_manager, second, "move_tab_to_new_column", monkeypatch)

        assert qt_order(tab_manager, 0) == [second]
        assert qt_order(tab_manager, 1) == [first]

    def test_move_tab_left_is_disabled_for_the_first_tab(
        self, tab_manager, manager, tmp_path, monkeypatch
    ):
        """A tab at the start of its column has nowhere to go left."""
        first, _second, _third = open_tabs(tab_manager, manager, tmp_path, 3)

        assert menu_item_enabled(tab_manager, first, "move_tab_left", monkeypatch) is False


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
