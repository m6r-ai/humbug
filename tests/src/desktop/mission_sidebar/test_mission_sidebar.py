"""Tests for the Missions panel of the sidebar."""

import json
import os

import pytest
from PySide6.QtCore import Qt
from PySide6.QtWidgets import QTreeWidget

# pylint: disable=wrong-import-position
from desktop.color_role import ColorRole
from desktop.conversation_sidebar.conversation_sidebar import ConversationSidebar
from desktop.mission_sidebar.mission_sidebar import MissionSidebar, MissionSidebarEntry


def _write_conv(path: str, messages: list[str], parent_message_id: str | None = None) -> None:
    """Write a conversation holding messages with these ids, delegated from a parent message if given."""
    parent = {"message_id": parent_message_id, "tool_call_id": "call"} if parent_message_id else None
    with open(path, "w", encoding="utf-8") as f:
        json.dump({
            "metadata": {"version": "0.1", "parent": parent},
            "conversation": [
                {"id": mid, "timestamp": "2024-01-01T00:00:00+00:00", "type": "user_message", "content": "x"}
                for mid in messages
            ],
        }, f)


def _mission(mission_id: str, lead_path: str, archived: bool = False) -> MissionSidebarEntry:
    """Return a mission row led by a conversation."""
    return MissionSidebarEntry(
        mission_id=mission_id,
        title=f"Mission {mission_id}",
        status_text="Working",
        status_role=ColorRole.TEXT_SUCCESS,
        lead_path=lead_path,
        archived=archived,
    )


@pytest.fixture
def panels(mindspace_sandbox):
    """The Conversations and Missions panels, for a mindspace with a lead, its helper and an ordinary conversation."""
    conv_dir = mindspace_sandbox.conversations_dir
    _write_conv(os.path.join(conv_dir, "chat.conv"), ["c1"])
    _write_conv(os.path.join(conv_dir, "Mission-lead.conv"), ["l1"])
    _write_conv(os.path.join(conv_dir, "helper.conv"), ["h1"], parent_message_id="l1")
    conversations = ConversationSidebar()
    conversations.set_mindspace(mindspace_sandbox.path)
    missions = MissionSidebar()
    missions.set_mindspace(mindspace_sandbox.path)
    yield conversations, missions, conv_dir
    conversations.deleteLater()
    missions.deleteLater()


def _show(panels, entries: list[MissionSidebarEntry]) -> None:
    """List missions as the main window does: in the Missions panel, and out of the Conversations panel."""
    conversations, missions, _conv_dir = panels
    conversations.set_mission_conversations({entry.lead_path for entry in entries if entry.lead_path})
    missions.set_missions(entries, conversations.conversations_index().get_children)


def _tree(widget: MissionSidebar) -> QTreeWidget:
    """Return the panel's tree."""
    tree = widget.findChild(QTreeWidget, "MissionSidebarTree")
    assert tree is not None
    return tree


def _in_conversations(widget: ConversationSidebar, path: str) -> bool:
    """Say whether a conversation is listed in the Conversations panel."""
    model = widget._dag_model  # pylint: disable=protected-access
    return model.natural_index_for_path(os.path.normpath(path)).isValid()


class TestMissionSidebar:
    """Missions have a sidebar panel of their own."""

    def test_with_no_missions_the_panel_says_so(self, panels):
        """With no missions, the panel shows a hint rather than an empty list."""
        _show(panels, [])
        assert _tree(panels[1]).isHidden()

    def test_missions_are_listed_with_their_conversations(self, panels):
        """Each mission is listed, and unfolds to its lead with the helpers the lead delegated to beneath it."""
        _conversations, missions, conv_dir = panels
        _show(panels, [_mission("m1", os.path.normpath(os.path.join(conv_dir, "Mission-lead.conv")))])

        tree = _tree(missions)
        assert not tree.isHidden()
        assert tree.topLevelItemCount() == 1
        lead_item = tree.topLevelItem(0).child(0)
        assert lead_item.text(0) == "Mission-lead"
        assert [lead_item.child(i).text(0) for i in range(lead_item.childCount())] == ["helper"]

    def test_mission_conversations_leave_the_conversations_panel(self, panels):
        """A mission's lead and its helpers are listed under the mission, not with the other conversations."""
        conversations, _missions, conv_dir = panels
        lead = os.path.join(conv_dir, "Mission-lead.conv")
        assert _in_conversations(conversations, lead)

        _show(panels, [_mission("m1", lead)])
        assert not _in_conversations(conversations, lead)
        assert not _in_conversations(conversations, os.path.join(conv_dir, "helper.conv"))
        assert _in_conversations(conversations, os.path.join(conv_dir, "chat.conv"))

        _show(panels, [])
        assert _in_conversations(conversations, lead)

    def test_clicking_a_mission_opens_it(self, panels):
        """A click opens a mission in a temporary tab, and a double click in one that stays."""
        _conversations, missions, conv_dir = panels
        _show(panels, [_mission("m1", os.path.join(conv_dir, "Mission-lead.conv"))])
        opened = []
        missions.mission_activated.connect(lambda m, ephemeral: opened.append((m, ephemeral)))

        tree = _tree(missions)
        tree.itemClicked.emit(tree.topLevelItem(0), 0)
        tree.itemDoubleClicked.emit(tree.topLevelItem(0), 0)
        assert opened == [("m1", True), ("m1", False)]

    def test_clicking_a_mission_conversation_opens_it(self, panels):
        """Clicking a conversation under a mission opens that conversation."""
        _conversations, missions, conv_dir = panels
        lead = os.path.normpath(os.path.join(conv_dir, "Mission-lead.conv"))
        _show(panels, [_mission("m1", lead)])
        clicked = []
        missions.file_clicked.connect(lambda panel, path, ephemeral: clicked.append((panel, path, ephemeral)))

        tree = _tree(missions)
        tree.itemClicked.emit(tree.topLevelItem(0).child(0), 0)
        assert clicked == [("conversations", lead, True)]

    def test_unfolded_missions_stay_unfolded(self, panels):
        """A mission the user unfolded is still unfolded after the list is updated."""
        _conversations, missions, conv_dir = panels
        entries = [_mission("m1", os.path.join(conv_dir, "Mission-lead.conv"))]
        _show(panels, entries)
        _tree(missions).topLevelItem(0).setExpanded(True)

        _show(panels, entries)
        assert _tree(missions).topLevelItem(0).isExpanded()

    def test_new_mission_button(self, panels):
        """The panel's + button asks for a new mission, and is unavailable without a mindspace."""
        _conversations, missions, _conv_dir = panels
        requested = []
        missions.new_mission_requested.connect(lambda: requested.append(True))
        button = missions._new_mission_button  # pylint: disable=protected-access
        assert button.isEnabled()
        button.click()
        assert requested == [True]

        missions.set_mindspace("")
        assert not button.isEnabled()

    def test_revealing_a_lead_selects_its_mission(self, panels):
        """Revealing a mission's lead conversation selects the mission it leads."""
        _conversations, missions, conv_dir = panels
        _show(panels, [_mission("m1", os.path.join(conv_dir, "Mission-lead.conv"))])

        missions.reveal_and_select_file(os.path.join(conv_dir, "Mission-lead.conv"))
        assert _tree(missions).currentItem() is _tree(missions).topLevelItem(0)

    def test_revealing_a_helper_unfolds_its_mission(self, panels):
        """Revealing a helper conversation unfolds its mission and selects the helper."""
        _conversations, missions, conv_dir = panels
        _show(panels, [_mission("m1", os.path.join(conv_dir, "Mission-lead.conv"))])

        missions.reveal_and_select_file(os.path.join(conv_dir, "helper.conv"))
        tree = _tree(missions)
        assert tree.topLevelItem(0).isExpanded()
        assert tree.currentItem().text(0) == "helper"

    def test_revealing_another_conversation_selects_nothing(self, panels):
        """Revealing a conversation that is not a mission's leaves nothing selected."""
        _conversations, missions, conv_dir = panels
        _show(panels, [_mission("m1", os.path.join(conv_dir, "Mission-lead.conv"))])
        missions.reveal_and_select_file(os.path.join(conv_dir, "Mission-lead.conv"))

        missions.reveal_and_select_file(os.path.join(conv_dir, "chat.conv"))
        assert _tree(missions).currentItem() is None

    def test_right_click_on_a_conversation_offers_its_actions(self, panels, monkeypatch):
        """Actions offered for conversations elsewhere are offered for a mission's conversations too."""
        _conversations, missions, conv_dir = panels
        lead = os.path.normpath(os.path.join(conv_dir, "Mission-lead.conv"))
        missions.add_file_menu_provider(lambda path: [("Switch", lambda: None)] if path == lead else [])
        _show(panels, [_mission("m1", lead)])

        menus = _record_menus(missions, monkeypatch)
        _tree(missions).topLevelItem(0).setExpanded(True)
        _right_click(missions, _tree(missions).topLevelItem(0).child(0))
        assert menus[0].labels == ["Switch"]

    def test_right_click_on_a_mission_offers_what_can_be_done_to_it(self, panels, monkeypatch):
        """A mission's menu opens, renames, runs again, archives and deletes it, then offers its lead's actions."""
        _conversations, missions, conv_dir = panels
        lead = os.path.join(conv_dir, "Mission-lead.conv")
        missions.add_file_menu_provider(lambda path: [("Switch", lambda: None)])
        _show(panels, [_mission("m1", lead)])
        asked: list[tuple] = []
        missions.mission_activated.connect(lambda m, ephemeral: asked.append(("open", m, ephemeral)))
        missions.mission_rename_requested.connect(lambda m: asked.append(("rename", m)))
        missions.mission_rerun_requested.connect(lambda m: asked.append(("rerun", m)))
        missions.mission_archive_requested.connect(lambda m, archived: asked.append(("archive", m, archived)))
        missions.mission_delete_requested.connect(lambda m: asked.append(("delete", m)))

        menus = _record_menus(missions, monkeypatch)
        _right_click(missions, _tree(missions).topLevelItem(0))
        strings = missions._language_manager.strings()  # pylint: disable=protected-access
        assert menus[0].labels == [
            strings.mindspace_mission_open, strings.mission_rename, strings.mission_run_again,
            strings.mission_archive, strings.mission_delete, "Switch",
        ]
        for callback in menus[0].callbacks[:5]:
            callback()

        assert asked == [("open", "m1", False), ("rename", "m1"), ("rerun", "m1"), ("archive", "m1", True), ("delete", "m1")]

    def test_an_archived_mission_can_be_brought_back(self, panels, monkeypatch):
        """An archived mission's menu offers to bring it back rather than archive it."""
        _conversations, missions, conv_dir = panels
        _show(panels, [_mission("m1", os.path.join(conv_dir, "Mission-lead.conv"), archived=True)])
        missions._archived_toggle.setChecked(True)  # pylint: disable=protected-access
        asked: list[tuple] = []
        missions.mission_archive_requested.connect(lambda m, archived: asked.append((m, archived)))

        menus = _record_menus(missions, monkeypatch)
        _right_click(missions, _tree(missions).topLevelItem(0))
        strings = missions._language_manager.strings()  # pylint: disable=protected-access
        menus[0].callbacks[menus[0].labels.index(strings.mission_unarchive)]()
        assert asked == [("m1", False)]


class TestFindingMissions:
    """Archived missions are put away, and a search helps once there are many missions."""

    @staticmethod
    def _entries() -> list[MissionSidebarEntry]:
        """Return five missions, the last of them archived."""
        entries = [
            MissionSidebarEntry(f"m{i}", title, "Working", ColorRole.TEXT_SUCCESS, "", search_text=f"{title}\n{request}")
            for i, (title, request) in enumerate([
                ("Write the guide", "docs"), ("Fix flaky tests", "CI"), ("Release notes", "v2"), ("Tidy", "lint"),
            ])
        ]
        entries.append(MissionSidebarEntry("old", "Old audit", "Done", ColorRole.TEXT_SUCCESS, "", archived=True))
        return entries

    @staticmethod
    def _titles(missions: MissionSidebar) -> list[str]:
        """Return the titles of the missions listed."""
        tree = _tree(missions)
        return [tree.topLevelItem(i).data(0, Qt.ItemDataRole.UserRole + 1).title for i in range(tree.topLevelItemCount())]

    def test_archived_missions_are_hidden_until_asked_for(self, qapp):  # pylint: disable=unused-argument
        """Archived missions are left out, and a button showing how many brings them into the list."""
        missions = MissionSidebar()
        missions.set_missions(self._entries(), lambda _path: [])
        toggle = missions._archived_toggle  # pylint: disable=protected-access
        assert "Old audit" not in self._titles(missions)
        assert not toggle.isHidden() and "1" in toggle.text()

        toggle.setChecked(True)
        assert "Old audit" in self._titles(missions)
        missions.deleteLater()

    def test_no_archived_missions_means_no_button(self, qapp):  # pylint: disable=unused-argument
        """Without archived missions there is nothing to show, so no button."""
        missions = MissionSidebar()
        missions.set_missions(self._entries()[:2], lambda _path: [])
        assert missions._archived_toggle.isHidden()  # pylint: disable=protected-access
        missions.deleteLater()

    def test_search_matches_title_and_request(self, qapp):  # pylint: disable=unused-argument
        """A search keeps the missions whose title or request has every word typed."""
        missions = MissionSidebar()
        missions.show()
        missions.set_missions(self._entries(), lambda _path: [])
        search = missions._search  # pylint: disable=protected-access
        assert not search.isHidden()

        search.setText("ci")
        assert self._titles(missions) == ["Fix flaky tests"]
        search.setText("nothing like this")
        assert self._titles(missions) == []
        assert not missions._empty_label.isHidden()  # pylint: disable=protected-access
        missions.deleteLater()

    def test_few_missions_need_no_search(self, qapp):  # pylint: disable=unused-argument
        """With only a few missions there is no search box."""
        missions = MissionSidebar()
        missions.set_missions(self._entries()[:3], lambda _path: [])
        assert missions._search.isHidden()  # pylint: disable=protected-access
        missions.deleteLater()


def _right_click(missions: MissionSidebar, item) -> None:
    """Ask for the menu of a row."""
    tree = _tree(missions)
    missions.show()
    tree.customContextMenuRequested.emit(tree.visualItemRect(item).center())


def _record_menus(missions: MissionSidebar, monkeypatch) -> list["_RecordingMenu"]:
    """Make menus record their actions instead of showing them, and return the menus made."""
    menus: list[_RecordingMenu] = []
    monkeypatch.setattr(
        missions._style_manager, "create_menu",  # pylint: disable=protected-access
        lambda _parent: menus.append(_RecordingMenu()) or menus[-1],
    )
    return menus


class _RecordingMenu:
    """A stand-in menu that records its actions instead of showing them."""

    def __init__(self) -> None:
        self.labels: list[str] = []
        self.callbacks: list = []

    def addAction(self, label: str) -> "_RecordingAction":  # pylint: disable=invalid-name
        """Record an action."""
        self.labels.append(label)
        action = _RecordingAction(self.callbacks)
        return action

    def addSeparator(self) -> None:  # pylint: disable=invalid-name
        """Ignore separators."""

    def isEmpty(self) -> bool:  # pylint: disable=invalid-name
        """Say whether any action was added."""
        return not self.labels

    def exec(self, _position) -> None:
        """Do nothing: the test runs the actions itself."""


class _RecordingAction:
    """A stand-in action whose triggered signal records the callback connected to it."""

    def __init__(self, callbacks: list) -> None:
        self.triggered = self
        self._callbacks = callbacks

    def connect(self, callback) -> None:
        """Record the callback."""
        self._callbacks.append(callback)
