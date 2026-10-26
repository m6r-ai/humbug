"""Tests for the Quick Switcher overlay (fuzzy filter over files, conversations, tabs)."""
# pylint: disable=protected-access, missing-class-docstring, missing-function-docstring

from PySide6.QtCore import QEvent, QPoint, Qt
from PySide6.QtGui import QKeyEvent

from desktop.quick_switcher.quick_switcher_widget import QuickSwitcherEntry, QuickSwitcherWidget


def make_switcher(host):
    switcher = QuickSwitcherWidget(host)
    switcher.setGeometry(host.rect())
    switcher.show()
    return switcher


def make_entries():
    return [
        QuickSwitcherEntry(
            entry_id="file:/mindspace/main.py", kind="file",
            title="main.py", subtitle=".", icon_name="files",
        ),
        QuickSwitcherEntry(
            entry_id="file:/mindspace/src/utils.py", kind="file",
            title="utils.py", subtitle="src", icon_name="files",
        ),
        QuickSwitcherEntry(
            entry_id="conversation:/mindspace/.humbug/conversations/plan.conv", kind="conversation",
            title="plan", subtitle=".humbug/conversations", icon_name="conversation",
        ),
        QuickSwitcherEntry(
            entry_id="tab:abc123", kind="tab",
            title="README.md", subtitle=".", icon_name="editor",
        ),
    ]


def open_conversation_tab():
    """An entry for a conversation that is currently open, so listed as its tab."""
    return QuickSwitcherEntry(
        entry_id="tab:def456", kind="tab",
        title="roadmap", subtitle=".humbug/conversations", icon_name="conversation",
    )


def press_key(qapp, widget, key, modifier=Qt.KeyboardModifier.NoModifier):
    qapp.sendEvent(widget, QKeyEvent(QEvent.Type.KeyPress, key, modifier))


class TestPopulation:
    def test_set_entries_shows_all_entries_with_empty_filter(self, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        assert switcher._list.count() == 4

    def test_set_entries_selects_first_row(self, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        assert switcher.selected_entry_id() == "file:/mindspace/main.py"

    def test_no_entries_leaves_no_selection(self, host):
        switcher = make_switcher(host)
        switcher.set_entries([])
        assert switcher.selected_entry_id() is None


class TestNotice:
    def test_notice_is_hidden_by_default(self, host):
        switcher = make_switcher(host)
        assert switcher._notice.isHidden()

    def test_setting_a_notice_shows_it(self, host):
        switcher = make_switcher(host)
        switcher.set_notice("Limited to the first 2000 files")
        assert not switcher._notice.isHidden()
        assert switcher._notice.text() == "Limited to the first 2000 files"

    def test_clearing_the_notice_hides_it_again(self, host):
        switcher = make_switcher(host)
        switcher.set_notice("Limited to the first 2000 files")
        switcher.set_notice("")
        assert switcher._notice.isHidden()


class TestStyleChanges:
    def test_style_change_restyles_the_panel(self, host):
        switcher = make_switcher(host)
        switcher._panel.setStyleSheet("/* stale */")

        switcher._style_manager.style_changed.emit()

        assert "QuickSwitcherPanel" in switcher._panel.styleSheet()


class TestFiltering:
    def test_typing_filters_out_non_matching_entries(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        switcher._input.setText("plan")
        qapp.processEvents()
        assert switcher._list.count() == 1
        assert switcher.selected_entry_id() == "conversation:/mindspace/.humbug/conversations/plan.conv"

    def test_typing_ranks_title_matches_above_subtitle_only_matches(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        switcher._input.setText("main")
        qapp.processEvents()
        assert switcher.selected_entry_id() == "file:/mindspace/main.py"

    def test_clearing_filter_restores_full_list(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        switcher._input.setText("plan")
        qapp.processEvents()
        switcher._input.setText("")
        qapp.processEvents()
        assert switcher._list.count() == 4

    def test_at_prefix_limits_results_to_conversations(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        switcher._input.setText("@plan")
        qapp.processEvents()
        assert switcher.selected_entry_id() == "conversation:/mindspace/.humbug/conversations/plan.conv"

    def test_line_prefix_creates_a_goto_result(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_goto_line("Go to line {0}", True)
        switcher.set_entries(make_entries())
        switcher._input.setText(":42")
        qapp.processEvents()
        assert switcher.selected_entry_id() == "goto:42"

    def test_command_prefix_filters_commands(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        switcher.set_command_entries([
            QuickSwitcherEntry("command:new-file", "command", "New File", "", "editor"),
            QuickSwitcherEntry("command:new-conversation", "command", "New Conversation", "", "conversation"),
        ])
        switcher._input.setText(">conversation")
        qapp.processEvents()
        assert switcher.selected_entry_id() == "command:new-conversation"

    def test_file_and_line_query_creates_a_file_line_result(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        switcher._input.setText("utils:18")
        qapp.processEvents()
        assert switcher.selected_entry_id() == "file-line:18:file:/mindspace/src/utils.py"

    def test_at_prefix_finds_a_conversation_that_is_already_open(self, qapp, host):
        """An open conversation is listed as a tab, and must still be reachable with @."""
        switcher = make_switcher(host)
        switcher.set_entries(make_entries() + [open_conversation_tab()])
        switcher._input.setText("@roadmap")
        qapp.processEvents()
        assert switcher.selected_entry_id() == "tab:def456"

    def test_at_prefix_still_excludes_non_conversation_tabs(self, qapp, host):
        """Widening @ to tabs must not let ordinary file tabs through."""
        switcher = make_switcher(host)
        switcher.set_entries(make_entries() + [open_conversation_tab()])
        switcher._input.setText("@README")
        qapp.processEvents()
        assert switcher._list.count() == 0

    def test_line_prefix_offers_nothing_without_an_editor(self, qapp, host):
        """Offering a jump that silently does nothing would be worse than offering none."""
        switcher = make_switcher(host)
        switcher.set_goto_line("Go to line {0}", False)
        switcher.set_entries(make_entries())
        switcher._input.setText(":42")
        qapp.processEvents()
        assert switcher._list.count() == 0

    def test_the_goto_line_label_comes_from_the_supplied_template(self, qapp, host):
        """The label is localized by the caller, so the widget must not build its own."""
        switcher = make_switcher(host)
        switcher.set_goto_line("Aller a la ligne {0}", True)
        switcher.set_entries(make_entries())
        switcher._input.setText(":42")
        qapp.processEvents()
        assert switcher._list.item(0).data(Qt.ItemDataRole.DisplayRole) == "Aller a la ligne 42"


class TestKeyboardInteraction:
    def test_escape_dismisses(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        dismissed = []
        switcher.dismissed.connect(lambda: dismissed.append(True))
        press_key(qapp, switcher._input, Qt.Key.Key_Escape)
        assert dismissed

    def test_down_moves_selection_and_clamps_at_end(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        for _ in range(10):
            press_key(qapp, switcher._input, Qt.Key.Key_Down)
        assert switcher._list.currentRow() == 3

    def test_up_clamps_at_start(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        press_key(qapp, switcher._input, Qt.Key.Key_Up)
        assert switcher._list.currentRow() == 0

    def test_enter_activates_current_selection(self, qapp, host):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        activated = []
        switcher.entry_activated.connect(activated.append)
        press_key(qapp, switcher._input, Qt.Key.Key_Down)
        press_key(qapp, switcher._input, Qt.Key.Key_Return)
        assert activated == ["file:/mindspace/src/utils.py"]


class TestMouseInteraction:
    def test_click_on_overlay_background_dismisses(self, qapp, host, mouse):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        qapp.processEvents()
        dismissed = []
        switcher.dismissed.connect(lambda: dismissed.append(True))
        mouse(switcher, QEvent.Type.MouseButtonPress, QPoint(5, 5))
        assert dismissed

    def test_clicking_a_row_activates_it(self, qapp, host, mouse):
        switcher = make_switcher(host)
        switcher.set_entries(make_entries())
        qapp.processEvents()
        activated = []
        switcher.entry_activated.connect(activated.append)
        item = switcher._list.item(2)
        rect = switcher._list.visualItemRect(item)
        mouse(switcher._list.viewport(), QEvent.Type.MouseButtonPress, rect.center())
        mouse(switcher._list.viewport(), QEvent.Type.MouseButtonRelease, rect.center())
        assert activated == ["conversation:/mindspace/.humbug/conversations/plan.conv"]
