"""Tests for the log tab's window over an unbounded interaction log."""
import pytest

from mindspace.mindspace_log_level import MindspaceLogLevel

from desktop.language.language_manager import LanguageManager
from desktop.log_tab.log_tab import LogTab
from desktop.log_tab.log_widget import LogWidget


def _add_entries(manager, contents: list[str]) -> None:
    """Append one entry per content string to the open mindspace's log."""
    for content in contents:
        manager.add_interaction(MindspaceLogLevel.INFO, content)


def _entries(count: int, needle_at: int = -1, needle: str = "needle") -> list[str]:
    """Return count entry contents, optionally putting a needle in one of them."""
    return [
        f"{needle} number {index}" if index == needle_at else f"entry {index}"
        for index in range(count)
    ]


@pytest.fixture
def log_widget(mindspace_sandbox, monkeypatch):
    """A log widget showing a five-entry page of the sandbox mindspace's log."""
    monkeypatch.setattr(LogWidget, "VIEW_PAGE_SIZE", 5)
    widget = LogWidget()
    yield widget
    widget.deleteLater()


class TestPageStatus:
    """Tests for the range of entries the view reports it is showing."""

    def test_reports_the_newest_page(self, mindspace_sandbox, log_widget) -> None:
        """The view shows the newest page of a log that is longer than one page."""
        _add_entries(mindspace_sandbox.manager, _entries(12))

        first, last, total = log_widget.page_status()

        assert (first, last, total) == (8, 12, 12)

    def test_reports_a_short_log_in_full(self, mindspace_sandbox, log_widget) -> None:
        """A log shorter than one page is shown in full."""
        _add_entries(mindspace_sandbox.manager, _entries(3))

        first, last, total = log_widget.page_status()

        assert (first, last, total) == (1, 3, 3)

    def test_reports_an_empty_log(self, mindspace_sandbox, log_widget) -> None:
        """An empty log reports no entries."""
        assert log_widget.page_status() == (0, 0, 0)


class TestTabStatus:
    """Tests for the status line the tab reports to the status bar."""

    def test_status_names_the_range_and_the_total(self, mindspace_sandbox, host, monkeypatch) -> None:
        """The status line reports which entries are shown and how many exist."""
        monkeypatch.setattr(LogWidget, "VIEW_PAGE_SIZE", 5)
        _add_entries(mindspace_sandbox.manager, _entries(12))
        tab = LogTab("log-tab", host)
        reported: list[str] = []
        tab.status_message.connect(lambda message: reported.append(message.text))

        tab.update_status()

        strings = LanguageManager().strings()
        assert reported == [strings.log_status_entries.format(first=8, last=12, total=12)]
        tab.deleteLater()


class TestContextDisplay:
    """Tests for showing which conversation an entry belongs to."""

    def test_context_is_shown_in_the_entry_header(self, mindspace_sandbox, log_widget) -> None:
        """An attributed entry shows its context in the header."""
        mindspace_sandbox.manager.add_interaction(
            MindspaceLogLevel.INFO,
            "AI response: 'done'",
            context={"conversation": ".humbug/conversations/one.conv"}
        )

        entry = log_widget._messages[-1]  # pylint: disable=protected-access
        label = entry._context_label  # pylint: disable=protected-access

        assert label.text() == "conversation: .humbug/conversations/one.conv"
        assert not label.isHidden()

    def test_entry_without_context_shows_no_context(self, mindspace_sandbox, log_widget) -> None:
        """An entry that belongs to no conversation shows no context."""
        _add_entries(mindspace_sandbox.manager, ["entry"])

        entry = log_widget._messages[-1]  # pylint: disable=protected-access

        assert entry._context_label.isHidden()  # pylint: disable=protected-access


class TestContextDisplay:
    """Tests for showing which conversation an entry belongs to."""

    def test_context_is_shown_in_the_entry_header(self, mindspace_sandbox, log_widget) -> None:
        """An attributed entry shows its context in the header."""
        mindspace_sandbox.manager.add_interaction(
            MindspaceLogLevel.INFO,
            "AI response: 'done'",
            context={"conversation": ".humbug/conversations/one.conv"}
        )

        entry = log_widget._messages[-1]  # pylint: disable=protected-access
        label = entry._context_label  # pylint: disable=protected-access

        assert label.text() == "conversation: .humbug/conversations/one.conv"
        assert not label.isHidden()

    def test_entry_without_context_shows_no_context(self, mindspace_sandbox, log_widget) -> None:
        """An entry that belongs to no conversation shows no context."""
        _add_entries(mindspace_sandbox.manager, ["entry"])

        entry = log_widget._messages[-1]  # pylint: disable=protected-access

        assert entry._context_label.isHidden()  # pylint: disable=protected-access


class TestFind:
    """Tests for searching the log rather than the loaded page."""

    def test_counts_matches_outside_the_loaded_page(self, mindspace_sandbox, log_widget) -> None:
        """Matches in entries the view is not showing are still found and counted."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))

        current, total, truncated = log_widget.find_text("needle")

        assert (current, total, truncated) == (1, 1, False)

    def test_shows_the_page_holding_the_match(self, mindspace_sandbox, log_widget) -> None:
        """Finding a match loads the page of entries that contains it."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))

        log_widget.find_text("needle")

        first, last, _ = log_widget.page_status()
        assert first <= 2 <= last

    def test_counts_every_match_in_the_log(self, mindspace_sandbox, log_widget) -> None:
        """The match count covers the whole log, not just the visible entries."""
        contents = _entries(12)
        contents[0] = "needle"
        contents[3] = "needle again"

        _add_entries(mindspace_sandbox.manager, contents)

        current, total, truncated = log_widget.find_text("needle")

        assert (current, total, truncated) == (1, 2, False)

    def test_navigation_wraps_at_the_end(self, mindspace_sandbox, log_widget) -> None:
        """Moving past the last match returns to the first."""
        _add_entries(mindspace_sandbox.manager, ["needle", "entry", "needle"])

        assert log_widget.find_text("needle")[0] == 1
        assert log_widget.find_text("needle")[0] == 2
        assert log_widget.find_text("needle")[0] == 1

    def test_navigation_backwards_wraps_at_the_start(self, mindspace_sandbox, log_widget) -> None:
        """Moving back past the first match returns to the last."""
        _add_entries(mindspace_sandbox.manager, ["needle", "entry", "needle"])
        log_widget.find_text("needle")

        current, _, _ = log_widget.find_text("needle", forward=False)

        assert current == 2

    def test_text_is_matched_literally_unless_regexp(self, mindspace_sandbox, log_widget) -> None:
        """Regular-expression metacharacters in plain text are matched literally."""
        _add_entries(mindspace_sandbox.manager, ["value (one)", "value two"])

        assert log_widget.find_text("(one)")[1] == 1
        assert log_widget.find_text("(one)", regexp=True)[1] == 1

    def test_case_sensitivity_is_honoured(self, mindspace_sandbox, log_widget) -> None:
        """Case matters only when case-sensitive matching is requested."""
        _add_entries(mindspace_sandbox.manager, ["Needle"])

        assert log_widget.find_text("needle")[1] == 1
        assert log_widget.find_text("needle", case_sensitive=True)[1] == 0

    def test_no_matches_reports_nothing_found(self, mindspace_sandbox, log_widget) -> None:
        """A search with no matches reports zero matches."""
        _add_entries(mindspace_sandbox.manager, _entries(3))

        assert log_widget.find_text("absent") == (0, 0, False)

    def test_clearing_find_returns_to_the_newest_page(self, mindspace_sandbox, log_widget) -> None:
        """Clearing the search puts the view back at the end of the log."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))
        log_widget.find_text("needle")

        log_widget.clear_highlights()

        first, last, total = log_widget.page_status()
        assert (first, last, total) == (8, 12, 12)

    def test_appends_do_not_move_a_pinned_view(self, mindspace_sandbox, log_widget) -> None:
        """New entries do not pull the view away from the match it is showing."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))
        log_widget.find_text("needle")
        pinned = log_widget.page_status()

        _add_entries(mindspace_sandbox.manager, ["later entry"])

        first, last, total = log_widget.page_status()
        assert (first, last) == pinned[:2]
        assert total == 13


class TestLoadEarlier:
    """Tests for loading the entries before the ones on show."""

    def test_loads_the_page_before_the_visible_one(self, mindspace_sandbox, log_widget) -> None:
        """Loading earlier extends the range backwards by one page."""
        _add_entries(mindspace_sandbox.manager, _entries(12))

        log_widget._load_earlier()

        first, last, total = log_widget.page_status()
        assert (first, last, total) == (3, 12, 12)

    def test_stops_when_the_start_of_the_log_is_reached(self, mindspace_sandbox, log_widget) -> None:
        """Loading earlier stops once the first entry is on show."""
        _add_entries(mindspace_sandbox.manager, _entries(12))

        for _ in range(5):
            log_widget._load_earlier()

        first, last, total = log_widget.page_status()
        assert (first, last, total) == (1, 12, 12)

    def test_stops_at_the_loaded_page_limit(self, mindspace_sandbox, log_widget) -> None:
        """The view holds no more than MAX_LOADED_PAGES pages of entries."""
        _add_entries(mindspace_sandbox.manager, _entries(60))

        for _ in range(10):
            log_widget._load_earlier()

        first, last, _ = log_widget.page_status()
        assert last == 60
        assert (last - first + 1) == LogWidget.MAX_LOADED_PAGES * LogWidget.VIEW_PAGE_SIZE

    def test_entries_already_on_show_stay_on_show(self, mindspace_sandbox, log_widget) -> None:
        """The entries already loaded are still shown after loading earlier."""
        _add_entries(mindspace_sandbox.manager, _entries(12))
        before = [message.sequence() for message in log_widget._messages]

        log_widget._load_earlier()

        assert [message.sequence() for message in log_widget._messages][-len(before):] == before

    def test_loaded_entries_are_in_sequence_order(self, mindspace_sandbox, log_widget) -> None:
        """Loaded entries are inserted above the existing ones, in order."""
        _add_entries(mindspace_sandbox.manager, _entries(12))

        log_widget._load_earlier()

        assert [message.sequence() for message in log_widget._messages] == list(range(3, 13))


class TestLoadLater:
    """Tests for loading the entries after the ones on show."""

    def test_loads_the_page_after_the_visible_one(self, mindspace_sandbox, log_widget) -> None:
        """Loading later extends the range forwards by one page."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))
        log_widget.find_text("needle")

        log_widget._load_later()

        assert log_widget.page_status() == (1, 10, 12)

    def test_stops_at_the_end_of_the_log(self, mindspace_sandbox, log_widget) -> None:
        """Loading later stops once the newest entry is on show."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))
        log_widget.find_text("needle")

        for _ in range(5):
            log_widget._load_later()

        assert log_widget.page_status() == (1, 12, 12)

    def test_reaching_the_end_resumes_following_new_entries(
        self, mindspace_sandbox, log_widget
    ) -> None:
        """Once the newest entry is on show, entries appended later join the view."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))
        log_widget.find_text("needle")
        for _ in range(5):
            log_widget._load_later()

        _add_entries(mindspace_sandbox.manager, ["later entry"])

        first, last, total = log_widget.page_status()
        assert (first, last, total) == (1, 13, 13)

    def test_loaded_entries_are_appended_in_order(self, mindspace_sandbox, log_widget) -> None:
        """Loaded entries follow the existing ones, in sequence order."""
        _add_entries(mindspace_sandbox.manager, _entries(12, needle_at=1))
        log_widget.find_text("needle")

        log_widget._load_later()

        assert [message.sequence() for message in log_widget._messages] == list(range(1, 11))


class TestFollowingTheTail:
    """Tests for the view keeping up with entries appended while at the end."""

    def test_appending_while_at_the_end_does_not_anchor_the_view(
        self, mindspace_sandbox, log_widget
    ) -> None:
        """Following the tail is never interrupted by an anchor to an older entry."""
        _add_entries(mindspace_sandbox.manager, _entries(12))

        for index in range(12, 20):
            _add_entries(mindspace_sandbox.manager, [f"entry {index}"])
            assert log_widget._pending_anchor is None

        assert log_widget.page_status() == (16, 20, 20)
