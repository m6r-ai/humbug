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
