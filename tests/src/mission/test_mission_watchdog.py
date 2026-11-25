"""Tests for the signs that a mission's work has stalled."""

import pytest

from ai_tool import AIToolCall, AIToolResult
from mission.mission_watchdog import RepeatedActionCounter, is_transient_error


@pytest.mark.parametrize("code", ["network_error", "rate_limit", "server_error", "429", "500", "503", "529"])
def test_errors_that_may_pass_are_transient(code):
    """A lost connection, a rate limit or a server error may pass if the request is tried later."""
    assert is_transient_error({"code": code, "message": "x"})


@pytest.mark.parametrize("code", ["400", "401", "403", "404", "cancelled", "error", "process_error", "backend_error", ""])
def test_errors_that_will_not_pass_are_not_transient(code):
    """A rejected key, a bad request, a cancellation or an unknown failure will not pass by waiting."""
    assert not is_transient_error({"code": code})


def test_missing_error_is_not_transient():
    """A message without an error is not a transient failure."""
    assert not is_transient_error(None)
    assert not is_transient_error({})


def _round(counter: RepeatedActionCounter, call_id: str, arguments: dict, content: str, error: str | None = None) -> int:
    """Make one tool call and return how many times in a row its outcome has been seen."""
    counter.note_calls([AIToolCall(id=call_id, name="terminal", arguments=arguments)])
    return counter.note_results([AIToolResult(id=call_id, name="terminal", content=content, error=error)])


def test_same_call_and_result_counts_up():
    """Each repeat of the same call with the same result adds one to the count."""
    counter = RepeatedActionCounter()
    counts = [_round(counter, f"c{i}", {"command": "make"}, "failed") for i in range(3)]
    assert counts == [1, 2, 3]


def test_different_result_starts_again():
    """A call whose result changes is making progress."""
    counter = RepeatedActionCounter()
    _round(counter, "c1", {"command": "make"}, "50%")
    assert _round(counter, "c2", {"command": "make"}, "60%") == 1


def test_different_arguments_start_again():
    """A different call is a different action, even with the same result."""
    counter = RepeatedActionCounter()
    _round(counter, "c1", {"path": "a"}, "not found")
    assert _round(counter, "c2", {"path": "b"}, "not found") == 1


def test_different_error_starts_again():
    """The same call failing in a new way is not a repeat."""
    counter = RepeatedActionCounter()
    _round(counter, "c1", {"command": "make"}, "", "timeout")
    assert _round(counter, "c2", {"command": "make"}, "", "permission denied") == 1


def test_argument_order_does_not_matter():
    """The same arguments in a different order are the same call."""
    counter = RepeatedActionCounter()
    _round(counter, "c1", {"a": 1, "b": 2}, "same")
    assert _round(counter, "c2", {"b": 2, "a": 1}, "same") == 2


def test_reset_forgets_the_repeats():
    """After a reset, the next outcome is counted from one."""
    counter = RepeatedActionCounter()
    _round(counter, "c1", {"command": "make"}, "failed")
    counter.reset()
    assert _round(counter, "c2", {"command": "make"}, "failed") == 1
