"""Tests for reading the AI reviewer's verdict."""

from mission.mission_reviewer import parse_verdict


class TestParseVerdict:
    """Only a clear approval approves; anything else leaves the request for the user."""

    def test_clear_approval_approves(self):
        """A JSON approval is read as approval, with its reason."""
        verdict = parse_verdict('{"decision": "approve", "reason": "Writes the requested doc"}')

        assert verdict.approved
        assert verdict.reason == "Writes the requested doc"

    def test_approval_inside_surrounding_text_is_read(self):
        """A JSON object wrapped in other text is still found."""
        assert parse_verdict('Here you go:\n```json\n{"decision": "approve", "reason": "ok"}\n```').approved

    def test_ask_human_and_unreadable_replies_do_not_approve(self):
        """Anything other than a clear approval leaves the request for the user."""
        assert not parse_verdict('{"decision": "ask_human", "reason": "Unclear scope"}').approved
        assert not parse_verdict("Sure, approve it.").approved
        assert not parse_verdict("{not json}").approved
        assert not parse_verdict('{"decision": "APPROVE"}').approved
