"""Tests for the readable labels given to tool calls."""

from ai_tool import AIToolCall
from mission.mission_tool_label import tool_call_label, tool_call_target


class TestToolCallLabel:
    """A tool call is named by its tool and, when it has one, its operation."""

    def test_operation_is_added_to_the_tool_name(self) -> None:
        """A call with an operation is labelled tool.operation."""
        call = AIToolCall(id="t1", name="filesystem", arguments={"operation": "write_file", "path": "a.md"})
        assert tool_call_label(call) == "filesystem.write_file"

    def test_tool_without_an_operation_is_labelled_by_its_name(self) -> None:
        """A call with no operation, or an empty one, is labelled by the tool alone."""
        assert tool_call_label(AIToolCall(id="t1", name="clock", arguments={})) == "clock"
        assert tool_call_label(AIToolCall(id="t2", name="clock", arguments={"operation": ""})) == "clock"


class TestToolCallTarget:
    """A tool call's target is the file, command or URL it acts on."""

    def test_target_is_the_file_command_or_url(self) -> None:
        """The most specific argument naming what the call acts on is its target."""
        assert tool_call_target(AIToolCall(id="t1", name="filesystem", arguments={"path": " docs/a.md "})) == "docs/a.md"
        assert tool_call_target(AIToolCall(id="t2", name="terminal", arguments={"command": "rm -rf build"})) == "rm -rf build"
        assert tool_call_target(AIToolCall(id="t3", name="http", arguments={"url": "https://example.com"})) == (
            "https://example.com"
        )

    def test_move_names_both_ends(self) -> None:
        """A move or copy is shown from its source to its destination."""
        call = AIToolCall(id="t1", name="filesystem", arguments={"path": "a.md", "destination": "b.md"})
        assert tool_call_target(call) == "a.md → b.md"

    def test_call_naming_nothing_has_no_target(self) -> None:
        """A call with no target argument, or only an empty one, has none."""
        assert tool_call_target(AIToolCall(id="t1", name="clock", arguments={})) == ""
        assert tool_call_target(AIToolCall(id="t2", name="filesystem", arguments={"path": " "})) == ""
