"""Tests for the readable labels given to tool calls."""

from ai_tool import AIToolCall
from mission.mission_tool_label import tool_call_label


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
