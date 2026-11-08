"""Tests for the editor tool apply_diff operation."""
import asyncio
import difflib
from pathlib import Path

import pytest

from ai_tool import AIToolApprovalKind, AIToolAuthorizationDenied, AIToolExecutionError

from tests.conftest import MockRequester
from tests.src.editor_ai_tool.conftest import (  # noqa: F401  pylint: disable=unused-import
    RecordingAuthorizer,
    editor_tool,
    make_editor,
    mock_authorization,
    mock_mindspace,
)


def make_diff(before: list[str], after: list[str]) -> str:
    """Build a unified diff between two sets of lines."""
    return '\n'.join(difflib.unified_diff(before, after, lineterm=''))


BETA_TO_UPPER = make_diff(["alpha", "beta", "gamma"], ["alpha", "BETA", "gamma"])

MISMATCHED_DIFF = make_diff(["nothing", "like this", "is here"], ["nothing", "or this", "is here"])


def apply_diff_call(make_tool_call, diff_content: str):
    """Build an apply_diff tool call."""
    return make_tool_call(
        "editor",
        {"operation": "apply_diff", "tab_id": "test-tab-id", "diff_content": diff_content}
    )


class TestEditorAIToolApplyDiff:
    """Tests for the editor apply_diff operation."""

    def test_apply_diff_writes_file(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """A diff changes the buffer and the result is written to the file."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")

        result = asyncio.run(tool.execute(
            apply_diff_call(make_tool_call, BETA_TO_UPPER),
            MockRequester(),
            mock_authorization
        ))

        assert editor.file_text() == "alpha\nBETA\ngamma"
        assert editor.buffer() == "alpha\nBETA\ngamma"
        assert "1 hunk(s)" in result.content
        assert "saved" in result.content

    def test_apply_diff_asks_with_the_file_change(self, editor_tool, make_tool_call, tmp_path):
        """The approval shows the difference the write makes to the file."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")
        authorizer = RecordingAuthorizer()

        asyncio.run(tool.execute(
            apply_diff_call(make_tool_call, BETA_TO_UPPER),
            MockRequester(),
            authorizer
        ))

        assert len(authorizer.requests) == 1
        _, _, context, diff, is_write, kind = authorizer.requests[0]
        assert "-beta" in diff
        assert "+BETA" in diff
        assert is_write
        assert kind == AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        assert "diff" in context

    def test_apply_diff_refused_leaves_everything_alone(
        self, editor_tool, make_tool_call, tmp_path
    ):
        """A refused diff leaves the buffer and the file as they were."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")
        authorizer = RecordingAuthorizer(grant=False)

        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(tool.execute(
                apply_diff_call(make_tool_call, BETA_TO_UPPER),
                MockRequester(),
                authorizer
            ))

        assert editor.buffer() == "alpha\nbeta\ngamma"
        assert editor.file_text() == "alpha\nbeta\ngamma"

    def test_apply_diff_failure_reports_without_asking(
        self, editor_tool, make_tool_call, tmp_path
    ):
        """A diff whose context does not match changes nothing and asks nothing."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")
        authorizer = RecordingAuthorizer()

        result = asyncio.run(tool.execute(
            apply_diff_call(make_tool_call, MISMATCHED_DIFF),
            MockRequester(),
            authorizer
        ))

        assert "Failed to apply diff" in result.content
        assert "No changes were made" in result.content
        assert not authorizer.requests
        assert editor.buffer() == "alpha\nbeta\ngamma"
        assert editor.file_text() == "alpha\nbeta\ngamma"

    def test_apply_diff_restoring_the_file_writes_nothing(
        self, editor_tool, make_tool_call, tmp_path
    ):
        """A diff that returns the buffer to the file's content writes nothing and asks nothing."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")
        editor.context.apply_diff(BETA_TO_UPPER)
        authorizer = RecordingAuthorizer()

        result = asyncio.run(tool.execute(
            apply_diff_call(make_tool_call, make_diff(
                ["alpha", "BETA", "gamma"], ["alpha", "beta", "gamma"]
            )),
            MockRequester(),
            authorizer
        ))

        assert "identical to the file on disk" in result.content
        assert not authorizer.requests
        assert editor.buffer() == "alpha\nbeta\ngamma"
        assert editor.file_text() == "alpha\nbeta\ngamma"

    def test_apply_diff_refuses_read_only_tab(self, editor_tool, make_tool_call, tmp_path):
        """A read-only view of an earlier version cannot be changed or written."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma", read_only=True)
        authorizer = RecordingAuthorizer()

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(
                apply_diff_call(make_tool_call, BETA_TO_UPPER),
                MockRequester(),
                authorizer
            ))

        assert "read-only view" in str(exc_info.value)
        assert not authorizer.requests
        assert editor.buffer() == "alpha\nbeta\ngamma"

    def test_apply_diff_refuses_untitled_tab(self, editor_tool, make_tool_call, tmp_path):
        """An untitled tab has nowhere to be written, so it is refused before changing."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma", untitled=True)
        authorizer = RecordingAuthorizer()

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(
                apply_diff_call(make_tool_call, BETA_TO_UPPER),
                MockRequester(),
                authorizer
            ))

        assert "untitled" in str(exc_info.value)
        assert not authorizer.requests
        assert editor.buffer() == "alpha\nbeta\ngamma"

    def test_apply_diff_asks_standard_approval_outside_mindspace(
        self, editor_tool, make_tool_call, tmp_path
    ):
        """A tab holding a file the journal does not record asks a standard approval."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")
        mindspace.journalled_path.return_value = None
        authorizer = RecordingAuthorizer()

        asyncio.run(tool.execute(
            apply_diff_call(make_tool_call, BETA_TO_UPPER),
            MockRequester(),
            authorizer
        ))

        assert authorizer.requests[0][5] == AIToolApprovalKind.STANDARD

    def test_apply_diff_logs_one_interaction(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """A written diff is logged once, saying it was saved."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")

        asyncio.run(tool.execute(
            apply_diff_call(make_tool_call, BETA_TO_UPPER),
            MockRequester(),
            mock_authorization
        ))

        mindspace.add_interaction.assert_called_once()
        message = mindspace.add_interaction.call_args[0][1]
        assert "saved" in message

    def test_apply_diff_writes_the_tab_file_and_no_other(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """The file written is the one the tab holds."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")
        other = Path(tmp_path / "other.txt")
        other.write_text("untouched", encoding='utf-8')

        asyncio.run(tool.execute(
            apply_diff_call(make_tool_call, BETA_TO_UPPER),
            MockRequester(),
            mock_authorization
        ))

        assert editor.file_text() == "alpha\nBETA\ngamma"
        assert other.read_text(encoding='utf-8') == "untouched"
