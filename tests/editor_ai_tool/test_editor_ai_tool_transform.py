"""Tests for the editor tool transform operation."""
import asyncio
import json
from unittest.mock import MagicMock

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied
from editor_ai_tool.editor_ai_tool import EditorAITool


@pytest.fixture
def mock_mindspace():
    """Fixture providing a minimal mock Mindspace."""
    mindspace = MagicMock()
    mindspace.add_interaction = MagicMock()
    mindspace.contexts.return_value = MagicMock()
    return mindspace


@pytest.fixture
def mock_authorization():
    """Fixture that always grants authorization."""
    async def _auth(*_args, **_kwargs):
        return True
    return _auth


@pytest.fixture
def mock_authorization_denied():
    """Fixture that always denies authorization."""
    async def _auth(*_args, **_kwargs):
        return False
    return _auth


def make_editor_context(content: str) -> MagicMock:
    """Create a mock EditorContext whose buffer holds the given content."""
    ctx = MagicMock()
    ctx.context_id.return_value = "test-tab-id"
    ctx.get_text_range.return_value = content
    ctx.get_editor_info.return_value = {"file_path": "/test/file.txt"}
    ctx.apply_diff.return_value = {"success": True, "message": "Applied", "hunks_applied": 1}
    return ctx


@pytest.fixture
def editor_tool(mock_mindspace):
    """Fixture providing an EditorAITool with a mocked mindspace."""
    tool = EditorAITool(mindspace=mock_mindspace)
    return tool, mock_mindspace


def _set_context(tool: EditorAITool, mindspace: MagicMock, ctx: MagicMock) -> None:
    """Wire a mock context into the tool's mindspace registry."""
    from editor_context.editor_context import EditorContext
    registry = MagicMock()
    registry.get_model.return_value = ctx
    mindspace.contexts.return_value = registry


class TestEditorAIToolTransform:
    """Tests for the editor transform operation."""

    def test_transform_string_result(self, editor_tool, mock_authorization, make_tool_call):
        """Transform returning a string applies the diff to the buffer."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("hello world")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "(string-upcase input-text)"}
        )
        result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]
        assert "save_file" in data["message"]
        ctx.apply_diff.assert_called_once()

    def test_transform_list_result(self, editor_tool, mock_authorization, make_tool_call):
        """Transform returning a list of strings is joined and applied."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("alpha\nbeta\ngamma")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "(list-reverse input-lines)"}
        )
        result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]

    def test_transform_no_change(self, editor_tool, mock_authorization, make_tool_call):
        """Transform producing identical content reports no changes and skips apply_diff."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("unchanged")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "input-text"}
        )
        result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
        assert "no changes" in result.content.lower()
        ctx.apply_diff.assert_not_called()

    def test_transform_authorization_denied(self, editor_tool, mock_authorization_denied, make_tool_call):
        """Transform is aborted when the user denies authorization."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("original")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "(string-upcase input-text)"}
        )
        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(tool.execute(tool_call, "", mock_authorization_denied))
        ctx.apply_diff.assert_not_called()

    def test_transform_invalid_program(self, editor_tool, mock_authorization, make_tool_call):
        """Transform raises AIToolExecutionError for a program with a syntax error."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("content")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "(integer+ 1 2"}
        )
        with pytest.raises(AIToolExecutionError):
            asyncio.run(tool.execute(tool_call, "", mock_authorization))

    def test_transform_wrong_return_type(self, editor_tool, mock_authorization, make_tool_call):
        """Transform raises AIToolExecutionError when the program returns a non-text type."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("content")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "42"}
        )
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", mock_authorization))
        assert "string or list of strings" in str(exc_info.value)

    def test_transform_empty_program(self, editor_tool, mock_authorization, make_tool_call):
        """Transform raises AIToolExecutionError for a blank program."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("content")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "   "}
        )
        with pytest.raises(AIToolExecutionError):
            asyncio.run(tool.execute(tool_call, "", mock_authorization))

    def test_transform_input_text_binding(self, editor_tool, mock_authorization, make_tool_call):
        """'input-text' is bound to the full buffer content as a single string."""
        tool, mindspace = editor_tool
        content = "line one\nline two"
        ctx = make_editor_context(content)
        _set_context(tool, mindspace, ctx)

        program = '(integer->string (string-length input-text))'
        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": program}
        )
        result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]
        diff_arg = ctx.apply_diff.call_args[0][0]
        assert str(len(content)) in diff_arg

    def test_transform_input_lines_binding(self, editor_tool, mock_authorization, make_tool_call):
        """'input-lines' is bound to a list of strings, one per line."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("alpha\nbeta\ngamma")
        _set_context(tool, mindspace, ctx)

        program = '(integer->string (list-length input-lines))'
        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": program}
        )
        result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]
        diff_arg = ctx.apply_diff.call_args[0][0]
        assert "3" in diff_arg

    def test_transform_logs_interaction(self, editor_tool, mock_authorization, make_tool_call):
        """A successful transform logs to the mindspace interaction log."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("hello")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "(string-upcase input-text)"}
        )
        asyncio.run(tool.execute(tool_call, "", mock_authorization))
        mindspace.add_interaction.assert_called_once()

    def test_transform_missing_tab(self, editor_tool, mock_authorization, make_tool_call):
        """Transform raises AIToolExecutionError when the tab_id is not found."""
        tool, mindspace = editor_tool
        registry = MagicMock()
        registry.get_model.return_value = None
        mindspace.contexts.return_value = registry

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "nonexistent-tab", "program": "input-text"}
        )
        with pytest.raises(AIToolExecutionError):
            asyncio.run(tool.execute(tool_call, "", mock_authorization))

    def test_transform_dry_run_returns_diff_without_applying(
        self, editor_tool, mock_authorization, make_tool_call
    ):
        """dry_run=True returns the diff and does not apply it to the buffer."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("hello world")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {
                "operation": "transform",
                "tab_id": "test-tab-id",
                "program": "(string-upcase input-text)",
                "dry_run": True
            }
        )
        result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
        assert "dry run" in result.content.lower()
        assert "+" in result.content
        ctx.apply_diff.assert_not_called()

    def test_transform_dry_run_no_change(
        self, editor_tool, mock_authorization, make_tool_call
    ):
        """dry_run=True on an identity transform reports no changes."""
        tool, mindspace = editor_tool
        ctx = make_editor_context("unchanged")
        _set_context(tool, mindspace, ctx)

        tool_call = make_tool_call(
            "editor",
            {"operation": "transform", "tab_id": "test-tab-id", "program": "input-text", "dry_run": True}
        )
        result = asyncio.run(tool.execute(tool_call, "", mock_authorization))
        assert "no changes" in result.content.lower()
        ctx.apply_diff.assert_not_called()
