"""Tests for the editor tool transform operation."""
import asyncio
import difflib
import json
from typing import Any
from unittest.mock import MagicMock

import pytest

from ai_tool import AIToolApprovalKind, AIToolAuthorizationDenied, AIToolExecutionError
from editor_context.editor_context import EditorContext
from editor_context.editor_context_document import EditorContextDocument

from tests.conftest import MockRequester
from tests.src.editor_ai_tool.conftest import (  # noqa: F401  pylint: disable=unused-import
    RecordingAuthorizer,
    editor_tool,
    make_editor,
    mock_authorization,
    mock_mindspace,
)


def transform_call(make_tool_call, program: str, **extra: Any):
    """Build a transform tool call."""
    arguments = {"operation": "transform", "tab_id": "test-tab-id", "program": program}
    arguments.update(extra)
    return make_tool_call("editor", arguments)


class TestEditorAIToolTransform:
    """Tests for the editor transform operation."""

    def test_transform_writes_file(self, editor_tool, mock_authorization, make_tool_call, tmp_path):
        """A transform replaces the buffer and saves the result to the file."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "hello world")

        result = asyncio.run(tool.execute(
            transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
            MockRequester(),
            mock_authorization
        ))

        assert editor.file_text() == "HELLO WORLD"
        assert editor.buffer() == "HELLO WORLD"
        assert "1 hunk(s)" in result.content
        assert "saved" in result.content

    def test_transform_reports_saved_file_path(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """The result names the file the change was written to, within the mindspace."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "hello")

        result = asyncio.run(tool.execute(
            transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
            MockRequester(),
            mock_authorization
        ))

        data = json.loads(result.content)
        assert "file.txt" in data["message"]
        assert str(tmp_path) not in data["message"]

    def test_transform_list_result(self, editor_tool, mock_authorization, make_tool_call, tmp_path):
        """Transform returning a list of strings is joined with newlines and saved."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")

        asyncio.run(tool.execute(
            transform_call(
                make_tool_call,
                '(list-reverse (string->list (dict-get inputs "input-text") "\\n"))'
            ),
            MockRequester(),
            mock_authorization
        ))

        assert editor.file_text() == "gamma\nbeta\nalpha"

    def test_transform_no_change(self, editor_tool, make_tool_call, tmp_path):
        """A transform producing identical content reports no change and writes nothing."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "unchanged")
        authorizer = RecordingAuthorizer()

        result = asyncio.run(tool.execute(
            transform_call(make_tool_call, '(dict-get inputs "input-text")'),
            MockRequester(),
            authorizer
        ))

        assert "no changes" in result.content.lower()
        assert not authorizer.requests
        assert editor.file_text() == "unchanged"

    def test_transform_invalid_program(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """Transform raises AIToolExecutionError for a program with a syntax error."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "content")

        with pytest.raises(AIToolExecutionError):
            asyncio.run(tool.execute(
                transform_call(make_tool_call, "(integer+ 1 2"),
                MockRequester(),
                mock_authorization
            ))

        assert editor.file_text() == "content"

    def test_transform_wrong_return_type(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """Transform raises AIToolExecutionError when the program returns a non-text type."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "content")

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(
                transform_call(make_tool_call, "42"),
                MockRequester(),
                mock_authorization
            ))

        assert "string or list of strings" in str(exc_info.value)
        assert editor.file_text() == "content"

    def test_transform_empty_program(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """Transform raises AIToolExecutionError for a blank program."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "content")

        with pytest.raises(AIToolExecutionError):
            asyncio.run(tool.execute(
                transform_call(make_tool_call, "   "),
                MockRequester(),
                mock_authorization
            ))

    def test_transform_writes_program_result(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """The 'inputs' dict carries the buffer content under "input-text" as a single string."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "line one\nline two")

        asyncio.run(tool.execute(
            transform_call(
                make_tool_call,
                '(integer->string (string-length (dict-get inputs "input-text")))'
            ),
            MockRequester(),
            mock_authorization
        ))

        assert editor.file_text() == "17"

    def test_transform_input_has_no_input_lines(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """The 'inputs' dict does not carry 'input-lines'; the program splits the text itself."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "alpha\nbeta\ngamma")

        asyncio.run(tool.execute(
            transform_call(
                make_tool_call,
                '(integer->string (list-length (string->list (dict-get inputs "input-text") "\\n")))'
            ),
            MockRequester(),
            mock_authorization
        ))

        assert editor.file_text() == "3"

    def test_transform_logs_interaction(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """A successful transform logs one interaction to the mindspace log."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "hello")

        asyncio.run(tool.execute(
            transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
            MockRequester(),
            mock_authorization
        ))

        mindspace.add_interaction.assert_called_once()
        message = mindspace.add_interaction.call_args[0][1]
        assert "saved" in message

    def test_transform_missing_tab(self, editor_tool, mock_authorization, make_tool_call):
        """Transform raises AIToolExecutionError when the tab_id is not found."""
        tool, mindspace = editor_tool
        registry = MagicMock()
        registry.get_model.return_value = None
        mindspace.contexts.return_value = registry

        with pytest.raises(AIToolExecutionError):
            asyncio.run(tool.execute(
                transform_call(make_tool_call, '(dict-get inputs "input-text")'),
                MockRequester(),
                mock_authorization
            ))

    def test_transform_dry_run_returns_diff_without_writing(
        self, editor_tool, make_tool_call, tmp_path
    ):
        """dry_run=True returns the diff, writes nothing and asks for nothing."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "hello world")
        authorizer = RecordingAuthorizer()

        result = asyncio.run(tool.execute(
            transform_call(
                make_tool_call,
                '(string-upcase (dict-get inputs "input-text"))',
                dry_run=True
            ),
            MockRequester(),
            authorizer
        ))

        assert "dry run" in result.content.lower()
        assert "+HELLO WORLD" in result.content
        assert not authorizer.requests
        assert editor.file_text() == "hello world"
        assert editor.buffer() == "hello world"

    def test_transform_dry_run_no_change(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """dry_run=True on an identity transform reports no changes."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "unchanged")

        result = asyncio.run(tool.execute(
            transform_call(make_tool_call, '(dict-get inputs "input-text")', dry_run=True),
            MockRequester(),
            mock_authorization
        ))

        assert "no changes" in result.content.lower()
        assert editor.file_text() == "unchanged"

    def test_transform_blocked_without_help(
        self, editor_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """Transform raises an error when Menai help has not been read."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "hello world")

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(
                transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
                MockRequester(menai_help_read=False),
                mock_authorization
            ))

        assert "must read the Menai language documentation" in str(exc_info.value)
        assert editor.file_text() == "hello world"

    def test_transform_refused_restores_buffer(self, editor_tool, make_tool_call, tmp_path):
        """A refused transform leaves both the buffer and the file as they were."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "hello world")
        authorizer = RecordingAuthorizer(grant=False)

        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(tool.execute(
                transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
                MockRequester(),
                authorizer
            ))

        assert editor.buffer() == "hello world"
        assert editor.file_text() == "hello world"

    def test_transform_asks_with_the_file_change(self, editor_tool, make_tool_call, tmp_path):
        """The approval shows the change the write makes to the file."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "hello world")
        authorizer = RecordingAuthorizer()

        asyncio.run(tool.execute(
            transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
            MockRequester(),
            authorizer
        ))

        assert len(authorizer.requests) == 1
        _, _, context, diff, is_write, kind = authorizer.requests[0]
        assert "hello world" in diff
        assert "HELLO WORLD" in diff
        assert is_write
        assert kind == AIToolApprovalKind.MINDSPACE_FILE_CHANGE
        assert "Menai transform" in context
        assert "file.txt" in context
        assert str(tmp_path) not in context

    def test_transform_asks_to_write_unsaved_buffer_changes(
        self, editor_tool, make_tool_call, tmp_path
    ):
        """The approval shows and mentions work the buffer already held unsaved."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "one\ntwo")
        editor.context.apply_diff('\n'.join(
            difflib.unified_diff(["one", "two"], ["ONE", "two"], lineterm='')
        ))
        authorizer = RecordingAuthorizer()

        asyncio.run(tool.execute(
            transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
            MockRequester(),
            authorizer
        ))

        _, _, context, diff, _, _ = authorizer.requests[0]
        assert "ONE" in diff
        assert "made outside this tool" in context
        assert editor.file_text() == "ONE\nTWO"

    def test_transform_asks_standard_approval_outside_mindspace(
        self, editor_tool, make_tool_call, tmp_path
    ):
        """A tab holding a file the journal does not record asks a standard approval."""
        tool, mindspace = editor_tool
        make_editor(tmp_path, mindspace, "hello")
        mindspace.journalled_path.return_value = None
        authorizer = RecordingAuthorizer()

        asyncio.run(tool.execute(
            transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
            MockRequester(),
            authorizer
        ))

        assert authorizer.requests[0][5] == AIToolApprovalKind.STANDARD

    def test_transform_refuses_read_only_tab(self, editor_tool, make_tool_call, tmp_path):
        """A read-only view of an earlier version cannot be changed or written."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "old version", read_only=True)
        authorizer = RecordingAuthorizer()

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(
                transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
                MockRequester(),
                authorizer
            ))

        assert "read-only view" in str(exc_info.value)
        assert not authorizer.requests
        assert editor.buffer() == "old version"

    def test_transform_refuses_untitled_tab(self, editor_tool, make_tool_call, tmp_path):
        """An untitled tab has nowhere to be written, so it is refused before changing."""
        tool, mindspace = editor_tool
        editor = make_editor(tmp_path, mindspace, "draft", untitled=True)
        authorizer = RecordingAuthorizer()

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(
                transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
                MockRequester(),
                authorizer
            ))

        assert "untitled" in str(exc_info.value)
        assert not authorizer.requests
        assert editor.buffer() == "draft"

    def test_transform_failed_write_restores_buffer(
        self, editor_tool, mock_mindspace, make_tool_call, tmp_path
    ):
        """A write that fails leaves neither the buffer nor the file changed."""
        tool, _ = editor_tool
        path = tmp_path / "file.txt"
        path.write_text("hello world", encoding='utf-8')

        document = EditorContextDocument(path=str(path), content="hello world")
        context = EditorContext(
            context_id="test-tab-id",
            document=document,
            save_cb=lambda _actor, _conversation: False
        )
        registry = MagicMock()
        registry.get_model.return_value = context
        mock_mindspace.contexts.return_value = registry

        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(
                transform_call(make_tool_call, '(string-upcase (dict-get inputs "input-text"))'),
                MockRequester(),
                RecordingAuthorizer()
            ))

        assert "write did not happen" in str(exc_info.value)
        assert document.to_plain_text() == "hello world"
        assert path.read_text(encoding='utf-8') == "hello world"
