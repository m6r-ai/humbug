"""Tests for the filesystem tool transform_file operation."""
import asyncio
import json
import os
from pathlib import Path
from unittest.mock import MagicMock

import pytest

from ai_tool import AIToolExecutionError, AIToolAuthorizationDenied
from filesystem_ai_tool.filesystem_ai_tool import FileSystemAITool
from filesystem_ai_tool.filesystem_access_settings import FilesystemAccessSettings


@pytest.fixture
def real_path_resolver():
    """Path resolver that accepts real absolute paths (no sandbox restriction)."""
    def resolver(path: str) -> tuple[Path, str]:
        abs_path = Path(path).resolve()
        return abs_path, str(abs_path)
    return resolver


@pytest.fixture
def real_access_settings():
    """Access settings that allow all local paths."""
    def get_settings() -> FilesystemAccessSettings:
        return FilesystemAccessSettings(
            allow_external_access=False,
            external_allowlist=[],
            external_denylist=[]
        )
    return get_settings


@pytest.fixture
def transform_tool(real_path_resolver, real_access_settings):
    """FileSystemAITool with a real path resolver for transform tests."""
    return FileSystemAITool(
        resolve_path=real_path_resolver,
        get_access_settings=real_access_settings,
        mindspace=MagicMock()
    )


@pytest.fixture
def mock_authorization():
    """Fixture that always grants authorization."""
    mock = MagicMock()

    async def _auth(*_args, **_kwargs):
        return True

    mock.side_effect = _auth
    return mock


@pytest.fixture
def mock_authorization_denied():
    """Fixture that always denies authorization."""
    mock = MagicMock()

    async def _auth(*_args, **_kwargs):
        return False

    mock.side_effect = _auth
    return mock


class TestFileSystemAIToolTransformFile:
    """Tests for the transform_file operation."""

    def test_transform_string_result(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """Transform program returning a string overwrites the file."""
        target = tmp_path / "hello.txt"
        target.write_text("hello world", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "(string-upcase input-text)"}
        )
        result = asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]
        assert target.read_text(encoding='utf-8') == "HELLO WORLD"

    def test_transform_list_result(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """Transform program returning a list of strings joins with newlines."""
        target = tmp_path / "lines.txt"
        target.write_text("alpha\nbeta\ngamma", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "(list-reverse input-lines)"}
        )
        result = asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]
        assert target.read_text(encoding='utf-8') == "gamma\nbeta\nalpha"

    def test_transform_no_change(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """Transform producing identical content reports no changes and skips the write."""
        target = tmp_path / "same.txt"
        original = "unchanged content"
        target.write_text(original, encoding='utf-8')
        mtime_before = os.path.getmtime(str(target))

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "input-text"}
        )
        result = asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        assert "no changes" in result.content.lower()
        assert target.read_text(encoding='utf-8') == original
        assert os.path.getmtime(str(target)) == mtime_before

    def test_transform_authorization_denied(self, transform_tool, mock_authorization_denied, make_tool_call, tmp_path):
        """Transform is aborted when the user denies authorization."""
        target = tmp_path / "secret.txt"
        target.write_text("original", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "(string-upcase input-text)"}
        )
        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(transform_tool.execute(tool_call, "", mock_authorization_denied))

        assert target.read_text(encoding='utf-8') == "original"

    def test_transform_missing_file(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """transform_file raises AIToolExecutionError when the file does not exist."""
        missing = str(tmp_path / "nonexistent.txt")
        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": missing, "program": "input-text"}
        )
        with pytest.raises(AIToolExecutionError):
            asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))

    def test_transform_invalid_program(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """transform_file raises AIToolExecutionError for a program with a syntax error."""
        target = tmp_path / "data.txt"
        target.write_text("content", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "(integer+ 1 2"}
        )
        with pytest.raises(AIToolExecutionError):
            asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))

    def test_transform_wrong_return_type(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """transform_file raises AIToolExecutionError when the program returns a non-text type."""
        target = tmp_path / "data.txt"
        target.write_text("content", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "42"}
        )
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        assert "string or list of strings" in str(exc_info.value)

    def test_transform_empty_program(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """transform_file raises AIToolExecutionError for a blank program."""
        target = tmp_path / "data.txt"
        target.write_text("content", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "   "}
        )
        with pytest.raises(AIToolExecutionError):
            asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))

    def test_transform_input_text_binding(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """'input-text' is bound to the full file content as a single string."""
        target = tmp_path / "data.txt"
        content = "line one\nline two"
        target.write_text(content, encoding='utf-8')

        program = '(integer->string (string-length input-text))'
        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": program}
        )
        result = asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]
        assert target.read_text(encoding='utf-8') == str(len(content))

    def test_transform_input_lines_binding(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """'input-lines' is bound to a list of strings, one per line."""
        target = tmp_path / "data.txt"
        target.write_text("alpha\nbeta\ngamma", encoding='utf-8')

        program = '(integer->string (list-length input-lines))'
        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": program}
        )
        result = asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        data = json.loads(result.content)
        assert "Transform applied" in data["message"]
        assert target.read_text(encoding='utf-8') == "3"

    def test_transform_atomic_write(self, transform_tool, mock_authorization, make_tool_call, tmp_path):
        """A successful transform produces the correct output and leaves no temp files."""
        target = tmp_path / "data.txt"
        target.write_text("hello", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "(string-upcase input-text)"}
        )
        asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        assert target.read_text(encoding='utf-8') == "HELLO"
        tmp_files = list(tmp_path.glob("*.tmp"))
        assert len(tmp_files) == 0

    def test_transform_dry_run_returns_diff_without_writing(
        self, transform_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """dry_run=True returns the diff and does not write the file."""
        target = tmp_path / "data.txt"
        original = "hello world"
        target.write_text(original, encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {
                "operation": "transform_file",
                "path": str(target),
                "program": "(string-upcase input-text)",
                "dry_run": True
            }
        )
        result = asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        assert target.read_text(encoding='utf-8') == original
        assert "dry run" in result.content.lower()
        assert "+" in result.content
        mock_authorization.assert_not_called()

    def test_transform_dry_run_no_change(
        self, transform_tool, mock_authorization, make_tool_call, tmp_path
    ):
        """dry_run=True on an identity transform reports no changes."""
        target = tmp_path / "data.txt"
        target.write_text("unchanged", encoding='utf-8')

        tool_call = make_tool_call(
            "filesystem",
            {"operation": "transform_file", "path": str(target), "program": "input-text", "dry_run": True}
        )
        result = asyncio.run(transform_tool.execute(tool_call, "", mock_authorization))
        assert "no changes" in result.content.lower()
        mock_authorization.assert_not_called()
