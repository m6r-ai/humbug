import asyncio
import json
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

from ai_tool import AIToolExecutionError
from ai_tool.filesystem.filesystem_access_settings import FilesystemAccessSettings
from ai_tool.filesystem.filesystem_ai_tool import FileSystemAITool


def make_tool_with_tmp(tmp_path):
    """Build a FileSystemAITool whose resolver maps any relative path into tmp_path."""
    def resolver(path_str):
        p = (tmp_path / path_str).resolve()
        if not str(p).startswith(str(tmp_path)):
            raise ValueError("Outside boundary")
        return p, path_str

    settings = FilesystemAccessSettings(
        allow_external_access=False,
        external_allowlist=[],
        external_denylist=[]
    )
    return FileSystemAITool(resolve_path=resolver, get_access_settings=lambda: settings)


class TestSearchFile:
    """Tests for the search_file operation."""

    def test_plain_text_match_case_insensitive_by_default(self, tmp_path, make_tool_call):
        """Plain-text search is case-insensitive by default and returns matching lines."""
        f = tmp_path / "hello.txt"
        f.write_text("Hello World\nfoo bar\nHELLO again\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "hello.txt",
            "search_text": "hello"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 2
        assert data["case_sensitive"] is False
        assert data["regexp"] is False
        assert data["truncated"] is False
        assert len(data["matches"]) == 2
        assert data["matches"][0]["line"] == 1
        assert data["matches"][0]["content"] == "Hello World"
        assert data["matches"][1]["line"] == 3
        assert data["matches"][1]["content"] == "HELLO again"

    def test_case_sensitive_excludes_wrong_case(self, tmp_path, make_tool_call):
        """Case-sensitive search only matches lines with the exact case."""
        f = tmp_path / "case.txt"
        f.write_text("Hello World\nfoo bar\nhello lower\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "case.txt",
            "search_text": "hello",
            "case_sensitive": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["case_sensitive"] is True
        assert data["match_count"] == 1
        assert data["matches"][0]["line"] == 3
        assert data["matches"][0]["content"] == "hello lower"

    def test_case_insensitive_explicit(self, tmp_path, make_tool_call):
        """Explicitly setting case_sensitive=False matches regardless of case."""
        f = tmp_path / "ci.txt"
        f.write_text("Alpha\nalpha\nALPHA\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "ci.txt",
            "search_text": "alpha",
            "case_sensitive": False
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 3

    def test_regexp_match(self, tmp_path, make_tool_call):
        """Regexp pattern matches lines according to the regular expression."""
        f = tmp_path / "re.txt"
        f.write_text("foo123\nbar456\nbaz\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "re.txt",
            "search_text": r"\d+",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["regexp"] is True
        assert data["match_count"] == 2
        lines = [m["line"] for m in data["matches"]]
        assert 1 in lines
        assert 2 in lines

    def test_regexp_no_matches(self, tmp_path, make_tool_call):
        """Regexp that matches nothing yields match_count=0 and empty matches list."""
        f = tmp_path / "empty_match.txt"
        f.write_text("hello\nworld\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "empty_match.txt",
            "search_text": r"\d{5}",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 0
        assert data["matches"] == []
        assert data["truncated"] is False

    def test_invalid_regexp_raises_execution_error(self, tmp_path, make_tool_call):
        """An invalid regular expression raises AIToolExecutionError."""
        f = tmp_path / "any.txt"
        f.write_text("some content\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "any.txt",
            "search_text": "[invalid((",
            "regexp": True
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "Invalid regular expression" in str(exc_info.value)

    def test_max_results_truncates_and_sets_truncated_flag(self, tmp_path, make_tool_call):
        """When max_results is reached, results are capped and truncated=True is set."""
        f = tmp_path / "many.txt"
        f.write_text("\n".join(["match"] * 10) + "\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "many.txt",
            "search_text": "match",
            "max_results": 3
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["truncated"] is True
        assert data["match_count"] == 3
        assert len(data["matches"]) == 3

    def test_max_results_not_exceeded_leaves_truncated_false(self, tmp_path, make_tool_call):
        """When fewer matches than max_results exist, truncated remains False."""
        f = tmp_path / "few.txt"
        f.write_text("match\nnope\nmatch\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "few.txt",
            "search_text": "match",
            "max_results": 10
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["truncated"] is False
        assert data["match_count"] == 2

    def test_file_not_found_raises_execution_error(self, tmp_path, make_tool_call):
        """Searching a non-existent file raises AIToolExecutionError."""
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "nonexistent.txt",
            "search_text": "anything"
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "does not exist" in str(exc_info.value)

    def test_path_is_directory_raises_execution_error(self, tmp_path, make_tool_call):
        """Searching a path that is a directory raises AIToolExecutionError."""
        subdir = tmp_path / "adir"
        subdir.mkdir()
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "adir",
            "search_text": "anything"
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "not a file" in str(exc_info.value)

    def test_file_too_large_raises_execution_error(self, tmp_path, make_tool_call):
        """A file exceeding the size limit raises AIToolExecutionError."""
        f = tmp_path / "big.txt"
        f.write_text("content\n")
        tool = make_tool_with_tmp(tmp_path)

        mock_stat = MagicMock()
        mock_stat.st_size = 11 * 1024 * 1024  # 11 MB

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "big.txt",
            "search_text": "content"
        })
        with patch("pathlib.Path.stat", return_value=mock_stat):
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(tool.execute(tool_call, "", None))

        assert "too large" in str(exc_info.value)

    def test_unicode_decode_error_raises_execution_error(self, tmp_path, make_tool_call):
        """A file that cannot be decoded with the given encoding raises AIToolExecutionError."""
        f = tmp_path / "binary.bin"
        f.write_bytes(b"\xff\xfe binary garbage \x00\x01")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "binary.bin",
            "search_text": "binary",
            "encoding": "utf-8"
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "decode" in str(exc_info.value).lower()

    def test_permission_error_raises_execution_error(self, tmp_path, make_tool_call):
        """A PermissionError when opening the file raises AIToolExecutionError."""
        f = tmp_path / "secret.txt"
        f.write_text("secret\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "secret.txt",
            "search_text": "secret"
        })
        with patch("builtins.open", side_effect=PermissionError("Access denied")):
            with pytest.raises(AIToolExecutionError) as exc_info:
                asyncio.run(tool.execute(tool_call, "", None))

        assert "Permission denied" in str(exc_info.value)

    def test_encoding_parameter_is_used(self, tmp_path, make_tool_call):
        """The encoding parameter is passed through to the file open call."""
        f = tmp_path / "latin.txt"
        f.write_bytes("caf\xe9\n".encode("latin-1"))
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "latin.txt",
            "search_text": "caf",
            "encoding": "latin-1"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 1
        assert "caf" in data["matches"][0]["content"]

    def test_result_json_structure(self, tmp_path, make_tool_call):
        """The result JSON contains all required top-level keys with correct types."""
        f = tmp_path / "struct.txt"
        f.write_text("needle in a haystack\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "struct.txt",
            "search_text": "needle"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        for key in ("path", "search_text", "case_sensitive", "regexp",
                    "match_count", "truncated", "matches"):
            assert key in data, f"Missing top-level key: {key}"

        assert isinstance(data["matches"], list)
        assert len(data["matches"]) == 1
        match = data["matches"][0]
        assert "line" in match
        assert "content" in match
        assert isinstance(match["line"], int)
        assert isinstance(match["content"], str)

    def test_matches_strip_trailing_newline(self, tmp_path, make_tool_call):
        """Match content does not include the trailing newline character."""
        f = tmp_path / "newlines.txt"
        f.write_text("hello world\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "newlines.txt",
            "search_text": "hello"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["matches"][0]["content"] == "hello world"

    def test_search_text_reflected_in_result(self, tmp_path, make_tool_call):
        """The search_text field in the result matches the input search_text."""
        f = tmp_path / "reflect.txt"
        f.write_text("something\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "reflect.txt",
            "search_text": "something"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["search_text"] == "something"


class TestFixEscapedPipePattern:
    """Tests for the _fix_escaped_pipe_pattern correction helper."""

    def test_search_file_escaped_pipe_corrected_and_warning_returned(self, tmp_path, make_tool_call):
        """When regexp uses '\\|' and finds nothing, it is corrected to '|' and a warning is added."""
        f = tmp_path / "alt.txt"
        f.write_text("foo\nbar\nbaz\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "alt.txt",
            "search_text": r"foo\|bar",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 2
        lines = [m["line"] for m in data["matches"]]
        assert 1 in lines
        assert 2 in lines
        assert "warning" in data
        assert r"\|" in data["warning"]

    def test_search_file_escaped_pipe_no_correction_when_original_matches(self, tmp_path, make_tool_call):
        """When the original pattern (with '\\|') already finds matches, no correction is attempted."""
        f = tmp_path / "literal.txt"
        f.write_text("foo|bar\nbaz\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "literal.txt",
            "search_text": r"foo\|bar",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 1
        assert "warning" not in data

    def test_search_file_no_escaped_pipe_no_warning(self, tmp_path, make_tool_call):
        """A regexp with no '\\|' and zero matches does not produce a warning."""
        f = tmp_path / "nomatch.txt"
        f.write_text("hello\nworld\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "nomatch.txt",
            "search_text": r"\d{5}",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 0
        assert "warning" not in data

    def test_search_file_plain_text_no_correction(self, tmp_path, make_tool_call):
        """Plain-text search (regexp=False) with no matches does not trigger correction."""
        f = tmp_path / "plain.txt"
        f.write_text("foo\nbar\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "plain.txt",
            "search_text": r"foo\|bar",
            "regexp": False
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["match_count"] == 0
        assert "warning" not in data

    def test_search_file_double_backslash_pipe_not_corrected(self, tmp_path, make_tool_call):
        """A '\\\\|' pattern (double backslash + pipe) is not treated as an escaped pipe mistake."""
        f = tmp_path / "bs.txt"
        f.write_text("foo\nbar\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_file",
            "path": "bs.txt",
            "search_text": "foo\\\\|bar",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert "warning" not in data

    def test_search_files_double_backslash_pipe_not_corrected(self, tmp_path, make_tool_call):
        """search_files: '\\\\|' pattern is not treated as an escaped pipe mistake."""
        (tmp_path / "a.txt").write_text("foo\nbar\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "foo\\\\|bar",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert "warning" not in data

    def test_search_files_escaped_pipe_corrected_and_warning_returned(self, tmp_path, make_tool_call):
        """search_files: '\\|' correction fires when the original finds nothing but corrected finds matches."""
        (tmp_path / "a.txt").write_text("foo\nbaz\n")
        (tmp_path / "b.txt").write_text("bar\nqux\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": r"foo\|bar",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["total_matches"] == 2
        assert data["files_with_matches"] == 2
        assert "warning" in data
        assert r"\|" in data["warning"]

    def test_search_files_escaped_pipe_no_correction_when_original_matches(self, tmp_path, make_tool_call):
        """search_files: no correction when the original '\\|' pattern already matches."""
        (tmp_path / "lit.txt").write_text("foo|bar\nother\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": r"foo\|bar",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["total_matches"] == 1
        assert "warning" not in data


class TestSearchFiles:
    """Tests for the search_files operation."""

    def test_basic_recursive_search_finds_matches_across_files(self, tmp_path, make_tool_call):
        """Recursive search returns matches from multiple files in a directory tree."""
        (tmp_path / "a.txt").write_text("needle here\nno match\n")
        sub = tmp_path / "sub"
        sub.mkdir()
        (sub / "b.txt").write_text("another needle\n")
        (sub / "c.txt").write_text("nothing relevant\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["total_matches"] == 2
        assert data["files_with_matches"] == 2
        assert data["truncated"] is False
        paths = [r["path"] for r in data["results"]]
        assert any("a.txt" in p for p in paths)
        assert any("b.txt" in p for p in paths)

    def test_include_glob_filters_to_matching_filenames(self, tmp_path, make_tool_call):
        """The include glob restricts search to files whose names match the pattern."""
        (tmp_path / "code.py").write_text("needle\n")
        (tmp_path / "notes.txt").write_text("needle\n")
        (tmp_path / "data.csv").write_text("needle\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle",
            "include": "*.py"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["files_with_matches"] == 1
        assert "code.py" in data["results"][0]["path"]

    def test_include_glob_no_matching_files_returns_empty_results(self, tmp_path, make_tool_call):
        """When include glob matches no files, results list is empty."""
        (tmp_path / "notes.txt").write_text("needle\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle",
            "include": "*.py"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["files_with_matches"] == 0
        assert data["results"] == []

    def test_case_sensitive_search(self, tmp_path, make_tool_call):
        """Case-sensitive search only matches lines with the correct case."""
        (tmp_path / "mixed.txt").write_text("Needle\nneedle\nNEEDLE\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle",
            "case_sensitive": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["total_matches"] == 1
        assert data["results"][0]["match_count"] == 1
        assert data["results"][0]["matches"][0]["content"] == "needle"

    def test_case_insensitive_search_by_default(self, tmp_path, make_tool_call):
        """Default (case-insensitive) search matches all case variants."""
        (tmp_path / "mixed.txt").write_text("Needle\nneedle\nNEEDLE\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["total_matches"] == 3

    def test_regexp_pattern_across_files(self, tmp_path, make_tool_call):
        """Regexp patterns are applied correctly across multiple files."""
        (tmp_path / "nums.txt").write_text("abc123\nnodigits\n456def\n")
        (tmp_path / "other.txt").write_text("789\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": r"\d+",
            "regexp": True
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["regexp"] is True
        assert data["total_matches"] == 3

    def test_invalid_regexp_raises_execution_error(self, tmp_path, make_tool_call):
        """An invalid regular expression raises AIToolExecutionError before any file is read."""
        (tmp_path / "f.txt").write_text("hello\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "[broken((",
            "regexp": True
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "Invalid regular expression" in str(exc_info.value)

    def test_max_results_truncates_across_files(self, tmp_path, make_tool_call):
        """When total_matches reaches max_results, truncated is True and collection stops."""
        for i in range(5):
            (tmp_path / f"f{i}.txt").write_text("match\nmatch\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "match",
            "max_results": 3
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["truncated"] is True
        assert data["total_matches"] == 3

    def test_directory_not_found_raises_execution_error(self, tmp_path, make_tool_call):
        """Searching a non-existent directory raises AIToolExecutionError."""
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": "ghost_dir",
            "search_text": "anything"
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "does not exist" in str(exc_info.value)

    def test_path_is_file_not_directory_raises_execution_error(self, tmp_path, make_tool_call):
        """Passing a file path instead of a directory raises AIToolExecutionError."""
        f = tmp_path / "afile.txt"
        f.write_text("content\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": "afile.txt",
            "search_text": "content"
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "not a directory" in str(exc_info.value)

    def test_binary_files_are_silently_skipped(self, tmp_path, make_tool_call):
        """Files that cannot be decoded are skipped without raising an error."""
        (tmp_path / "text.txt").write_text("needle\n")
        (tmp_path / "binary.bin").write_bytes(b"\xff\xfe\x00\x01 binary garbage")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle",
            "encoding": "utf-8"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["files_with_matches"] == 1
        assert "text.txt" in data["results"][0]["path"]

    def test_oversized_files_are_silently_skipped(self, tmp_path, make_tool_call):
        """Files that exceed the size limit are skipped without raising an error."""
        (tmp_path / "normal.txt").write_text("needle\n")
        big = tmp_path / "big.txt"
        big.write_text("needle\n")
        tool = make_tool_with_tmp(tmp_path)

        original_stat = Path.stat

        def patched_stat(self, *args, **kwargs):
            if self == big:
                mock = MagicMock()
                mock.st_size = 11 * 1024 * 1024
                return mock
            return original_stat(self, *args, **kwargs)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle"
        })
        with patch.object(Path, "stat", patched_stat):
            result = asyncio.run(tool.execute(tool_call, "", None))

        data = json.loads(result.content)
        assert data["files_with_matches"] == 1
        assert "normal.txt" in data["results"][0]["path"]

    def test_result_json_structure(self, tmp_path, make_tool_call):
        """The result JSON contains all required top-level and per-file keys."""
        (tmp_path / "file.txt").write_text("needle\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        for key in ("directory", "search_text", "case_sensitive", "regexp", "include",
                    "total_matches", "truncated", "files_with_matches", "results"):
            assert key in data, f"Missing top-level key: {key}"

        assert isinstance(data["results"], list)
        file_result = data["results"][0]
        assert "path" in file_result
        assert "match_count" in file_result
        assert "matches" in file_result
        assert isinstance(file_result["matches"], list)
        match = file_result["matches"][0]
        assert "line" in match
        assert "content" in match

    def test_files_with_matches_count_reflects_number_of_matched_files(self, tmp_path, make_tool_call):
        """files_with_matches equals the number of files that had at least one match."""
        (tmp_path / "a.txt").write_text("needle\n")
        (tmp_path / "b.txt").write_text("needle\n")
        (tmp_path / "c.txt").write_text("no match here\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["files_with_matches"] == 2
        assert len(data["results"]) == 2

    def test_denylist_blocks_individual_files_silently(self, tmp_path, make_tool_call):
        """Files that fail _validate_and_resolve_path with AIToolExecutionError are silently skipped."""
        allowed = tmp_path / "allowed.txt"
        allowed.write_text("needle\n")
        blocked = tmp_path / "blocked.txt"
        blocked.write_text("needle\n")

        blocked_resolved = blocked.resolve()

        def resolver(path_str):
            p = (tmp_path / path_str).resolve()
            if not str(p).startswith(str(tmp_path)):
                raise ValueError("Outside boundary")
            if p == blocked_resolved:
                raise AIToolExecutionError("access denied: matches a denied path pattern.")
            return p, path_str

        settings = FilesystemAccessSettings(
            allow_external_access=False,
            external_allowlist=[],
            external_denylist=[]
        )
        tool = FileSystemAITool(resolve_path=resolver, get_access_settings=lambda: settings)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["files_with_matches"] == 1
        assert "allowed.txt" in data["results"][0]["path"]

    def test_empty_results_when_nothing_matches(self, tmp_path, make_tool_call):
        """When no lines match the search text, results is an empty list and counts are zero."""
        (tmp_path / "a.txt").write_text("hello world\n")
        (tmp_path / "b.txt").write_text("goodbye world\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "zzznomatch"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["total_matches"] == 0
        assert data["files_with_matches"] == 0
        assert data["results"] == []
        assert data["truncated"] is False

    def test_include_none_reflected_in_result(self, tmp_path, make_tool_call):
        """When no include glob is given, the result contains include=null."""
        (tmp_path / "f.txt").write_text("x\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "x"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["include"] is None

    def test_include_value_reflected_in_result(self, tmp_path, make_tool_call):
        """When an include glob is given, it is reflected in the result JSON."""
        (tmp_path / "f.py").write_text("x\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "x",
            "include": "*.py"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["include"] == "*.py"

    def test_per_file_match_count_is_correct(self, tmp_path, make_tool_call):
        """Each file result's match_count reflects the number of matching lines in that file."""
        (tmp_path / "multi.txt").write_text("needle\nno\nneedle\nneedle\n")
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle"
        })
        result = asyncio.run(tool.execute(tool_call, "", None))
        data = json.loads(result.content)

        assert data["results"][0]["match_count"] == 3
        assert data["total_matches"] == 3

    def test_response_size_limit_raises_error_when_exceeded(self, tmp_path, make_tool_call):
        """search_files raises AIToolExecutionError when match content exceeds 64KB."""
        long_line = "needle " + "x" * 2000 + "\n"
        for i in range(40):
            (tmp_path / f"file_{i:02d}.txt").write_text(long_line)
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle",
            "max_results": 1000
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "64KB" in str(exc_info.value)
        assert "response size limit" in str(exc_info.value)

    def test_response_size_limit_raises_error_in_escaped_pipe_fallback(self, tmp_path, make_tool_call):
        """search_files raises AIToolExecutionError when the escaped-pipe fallback exceeds 64KB."""
        long_line = "needle\\|mark " + "x" * 2000 + "\n"
        for i in range(40):
            (tmp_path / f"file_{i:02d}.txt").write_text(long_line)
        tool = make_tool_with_tmp(tmp_path)

        tool_call = make_tool_call("filesystem", {
            "operation": "search_files",
            "path": ".",
            "search_text": "needle\\|mark",
            "regexp": True,
            "max_results": 1000
        })
        with pytest.raises(AIToolExecutionError) as exc_info:
            asyncio.run(tool.execute(tool_call, "", None))

        assert "64KB" in str(exc_info.value)
        assert "response size limit" in str(exc_info.value)
