"""Tests for the EditorContextDocument line-based text model.

These tests use stdlib only — no Qt dependencies.
"""

import os
import tempfile

import pytest

from editor_context.editor_document import EditorContextDocument


class TestEditorContextDocumentBasic:
    """Test basic content access and line counting."""

    def test_empty_document(self):
        """A new document is empty with zero lines."""
        doc = EditorContextDocument()
        assert doc.to_plain_text() == ""
        assert doc.block_count() == 0
        assert doc.path() == ""

    def test_set_text_basic(self):
        """set_text stores content and block_count returns the line count."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        assert doc.block_count() == 3
        assert doc.to_plain_text() == "a\nb\nc"

    def test_set_text_trailing_newline(self):
        """A trailing newline is stored as an empty final element."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\n")
        assert doc.block_count() == 3
        assert doc.to_plain_text() == "a\nb\nc\n"

    def test_set_text_single_line_no_newline(self):
        """A single line without trailing newline has block_count 1."""
        doc = EditorContextDocument()
        doc.set_text("hello")
        assert doc.block_count() == 1
        assert doc.to_plain_text() == "hello"

    def test_set_text_single_line_with_newline(self):
        """A single line with trailing newline has block_count 1."""
        doc = EditorContextDocument()
        doc.set_text("hello\n")
        assert doc.block_count() == 1
        assert doc.to_plain_text() == "hello\n"

    def test_set_text_empty_string(self):
        """An empty string produces an empty document."""
        doc = EditorContextDocument()
        doc.set_text("")
        assert doc.block_count() == 0
        assert doc.to_plain_text() == ""

    def test_set_text_newline_only(self):
        """A single newline produces one blank line with trailing newline."""
        doc = EditorContextDocument()
        doc.set_text("\n")
        assert doc.block_count() == 1
        assert doc.to_plain_text() == "\n"

    def test_set_text_blank_lines(self):
        """Multiple blank lines are counted correctly."""
        doc = EditorContextDocument()
        doc.set_text("\n\n\n")
        assert doc.block_count() == 3
        assert doc.to_plain_text() == "\n\n\n"


class TestEditorContextDocumentGetTextRange:
    """Test get_text_range with various line ranges."""

    def test_get_text_range_full(self):
        """Full document is returned when no range is specified."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\nd\ne")
        assert doc.get_text_range() == "a\nb\nc\nd\ne"

    def test_get_text_range_partial(self):
        """A partial range returns the selected lines."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\nd\ne")
        assert doc.get_text_range(2, 4) == "b\nc\nd"

    def test_get_text_range_start_only(self):
        """Specifying only start_line returns from that line to the end."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\nd\ne")
        assert doc.get_text_range(3) == "c\nd\ne"

    def test_get_text_range_end_only(self):
        """Specifying only end_line returns from line 1 to that line."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\nd\ne")
        assert doc.get_text_range(None, 2) == "a\nb"

    def test_get_text_range_single_line(self):
        """A single-line range returns one line."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        assert doc.get_text_range(2, 2) == "b"

    def test_get_text_range_start_too_low(self):
        """start_line < 1 raises ValueError."""
        doc = EditorContextDocument()
        doc.set_text("a\nb")
        with pytest.raises(ValueError, match="start_line must be >= 1"):
            doc.get_text_range(0, 2)

    def test_get_text_range_end_before_start(self):
        """end_line < start_line raises ValueError."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        with pytest.raises(ValueError, match="end_line .* must be >= start_line"):
            doc.get_text_range(3, 2)

    def test_get_text_range_start_exceeds_length(self):
        """start_line beyond document length raises ValueError."""
        doc = EditorContextDocument()
        doc.set_text("a\nb")
        with pytest.raises(ValueError, match="exceeds document length"):
            doc.get_text_range(10, 20)

    def test_get_text_range_end_clamped(self):
        """end_line beyond document length is clamped to the last line."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        assert doc.get_text_range(1, 100) == "a\nb\nc"

    def test_get_text_range_trailing_newline(self):
        """get_text_range works correctly with a trailing newline."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\n")
        assert doc.get_text_range(1, 3) == "a\nb\nc"
        assert doc.get_text_range() == "a\nb\nc\n"


class TestEditorContextDocumentGetLine:
    """Test get_line and get_lines."""

    def test_get_line(self):
        """get_line returns a single line (1-indexed)."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        assert doc.get_line(1) == "a"
        assert doc.get_line(2) == "b"
        assert doc.get_line(3) == "c"

    def test_get_line_out_of_range(self):
        """get_line raises ValueError for out-of-range lines."""
        doc = EditorContextDocument()
        doc.set_text("a\nb")
        with pytest.raises(ValueError, match="exceeds document length"):
            doc.get_line(3)

    def test_get_line_zero(self):
        """get_line raises ValueError for line 0."""
        doc = EditorContextDocument()
        doc.set_text("a\nb")
        with pytest.raises(ValueError, match="must be >= 1"):
            doc.get_line(0)

    def test_get_lines(self):
        """get_lines returns a slice of lines."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\nd\ne")
        assert doc.get_lines(2, 3) == ["b", "c", "d"]

    def test_get_lines_at_end(self):
        """get_lines returns fewer lines if document is shorter."""
        doc = EditorContextDocument()
        doc.set_text("a\nb")
        assert doc.get_lines(1, 10) == ["a", "b"]


class TestEditorContextDocumentSearch:
    """Test find_all_occurrences."""

    def test_find_plain(self):
        """Plain text search finds matches on each line."""
        doc = EditorContextDocument()
        doc.set_text("foo bar\nbaz foo\nqux")
        matches = doc.find_all_occurrences("foo")
        assert len(matches) == 2
        assert matches[0]['line'] == 1
        assert matches[0]['column'] == 1
        assert matches[0]['match_text'] == "foo"
        assert matches[0]['context'] == "foo bar"
        assert matches[1]['line'] == 2
        assert matches[1]['column'] == 5

    def test_find_case_insensitive(self):
        """Case-insensitive search finds matches regardless of case."""
        doc = EditorContextDocument()
        doc.set_text("Foo\nfoo\nFOO")
        matches = doc.find_all_occurrences("foo", case_sensitive=False)
        assert len(matches) == 3

    def test_find_case_sensitive(self):
        """Case-sensitive search only finds exact case matches."""
        doc = EditorContextDocument()
        doc.set_text("Foo\nfoo\nFOO")
        matches = doc.find_all_occurrences("foo", case_sensitive=True)
        assert len(matches) == 1
        assert matches[0]['line'] == 2

    def test_find_regex(self):
        """Regex search finds pattern matches."""
        doc = EditorContextDocument()
        doc.set_text("abc123\ndef456\nghi")
        matches = doc.find_all_occurrences(r"\d+", regexp=True)
        assert len(matches) == 2
        assert matches[0]['match_text'] == "123"
        assert matches[1]['match_text'] == "456"

    def test_find_regex_case_insensitive(self):
        """Regex search respects case_sensitive flag."""
        doc = EditorContextDocument()
        doc.set_text("Hello\nhello")
        matches = doc.find_all_occurrences(r"hello", regexp=True, case_sensitive=False)
        assert len(matches) == 2

    def test_find_invalid_regex(self):
        """Invalid regex raises ValueError."""
        doc = EditorContextDocument()
        doc.set_text("test")
        with pytest.raises(ValueError, match="Invalid regular expression"):
            doc.find_all_occurrences("[invalid", regexp=True)

    def test_find_no_matches(self):
        """Search with no matches returns an empty list."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        assert doc.find_all_occurrences("xyz") == []

    def test_find_empty_search(self):
        """Empty search text returns an empty list."""
        doc = EditorContextDocument()
        doc.set_text("a\nb")
        assert doc.find_all_occurrences("") == []

    def test_find_multiple_per_line(self):
        """Multiple matches on the same line are all found."""
        doc = EditorContextDocument()
        doc.set_text("foo foo foo")
        matches = doc.find_all_occurrences("foo")
        assert len(matches) == 3
        assert matches[0]['column'] == 1
        assert matches[1]['column'] == 5
        assert matches[2]['column'] == 9


class TestEditorContextDocumentModified:
    """Test is_modified and saved state."""

    def test_is_modified_initial(self):
        """A new document is not modified."""
        doc = EditorContextDocument()
        assert not doc.is_modified()

    def test_is_modified_after_set_text(self):
        """Setting text makes the document modified."""
        doc = EditorContextDocument()
        doc.set_text("hello")
        assert doc.is_modified()

    def test_is_modified_after_mark_saved(self):
        """mark_saved clears the modified flag."""
        doc = EditorContextDocument()
        doc.set_text("hello")
        doc.mark_saved()
        assert not doc.is_modified()

    def test_is_modified_after_revert(self):
        """Reverting to saved content clears the modified flag."""
        doc = EditorContextDocument()
        doc.set_text("hello")
        doc.mark_saved()
        doc.set_text("world")
        assert doc.is_modified()
        doc.set_text("hello")
        assert not doc.is_modified()

    def test_saved_content(self):
        """saved_content returns the content at last mark_saved."""
        doc = EditorContextDocument()
        doc.set_text("original")
        doc.mark_saved()
        doc.set_text("modified")
        assert doc.saved_content() == "original"


class TestEditorContextDocumentFileOps:
    """Test load_from_disk, save_to_disk, and get_diff."""

    def test_save_and_load(self, tmp_path):
        """save_to_disk writes the file and load_from_disk reads it back."""
        file_path = str(tmp_path / "test.txt")
        doc = EditorContextDocument(path=file_path)
        doc.set_text("line1\nline2\nline3")
        doc.save_to_disk()
        assert not doc.is_modified()

        doc2 = EditorContextDocument(path=file_path)
        doc2.load_from_disk()
        assert doc2.to_plain_text() == "line1\nline2\nline3"
        assert not doc2.is_modified()

    def test_get_diff_no_changes(self):
        """get_diff returns empty string when content matches saved state."""
        doc = EditorContextDocument(path="/tmp/test.txt")
        doc.set_text("a\nb")
        doc.mark_saved()
        assert doc.get_diff() == ""

    def test_get_diff_with_changes(self):
        """get_diff returns a unified diff when content differs from saved."""
        doc = EditorContextDocument(path="/tmp/test.txt")
        doc.set_text("a\nb\nc")
        doc.mark_saved()
        doc.set_text("a\nB\nc")
        diff = doc.get_diff()
        assert diff != ""
        assert "-b" in diff
        assert "+B" in diff

    def test_get_diff_no_path(self):
        """get_diff returns empty string when no path is set."""
        doc = EditorContextDocument()
        doc.set_text("a\nb")
        assert doc.get_diff() == ""

    def test_get_diff_not_modified(self):
        """get_diff returns empty string when not modified."""
        doc = EditorContextDocument(path="/tmp/test.txt")
        doc.set_text("a\nb")
        doc.mark_saved()
        assert doc.get_diff() == ""


class TestEditorContextDocumentListeners:
    """Test change notification listeners."""

    def test_listener_called_on_set_text(self):
        """Listener is called when set_text modifies the document."""
        doc = EditorContextDocument()
        called = []
        doc.add_listener(lambda: called.append(True))
        doc.set_text("hello")
        assert len(called) == 1

    def test_listener_called_on_replace_text(self):
        """Listener is called when replace_text modifies the document."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        called = []
        doc.add_listener(lambda: called.append(True))
        doc.replace_text(2, 2, "X")
        assert len(called) == 1
        assert doc.to_plain_text() == "a\nX\nc"

    def test_listener_remove(self):
        """Removed listeners are not called."""
        doc = EditorContextDocument()
        called = []
        listener = lambda: called.append(True)  # noqa: E731
        doc.add_listener(listener)
        doc.remove_listener(listener)
        doc.set_text("hello")
        assert len(called) == 0

    def test_listener_exception_does_not_propagate(self):
        """A listener exception is logged and does not prevent other listeners."""
        doc = EditorContextDocument()
        called = []
        doc.add_listener(lambda: (_ for _ in ()).throw(RuntimeError("boom")))
        doc.add_listener(lambda: called.append(True))
        doc.set_text("hello")
        assert len(called) == 1


class TestEditorContextDocumentReplaceText:
    """Test replace_text."""

    def test_replace_single_line(self):
        """Replacing a single line with new content."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        doc.replace_text(2, 2, "X")
        assert doc.to_plain_text() == "a\nX\nc"

    def test_replace_multiple_lines(self):
        """Replacing multiple lines with a single line."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc\nd")
        doc.replace_text(2, 3, "X")
        assert doc.to_plain_text() == "a\nX\nd"

    def test_replace_with_multiple_lines(self):
        """Replacing a single line with multiple lines."""
        doc = EditorContextDocument()
        doc.set_text("a\nb\nc")
        doc.replace_text(2, 2, "X\nY")
        assert doc.to_plain_text() == "a\nX\nY\nc"
