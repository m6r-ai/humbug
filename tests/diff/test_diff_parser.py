"""Tests for diff parser."""

import pytest

from diff.diff_parser import DiffParser
from diff.diff_exceptions import DiffParseError
from diff.diff_types import DiffLine, DiffHunk


class TestDiffParserBasic:
    """Test basic diff parsing functionality."""

    def test_parse_simple_hunk(self):
        """Test parsing a simple single-hunk diff."""
        diff_text = """@@ -10,3 +10,3 @@
 context line
-old line
+new line
 context line
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        hunk = hunks[0]
        assert hunk.old_start == 10
        assert hunk.old_count == 3
        assert hunk.new_start == 10
        assert hunk.new_count == 3
        assert len(hunk.lines) == 4

        assert hunk.lines[0].type == ' '
        assert hunk.lines[0].content == 'context line'
        assert hunk.lines[1].type == '-'
        assert hunk.lines[1].content == 'old line'
        assert hunk.lines[2].type == '+'
        assert hunk.lines[2].content == 'new line'
        assert hunk.lines[3].type == ' '
        assert hunk.lines[3].content == 'context line'

    def test_parse_with_file_headers(self):
        """Test parsing diff with file headers."""
        diff_text = """--- a/file.txt
+++ b/file.txt
@@ -1,2 +1,2 @@
-old
+new
 context
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_start == 1
        assert len(hunks[0].lines) == 3  # -old, +new, and context line

    def test_parse_multiple_hunks(self):
        """Test parsing diff with multiple hunks."""
        diff_text = """@@ -10,2 +10,2 @@
 context
-old1
+new1
@@ -20,2 +20,2 @@
 context
-old2
+new2
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 2
        assert hunks[0].old_start == 10
        assert hunks[1].old_start == 20

    def test_parse_insertion_only(self):
        """Test parsing insertion-only hunk."""
        diff_text = """@@ -10,0 +10,2 @@
+new line 1
+new line 2
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        hunk = hunks[0]
        assert hunk.old_count == 0
        assert hunk.new_count == 2
        assert all(line.type == '+' for line in hunk.lines)

    def test_parse_deletion_only(self):
        """Test parsing deletion-only hunk."""
        diff_text = """@@ -10,2 +10,0 @@
-old line 1
-old line 2
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        hunk = hunks[0]
        assert hunk.old_count == 2
        assert hunk.new_count == 0
        assert all(line.type == '-' for line in hunk.lines)

    def test_parse_single_line_count(self):
        """Test parsing hunk with implicit single line count."""
        diff_text = """@@ -10 +10 @@
-old
+new
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_count == 1
        assert hunks[0].new_count == 1


class TestDiffParserEdgeCases:
    """Test edge cases and error conditions."""

    def test_parse_empty_diff_raises_error(self):
        """Test that empty diff raises error."""
        parser = DiffParser()

        with pytest.raises(DiffParseError, match="Empty diff"):
            parser.parse("")

        with pytest.raises(DiffParseError, match="Empty diff"):
            parser.parse("   \n\n  ")

    def test_parse_no_hunks_raises_error(self):
        """Test that diff with no hunks raises error."""
        parser = DiffParser()
        diff_text = """--- a/file.txt
+++ b/file.txt
"""
        with pytest.raises(DiffParseError, match="No valid hunks"):
            parser.parse(diff_text)

    def test_parse_invalid_hunk_header_raises_error(self):
        """Test that invalid hunk header raises error."""
        parser = DiffParser()
        diff_text = """@@ invalid header @@
-old
+new
"""
        with pytest.raises(DiffParseError, match="Invalid hunk header"):
            parser.parse(diff_text)

    def test_parse_missing_hunk_header_raises_error(self):
        """Test that diff without hunk header raises error."""
        parser = DiffParser()
        diff_text = """-old line
+new line
"""
        with pytest.raises(DiffParseError, match="No valid hunks"):
            parser.parse(diff_text)

    def test_parse_empty_hunk(self):
        """Test parsing hunk with no content lines."""
        diff_text = """@@ -10,0 +10,0 @@
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert len(hunks[0].lines) == 0

    def test_parse_hunk_with_empty_lines(self):
        """Test parsing hunk with empty lines in content."""
        diff_text = """@@ -1,3 +1,3 @@
 line 1
-
+
 line 3
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert len(hunks[0].lines) == 4
        assert hunks[0].lines[1].content == ''
        assert hunks[0].lines[2].content == ''

    def test_parse_no_newline_marker(self):
        """Test parsing diff with 'No newline at end of file' marker."""
        diff_text = """@@ -1,2 +1,2 @@
 line 1
-old line
+new line
\\ No newline at end of file
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        # The marker should be ignored
        assert len(hunks) == 1
        assert len(hunks[0].lines) == 3

    def test_parse_line_without_prefix(self):
        """Test parsing line without prefix (treated as context)."""
        diff_text = """@@ -1,2 +1,2 @@
line without prefix
-old line
+new line
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        # Line without prefix should be treated as context
        assert hunks[0].lines[0].type == ' '
        assert hunks[0].lines[0].content == 'line without prefix'


class TestDiffParserComplexScenarios:
    """Test complex diff parsing scenarios."""

    def test_parse_large_hunk(self):
        """Test parsing a large hunk with many lines."""
        lines = []
        for i in range(100):
            if i % 3 == 0:
                lines.append(f"-old line {i}")
            elif i % 3 == 1:
                lines.append(f"+new line {i}")
            else:
                lines.append(f" context line {i}")

        diff_text = "@@ -1,100 +1,100 @@\n" + "\n".join(lines)

        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert len(hunks[0].lines) == 100

    def test_parse_multiple_hunks_various_types(self):
        """Test parsing multiple hunks with different operations."""
        diff_text = """@@ -10,2 +10,3 @@
 context
-old
+new1
+new2
@@ -20,3 +21,2 @@
 context
-delete1
-delete2
 context
@@ -30,0 +31,1 @@
+pure insertion
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 3

        # First hunk: replacement with extra line
        assert hunks[0].old_count == 2
        assert hunks[0].new_count == 3

        # Second hunk: deletion
        assert hunks[1].old_count == 3
        assert hunks[1].new_count == 2

        # Third hunk: pure insertion
        assert hunks[2].old_count == 0
        assert hunks[2].new_count == 1

    def test_parse_hunks_with_similar_content(self):
        """Test parsing hunks that modify similar lines."""
        diff_text = """@@ -1,3 +1,3 @@
 def foo():
-    return 1
+    return 2
     pass
@@ -10,3 +10,3 @@
 def bar():
-    return 1
+    return 3
     pass
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 2
        assert hunks[0].lines[1].content == '    return 1'
        assert hunks[0].lines[2].content == '    return 2'
        assert hunks[1].lines[1].content == '    return 1'
        assert hunks[1].lines[2].content == '    return 3'

    def test_parse_preserves_whitespace(self):
        """Test that parser preserves whitespace in content."""
        diff_text = """@@ -1,3 +1,3 @@
     leading spaces
-\told line with tab
+\tnew line with tab
 trailing spaces   
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert hunks[0].lines[0].content == '    leading spaces'
        assert hunks[0].lines[1].content == '\told line with tab'
        assert hunks[0].lines[2].content == '\tnew line with tab'
        assert hunks[0].lines[3].content == 'trailing spaces   '

    def test_parse_special_characters(self):
        """Test parsing content with special characters."""
        diff_text = """@@ -1,3 +1,3 @@
 line with "quotes"
-line with 'apostrophes'
+line with `backticks`
 line with @#$%^&*()
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].lines[0].content == 'line with "quotes"'
        assert hunks[0].lines[1].content == "line with 'apostrophes'"
        assert hunks[0].lines[2].content == 'line with `backticks`'
        assert hunks[0].lines[3].content == 'line with @#$%^&*()'

    def test_parse_unicode_content(self):
        """Test parsing content with unicode characters."""
        diff_text = """@@ -1,2 +1,2 @@
-Hello 世界
+你好 World
 Emoji: 😀🎉
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].lines[0].content == 'Hello 世界'
        assert hunks[0].lines[1].content == '你好 World'
        assert hunks[0].lines[2].content == 'Emoji: 😀🎉'


class TestDiffParserHunkHeaderVariations:
    """Test various hunk header format variations."""

    def test_parse_minimal_header(self):
        """Test parsing minimal hunk header format."""
        diff_text = """@@ -1 +1 @@
-old
+new
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_start == 1
        assert hunks[0].old_count == 1
        assert hunks[0].new_start == 1
        assert hunks[0].new_count == 1

    def test_parse_header_with_function_context(self):
        """Test parsing hunk header with function context (ignored)."""
        diff_text = """@@ -10,3 +10,3 @@ def function_name():
 context
-old
+new
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_start == 10

    def test_parse_header_with_extra_whitespace(self):
        """Test parsing hunk header with extra whitespace."""
        diff_text = """@@  -10,3  +10,3  @@
 context
-old
+new
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_start == 10

    def test_parse_zero_line_start(self):
        """Test parsing hunk starting at line 0 (file start insertion)."""
        diff_text = """@@ -0,0 +1,2 @@
+new line 1
+new line 2
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_start == 0
        assert hunks[0].new_start == 1

    def test_parse_large_line_numbers(self):
        """Test parsing hunk with large line numbers."""
        diff_text = """@@ -99999,2 +99999,2 @@
 context
-old
+new
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_start == 99999
        assert hunks[0].new_start == 99999


class TestDiffParserIntegration:
    """Test parser integration scenarios."""

    def test_parse_real_world_python_diff(self):
        """Test parsing a realistic Python file diff."""
        diff_text = """--- a/example.py
+++ b/example.py
@@ -1,10 +1,11 @@
 import os
 import sys
+import json
 
 def main():
-    print("Hello")
+    print("Hello, World!")
     return 0
 
 if __name__ == "__main__":
     main()
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        hunk = hunks[0]
        assert hunk.old_start == 1
        assert hunk.old_count == 10
        assert hunk.new_count == 11

    def test_parse_real_world_text_diff(self):
        """Test parsing a realistic text file diff."""
        diff_text = """@@ -15,7 +15,8 @@
 This is a paragraph of text.
 It contains multiple sentences.
-The old version had this text.
+The new version has this text instead.
+And an additional line was added.
 
 This paragraph remains unchanged.
 As does this one.
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        assert len(hunks) == 1
        assert hunks[0].old_start == 15
        assert hunks[0].old_count == 7
        assert hunks[0].new_count == 8

    def test_parse_multiple_files_hunks(self):
        """Test parsing diff affecting multiple locations."""
        diff_text = """--- a/file1.txt
+++ b/file1.txt
@@ -5,2 +5,2 @@
-old in file1
+new in file1
 context
--- a/file2.txt
+++ b/file2.txt
@@ -10,2 +10,2 @@
-old in file2
+new in file2
 context
"""
        parser = DiffParser()
        hunks = parser.parse(diff_text)

        # Parser should find both hunks
        assert len(hunks) == 2
        assert hunks[0].old_start == 5
        assert hunks[1].old_start == 10

    def test_parser_reusability(self):
        """Test that parser can be reused for multiple diffs."""
        parser = DiffParser()

        diff1 = """@@ -1,1 +1,1 @@
-old1
+new1
"""
        diff2 = """@@ -2,1 +2,1 @@
-old2
+new2
"""

        hunks1 = parser.parse(diff1)
        hunks2 = parser.parse(diff2)

        assert len(hunks1) == 1
        assert len(hunks2) == 1
        assert hunks1[0].lines[0].content == 'old1'
        assert hunks2[0].lines[0].content == 'old2'
