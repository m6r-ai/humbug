"""Integration tests for the diff module."""

import pytest

from diff import (
    DiffParser,
    DiffMatcher,
    DiffApplier,
    DiffParseError,
    DiffMatchError,
    DiffValidationError,
)


class TestDiffModuleIntegration:
    """Test integration of parser, matcher, and applier."""

    def test_complete_diff_workflow(self, simple_applier, helpers):
        """Test complete workflow from parsing to application."""
        original_code = """def hello():
    print("Hello")
    return 0

def world():
    print("World")
    return 1
"""

        diff_text = """@@ -2,1 +2,1 @@
-    print("Hello")
+    print("Hello, World!")
@@ -6,1 +6,1 @@
-    print("World")
+    print("Beautiful World!")
"""

        document = helpers.string_to_document(original_code)
        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 2

        final_code = helpers.document_to_string(document)
        assert 'print("Hello, World!")' in final_code
        assert 'print("Beautiful World!")' in final_code

    def test_realistic_python_refactoring(self, simple_applier, helpers):
        """Test realistic Python code refactoring."""
        original = """import os
import sys

def process_data(data):
    result = []
    for item in data:
        result.append(item * 2)
    return result

def main():
    data = [1, 2, 3]
    print(process_data(data))
"""

        diff_text = """@@ -1,2 +1,3 @@
 import os
 import sys
+from typing import List
@@ -4,4 +5,4 @@
-def process_data(data):
-    result = []
-    for item in data:
-        result.append(item * 2)
+def process_data(data: List[int]) -> List[int]:
+    # Use list comprehension for better performance
+    result = [item * 2 for item in data]
     return result
"""

        document = helpers.string_to_document(original)
        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 2

        final_code = helpers.document_to_string(document)
        assert "from typing import List" in final_code
        assert "List[int]" in final_code
        assert "list comprehension" in final_code

    def test_dry_run_then_apply(self, simple_applier, helpers):
        """Test dry run validation followed by actual application."""
        document = helpers.create_simple_document([
            "line 1",
            "old line",
            "line 3"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-old line
+new line
"""

        # First, dry run to validate
        dry_result = simple_applier.apply_diff(diff_text, document, dry_run=True)
        assert dry_result.success is True
        assert "validation successful" in dry_result.message.lower()
        assert document[1] == "old line"  # Unchanged

        # Then apply for real
        apply_result = simple_applier.apply_diff(diff_text, document)
        assert apply_result.success is True
        assert document[1] == "new line"  # Changed

    def test_error_recovery_workflow(self, simple_applier, helpers):
        """Test error detection and recovery workflow."""
        document = helpers.create_simple_document([
            "line 1",
            "actual content",
            "line 3"
        ])

        # First attempt with wrong context
        bad_diff = """@@ -2,1 +2,1 @@
-wrong content
+new content
"""

        with pytest.raises(DiffMatchError) as exc_info:
            simple_applier.apply_diff(bad_diff, document)

        # Check we got useful error information
        error = exc_info.value
        assert error.error_details is not None
        assert 'best_match' in error.error_details

        # Second attempt with correct context
        good_diff = """@@ -2,1 +2,1 @@
-actual content
+new content
"""

        result = simple_applier.apply_diff(good_diff, document)
        assert result.success is True
        assert document[1] == "new content"


class TestDiffModuleRealWorldScenarios:
    """Test real-world usage scenarios."""

    def test_documentation_update(self, simple_applier, helpers):
        """Test updating documentation."""
        doc = """# Project Title

## Overview
This is the old description.

## Installation
pip install package

## Usage
See examples below.
"""

        diff_text = """@@ -3,1 +3,2 @@
 ## Overview
-This is the old description.
+This is the new and improved description.
+It now has multiple lines.
@@ -7,1 +8,2 @@
 ## Usage
+First, import the package.
 See examples below.
"""

        document = helpers.string_to_document(doc)
        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        final_doc = helpers.document_to_string(document)
        assert "new and improved description" in final_doc
        assert "First, import the package" in final_doc

    def test_config_file_update(self, simple_applier, helpers):
        """Test updating configuration file."""
        config = """[database]
host = localhost
port = 5432
user = admin

[cache]
enabled = false
ttl = 3600
"""

        diff_text = """@@ -2,2 +2,2 @@
 [database]
-host = localhost
-port = 5432
+host = prod-server.example.com
+port = 5433
@@ -6,1 +6,1 @@
 [cache]
-enabled = false
+enabled = true
"""

        document = helpers.string_to_document(config)
        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        final_config = helpers.document_to_string(document)
        assert "prod-server.example.com" in final_config
        assert "port = 5433" in final_config
        assert "enabled = true" in final_config

    def test_multi_language_code(self, simple_applier, helpers):
        """Test with multi-language content."""
        code = """# Python code with comments
def greet():
    # 打印问候语
    print("你好")  # Chinese greeting
    print("Hello")  # English greeting
    print("Bonjour")  # French greeting
"""

        diff_text = """@@ -3,2 +3,2 @@
     # 打印问候语
-    print("你好")  # Chinese greeting
-    print("Hello")  # English greeting
+    print("你好世界")  # Chinese greeting
+    print("Hello World")  # English greeting
"""

        document = helpers.string_to_document(code)
        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        final_code = helpers.document_to_string(document)
        assert "你好世界" in final_code
        assert "Hello World" in final_code

    def test_large_file_modification(self, simple_applier, helpers):
        """Test modifying a large file."""
        # Create a large document
        lines = []
        for i in range(1, 1001):
            if i % 100 == 0:
                lines.append(f"# Section {i // 100}")
            lines.append(f"line {i}")

        document = helpers.create_simple_document(lines)

        # Modify several sections
        diff_text = """@@ -199,1 +199,1 @@
-line 199
+modified line 199
@@ -500,1 +500,1 @@
-line 500
+modified line 500
@@ -999,1 +999,1 @@
-line 999
+modified line 999
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 3
        assert "modified line 199" in document
        assert "modified line 500" in document
        assert "modified line 999" in document


class TestDiffModuleEdgeCases:
    """Test edge cases in integrated scenarios."""

    def test_empty_file_to_content(self, simple_applier, helpers):
        """Test adding content to empty file."""
        document = helpers.create_simple_document([])

        diff_text = """@@ -0,0 +1,3 @@
+line 1
+line 2
+line 3
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert len(document) == 3

    def test_content_to_empty_file(self, simple_applier, helpers):
        """Test removing all content from file."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3"
        ])

        diff_text = """@@ -1,3 +1,0 @@
-line 1
-line 2
-line 3
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert len(document) == 0

    def test_single_line_file(self, simple_applier, helpers):
        """Test modifying single-line file."""
        document = helpers.create_simple_document(["single line"])

        diff_text = """@@ -1,1 +1,1 @@
-single line
+modified line
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document == ["modified line"]

    def test_many_small_changes(self, simple_applier, helpers):
        """Test many small changes throughout file."""
        document = helpers.create_simple_document(
            [f"line {i}" for i in range(1, 21)]
        )

        # Change every other line
        hunks = []
        for i in range(2, 21, 2):
            hunks.append(f"@@ -{i},1 +{i},1 @@\n-line {i}\n+modified {i}")

        diff_text = "\n".join(hunks)

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 10

        # Verify alternating pattern
        for i in range(1, 21):
            if i % 2 == 0:
                assert document[i-1] == f"modified {i}"
            else:
                assert document[i-1] == f"line {i}"

    def test_whitespace_only_changes(self, simple_applier, helpers):
        """Test changes that only affect whitespace."""
        document = helpers.create_simple_document([
            "def foo():",
            "  return 42"
        ])

        diff_text = """@@ -2,1 +2,1 @@
-  return 42
+    return 42
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert document[1] == "    return 42"


class TestDiffModuleErrorScenarios:
    """Test error scenarios in integrated context."""

    def test_conflicting_changes(self, simple_applier, helpers):
        """Test detecting conflicting changes."""
        document = helpers.create_simple_document([
            "line 1",
            "line 2",
            "line 3"
        ])

        # Overlapping hunks that conflict
        diff_text = """@@ -1,2 +1,2 @@
-line 1
-line 2
+new 1
+new 2
@@ -2,2 +2,2 @@
-line 2
-line 3
+different 2
+different 3
"""

        with pytest.raises(DiffValidationError):
            simple_applier.apply_diff(diff_text, document)

    def test_context_mismatch_multiple_hunks(self, simple_applier, helpers):
        """Test context mismatch with multiple hunks."""
        document = helpers.create_simple_document([
            "line 1",
            "wrong content",
            "line 3",
            "line 4"
        ])

        # First hunk OK, second hunk fails
        diff_text = """@@ -1,1 +1,1 @@
-line 1
+new 1
@@ -2,1 +2,1 @@
-expected content
+new content
"""

        with pytest.raises(DiffMatchError) as exc_info:
            simple_applier.apply_diff(diff_text, document)

        # Should fail on second hunk
        error = exc_info.value
        assert error.error_details['failed_hunk'] == 2

        # Document should be unchanged (atomic)
        assert document[0] == "line 1"

    def test_malformed_diff_handling(self, simple_applier, helpers):
        """Test handling of malformed diffs."""
        document = helpers.create_simple_document(["line 1"])

        malformed_diffs = [
            "@@ invalid @@\n-old\n+new",  # Invalid header
            "",  # Empty
            "not a diff at all",  # No hunks
        ]

        for bad_diff in malformed_diffs:
            with pytest.raises(DiffParseError):
                simple_applier.apply_diff(bad_diff, document)


class TestDiffModulePerformance:
    """Test performance-related scenarios."""

    def test_large_hunk(self, simple_applier, helpers):
        """Test applying large hunk."""
        # Create document with 1000 lines
        document = helpers.create_simple_document(
            [f"line {i}" for i in range(1, 1001)]
        )

        # Create large hunk modifying 100 lines
        lines = []
        for i in range(1, 101):
            lines.append(f"-line {i}")
            lines.append(f"+modified {i}")

        diff_text = f"@@ -1,100 +1,100 @@\n" + "\n".join(lines)

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        # Verify first few changes
        assert document[0] == "modified 1"
        assert document[50] == "modified 51"
        assert document[99] == "modified 100"

    def test_many_hunks(self, simple_applier, helpers):
        """Test applying many hunks."""
        # Create document with 200 lines
        document = helpers.create_simple_document(
            [f"line {i}" for i in range(1, 201)]
        )

        # Create 50 hunks
        hunks = []
        for i in range(1, 200, 4):
            hunks.append(f"@@ -{i},1 +{i},1 @@\n-line {i}\n+modified {i}")

        diff_text = "\n".join(hunks)

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert result.hunks_applied == 50

    def test_fuzzy_search_performance(self, simple_applier, helpers):
        """Test fuzzy search with large search window."""
        # Create document with repetitive content
        document = helpers.create_simple_document(
            ["common line"] * 50 + ["unique line"] + ["common line"] * 50
        )

        diff_text = """@@ -1,1 +1,1 @@
-unique line
+modified line
"""

        result = simple_applier.apply_diff(diff_text, document)

        assert result.success is True
        assert "modified line" in document


class TestDiffModulePublicAPI:
    """Test the public API of the diff module."""

    def test_import_all_public_classes(self):
        """Test that all public classes can be imported."""
        from diff import (
            DiffParser,
            DiffMatcher,
            DiffApplier,
            DiffError,
            DiffParseError,
            DiffMatchError,
            DiffApplicationError,
            DiffValidationError,
            DiffLine,
            DiffHunk,
            MatchResult,
            DiffApplicationResult,
        )

        # All should be classes or exceptions
        assert callable(DiffParser)
        assert callable(DiffMatcher)
        assert callable(DiffApplier)
        assert issubclass(DiffError, Exception)
        assert issubclass(DiffParseError, DiffError)
        assert issubclass(DiffMatchError, DiffError)
        assert issubclass(DiffApplicationError, DiffError)
        assert issubclass(DiffValidationError, DiffError)

    def test_parser_standalone_usage(self):
        """Test using parser standalone."""
        from diff import DiffParser

        parser = DiffParser()
        diff_text = """@@ -1,1 +1,1 @@
-old
+new
"""

        hunks = parser.parse(diff_text)
        assert len(hunks) == 1
        assert hunks[0].old_start == 1

    def test_matcher_standalone_usage(self, simple_matcher):
        """Test using matcher standalone."""
        from diff import DiffHunk, DiffLine

        document = ["line 1", "line 2", "line 3"]

        hunk = DiffHunk(
            old_start=2,
            old_count=1,
            new_start=2,
            new_count=1,
            lines=[
                DiffLine('-', 'line 2'),
                DiffLine('+', 'new line 2')
            ]
        )

        result = simple_matcher.find_match(hunk, document)
        assert result.success is True
        assert result.location == 2
