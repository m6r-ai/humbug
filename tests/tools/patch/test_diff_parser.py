"""Tests for unified diff parser."""

import pytest
from pathlib import Path

from tools.patch.diff_parser import UnifiedDiffParser


class TestUnifiedDiffParser:
    """Test cases for UnifiedDiffParser."""

    def setup_method(self):
        """Set up test fixtures."""
        self.parser = UnifiedDiffParser()

    def test_simple_diff(self):
        """Test parsing a simple unified diff."""
        diff_text = """--- a/test.py
+++ b/test.py
@@ -1,5 +1,5 @@
 line 1
 line 2
-line 3
+line 3 modified
 line 4
 line 5
"""

        filename, hunks = self.parser.parse(diff_text)

        assert filename == "test.py"
        assert len(hunks) == 1

        hunk = hunks[0]
        assert hunk['start_line'] == 0  # 0-based
        assert hunk['old_count'] == 5
        assert hunk['new_count'] == 5
        assert len(hunk['changes']) == 6

        # Check changes
        changes = hunk['changes']
        assert changes[0] == ('context', 'line 1')
        assert changes[1] == ('context', 'line 2')
        assert changes[2] == ('delete', 'line 3')
        assert changes[3] == ('insert', 'line 3 modified')
        assert changes[4] == ('context', 'line 4')
        assert changes[5] == ('context', 'line 5')

    def test_multiple_hunks(self):
        """Test parsing multiple hunks."""
        diff_text = """--- a/test.py
+++ b/test.py
@@ -1,3 +1,3 @@
 line 1
-line 2
+line 2 modified
 line 3
@@ -10,3 +10,3 @@
 line 10
-line 11
+line 11 modified
 line 12
"""

        filename, hunks = self.parser.parse(diff_text)

        assert filename == "test.py"
        assert len(hunks) == 2

        # First hunk
        assert hunks[0]['start_line'] == 0
        assert len(hunks[0]['changes']) == 4

        # Second hunk
        assert hunks[1]['start_line'] == 9
        assert len(hunks[1]['changes']) == 4

    def test_fixture_file(self):
        """Test parsing the example fixture file."""
        fixture_path = Path(__file__).parent / "fixtures" / "example.diff"

        if fixture_path.exists():
            filename, hunks = self.parser.parse_file(str(fixture_path))

            assert filename == "test_example.py"
            assert len(hunks) == 2

            # Verify first hunk structure
            assert hunks[0]['start_line'] == 6
            assert hunks[0]['old_count'] == 7
            assert hunks[0]['new_count'] == 7

            # Verify second hunk structure
            assert hunks[1]['start_line'] == 17
            assert hunks[1]['old_count'] == 6
            assert hunks[1]['new_count'] == 10


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
