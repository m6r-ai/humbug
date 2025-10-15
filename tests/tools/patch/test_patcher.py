"""Integration tests for the patcher."""

import pytest
from pathlib import Path

from tools.patch.aifpl_bridge import AIFPLPatchBridge


class TestPatcherIntegration:
    """Integration tests for the complete patcher."""

    def setup_method(self):
        """Set up test fixtures."""
        self.bridge = AIFPLPatchBridge(fuzz_range=50)

        # Load fixtures
        self.fixtures_dir = Path(__file__).parent / "fixtures"

    def test_apply_example_patch(self):
        """Test applying the example patch to the example file."""
        # Read fixture files
        example_file = self.fixtures_dir / "example.py"
        example_diff = self.fixtures_dir / "example.diff"

        if not example_file.exists() or not example_diff.exists():
            pytest.skip("Fixture files not found")

        # Read source file
        with open(example_file, 'r') as f:
            content = f.read()
        file_lines = content.split('\n')
        if file_lines and file_lines[-1] == '':
            file_lines = file_lines[:-1]

        # Read diff file
        with open(example_diff, 'r') as f:
            diff_text = f.read()

        # Parse diff
        filename, hunks = self.bridge.parse_diff(diff_text)

        # Validate patch
        valid, validation_info = self.bridge.validate_patch(file_lines, hunks)
        assert valid, f"Patch validation failed: {validation_info}"

        # Apply patch
        success, result = self.bridge.apply_patch(file_lines, hunks)
        assert success, f"Patch application failed: {result}"

        # Verify result
        assert isinstance(result, list)
        assert len(result) > len(file_lines)  # Should have more lines after insertions

    def test_apply_example_patch_optimized(self):
        """Test applying the example patch using the optimized one-shot API."""
        # Read fixture files
        example_file = self.fixtures_dir / "example.py"
        example_diff = self.fixtures_dir / "example.diff"

        if not example_file.exists() or not example_diff.exists():
            pytest.skip("Fixture files not found")

        # Read source file
        with open(example_file, 'r') as f:
            content = f.read()
        file_lines = content.split('\n')
        if file_lines and file_lines[-1] == '':
            file_lines = file_lines[:-1]

        # Read diff file
        with open(example_diff, 'r') as f:
            diff_text = f.read()

        # Apply patch using optimized API
        success, result = self.bridge.parse_and_apply_patch(diff_text, file_lines)
        assert success, f"Patch application failed: {result}"

        # Verify result
        assert isinstance(result, list)
        assert len(result) > len(file_lines)  # Should have more lines after insertions

    def test_fuzzy_matching(self):
        """Test that fuzzy matching works when line numbers are off."""
        # Create a file with extra lines at the beginning
        file_lines = [
            "# Extra line 1",
            "# Extra line 2",
            "# Extra line 3",
            "",
            "def hello_world():",
            '    """Print hello world."""',
            '    print("Hello, World!")',
            "",
            "",
            "def add_numbers(a, b):",
            '    """Add two numbers."""',
            "    return a + b",
        ]

        # Create a hunk that expects to match at line 8 (0-based)
        # but will actually match at line 11 due to the extra lines
        hunk = {
            'start_line': 8,  # Expected position
            'old_count': 3,
            'new_count': 3,
            'changes': [
                ('context', 'def add_numbers(a, b):'),
                ('delete', '    """Add two numbers."""'),
                ('insert', '    """Add two numbers together."""'),
                ('context', '    return a + b'),
            ]
        }

        hunks = [hunk]

        # Validate - should find the match despite offset
        valid, validation_info = self.bridge.validate_patch(file_lines, hunks)
        assert valid, f"Fuzzy matching failed: {validation_info}"

        # Apply
        success, result = self.bridge.apply_patch(file_lines, hunks)
        assert success, f"Patch application failed: {result}"

        # Verify the change was applied
        assert '    """Add two numbers together."""' in result
        assert '    """Add two numbers."""' not in result


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
