"""Tests for Menai module path security and mindspace integration.

Tests cover:
- Rejection of absolute paths in module names
- Rejection of relative path navigation (. and ..)
- Valid subdirectory paths
- Module path updates
- Module cache clearing on path changes
- Loading stack clearing on path changes
"""

import pytest

from menai import Menai
from menai.menai_error import MenaiModuleError, MenaiModuleNotFoundError


class TestAbsolutePathRejection:
    """Test that absolute paths are rejected in module names."""

    def test_reject_unix_absolute_path(self):
        """Test that Unix-style absolute paths are rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("/etc/passwd")

        error_msg = str(exc_info.value)
        assert "absolute" in error_msg.lower()
        assert "not allowed" in error_msg.lower()

    def test_reject_absolute_path_in_import(self, tmp_path):
        """Test that absolute paths are rejected in import expressions."""
        menai = Menai(module_path=[str(tmp_path)])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.evaluate('(import "/etc/passwd")')

        error_msg = str(exc_info.value)
        assert "absolute" in error_msg.lower()

    def test_reject_root_path(self):
        """Test that root path is rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("/")

        error_msg = str(exc_info.value)
        assert "absolute" in error_msg.lower()

    def test_reject_absolute_path_with_subdirectory(self):
        """Test that absolute paths with subdirectories are rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("/usr/local/lib/module")

        error_msg = str(exc_info.value)
        assert "absolute" in error_msg.lower()


class TestRelativePathNavigationRejection:
    """Test that relative path navigation is rejected."""

    def test_reject_current_directory_prefix(self):
        """Test that ./ prefix is rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("./module")

        error_msg = str(exc_info.value)
        assert "relative path navigation" in error_msg.lower()
        assert "not allowed" in error_msg.lower()

    def test_reject_parent_directory_prefix(self):
        """Test that ../ prefix is rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("../module")

        error_msg = str(exc_info.value)
        assert "relative path navigation" in error_msg.lower()

    def test_reject_current_directory_in_middle(self):
        """Test that ./ in middle of path is rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("lib/./utils")

        error_msg = str(exc_info.value)
        assert "relative path navigation" in error_msg.lower()

    def test_reject_parent_directory_in_middle(self):
        """Test that ../ in middle of path is rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("lib/../secrets")

        error_msg = str(exc_info.value)
        assert "relative path navigation" in error_msg.lower()

    def test_reject_multiple_parent_directories(self):
        """Test that multiple ../ are rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("../../etc/passwd")

        error_msg = str(exc_info.value)
        assert "relative path navigation" in error_msg.lower()

    def test_reject_relative_navigation_in_import(self, tmp_path):
        """Test that relative navigation is rejected in import expressions."""
        menai = Menai(module_path=[str(tmp_path)])

        test_cases = ["./module", "../module", "lib/../module", "lib/./utils"]

        for module_name in test_cases:
            with pytest.raises(MenaiModuleError) as exc_info:
                menai.evaluate(f'(import "{module_name}")')

            error_msg = str(exc_info.value)
            assert "relative path navigation" in error_msg.lower()


class TestValidPaths:
    """Test that valid module paths are accepted."""

    def test_simple_module_name(self, tmp_path):
        """Test that simple module names work."""
        module_file = tmp_path / "calendar.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(tmp_path)])

        # Should not raise
        result = menai.resolve_module("calendar")
        assert result == str(module_file)

    def test_subdirectory_path(self, tmp_path):
        """Test that subdirectory paths work."""
        lib_dir = tmp_path / "lib"
        lib_dir.mkdir()

        module_file = lib_dir / "utils.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(tmp_path)])

        # Should not raise
        result = menai.resolve_module("lib/utils")
        assert result == str(module_file)

    def test_nested_subdirectories(self, tmp_path):
        """Test that nested subdirectory paths work."""
        nested_dir = tmp_path / "lib" / "internal" / "helpers"
        nested_dir.mkdir(parents=True)

        module_file = nested_dir / "validation.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(tmp_path)])

        # Should not raise
        result = menai.resolve_module("lib/internal/helpers/validation")
        assert result == str(module_file)

    def test_module_name_with_hyphens(self, tmp_path):
        """Test that module names with hyphens work."""
        module_file = tmp_path / "my-module.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(tmp_path)])

        # Should not raise
        result = menai.resolve_module("my-module")
        assert result == str(module_file)

    def test_module_name_with_underscores(self, tmp_path):
        """Test that module names with underscores work."""
        module_file = tmp_path / "my_module.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(tmp_path)])

        # Should not raise
        result = menai.resolve_module("my_module")
        assert result == str(module_file)

    def test_import_from_subdirectory(self, tmp_path):
        """Test that importing from subdirectories works."""
        lib_dir = tmp_path / "lib"
        lib_dir.mkdir()

        module_file = lib_dir / "utils.menai"
        module_file.write_text("(dict (list \"square\" (lambda (x) (integer* x x))))")

        menai = Menai(module_path=[str(tmp_path)])

        result = menai.evaluate('''
(let ((utils (import "lib/utils")))
  ((dict-get utils "square") 5))
''')

        assert result == 25


class TestModulePathUpdate:
    """Test the set_module_path functionality."""

    def test_set_module_path_updates_path(self):
        """Test that set_module_path updates the module path."""
        menai = Menai(module_path=["/old/path"])

        menai.set_module_path(["/new/path"])

        assert menai.module_path() == ["/new/path"]

    def test_set_module_path_clears_cache(self, tmp_path):
        """Test that set_module_path clears the module cache."""
        # Create a module
        module_file = tmp_path / "test.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(tmp_path)])

        # Load module (gets cached)
        menai.evaluate('(import "test")')
        assert "test" in menai.module_cache

        # Change module path
        menai.set_module_path(["/new/path"])

        # Cache should be cleared
        assert len(menai.module_cache) == 0

    def test_set_module_path_clears_loading_stack(self):
        """Test that set_module_path clears the loading stack."""
        menai = Menai(module_path=["/old/path"])

        # Manually add to loading stack (simulating in-progress load)
        menai.loading_stack.append("module1")
        menai.loading_stack.append("module2")

        # Change module path
        menai.set_module_path(["/new/path"])

        # Loading stack should be cleared
        assert len(menai.loading_stack) == 0

    def test_set_module_path_allows_reload_from_new_location(self, tmp_path):
        """Test that modules can be reloaded from new location after path change."""
        # Create two directories with different modules
        dir1 = tmp_path / "dir1"
        dir2 = tmp_path / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        # Same module name, different content
        (dir1 / "test.menai").write_text("(dict (list \"value\" 1))")
        (dir2 / "test.menai").write_text("(dict (list \"value\" 2))")

        menai = Menai(module_path=[str(dir1)])

        # Load from first directory
        result1 = menai.evaluate('''
(let ((mod (import "test")))
  (dict-get mod "value"))
''')
        assert result1 == 1

        # Change path to second directory
        menai.set_module_path([str(dir2)])

        # Load again - should get module from second directory
        result2 = menai.evaluate('''
(let ((mod (import "test")))
  (dict-get mod "value"))
''')
        assert result2 == 2

    def test_set_module_path_with_multiple_directories(self, tmp_path):
        """Test that set_module_path works with multiple directories."""
        dir1 = tmp_path / "dir1"
        dir2 = tmp_path / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        menai = Menai(module_path=[str(dir1)])

        menai.set_module_path([str(dir1), str(dir2)])

        assert len(menai.module_path()) == 2
        assert str(dir1) in menai.module_path()
        assert str(dir2) in menai.module_path()

    def test_set_module_path_with_empty_list(self):
        """Test that set_module_path works with empty list."""
        menai = Menai(module_path=["/some/path"])

        # Should not raise
        menai.set_module_path([])

        assert menai.module_path() == []


class TestCacheClearingBehavior:
    """Test module cache clearing behavior in detail."""

    def test_clear_module_cache_removes_all_modules(self, tmp_path):
        """Test that clear_module_cache removes all cached modules."""
        # Create multiple modules
        (tmp_path / "mod1.menai").write_text("(dict (list \"x\" 1))")
        (tmp_path / "mod2.menai").write_text("(dict (list \"x\" 2))")
        (tmp_path / "mod3.menai").write_text("(dict (list \"x\" 3))")

        menai = Menai(module_path=[str(tmp_path)])

        # Load all modules
        menai.evaluate('(import "mod1")')
        menai.evaluate('(import "mod2")')
        menai.evaluate('(import "mod3")')

        assert len(menai.module_cache) == 3

        # Clear cache
        menai.clear_module_cache()

        assert len(menai.module_cache) == 0

    def test_cache_persists_across_evaluations(self, tmp_path):
        """Test that cache persists across multiple evaluations."""
        module_file = tmp_path / "persistent.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(tmp_path)])

        # First evaluation
        menai.evaluate('(import "persistent")')
        assert "persistent" in menai.module_cache

        # Second evaluation - cache should still have it
        menai.evaluate('(integer+ 1 1)')  # Unrelated evaluation
        assert "persistent" in menai.module_cache

        # Third evaluation using the module
        result = menai.evaluate('''
(let ((mod (import "persistent")))
  (dict-get mod "value"))
''')
        assert result == 42
        assert "persistent" in menai.module_cache

    def test_cache_not_shared_between_instances(self, tmp_path):
        """Test that different Menai instances don't share cache."""
        module_file = tmp_path / "test.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai1 = Menai(module_path=[str(tmp_path)])
        menai2 = Menai(module_path=[str(tmp_path)])

        # Load in first instance
        menai1.evaluate('(import "test")')

        # First instance has it cached
        assert "test" in menai1.module_cache

        # Second instance doesn't
        assert "test" not in menai2.module_cache

        # Clear first instance cache
        menai1.clear_module_cache()
        assert "test" not in menai1.module_cache

        # Second instance still doesn't have it
        assert "test" not in menai2.module_cache


class TestSecurityScenarios:
    """Test security-related scenarios."""

    def test_cannot_escape_module_path_with_absolute(self, tmp_path):
        """Test that absolute paths cannot escape module path restrictions."""
        # Create a module in the allowed directory
        allowed_dir = tmp_path / "allowed"
        allowed_dir.mkdir()
        (allowed_dir / "safe.menai").write_text("(dict (list \"x\" 1))")

        menai = Menai(module_path=[str(allowed_dir)])

        # Try to access something outside with absolute path
        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("/etc/passwd")

        assert "absolute" in str(exc_info.value).lower()

    def test_cannot_escape_module_path_with_parent_nav(self, tmp_path):
        """Test that parent directory navigation cannot escape module path."""
        # Create nested directory structure
        allowed_dir = tmp_path / "allowed"
        forbidden_dir = tmp_path / "forbidden"
        allowed_dir.mkdir()
        forbidden_dir.mkdir()

        (forbidden_dir / "secret.menai").write_text("(dict (list \"secret\" 42))")

        menai = Menai(module_path=[str(allowed_dir)])

        # Try to access forbidden directory with ../
        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("../forbidden/secret")

        assert "relative path navigation" in str(exc_info.value).lower()

    def test_subdirectory_traversal_within_module_path_is_safe(self, tmp_path):
        """Test that normal subdirectory access within module path works."""
        # Create nested structure
        base_dir = tmp_path / "modules"
        lib_dir = base_dir / "lib"
        utils_dir = lib_dir / "utils"
        utils_dir.mkdir(parents=True)

        module_file = utils_dir / "helper.menai"
        module_file.write_text("(dict (list \"value\" 42))")

        menai = Menai(module_path=[str(base_dir)])

        # This should work - it's within the module path
        result = menai.resolve_module("lib/utils/helper")
        assert result == str(module_file)

    def test_error_message_provides_helpful_suggestion(self):
        """Test that error messages provide helpful suggestions."""
        menai = Menai(module_path=["."])

        # Absolute path error
        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("/etc/passwd")

        error_msg = str(exc_info.value)
        assert "suggestion" in error_msg.lower() or "use" in error_msg.lower()

        # Relative navigation error
        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("../module")

        error_msg = str(exc_info.value)
        assert "suggestion" in error_msg.lower() or "use" in error_msg.lower()


class TestEdgeCases:
    """Test edge cases in path validation."""

    def test_empty_module_name_still_rejected(self):
        """Test that empty module names are still rejected."""
        menai = Menai(module_path=["."])

        # Empty string should fail at semantic analysis
        with pytest.raises(Exception):  # Could be MenaiEvalError or similar
            menai.evaluate('(import "")')

    def test_module_name_with_only_slashes(self):
        """Test that paths with only slashes are rejected."""
        menai = Menai(module_path=["."])

        with pytest.raises(MenaiModuleError) as exc_info:
            menai.resolve_module("/")

        assert "absolute" in str(exc_info.value).lower()

    def test_module_name_with_trailing_slash(self, tmp_path):
        """Test module names with trailing slashes."""
        # This should fail to find the module (looking for "module/.menai")
        menai = Menai(module_path=[str(tmp_path)])

        with pytest.raises(MenaiModuleNotFoundError):
            menai.resolve_module("module/")

    def test_whitespace_in_module_name(self, tmp_path):
        """Test that module names with spaces work if file exists."""
        # Create module with space in name
        module_file = tmp_path / "my module.menai"
        module_file.write_text("(dict (list \"x\" 1))")

        menai = Menai(module_path=[str(tmp_path)])

        # Should work if the file actually exists with that name
        result = menai.resolve_module("my module")
        assert result == str(module_file)

    def test_unicode_in_module_name(self, tmp_path):
        """Test that module names with unicode characters work."""
        # Create module with unicode name
        module_file = tmp_path / "日本語.menai"
        module_file.write_text("(dict (list \"x\" 1))")

        menai = Menai(module_path=[str(tmp_path)])

        # Should work
        result = menai.resolve_module("日本語")
        assert result == str(module_file)
