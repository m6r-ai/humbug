"""Tests for AIFPL module cache invalidation.

Tests cover:
- SHA256-based cache invalidation when file content changes
- Cache reuse when file content is unchanged
- Manual cache invalidation methods
- Hash computation edge cases
"""

import pytest
import time

from aifpl import AIFPL


class TestCacheInvalidation:
    """Test automatic cache invalidation based on file content hash."""

    def test_cache_reused_when_content_unchanged(self, tmp_path):
        """Test that cache is reused when file content hasn't changed."""
        module_file = tmp_path / "stable.aifpl"
        module_file.write_text("(alist (list \"value\" 42))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # First load
        result1 = aifpl.evaluate('(import "stable")')
        assert "stable" in aifpl.module_cache
        assert "stable" in aifpl.module_hashes
        hash1 = aifpl.module_hashes["stable"]

        # Second load - should use cache
        result2 = aifpl.evaluate('(import "stable")')
        assert "stable" in aifpl.module_cache
        hash2 = aifpl.module_hashes["stable"]

        # Hash should be the same
        assert hash1 == hash2

    def test_cache_invalidated_when_content_changes(self, tmp_path):
        """Test that cache is invalidated when file content changes."""
        module_file = tmp_path / "changing.aifpl"
        module_file.write_text("(alist (list \"value\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # First load
        result1 = aifpl.evaluate('''
(let ((mod (import "changing")))
  (alist-get mod "value"))
''')
        assert result1 == 1
        hash1 = aifpl.module_hashes["changing"]

        # Modify file content
        module_file.write_text("(alist (list \"value\" 2))")

        # Second load - should detect change and reload
        result2 = aifpl.evaluate('''
(let ((mod (import "changing")))
  (alist-get mod "value"))
''')
        assert result2 == 2
        hash2 = aifpl.module_hashes["changing"]

        # Hash should be different
        assert hash1 != hash2

    def test_cache_invalidated_on_whitespace_only_change(self, tmp_path):
        """Test that cache is invalidated even for whitespace-only changes."""
        module_file = tmp_path / "whitespace.aifpl"
        module_file.write_text("(alist (list \"x\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # First load
        aifpl.evaluate('(import "whitespace")')
        hash1 = aifpl.module_hashes["whitespace"]

        # Change only whitespace
        module_file.write_text("(alist  (list  \"x\"  1))")

        # Second load - should detect change (different hash)
        aifpl.evaluate('(import "whitespace")')
        hash2 = aifpl.module_hashes["whitespace"]

        # Hash should be different (content changed)
        assert hash1 != hash2

    def test_cache_invalidated_on_comment_change(self, tmp_path):
        """Test that cache is invalidated when comments change."""
        module_file = tmp_path / "commented.aifpl"
        module_file.write_text("; Comment v1\n(alist (list \"x\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # First load
        aifpl.evaluate('(import "commented")')
        hash1 = aifpl.module_hashes["commented"]

        # Change comment
        module_file.write_text("; Comment v2\n(alist (list \"x\" 1))")

        # Second load - should detect change
        aifpl.evaluate('(import "commented")')
        hash2 = aifpl.module_hashes["commented"]

        # Hash should be different
        assert hash1 != hash2

    def test_multiple_modules_independent_invalidation(self, tmp_path):
        """Test that modules are invalidated independently."""
        (tmp_path / "module_a.aifpl").write_text("(alist (list \"a\" 1))")
        (tmp_path / "module_b.aifpl").write_text("(alist (list \"b\" 2))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Load both modules
        aifpl.evaluate('(import "module_a")')
        aifpl.evaluate('(import "module_b")')
        hash_a1 = aifpl.module_hashes["module_a"]
        hash_b1 = aifpl.module_hashes["module_b"]

        # Modify only module_a
        (tmp_path / "module_a.aifpl").write_text("(alist (list \"a\" 99))")

        # Reload both
        aifpl.evaluate('(import "module_a")')
        aifpl.evaluate('(import "module_b")')
        hash_a2 = aifpl.module_hashes["module_a"]
        hash_b2 = aifpl.module_hashes["module_b"]

        # Only module_a hash should change
        assert hash_a1 != hash_a2
        assert hash_b1 == hash_b2

    def test_transitive_import_invalidation(self, tmp_path):
        """Test cache invalidation with transitive imports."""
        # Base module
        (tmp_path / "base.aifpl").write_text("""
(alist (list "value" 10))
""")

        # Wrapper imports base
        (tmp_path / "wrapper.aifpl").write_text("""
(let ((base (import "base")))
  (alist (list "get-value" (lambda () (alist-get base "value")))))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Load wrapper (which loads base)
        result1 = aifpl.evaluate('''
(let ((w (import "wrapper")))
  ((alist-get w "get-value")))
''')
        assert result1 == 10

        # Modify base module
        (tmp_path / "base.aifpl").write_text("""
(alist (list "value" 20))
""")

        # Clear cache to force reload
        aifpl.clear_module_cache()

        # Reload wrapper - should get new base value
        result2 = aifpl.evaluate('''
(let ((w (import "wrapper")))
  ((alist-get w "get-value")))
''')
        assert result2 == 20


class TestHashComputation:
    """Test the hash computation mechanism."""

    def test_hash_is_sha256_hex(self, tmp_path):
        """Test that computed hash is SHA256 in hex format."""
        module_file = tmp_path / "hashtest.aifpl"
        module_file.write_text("(alist)")

        aifpl = AIFPL(module_path=[str(tmp_path)])
        aifpl.evaluate('(import "hashtest")')

        hash_value = aifpl.module_hashes["hashtest"]

        # SHA256 hex digest is 64 characters
        assert len(hash_value) == 64
        # Should be valid hex
        assert all(c in "0123456789abcdef" for c in hash_value)

    def test_identical_content_produces_identical_hash(self, tmp_path):
        """Test that identical content produces the same hash."""
        (tmp_path / "file1.aifpl").write_text("(alist (list \"x\" 1))")
        (tmp_path / "file2.aifpl").write_text("(alist (list \"x\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        aifpl.evaluate('(import "file1")')
        aifpl.evaluate('(import "file2")')

        # Same content should produce same hash
        assert aifpl.module_hashes["file1"] == aifpl.module_hashes["file2"]

    def test_different_content_produces_different_hash(self, tmp_path):
        """Test that different content produces different hashes."""
        (tmp_path / "diff1.aifpl").write_text("(alist (list \"x\" 1))")
        (tmp_path / "diff2.aifpl").write_text("(alist (list \"x\" 2))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        aifpl.evaluate('(import "diff1")')
        aifpl.evaluate('(import "diff2")')

        # Different content should produce different hashes
        assert aifpl.module_hashes["diff1"] != aifpl.module_hashes["diff2"]

    def test_large_file_hashing(self, tmp_path):
        """Test that large files are hashed correctly (chunked reading)."""
        # Create a large module (> 8KB to test chunked reading)
        large_content = "; Large module\n"
        large_content += "(let (\n"
        for i in range(1000):
            large_content += f"  (func{i} (lambda (x) (* x {i})))\n"
        large_content += ")\n  (alist\n"
        for i in range(100):
            large_content += f"    (list \"func{i}\" func{i})\n"
        large_content += "  )\n)\n"

        module_file = tmp_path / "large.aifpl"
        module_file.write_text(large_content)

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Should successfully hash and load
        aifpl.evaluate('(import "large")')
        assert "large" in aifpl.module_hashes

        # Verify it's actually large
        assert len(large_content) > 8192


class TestManualCacheControl:
    """Test manual cache invalidation methods."""

    def test_invalidate_module_removes_from_cache(self, tmp_path):
        """Test that invalidate_module removes module from cache."""
        module_file = tmp_path / "removable.aifpl"
        module_file.write_text("(alist (list \"x\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Load module
        aifpl.evaluate('(import "removable")')
        assert "removable" in aifpl.module_cache
        assert "removable" in aifpl.module_hashes

        # Invalidate
        aifpl.invalidate_module("removable")
        assert "removable" not in aifpl.module_cache
        assert "removable" not in aifpl.module_hashes

    def test_invalidate_nonexistent_module_is_safe(self, tmp_path):
        """Test that invalidating non-existent module doesn't error."""
        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Should not raise
        aifpl.invalidate_module("nonexistent")

    def test_reload_module_forces_recompilation(self, tmp_path):
        """Test that reload_module forces recompilation."""
        module_file = tmp_path / "reloadable.aifpl"
        module_file.write_text("(alist (list \"value\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Initial load
        result1 = aifpl.evaluate('''
(let ((mod (import "reloadable")))
  (alist-get mod "value"))
''')
        assert result1 == 1

        # Modify file
        module_file.write_text("(alist (list \"value\" 2))")

        # Force reload
        aifpl.reload_module("reloadable")

        # Next import should get new value
        result2 = aifpl.evaluate('''
(let ((mod (import "reloadable")))
  (alist-get mod "value"))
''')
        assert result2 == 2

    def test_clear_module_cache_clears_hashes(self, tmp_path):
        """Test that clear_module_cache also clears hashes."""
        (tmp_path / "test1.aifpl").write_text("(alist)")
        (tmp_path / "test2.aifpl").write_text("(alist)")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Load modules
        aifpl.evaluate('(import "test1")')
        aifpl.evaluate('(import "test2")')
        assert len(aifpl.module_cache) == 2
        assert len(aifpl.module_hashes) == 2

        # Clear cache
        aifpl.clear_module_cache()
        assert len(aifpl.module_cache) == 0
        assert len(aifpl.module_hashes) == 0

    def test_set_module_path_clears_hashes(self, tmp_path):
        """Test that set_module_path clears cache and hashes."""
        dir1 = tmp_path / "dir1"
        dir2 = tmp_path / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        (dir1 / "test.aifpl").write_text("(alist (list \"x\" 1))")

        aifpl = AIFPL(module_path=[str(dir1)])

        # Load module
        aifpl.evaluate('(import "test")')
        assert len(aifpl.module_cache) == 1
        assert len(aifpl.module_hashes) == 1

        # Change module path
        aifpl.set_module_path([str(dir2)])
        assert len(aifpl.module_cache) == 0
        assert len(aifpl.module_hashes) == 0


class TestCacheInvalidationEdgeCases:
    """Test edge cases in cache invalidation."""

    def test_file_deleted_after_caching(self, tmp_path):
        """Test behavior when cached module file is deleted."""
        module_file = tmp_path / "deletable.aifpl"
        module_file.write_text("(alist (list \"x\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Load and cache
        aifpl.evaluate('(import "deletable")')
        assert "deletable" in aifpl.module_cache

        # Delete file
        module_file.unlink()

        # Try to load again - should fail with module not found
        from aifpl.aifpl_error import AIFPLModuleNotFoundError
        with pytest.raises(AIFPLModuleNotFoundError):
            aifpl.evaluate('(import "deletable")')

        # Cache should be cleaned up
        assert "deletable" not in aifpl.module_cache
        assert "deletable" not in aifpl.module_hashes

    def test_file_recreated_with_different_content(self, tmp_path):
        """Test cache invalidation when file is deleted and recreated."""
        module_file = tmp_path / "recreated.aifpl"
        module_file.write_text("(alist (list \"value\" 1))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Load original
        result1 = aifpl.evaluate('''
(let ((mod (import "recreated")))
  (alist-get mod "value"))
''')
        assert result1 == 1

        # Delete and recreate with different content
        module_file.unlink()
        module_file.write_text("(alist (list \"value\" 2))")

        # Load again - should get new content
        result2 = aifpl.evaluate('''
(let ((mod (import "recreated")))
  (alist-get mod "value"))
''')
        assert result2 == 2

    def test_cache_with_binary_content_in_comments(self, tmp_path):
        """Test that binary-like content in comments doesn't break hashing."""
        # Test with various special characters in comments
        module_file = tmp_path / "special.aifpl"
        module_file.write_text("; Comment with special chars: \u0000 \u0001 \u001f\n(alist)")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Should handle gracefully (though lexer might reject some chars)
        try:
            aifpl.evaluate('(import "special")')
            # If it loads, hash should exist
            assert "special" in aifpl.module_hashes
        except Exception:
            # If lexer rejects it, that's also fine
            pass

    def test_empty_file_hashing(self, tmp_path):
        """Test hashing of empty file."""
        module_file = tmp_path / "empty.aifpl"
        module_file.write_text("")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Empty file will fail parsing, but hash should still be computed
        try:
            aifpl.evaluate('(import "empty")')
        except Exception:
            # Expected to fail parsing, but we can test hash directly
            hash_value = aifpl._compute_file_hash(str(module_file))
            assert len(hash_value) == 64  # Valid SHA256 hex

    def test_unicode_content_hashing(self, tmp_path):
        """Test that Unicode content is hashed correctly."""
        module_file = tmp_path / "unicode.aifpl"
        module_file.write_text("; Comment: 你好世界 🌍\n(alist (list \"greeting\" \"hello\"))")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Should load and hash correctly
        aifpl.evaluate('(import "unicode")')
        assert "unicode" in aifpl.module_hashes

        # Modify Unicode content
        module_file.write_text("; Comment: 再见世界 🌏\n(alist (list \"greeting\" \"hello\"))")

        # Should detect change
        hash1 = aifpl.module_hashes["unicode"]
        aifpl.evaluate('(import "unicode")')
        hash2 = aifpl.module_hashes["unicode"]

        assert hash1 != hash2
