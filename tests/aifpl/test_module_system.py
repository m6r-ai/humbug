"""Tests for the AIFPL module system.

Tests cover:
- Basic module import and usage
- Module caching
- Module search paths
- Circular dependency detection
- Module not found errors
- Nested/transitive imports
- Module with multiple exports
- Semantic validation of import expressions
"""

import pytest

from aifpl import AIFPL
from aifpl.aifpl_error import (
    AIFPLModuleNotFoundError,
    AIFPLCircularImportError,
    AIFPLEvalError
)


class TestModuleSystemBasics:
    """Test basic module loading and import functionality."""

    def test_simple_module_import(self, tmp_path):
        """Test importing and using a simple module."""
        # Create a simple module
        module_file = tmp_path / "math_utils.aifpl"
        module_file.write_text("""
(let ((square (lambda (x) (integer* x x))))
  (dict (list "square" square)))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((math (import "math_utils")))
  ((dict-get math "square") 5))
''')

        assert result == 25

    def test_module_with_multiple_exports(self, tmp_path):
        """Test module with multiple exported functions."""
        module_file = tmp_path / "utils.aifpl"
        module_file.write_text("""
(let ((add-one (lambda (x) (integer+ x 1)))
      (double (lambda (x) (integer* x 2)))
      (negate (lambda (x) (integer-neg x))))
  (dict
    (list "add-one" add-one)
    (list "double" double)
    (list "negate" negate)))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((utils (import "utils")))
  (integer+ ((dict-get utils "add-one") 10)
     ((dict-get utils "double") 5)
     ((dict-get utils "negate") 3)))
''')

        # add-one(10) = 11, double(5) = 10, negate(3) = -3
        # 11 + 10 + (-3) = 18
        assert result == 18

    def test_module_with_private_functions(self, tmp_path):
        """Test that functions not in dict are private."""
        module_file = tmp_path / "private_test.aifpl"
        module_file.write_text("""
(letrec ((helper (lambda (x) (integer* x 2)))
         (public-fn (lambda (x) (helper x))))
  (dict
    (list "public" public-fn)
    (list "also-public" (lambda (x) (integer+ x 1)))))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Can call public function
        result = aifpl.evaluate('''
(let ((mod (import "private_test")))
  ((dict-get mod "public") 5))
''')
        assert result == 10

        # Cannot access helper directly (it's not in the dict)
        result = aifpl.evaluate('''
(let ((mod (import "private_test")))
  (dict-has? mod "helper"))
''')
        assert result is False


class TestModuleCaching:
    """Test module caching behavior."""

    def test_module_cached_on_first_load(self, tmp_path):
        """Test that modules are cached after first load."""
        module_file = tmp_path / "cached.aifpl"
        module_file.write_text("""
(dict (list "value" 42))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        # Import twice
        aifpl.evaluate('(import "cached")')

        # Check cache
        assert "cached" in aifpl.module_cache

    def test_same_module_imported_multiple_times(self, tmp_path):
        """Test that importing same module multiple times uses cache."""
        module_file = tmp_path / "multi.aifpl"
        module_file.write_text("""
(let ((fn (lambda (x) (integer* x x))))
  (dict (list "square" fn)))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((m1 (import "multi"))
      (m2 (import "multi")))
  (integer+ ((dict-get m1 "square") 3)
     ((dict-get m2 "square") 4)))
''')

        # Should work correctly: 9 + 16 = 25
        assert result == 25

        # Module should only be in cache once
        assert "multi" in aifpl.module_cache

    def test_clear_module_cache(self, tmp_path):
        """Test clearing the module cache."""
        module_file = tmp_path / "clearable.aifpl"
        module_file.write_text("""
(dict (list "x" 1))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        aifpl.evaluate('(import "clearable")')
        assert "clearable" in aifpl.module_cache

        aifpl.clear_module_cache()
        assert "clearable" not in aifpl.module_cache


class TestModuleSearchPath:
    """Test module search path resolution."""

    def test_single_directory_search_path(self, tmp_path):
        """Test module resolution with single directory."""
        module_file = tmp_path / "single.aifpl"
        module_file.write_text('(dict (list "val" 1))')

        aifpl = AIFPL(module_path=[str(tmp_path)])
        result = aifpl.evaluate('(import "single")')

        # Should successfully load
        assert result is not None

    def test_multiple_directory_search_path(self, tmp_path):
        """Test module resolution with multiple directories."""
        dir1 = tmp_path / "dir1"
        dir2 = tmp_path / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        # Module in first directory
        (dir1 / "first.aifpl").write_text('(dict (list "val" 1))')

        # Module in second directory
        (dir2 / "second.aifpl").write_text('(dict (list "val" 2))')

        aifpl = AIFPL(module_path=[str(dir1), str(dir2)])

        # Can import from both
        aifpl.evaluate('(import "first")')
        aifpl.evaluate('(import "second")')

    def test_first_match_wins_in_search_path(self, tmp_path):
        """Test that first matching module in search path is used."""
        dir1 = tmp_path / "dir1"
        dir2 = tmp_path / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        # Same module name in both directories with different values
        (dir1 / "duplicate.aifpl").write_text('(dict (list "val" 1))')
        (dir2 / "duplicate.aifpl").write_text('(dict (list "val" 2))')

        # dir1 is first in search path
        aifpl = AIFPL(module_path=[str(dir1), str(dir2)])

        result = aifpl.evaluate('''
(let ((mod (import "duplicate")))
  (dict-get mod "val"))
''')

        # Should get value from dir1 (list-first in search path)
        assert result == 1

    def test_subdirectory_modules(self, tmp_path):
        """Test importing modules from subdirectories."""
        subdir = tmp_path / "lib"
        subdir.mkdir()

        (subdir / "helper.aifpl").write_text('(dict (list "val" 42))')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((mod (import "lib/helper")))
  (dict-get mod "val"))
''')

        assert result == 42


class TestModuleErrors:
    """Test error handling in module system."""

    def test_module_not_found_error(self, tmp_path):
        """Test error when module file doesn't exist."""
        aifpl = AIFPL(module_path=[str(tmp_path)])

        with pytest.raises(AIFPLModuleNotFoundError) as exc_info:
            aifpl.evaluate('(import "nonexistent")')

        error_msg = str(exc_info.value)
        assert "not found" in error_msg.lower()
        assert "nonexistent" in error_msg

    def test_module_not_found_shows_search_paths(self, tmp_path):
        """Test that module not found error shows searched paths."""
        dir1 = tmp_path / "dir1"
        dir2 = tmp_path / "dir2"
        dir1.mkdir()
        dir2.mkdir()

        aifpl = AIFPL(module_path=[str(dir1), str(dir2)])

        with pytest.raises(AIFPLModuleNotFoundError) as exc_info:
            aifpl.evaluate('(import "missing")')

        error_msg = str(exc_info.value)
        assert str(dir1) in error_msg or "dir1" in error_msg
        assert str(dir2) in error_msg or "dir2" in error_msg

    def test_import_with_wrong_number_of_arguments(self):
        """Test that import requires exactly one argument."""
        aifpl = AIFPL()

        # No arguments
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(import)')
        assert "wrong number of arguments" in str(exc_info.value).lower()

        # Too many arguments
        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(import "mod1" "mod2")')
        assert "wrong number of arguments" in str(exc_info.value).lower()

    def test_import_requires_string_literal(self):
        """Test that import requires a string literal, not a variable."""
        aifpl = AIFPL()

        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(import 42)')
        assert "string literal" in str(exc_info.value).lower()

    def test_import_empty_string_error(self):
        """Test that import rejects empty module names."""
        aifpl = AIFPL()

        with pytest.raises(AIFPLEvalError) as exc_info:
            aifpl.evaluate('(import "")')
        assert "empty" in str(exc_info.value).lower()

    def test_module_with_syntax_error(self, tmp_path):
        """Test error when module has syntax errors."""
        module_file = tmp_path / "broken.aifpl"
        module_file.write_text('(this is not valid AIFPL')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        with pytest.raises(Exception):  # Will be a parse error
            aifpl.evaluate('(import "broken")')


class TestCircularImports:
    """Test circular dependency detection."""

    def test_direct_circular_import(self, tmp_path):
        """Test detection of direct circular dependency (A imports B imports A)."""
        (tmp_path / "module_a.aifpl").write_text('(import "module_b")')
        (tmp_path / "module_b.aifpl").write_text('(import "module_a")')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        with pytest.raises(AIFPLCircularImportError) as exc_info:
            aifpl.evaluate('(import "module_a")')

        error_msg = str(exc_info.value)
        assert "circular" in error_msg.lower()
        assert "module_a" in error_msg
        assert "module_b" in error_msg

    def test_circular_import_shows_chain(self, tmp_path):
        """Test that circular import error shows the import chain."""
        (tmp_path / "a.aifpl").write_text('(import "b")')
        (tmp_path / "b.aifpl").write_text('(import "a")')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        with pytest.raises(AIFPLCircularImportError) as exc_info:
            aifpl.evaluate('(import "a")')

        error_msg = str(exc_info.value)
        # Should show the chain: a -> b -> a
        assert "a" in error_msg
        assert "b" in error_msg
        assert "->" in error_msg or "chain" in error_msg.lower()

    def test_three_way_circular_import(self, tmp_path):
        """Test detection of three-way circular dependency (A -> B -> C -> A)."""
        (tmp_path / "x.aifpl").write_text('(import "y")')
        (tmp_path / "y.aifpl").write_text('(import "z")')
        (tmp_path / "z.aifpl").write_text('(import "x")')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        with pytest.raises(AIFPLCircularImportError) as exc_info:
            aifpl.evaluate('(import "x")')

        error_msg = str(exc_info.value)
        assert "circular" in error_msg.lower()

    def test_self_import(self, tmp_path):
        """Test detection of module importing itself."""
        (tmp_path / "self.aifpl").write_text('(import "self")')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        with pytest.raises(AIFPLCircularImportError) as exc_info:
            aifpl.evaluate('(import "self")')

        error_msg = str(exc_info.value)
        assert "circular" in error_msg.lower()


class TestTransitiveImports:
    """Test modules that import other modules (nested/transitive imports)."""

    def test_two_level_import(self, tmp_path):
        """Test module that imports another module."""
        # Base module
        (tmp_path / "base.aifpl").write_text("""
(let ((add (lambda (x y) (integer+ x y))))
  (dict (list "add" add)))
""")

        # Module that uses base
        (tmp_path / "wrapper.aifpl").write_text("""
(let ((base (import "base")))
  (let ((add-ten (lambda (x) ((dict-get base "add") x 10))))
    (dict (list "add-ten" add-ten))))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((w (import "wrapper")))
  ((dict-get w "add-ten") 5))
''')

        assert result == 15

    def test_three_level_import_chain(self, tmp_path):
        """Test three-level import chain (A imports B imports C)."""
        # Level 3 (deepest)
        (tmp_path / "level3.aifpl").write_text("""
(dict (list "value" 1))
""")

        # Level 2
        (tmp_path / "level2.aifpl").write_text("""
(let ((l3 (import "level3")))
  (dict (list "get-value" (lambda () (dict-get l3 "value")))))
""")

        # Level 1
        (tmp_path / "level1.aifpl").write_text("""
(let ((l2 (import "level2")))
  (dict (list "get-nested" (lambda () ((dict-get l2 "get-value"))))))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((l1 (import "level1")))
  ((dict-get l1 "get-nested")))
''')

        assert result == 1

    def test_diamond_dependency(self, tmp_path):
        """Test diamond dependency pattern (A imports B and C, both import D)."""
        # Base module (D)
        (tmp_path / "base.aifpl").write_text("""
(dict (list "value" 10))
""")

        # B imports base
        (tmp_path / "left.aifpl").write_text("""
(let ((base (import "base")))
  (dict (list "get-left" (lambda () (dict-get base "value")))))
""")

        # C imports base
        (tmp_path / "right.aifpl").write_text("""
(let ((base (import "base")))
  (dict (list "get-right" (lambda () (dict-get base "value")))))
""")

        # A imports both B and C
        (tmp_path / "top.aifpl").write_text("""
(let ((left (import "left"))
      (right (import "right")))
  (dict
    (list "sum" (lambda ()
      (integer+ ((dict-get left "get-left"))
         ((dict-get right "get-right")))))))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((top (import "top")))
  ((dict-get top "sum")))
''')

        # Should get 10 + 10 = 20 (base module cached and reused)
        assert result == 20


class TestModuleCompilation:
    """Test module system integration with compilation pipeline."""

    def test_module_with_let_bindings(self, tmp_path):
        """Test module using let bindings."""
        (tmp_path / "let_test.aifpl").write_text("""
(let ((x 10)
      (y 20))
  (let ((sum (lambda () (integer+ x y))))
    (dict (list "sum" sum))))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((mod (import "let_test")))
  ((dict-get mod "sum")))
''')

        assert result == 30

    def test_module_with_letrec(self, tmp_path):
        """Test module using letrec for recursion."""
        (tmp_path / "recursive.aifpl").write_text("""
(letrec ((factorial (lambda (n)
                      (if (integer<=? n 1)
                          1
                          (integer* n (factorial (integer- n 1)))))))
  (dict (list "factorial" factorial)))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((mod (import "recursive")))
  ((dict-get mod "factorial") 5))
''')

        assert result == 120

    def test_module_with_conditionals(self, tmp_path):
        """Test module using if expressions."""
        (tmp_path / "cond_test.aifpl").write_text("""
(let ((abs-val (lambda (x)
                 (if (integer<? x 0)
                     (integer-neg x)
                     x))))
  (dict (list "abs" abs-val)))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((mod (import "cond_test")))
  ((dict-get mod "abs") -42))
''')

        assert result == 42

    def test_module_with_higher_order_functions(self, tmp_path):
        """Test module using map, filter, fold."""
        (tmp_path / "hof.aifpl").write_text("""
(let ((sum-squares (lambda (lst)
                     (list-fold integer+ 0 (list-map (lambda (x) (integer* x x)) lst)))))
  (dict (list "sum-squares" sum-squares)))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((mod (import "hof")))
  ((dict-get mod "sum-squares") (list 1 2 3 4)))
''')

        # 1^2 + 2^2 + 3^2 + 4^2 = 1 + 4 + 9 + 16 = 30
        assert result == 30


class TestModuleEdgeCases:
    """Test edge cases and unusual module scenarios."""

    def test_empty_module(self, tmp_path):
        """Test module that exports empty dict."""
        (tmp_path / "empty.aifpl").write_text('(dict)')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((mod (import "empty")))
  (dict-length mod))
''')

        assert result == 0

    def test_module_returning_non_dict(self, tmp_path):
        """Test that modules can return any value (boolean-not just dicts)."""
        (tmp_path / "number.aifpl").write_text('42')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('(import "number")')

        assert result == 42

    def test_module_with_complex_data_structures(self, tmp_path):
        """Test module with nested dicts and lists."""
        (tmp_path / "complex.aifpl").write_text("""
(dict
  (list "data" (list 1 2 3))
  (list "nested" (dict (list "inner" 42))))
""")

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('''
(let ((mod (import "complex")))
  (let ((data (dict-get mod "data"))
        (nested (dict-get mod "nested")))
    (integer+ (list-first data)
       (dict-get nested "inner"))))
''')

        # 1 + 42 = 43
        assert result == 43

    def test_multiple_aifpl_instances_separate_caches(self, tmp_path):
        """Test that different AIFPL instances have separate module caches."""
        (tmp_path / "test.aifpl").write_text('(dict (list "val" 1))')

        aifpl1 = AIFPL(module_path=[str(tmp_path)])
        aifpl2 = AIFPL(module_path=[str(tmp_path)])

        aifpl1.evaluate('(import "test")')

        # aifpl1 has it cached
        assert "test" in aifpl1.module_cache

        # aifpl2 doesn't
        assert "test" not in aifpl2.module_cache

    def test_module_name_with_special_characters(self, tmp_path):
        """Test module names with underscores and hyphens."""
        (tmp_path / "my_module-v2.aifpl").write_text('(dict (list "x" 1))')

        aifpl = AIFPL(module_path=[str(tmp_path)])

        result = aifpl.evaluate('(import "my_module-v2")')

        assert result is not None
