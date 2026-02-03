"""Main AIFPL (AI Functional Programming Language) class with enhanced error messages."""

import hashlib
import math
from pathlib import Path
import os
from typing import Union, Dict, List

from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_value import AIFPLFunction, AIFPLFloat, AIFPLBoolean, AIFPLValue
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_error import AIFPLModuleNotFoundError, AIFPLModuleError


class AIFPL:
    """
    AIFPL (AI Functional Programming Language) calculator with LISP-like syntax and enhanced error messages.

    This version provides comprehensive error reporting with:
    - Clear explanations of what went wrong
    - Context showing the problematic input
    - Suggestions for how to fix the problem
    - Examples of correct usage
    - Position information where helpful

    Designed specifically to help LLMs understand and self-correct errors.

    Execution Model:
    - Uses bytecode ir_builder and VM for all evaluation
    - Tail-call optimized for recursive functions
    - High performance through bytecode compilation and optimized VM
    """

    # AIFPL implementations of higher-order functions
    _PRELUDE_SOURCE = {
        'map': """(lambda (f lst)
                    (letrec ((helper (lambda (f lst acc)
                                       (if (null? lst) (reverse acc)
                                           (helper f (rest lst) (cons (f (first lst)) acc))))))
                    (helper f lst (list))))""",
        'filter': """(lambda (pred lst)
                    (letrec ((helper (lambda (pred lst acc)
                                       (if (null? lst) (reverse acc)
                                           (if (pred (first lst))
                                               (helper pred (rest lst) (cons (first lst) acc))
                                               (helper pred (rest lst) acc))))))
                        (helper pred lst (list))))""",
        'fold': """(lambda (f init lst)
                    (letrec ((helper (lambda (f acc lst)
                                       (if (null? lst) acc
                                           (helper f (f acc (first lst)) (rest lst))))))
                    (helper f init lst)))""",
        'find': """(lambda (pred lst)
                    (letrec ((find (lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) (first lst) (find pred (rest lst)))))))
                    (find pred lst)))""",
        'any?': """(lambda (pred lst)
                    (letrec ((any? (lambda (pred lst) (if (null? lst) #f (if (pred (first lst)) #t (any? pred (rest lst)))))))
                    (any? pred lst)))""",
        'all?': """(lambda (pred lst)
                    (letrec ((all? (lambda (pred lst) (if (null? lst) #t (if (pred (first lst)) (all? pred (rest lst)) #f)))))
                    (all? pred lst)))""",
    }

    # Mathematical constants
    CONSTANTS = {
        'pi': AIFPLFloat(math.pi),
        'e': AIFPLFloat(math.e),
        'true': AIFPLBoolean(True),
        'false': AIFPLBoolean(False),
    }

    # Class-level cache for prelude functions
    _prelude_cache = None

    @classmethod
    def _load_prelude(
        cls,
        compiler: AIFPLCompiler,
        vm: AIFPLVM
    ) -> Dict[str, AIFPLFunction]:
        """Load prelude as bytecode AIFPLFunction objects (cached)."""
        if cls._prelude_cache is not None:
            return cls._prelude_cache

        bytecode_prelude: dict[str, AIFPLFunction] = {}
        for name, source_code in cls._PRELUDE_SOURCE.items():
            bytecode = compiler.compile(source_code, name=f"<prelude:{name}>")
            func = vm.execute(bytecode, cls.CONSTANTS, {})
            if isinstance(func, AIFPLFunction):
                bytecode_prelude[name] = func

        cls._prelude_cache = bytecode_prelude
        return bytecode_prelude

    def __init__(self, max_depth: int = 1000, module_path: List[str] | None = None):
        """
        Initialize AIFPL calculator.

        Args:
            max_depth: Maximum recursion depth (kept for compatibility, may be removed in future)
                      Note: VM uses tail-call optimization, so deep recursion is supported
            module_path: List of directories to search for modules (default: ["."])
        """
        self.max_depth = max_depth
        self.module_path = module_path or ["."]

        # Module system state
        self.module_cache: Dict[str, AIFPLValue] = {}  # module_name -> alist
        self.module_hashes: Dict[str, str] = {}  # module_name -> sha256 hex digest
        self.loading_stack: List[str] = []  # Track currently-loading modules for circular detection

        # Compiler and VM
        self.compiler = AIFPLCompiler(module_loader=self)
        self.vm = AIFPLVM()

        # Load prelude once at initialization
        self._prelude = self._load_prelude(self.compiler, self.vm)

    def _evaluate_raw(self, expression: str) -> 'AIFPLValue':
        """
        Evaluate an AIFPL expression without error handling.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression as AIFPLValue
        """
        # Compile (lexing, parsing, semantic analysis, IR building, code generation)
        code = self.compiler.compile(expression)

        # Execute
        result = self.vm.execute(code, self.CONSTANTS, self._prelude)

        return result

    def evaluate(self, expression: str) -> Union[int, float, complex, str, bool, list, AIFPLFunction]:
        """
        Evaluate an AIFPL expression with comprehensive enhanced error reporting.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            The result of evaluating the expression converted to Python types

        Raises:
            AIFPLTokenError: If tokenization fails (with detailed context and suggestions)
            AIFPLParseError: If parsing fails (with detailed context and suggestions)
            AIFPLEvalError: If evaluation fails (with detailed context and suggestions)
        """
        result = self._evaluate_raw(expression)
        return result.to_python()

    def evaluate_and_format(self, expression: str) -> str:
        """
        Evaluate an AIFPL expression and return formatted result with comprehensive enhanced error reporting.

        Args:
            expression: AIFPL expression string to evaluate

        Returns:
            String representation of the result using LISP conventions

        Raises:
            AIFPLTokenError: If tokenization fails (with detailed context and suggestions)
            AIFPLParseError: If parsing fails (with detailed context and suggestions)
            AIFPLEvalError: If evaluation fails (with detailed context and suggestions)
        """
        result = self._evaluate_raw(expression)
        return result.describe()

    # Module System Implementation (ModuleLoader interface)

    def _compute_file_hash(self, file_path: str) -> str:
        """
        Compute SHA256 hash of file content.

        Uses chunked reading for memory efficiency with large files.

        Args:
            file_path: Path to file to hash

        Returns:
            SHA256 hash as hex string
        """
        hasher = hashlib.sha256()
        with open(file_path, 'rb') as f:
            while chunk := f.read(8192):
                hasher.update(chunk)
        return hasher.hexdigest()

    def resolve_module(self, module_name: str) -> str:
        """
        Find module file in search path.

        Security: Module names must not use absolute or relative path navigation.
        Only simple names (e.g., "calendar") or subdirectory paths (e.g., "lib/validation")
        are allowed. This prevents escaping the configured module directories.

        Args:
            module_name: Name like "calendar" or "lib/validation"

        Returns:
            Full path to module file

        Raises:
            AIFPLModuleNotFoundError: If module not found in search path
            AIFPLModuleError: If module name contains invalid path components
        """
        # Reject absolute paths
        if module_name.startswith('/') or (os.sep != '/' and module_name.startswith(os.sep)):
            raise AIFPLModuleError(
                message=f"Absolute module paths are not allowed: '{module_name}'",
                context="Module names must be relative to the module search path",
                suggestion="Use a simple module name like 'calendar' or 'lib/validation'"
            )

        # Reject relative path navigation (. or ..)
        if module_name.startswith('./') or module_name.startswith('../') or '/./' in module_name or '/../' in module_name:
            raise AIFPLModuleError(
                message=f"Relative path navigation is not allowed in module names: '{module_name}'",
                context="Module names must not contain './' or '../' path components",
                suggestion="Use a simple module name like 'calendar' or 'lib/validation'"
            )

        # Search for module in configured paths
        for directory in self.module_path:
            module_path = Path(directory) / f"{module_name}.aifpl"
            if module_path.exists():
                return str(module_path)

        raise AIFPLModuleNotFoundError(
            module_name=module_name,
            search_paths=self.module_path
        )

    def load_module(self, module_name: str) -> AIFPLValue:
        """
        Load and compile a module to a fully resolved AST.

        This implements the ModuleLoader interface. It compiles the module through
        the full front-end pipeline (lex, parse, semantic analysis, module resolution).
        The result is cached for subsequent imports. Cache is automatically invalidated
        when the module file content changes (detected via SHA256 hash).

        Args:
            module_name: Name of module to load

        Returns:
            Fully resolved AST of the module (all imports already resolved)

        Raises:
            AIFPLModuleNotFoundError: If module file not found
            AIFPLCircularImportError: If circular dependency detected (by module resolver)
            AIFPLError: If module compilation fails
        """
        # Resolve to file path
        try:
            module_path = self.resolve_module(module_name)

        except AIFPLModuleNotFoundError:
            # File doesn't exist - clean up any stale cache entries
            self.module_cache.pop(module_name, None)
            self.module_hashes.pop(module_name, None)
            raise

        # Compute current file hash for cache invalidation
        try:
            current_hash = self._compute_file_hash(module_path)

        except OSError:
            # File disappeared after resolve - clean up cache and raise
            self.module_cache.pop(module_name, None)
            self.module_hashes.pop(module_name, None)
            raise AIFPLModuleNotFoundError(
                module_name=module_name,
                search_paths=self.module_path
            )

        # Check cache validity using content hash
        if module_name in self.module_cache:
            cached_hash = self.module_hashes.get(module_name)
            if cached_hash == current_hash:
                # Cache is valid - return cached AST
                return self.module_cache[module_name]
            # Cache is stale - will reload below

        # Load source code
        with open(module_path, 'r', encoding='utf-8') as f:
            code = f.read()

        # Compile through the front-end pipeline (lex, parse, analyze, resolve imports)
        # This will recursively handle any imports within this module
        resolved_ast = self.compiler.compile_to_resolved_ast(code)

        # Update hash after successful compilation
        # Note: module_cache is updated by module_resolver after resolution
        self.module_hashes[module_name] = current_hash

        return resolved_ast

    def clear_module_cache(self) -> None:
        """Clear the module cache and hashes. Useful for development/testing."""
        self.module_cache.clear()
        self.module_hashes.clear()

    def invalidate_module(self, module_name: str) -> None:
        """
        Invalidate a specific module in the cache, forcing reload on next import.

        Args:
            module_name: Name of module to invalidate (e.g., "calendar" or "lib/validation")
        """
        self.module_cache.pop(module_name, None)
        self.module_hashes.pop(module_name, None)

    def reload_module(self, module_name: str) -> AIFPLValue:
        """
        Force reload a module, bypassing cache.

        Args:
            module_name: Name of module to reload

        Returns:
            Fully resolved AST of the reloaded module
        """
        self.invalidate_module(module_name)
        return self.load_module(module_name)

    def set_module_path(self, module_path: List[str]) -> None:
        """
        Set the module search path and clear the module cache.

        This should be called when the base directory changes (e.g., when switching
        mindspaces in Humbug) to ensure modules are loaded from the correct location
        and old cached modules are discarded.

        Args:
            module_path: List of directories to search for modules
        """
        self.module_path = module_path
        # Clear the cache since modules from the old path are no longer valid
        self.clear_module_cache()
        # Also clear the loading stack to ensure clean state
        self.loading_stack.clear()
