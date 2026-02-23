"""Main AIFPL (AI Functional Programming Language) class with enhanced error messages."""

import hashlib
import math
from pathlib import Path
import os
from typing import Union, Dict, List, Iterator
from contextlib import contextmanager

from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_ast import AIFPLASTNode
from aifpl.aifpl_value import AIFPLFunction, AIFPLFloat, AIFPLBoolean, AIFPLValue
from aifpl.aifpl_vm import AIFPLVM, AIFPLTraceWatcher
from aifpl.aifpl_error import AIFPLModuleNotFoundError, AIFPLModuleError, AIFPLCircularImportError


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
        # Variadic arithmetic builtins — implemented in AIFPL using rest parameters.
        # The bodies use 2-arg calls which the desugarer/codegen resolve to opcodes
        # directly, so there is no circular dependency on the function objects.
        '+': """(lambda (. args)
                  (if (null? args) 0
                    (letrec ((loop (lambda (lst acc)
                                     (if (null? lst) acc
                                         (loop (rest lst) (+ acc (first lst)))))))
                      (loop (rest args) (first args)))))""",
        '-': """(lambda (. args)
                  (if (null? args)
                    (error "Function '-' requires at least 1 argument, got 0")
                    (if (null? (rest args))
                      (- 0 (first args))
                      (letrec ((loop (lambda (lst acc)
                                       (if (null? lst) acc
                                           (loop (rest lst) (- acc (first lst)))))))
                        (loop (rest args) (first args))))))""",
        '*': """(lambda (. args)
                  (if (null? args) 1
                    (letrec ((loop (lambda (lst acc)
                                     (if (null? lst) acc
                                         (loop (rest lst) (* acc (first lst)))))))
                      (loop (rest args) (first args)))))""",
        '/': """(lambda (. args)
                  (if (< (length args) 2)
                    (error "Function '/' requires at least 2 arguments")
                    (letrec ((loop (lambda (lst acc)
                                     (if (null? lst) acc
                                         (loop (rest lst) (/ acc (first lst)))))))
                      (loop (rest args) (first args)))))""",
        # Comparison chains — pairwise semantics: (= a b c) means (= a b) and (= b c)
        '=': """(lambda (. args)
                  (if (< (length args) 2)
                    (error "Function '=' requires at least 2 arguments")
                    (letrec ((loop (lambda (lst prev)
                                     (if (null? lst) #t
                                         (if (= prev (first lst))
                                             (loop (rest lst) (first lst))
                                             #f)))))
                      (loop (rest args) (first args)))))""",
        '<': """(lambda (. args)
                  (if (< (length args) 2)
                    (error "Function '<' requires at least 2 arguments")
                    (letrec ((loop (lambda (lst prev)
                                     (if (null? lst) #t
                                         (if (< prev (first lst))
                                             (loop (rest lst) (first lst))
                                             #f)))))
                      (loop (rest args) (first args)))))""",
        '>': """(lambda (. args)
                  (if (< (length args) 2)
                    (error "Function '>' requires at least 2 arguments")
                    (letrec ((loop (lambda (lst prev)
                                     (if (null? lst) #t
                                         (if (> prev (first lst))
                                             (loop (rest lst) (first lst))
                                             #f)))))
                      (loop (rest args) (first args)))))""",
        '<=': """(lambda (. args)
                   (if (< (length args) 2)
                     (error "Function '<=' requires at least 2 arguments")
                     (letrec ((loop (lambda (lst prev)
                                      (if (null? lst) #t
                                          (if (<= prev (first lst))
                                              (loop (rest lst) (first lst))
                                              #f)))))
                       (loop (rest args) (first args)))))""",
        '>=': """(lambda (. args)
                   (if (< (length args) 2)
                     (error "Function '>=' requires at least 2 arguments")
                     (letrec ((loop (lambda (lst prev)
                                      (if (null? lst) #t
                                          (if (>= prev (first lst))
                                              (loop (rest lst) (first lst))
                                              #f)))))
                       (loop (rest args) (first args)))))""",
        # != — all-pairs distinct: every pair (i,j) where i<j must satisfy i != j
        '!=': """(lambda (. args)
                   (if (< (length args) 2)
                     (error "Function '!=' requires at least 2 arguments")
                     (letrec ((outer (lambda (lst)
                                       (if (null? lst) #t
                                           (letrec ((inner (lambda (rest-lst)
                                                             (if (null? rest-lst)
                                                                 (outer (rest lst))
                                                                 (if (!= (first lst) (first rest-lst))
                                                                     (inner (rest rest-lst))
                                                                     #f)))))
                                             (inner (rest lst)))))))
                       (outer args))))""",
        # Fold-reducible bitwise ops — identity 0
        'bit-or': """(lambda (. args)
                       (if (null? args) 0
                         (letrec ((loop (lambda (lst acc)
                                          (if (null? lst) acc
                                              (loop (rest lst) (bit-or acc (first lst)))))))
                           (loop (rest args) (first args)))))""",
        'bit-and': """(lambda (. args)
                        (if (null? args) 0
                          (letrec ((loop (lambda (lst acc)
                                           (if (null? lst) acc
                                               (loop (rest lst) (bit-and acc (first lst)))))))
                            (loop (rest args) (first args)))))""",
        'bit-xor': """(lambda (. args)
                        (if (null? args) 0
                          (letrec ((loop (lambda (lst acc)
                                           (if (null? lst) acc
                                               (loop (rest lst) (bit-xor acc (first lst)))))))
                            (loop (rest args) (first args)))))""",
        # Fold-reducible list/string ops
        'append': """(lambda (. args)
                       (if (null? args) (list)
                         (letrec ((loop (lambda (lst acc)
                                          (if (null? lst) acc
                                              (loop (rest lst) (append acc (first lst)))))))
                           (loop (rest args) (first args)))))""",
        'string-append': """(lambda (. args)
                              (if (null? args) ""
                                (letrec ((loop (lambda (lst acc)
                                                 (if (null? lst) acc
                                                     (loop (rest lst) (string-append acc (first lst)))))))
                                  (loop (rest args) (first args)))))""",
        # Fold-reducible min/max — require at least 1 arg
        'min': """(lambda (. args)
                    (if (null? args)
                      (error "Function 'min' requires at least 1 argument")
                      (letrec ((loop (lambda (lst acc)
                                       (if (null? lst) acc
                                           (loop (rest lst) (min acc (first lst)))))))
                        (loop (rest args) (first args)))))""",
        'max': """(lambda (. args)
                    (if (null? args)
                      (error "Function 'max' requires at least 1 argument")
                      (letrec ((loop (lambda (lst acc)
                                       (if (null? lst) acc
                                           (loop (rest lst) (max acc (first lst)))))))
                        (loop (rest args) (first args)))))""",
        # Typed equality chains — pairwise, same pattern as =
        'string=?': """(lambda (. args)
                         (if (< (length args) 2)
                           (error "Function 'string=?' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst prev)
                                            (if (null? lst) #t
                                                (if (string=? prev (first lst))
                                                    (loop (rest lst) (first lst))
                                                    #f)))))
                             (loop (rest args) (first args)))))""",
        'number=?': """(lambda (. args)
                         (if (< (length args) 2)
                           (error "Function 'number=?' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst prev)
                                            (if (null? lst) #t
                                                (if (number=? prev (first lst))
                                                    (loop (rest lst) (first lst))
                                                    #f)))))
                             (loop (rest args) (first args)))))""",
        'integer=?': """(lambda (. args)
                          (if (< (length args) 2)
                            (error "Function 'integer=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (null? lst) #t
                                                 (if (integer=? prev (first lst))
                                                     (loop (rest lst) (first lst))
                                                     #f)))))
                              (loop (rest args) (first args)))))""",
        'float=?': """(lambda (. args)
                        (if (< (length args) 2)
                          (error "Function 'float=?' requires at least 2 arguments")
                          (letrec ((loop (lambda (lst prev)
                                           (if (null? lst) #t
                                               (if (float=? prev (first lst))
                                                   (loop (rest lst) (first lst))
                                                   #f)))))
                            (loop (rest args) (first args)))))""",
        'complex=?': """(lambda (. args)
                          (if (< (length args) 2)
                            (error "Function 'complex=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (null? lst) #t
                                                 (if (complex=? prev (first lst))
                                                     (loop (rest lst) (first lst))
                                                     #f)))))
                              (loop (rest args) (first args)))))""",
        'boolean=?': """(lambda (. args)
                          (if (< (length args) 2)
                            (error "Function 'boolean=?' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst prev)
                                             (if (null? lst) #t
                                                 (if (boolean=? prev (first lst))
                                                     (loop (rest lst) (first lst))
                                                     #f)))))
                              (loop (rest args) (first args)))))""",
        'list=?': """(lambda (. args)
                       (if (< (length args) 2)
                         (error "Function 'list=?' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst prev)
                                          (if (null? lst) #t
                                              (if (list=? prev (first lst))
                                                  (loop (rest lst) (first lst))
                                                  #f)))))
                           (loop (rest args) (first args)))))""",
        'alist=?': """(lambda (. args)
                        (if (< (length args) 2)
                          (error "Function 'alist=?' requires at least 2 arguments")
                          (letrec ((loop (lambda (lst prev)
                                           (if (null? lst) #t
                                               (if (alist=? prev (first lst))
                                                   (loop (rest lst) (first lst))
                                                   #f)))))
                            (loop (rest args) (first args)))))""",
        # list — trivial: rest args IS the list
        'list': """(lambda (. args) args)""",
        # alist — collect (list key val) pairs into an alist
        'alist': """(lambda (. args)
                      (letrec ((loop (lambda (pairs acc)
                                       (if (null? pairs) acc
                                           (if (not (list? (first pairs)))
                                               (error "alist: each argument must be a 2-element list")
                                               (if (!= (length (first pairs)) 2)
                                                   (error "alist: each argument must be a 2-element list")
                                                   (loop (rest pairs)
                                                         (alist-set acc
                                                                    (first (first pairs))
                                                                    (first (rest (first pairs)))))))))))
                        (loop args (alist))))""",
        # Optional-arg builtins with mixed fixed+rest parameters
        'alist-get': """(lambda (a-list key . rest)
                          (alist-get a-list key (if (null? rest) #f (first rest))))""",
        'range': """(lambda (start end . rest)
                      (range start end (if (null? rest) 1 (first rest))))""",
        # Higher-order functions
        # Type-specific variadic integer arithmetic
        'integer+': """(lambda (. args)
                         (if (null? args) 0
                           (letrec ((loop (lambda (lst acc)
                                            (if (null? lst) acc
                                                (loop (rest lst) (integer+ acc (first lst)))))))
                             (loop (rest args) (first args)))))""",
        'integer-': """(lambda (. args)
                         (if (null? args)
                           (error "Function 'integer-' requires at least 1 argument, got 0")
                           (if (null? (rest args))
                             (integer-negate (first args))
                             (letrec ((loop (lambda (lst acc)
                                              (if (null? lst) acc
                                                  (loop (rest lst) (integer- acc (first lst)))))))
                               (loop (rest args) (first args))))))""",
        'integer*': """(lambda (. args)
                         (if (null? args) 1
                           (letrec ((loop (lambda (lst acc)
                                            (if (null? lst) acc
                                                (loop (rest lst) (integer* acc (first lst)))))))
                             (loop (rest args) (first args)))))""",
        'integer/': """(lambda (. args)
                         (if (< (length args) 2)
                           (error "Function 'integer/' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst acc)
                                            (if (null? lst) acc
                                                (loop (rest lst) (integer/ acc (first lst)))))))
                             (loop (rest args) (first args)))))""",
        # Type-specific variadic float arithmetic
        'float+': """(lambda (. args)
                       (if (null? args) 0.0
                         (letrec ((loop (lambda (lst acc)
                                          (if (null? lst) acc
                                              (loop (rest lst) (float+ acc (first lst)))))))
                           (loop (rest args) (first args)))))""",
        'float-': """(lambda (. args)
                       (if (null? args)
                         (error "Function 'float-' requires at least 1 argument, got 0")
                         (if (null? (rest args))
                           (float-negate (first args))
                           (letrec ((loop (lambda (lst acc)
                                            (if (null? lst) acc
                                                (loop (rest lst) (float- acc (first lst)))))))
                             (loop (rest args) (first args))))))""",
        'float*': """(lambda (. args)
                       (if (null? args) 1.0
                         (letrec ((loop (lambda (lst acc)
                                          (if (null? lst) acc
                                              (loop (rest lst) (float* acc (first lst)))))))
                           (loop (rest args) (first args)))))""",
        'float/': """(lambda (. args)
                       (if (< (length args) 2)
                         (error "Function 'float/' requires at least 2 arguments")
                         (letrec ((loop (lambda (lst acc)
                                          (if (null? lst) acc
                                              (loop (rest lst) (float/ acc (first lst)))))))
                           (loop (rest args) (first args)))))""",
        'float-pow': """(lambda (. args)
                          (if (< (length args) 2)
                            (error "Function 'float-pow' requires at least 2 arguments")
                            (letrec ((loop (lambda (lst acc)
                                             (if (null? lst) acc
                                                 (loop (rest lst) (float-pow acc (first lst)))))))
                              (loop (rest args) (first args)))))""",
        # Type-specific variadic complex arithmetic
        'complex+': """(lambda (. args)
                         (if (null? args)
                           (error "Function 'complex+' requires at least 1 argument, got 0")
                           (letrec ((loop (lambda (lst acc)
                                            (if (null? lst) acc
                                                (loop (rest lst) (complex+ acc (first lst)))))))
                             (loop (rest args) (first args)))))""",
        'complex-': """(lambda (. args)
                         (if (null? args)
                           (error "Function 'complex-' requires at least 1 argument, got 0")
                           (if (null? (rest args))
                             (complex-negate (first args))
                             (letrec ((loop (lambda (lst acc)
                                              (if (null? lst) acc
                                                  (loop (rest lst) (complex- acc (first lst)))))))
                               (loop (rest args) (first args))))))""",
        'complex*': """(lambda (. args)
                         (if (null? args)
                           (error "Function 'complex*' requires at least 1 argument, got 0")
                           (letrec ((loop (lambda (lst acc)
                                            (if (null? lst) acc
                                                (loop (rest lst) (complex* acc (first lst)))))))
                             (loop (rest args) (first args)))))""",
        'complex/': """(lambda (. args)
                         (if (< (length args) 2)
                           (error "Function 'complex/' requires at least 2 arguments")
                           (letrec ((loop (lambda (lst acc)
                                            (if (null? lst) acc
                                                (loop (rest lst) (complex/ acc (first lst)))))))
                             (loop (rest args) (first args)))))""",
        'complex-pow': """(lambda (. args)
                            (if (< (length args) 2)
                              (error "Function 'complex-pow' requires at least 2 arguments")
                              (letrec ((loop (lambda (lst acc)
                                               (if (null? lst) acc
                                                   (loop (rest lst) (complex-pow acc (first lst)))))))
                                (loop (rest args) (first args)))))""",
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

    def __init__(self, module_path: List[str] | None = None):
        """
        Initialize AIFPL calculator.

        Args:
            module_path: List of directories to search for modules (default: ["."])
        """
        self._module_path = module_path or ["."]

        # Module system state
        self.module_cache: Dict[str, AIFPLASTNode] = {}  # module_name -> alist
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

    @contextmanager
    def begin_loading(self, module_name: str) -> Iterator[None]:
        """
        Begin loading a module with circular import detection.

        This context manager tracks the module in the loading stack and
        automatically cleans up when exiting (even on exception).

        Args:
            module_name: Name of module being loaded

        Yields:
            None

        Raises:
            AIFPLCircularImportError: If this module is already being loaded
        """
        # Check for circular dependency BEFORE adding to stack
        if module_name in self.loading_stack:
            cycle = self.loading_stack + [module_name]
            raise AIFPLCircularImportError(import_chain=cycle)

        # Add to loading stack
        self.loading_stack.append(module_name)
        try:
            yield

        finally:
            # Always remove from stack, even if loading fails
            self.loading_stack.pop()

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
        for directory in self._module_path:
            module_path = Path(directory) / f"{module_name}.aifpl"
            if module_path.exists():
                return str(module_path)

        raise AIFPLModuleNotFoundError(
            module_name=module_name,
            search_paths=self._module_path
        )

    def load_module(self, module_name: str) -> AIFPLASTNode:
        """
        Load and compile a module to a fully resolved AST.

        This implements the ModuleLoader interface. It compiles the module through
        the full front-end pipeline (lex, parse, semantic analysis, module resolution).
        The result is cached for subsequent imports. Cache is automatically invalidated
        when the module file content changes (detected via SHA256 hash).

        Note: Callers should use begin_loading() before calling this method to enable
        circular import detection. The module resolver handles this automatically.

        Args:
            module_name: Name of module to load

        Returns:
            Fully resolved AST of the module (all imports already resolved)

        Raises:
            AIFPLModuleNotFoundError: If module file not found
            AIFPLCircularImportError: If circular dependency detected (via begin_loading)
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

        except OSError as e:
            # File disappeared after resolve - clean up cache and raise
            self.module_cache.pop(module_name, None)
            self.module_hashes.pop(module_name, None)
            raise AIFPLModuleNotFoundError(
                module_name=module_name,
                search_paths=self._module_path
            ) from e

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
        # This will recursively handle any imports within this module.
        # The module resolver will call begin_loading() for each nested import,
        # which provides circular import detection.
        # Use module name with .aifpl extension for source_file (relative path)
        resolved_ast = self.compiler.compile_to_resolved_ast(code, f"{module_name}.aifpl")

        # Cache the resolved module and update hash after successful compilation
        self.module_cache[module_name] = resolved_ast
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

    def reload_module(self, module_name: str) -> AIFPLASTNode:
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
        self._module_path = module_path
        # Clear the cache since modules from the old path are no longer valid
        self.clear_module_cache()
        # Also clear the loading stack to ensure clean state
        self.loading_stack.clear()

    def module_path(self) -> List[str]:
        """
        Get the current module search path.

        Returns:
            List of directories in the module search path
        """
        return self._module_path

    def set_trace_watcher(self, watcher: AIFPLTraceWatcher | None) -> None:
        """
        Set the trace watcher for this AIFPL instance.

        The trace watcher receives messages from (trace ...) calls during evaluation.

        Args:
            watcher: AIFPLTraceWatcher instance or None to disable tracing
        """
        self.vm.set_trace_watcher(watcher)
