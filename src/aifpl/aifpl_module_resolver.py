"""AIFPL Module Resolver Pass - resolves import expressions at compile-time.

This pass walks the AST looking for (import "module-name") expressions and
replaces them with the loaded module's alist value. It uses a ModuleLoader
interface to delegate the actual module loading logic.
"""

from typing import Protocol, ContextManager

from aifpl.aifpl_error import AIFPLModuleError
from aifpl.aifpl_ast import AIFPLASTNode, AIFPLASTSymbol, AIFPLASTList, AIFPLASTString


class ModuleLoader(Protocol):
    """Interface for loading AIFPL modules at compile-time.

    The module loader is responsible for:
    - Locating module files in the search path
    - Reading and compiling module source code
    - Caching compiled modules
    - Detecting and preventing circular import dependencies
    """

    def begin_loading(self, module_name: str) -> ContextManager[None]:
        """
        Begin loading a module and return a context manager for tracking.

        This method is called before load_module() to enable circular import detection.
        The context manager should track the module in a loading stack and automatically
        clean up when exiting (even on exception).

        Args:
            module_name: Name of module being loaded

        Returns:
            Context manager that tracks the loading state

        Raises:
            AIFPLCircularImportError: If this module is already being loaded (circular dependency)
        """
        ...  # pylint: disable=unnecessary-ellipsis

    def load_module(self, module_name: str) -> AIFPLASTNode:
        """
        Load and compile a module to a fully resolved AST.

        This should compile the module through the full front-end pipeline:
        - Lexing, parsing, semantic analysis, and module resolution

        The returned AST should have all imports already resolved.

        Note: Callers should use begin_loading() before calling this method
        to enable circular import detection.

        Args:
            module_name: Name of module (e.g., "calendar", "lib/validation")

        Returns:
            Fully resolved AST of the module (ready for inlining into parent AST)

        Raises:
            AIFPLModuleError: If module not found or fails to load
            AIFPLCircularImportError: If circular dependency detected
        """
        ...  # pylint: disable=unnecessary-ellipsis


class AIFPLModuleResolver:
    """
    Resolves import expressions by loading modules and replacing imports with their values.

    This pass transforms:
        (import "calendar")
    Into:
        (alist (list "add-days" <function>) (list "working-days" <function>) ...)

    The actual module loading is delegated to a ModuleLoader interface.
    """

    def __init__(self, module_loader: ModuleLoader | None = None):
        """
        Initialize module resolver pass.

        Args:
            module_loader: Optional module loader interface. If None, imports will fail.
        """
        self.module_loader = module_loader

    def resolve(self, expr: AIFPLASTNode) -> AIFPLASTNode:
        """
        Resolve imports in an expression recursively.

        Args:
            expr: AST to resolve imports in

        Returns:
            AST with all imports replaced by loaded module values
        """
        # Only lists need inspection
        if not isinstance(expr, AIFPLASTList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()

        # Check for import special form
        if isinstance(first, AIFPLASTSymbol) and first.name == 'import':
            return self._resolve_import(expr)

        # Check for quote - don't resolve imports inside quoted expressions
        if isinstance(first, AIFPLASTSymbol) and first.name == 'quote':
            return expr

        # Recursively resolve imports in all subexpressions
        resolved_elements = tuple(self.resolve(elem) for elem in expr.elements)
        return AIFPLASTList(resolved_elements, line=expr.line, column=expr.column)

    def _resolve_import(self, expr: AIFPLASTList) -> AIFPLASTNode:
        """
        Resolve an import expression by loading the module.

        Args:
            expr: Import expression (validated by semantic analyzer)

        Returns:
            The loaded module's alist value

        Raises:
            AIFPLModuleError: If module cannot be loaded
        """
        # Validation already done by semantic analyzer
        assert len(expr.elements) == 2, "Import should have exactly 2 elements (validated by semantic analyzer)"

        _, module_name_expr = expr.elements
        assert isinstance(module_name_expr, AIFPLASTString), "Module name should be a string (validated by semantic analyzer)"

        module_name = module_name_expr.value

        # Delegate to module loader
        if self.module_loader is None:
            raise AIFPLModuleError(
                message="No module loader configured",
                context=f"Attempted to import module '{module_name}'",
                suggestion="Module loader must be provided to compiler to use import"
            )

        # Use the module loader's context manager for circular import detection
        with self.module_loader.begin_loading(module_name):
            # Load the module (this will recursively compile if the module has imports)
            # The module loader handles circular detection and will raise AIFPLCircularImportError if needed
            # The returned AST has all imports already resolved
            return self.module_loader.load_module(module_name)
