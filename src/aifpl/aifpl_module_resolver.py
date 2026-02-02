"""AIFPL Module Resolver Pass - resolves import expressions at compile-time.

This pass walks the AST looking for (import "module-name") expressions and
replaces them with the loaded module's alist value. It uses a ModuleLoader
interface to delegate the actual module loading logic.
"""

from typing import Protocol

from aifpl.aifpl_error import AIFPLCircularImportError, AIFPLModuleError
from aifpl.aifpl_value import AIFPLValue, AIFPLSymbol, AIFPLList, AIFPLString


class ModuleLoader(Protocol):
    """Interface for loading AIFPL modules at compile-time."""

    def load_module(self, module_name: str) -> AIFPLValue:
        """
        Load and compile a module to a fully resolved AST.

        This should compile the module through the full front-end pipeline:
        - Lexing, parsing, semantic analysis, and module resolution

        The returned AST should have all imports already resolved.

        Args:
            module_name: Name of module (e.g., "calendar", "lib/validation")

        Returns:
            Fully resolved AST of the module (ready for inlining into parent AST)

        Raises:
            AIFPLModuleError: If module not found or fails to load
        """
        ...


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

    def resolve(self, expr: AIFPLValue) -> AIFPLValue:
        """
        Resolve imports in an expression recursively.

        Args:
            expr: AST to resolve imports in

        Returns:
            AST with all imports replaced by loaded module values
        """
        # Only lists need inspection
        if not isinstance(expr, AIFPLList):
            return expr

        if expr.is_empty():
            return expr

        first = expr.first()

        # Check for import special form
        if isinstance(first, AIFPLSymbol) and first.name == 'import':
            return self._resolve_import(expr)

        # Check for quote - don't resolve imports inside quoted expressions
        if isinstance(first, AIFPLSymbol) and first.name == 'quote':
            return expr

        # Recursively resolve imports in all subexpressions
        resolved_elements = tuple(self.resolve(elem) for elem in expr.elements)
        return AIFPLList(resolved_elements)

    def _resolve_import(self, expr: AIFPLList) -> AIFPLValue:
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
        assert isinstance(module_name_expr, AIFPLString), "Module name should be a string (validated by semantic analyzer)"

        module_name = module_name_expr.value

        # Delegate to module loader
        if self.module_loader is None:
            raise AIFPLModuleError(
                message="No module loader configured",
                context=f"Attempted to import module '{module_name}'",
                suggestion="Module loader must be provided to compiler to use import"
            )

        # Check for circular dependency BEFORE loading
        # The module loader should have a loading_stack attribute for tracking
        if hasattr(self.module_loader, 'loading_stack'):
            if module_name in self.module_loader.loading_stack:
                cycle = self.module_loader.loading_stack + [module_name]
                raise AIFPLCircularImportError(import_chain=cycle)

        # Mark as currently resolving (for circular dependency detection)
        # Must be done BEFORE loading to catch circular imports during compilation
        if hasattr(self.module_loader, 'loading_stack'):
            self.module_loader.loading_stack.append(module_name)

        try:
            # Load the module (this will recursively compile if the module has imports)
            # The module is compiled through the full front-end pipeline
            module_value = self.module_loader.load_module(module_name)

            # Recursively resolve any imports in the loaded module's AST
            # NOTE: If load_module properly compiles the module, this should be a no-op
            # (all imports already resolved). But we keep it for safety.
            resolved_module = self.resolve(module_value)

            # Cache the resolved module
            if hasattr(self.module_loader, 'module_cache'):
                self.module_loader.module_cache[module_name] = resolved_module

            return resolved_module

        finally:
            # Always pop from stack, even if resolution fails
            if hasattr(self.module_loader, 'loading_stack'):
                self.module_loader.loading_stack.pop()
