"""Symbol Table for AIFPL two-pass compiler.

The symbol table tracks variable bindings across lexical scopes during Pass 1 (analysis).
This allows the analyzer to resolve variables and calculate proper indices without
emitting code.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional

from aifpl.aifpl_value import AIFPLValue


@dataclass
class SymbolInfo:
    """Information about a symbol (variable) in the symbol table.
    
    Tracks the symbol's name, type, scope information, and where it was defined.
    """
    name: str
    symbol_type: str  # 'parameter', 'local', 'global', 'builtin'
    scope_depth: int  # How many scopes deep (0 = global/module level)
    var_index: int    # Index within the scope's local variables
    is_mutable: bool = False  # For future use (currently all immutable)
    defined_at: Optional[AIFPLValue] = None  # AST node where defined (for error messages)
    
    def __repr__(self) -> str:
        """Human-readable representation."""
        return (f"SymbolInfo({self.name}, {self.symbol_type}, "
                f"depth={self.scope_depth}, idx={self.var_index})")


@dataclass
class Scope:
    """A lexical scope containing symbol bindings.
    
    Scopes form a tree structure with parent/child relationships.
    Each scope tracks its symbols and can look up symbols in parent scopes.
    """
    parent: Optional['Scope']
    depth: int
    symbols: Dict[str, SymbolInfo] = field(default_factory=dict)
    children: List['Scope'] = field(default_factory=list)
    next_var_index: int = 0  # Next available variable index in this scope
    
    def lookup(self, name: str) -> Optional[SymbolInfo]:
        """Look up a symbol in this scope or parent scopes.
        
        Args:
            name: Symbol name to look up
            
        Returns:
            SymbolInfo if found, None otherwise
        """
        # Check this scope first
        if name in self.symbols:
            return self.symbols[name]
        
        # Check parent scopes
        if self.parent:
            return self.parent.lookup(name)
        
        return None
    
    def lookup_local(self, name: str) -> Optional[SymbolInfo]:
        """Look up a symbol only in this scope (not parent scopes).
        
        Args:
            name: Symbol name to look up
            
        Returns:
            SymbolInfo if found in this scope, None otherwise
        """
        return self.symbols.get(name)
    
    def add_symbol(self, name: str, symbol_type: str, 
                   defined_at: Optional[AIFPLValue] = None) -> SymbolInfo:
        """Add a symbol to this scope.
        
        Args:
            name: Symbol name
            symbol_type: Type of symbol ('parameter', 'local', etc.)
            defined_at: AST node where defined (optional)
            
        Returns:
            The created SymbolInfo
            
        Note:
            If a symbol with the same name already exists, it will be shadowed
            (the new symbol takes precedence in this scope).
        """
        var_index = self.next_var_index
        self.next_var_index += 1
        
        info = SymbolInfo(
            name=name,
            symbol_type=symbol_type,
            scope_depth=self.depth,
            var_index=var_index,
            defined_at=defined_at
        )
        
        self.symbols[name] = info
        return info
    
    def get_all_symbols(self) -> List[SymbolInfo]:
        """Get all symbols defined in this scope.
        
        Returns:
            List of SymbolInfo objects
        """
        return list(self.symbols.values())
    
    def __repr__(self) -> str:
        """Human-readable representation."""
        symbol_names = list(self.symbols.keys())
        return f"Scope(depth={self.depth}, symbols={symbol_names})"


class SymbolTable:
    """Manages symbol tables for the analysis pass.
    
    The symbol table maintains a tree of scopes and tracks the current scope
    as the analyzer traverses the AST. It provides methods to enter/exit scopes
    and resolve symbols.
    
    Example usage:
        table = SymbolTable()
        
        # Add a global
        table.add_symbol("pi", "global")
        
        # Enter a function scope
        table.enter_scope()
        table.add_symbol("x", "parameter")
        table.add_symbol("y", "local")
        
        # Resolve a symbol
        info = table.resolve("x")  # Returns SymbolInfo for x
        
        # Exit the function scope
        table.exit_scope()
    """
    
    def __init__(self):
        """Initialize symbol table with a root scope."""
        self.root_scope = Scope(parent=None, depth=0)
        self.current_scope = self.root_scope
        self._scope_stack: List[Scope] = [self.root_scope]
    
    def enter_scope(self) -> Scope:
        """Enter a new lexical scope.
        
        Creates a new child scope of the current scope and makes it current.
        
        Returns:
            The newly created scope
        """
        new_scope = Scope(
            parent=self.current_scope,
            depth=self.current_scope.depth + 1
        )
        self.current_scope.children.append(new_scope)
        self.current_scope = new_scope
        self._scope_stack.append(new_scope)
        return new_scope
    
    def exit_scope(self) -> Scope:
        """Exit the current scope, returning to parent scope.
        
        Returns:
            The scope that was exited
            
        Raises:
            RuntimeError: If trying to exit the root scope
        """
        if self.current_scope.parent is None:
            raise RuntimeError("Cannot exit root scope")
        
        exited_scope = self.current_scope
        self._scope_stack.pop()
        self.current_scope = self.current_scope.parent
        return exited_scope
    
    def add_symbol(self, name: str, symbol_type: str,
                   defined_at: Optional[AIFPLValue] = None) -> SymbolInfo:
        """Add a symbol to the current scope.
        
        Args:
            name: Symbol name
            symbol_type: Type of symbol ('parameter', 'local', 'global', 'builtin')
            defined_at: AST node where defined (optional)
            
        Returns:
            The created SymbolInfo
        """
        return self.current_scope.add_symbol(name, symbol_type, defined_at)
    
    def resolve(self, name: str) -> Optional[SymbolInfo]:
        """Resolve a symbol, searching current scope and parent scopes.
        
        Args:
            name: Symbol name to resolve
            
        Returns:
            SymbolInfo if found, None otherwise
        """
        return self.current_scope.lookup(name)
    
    def resolve_local(self, name: str) -> Optional[SymbolInfo]:
        """Resolve a symbol only in the current scope.
        
        Args:
            name: Symbol name to resolve
            
        Returns:
            SymbolInfo if found in current scope, None otherwise
        """
        return self.current_scope.lookup_local(name)
    
    def get_current_depth(self) -> int:
        """Get the depth of the current scope.
        
        Returns:
            Scope depth (0 = root/global scope)
        """
        return self.current_scope.depth
    
    def get_scope_stack(self) -> List[Scope]:
        """Get the stack of active scopes from root to current.
        
        Returns:
            List of scopes from root to current
        """
        return list(self._scope_stack)
    
    def get_all_symbols_in_scope(self) -> List[SymbolInfo]:
        """Get all symbols in the current scope.
        
        Returns:
            List of SymbolInfo objects
        """
        return self.current_scope.get_all_symbols()
    
    def calculate_variable_offset(self, name: str) -> tuple[int, int]:
        """Calculate the (depth, index) offset for a variable.
        
        This is used during code generation to emit LOAD_VAR instructions.
        
        Args:
            name: Variable name
            
        Returns:
            Tuple of (depth_offset, var_index) where:
            - depth_offset: How many scopes up to find the variable (0 = current)
            - var_index: Index within that scope's locals
            
        Raises:
            ValueError: If variable not found
        """
        # Search from current scope upward
        for i, scope in enumerate(reversed(self._scope_stack)):
            if name in scope.symbols:
                depth_offset = i
                var_index = scope.symbols[name].var_index
                return (depth_offset, var_index)
        
        raise ValueError(f"Variable '{name}' not found in scope stack")
    
    def dump(self, scope: Optional[Scope] = None, indent: int = 0) -> str:
        """Dump the symbol table for debugging.
        
        Args:
            scope: Scope to dump (defaults to root)
            indent: Indentation level
            
        Returns:
            String representation of the symbol table tree
        """
        if scope is None:
            scope = self.root_scope
        
        lines = []
        prefix = "  " * indent
        
        # Dump this scope
        lines.append(f"{prefix}{scope}")
        for name, info in scope.symbols.items():
            lines.append(f"{prefix}  {name}: {info}")
        
        # Dump child scopes
        for child in scope.children:
            lines.append(self.dump(child, indent + 1))
        
        return "\n".join(lines)
    
    def __repr__(self) -> str:
        """Human-readable representation."""
        return f"SymbolTable(depth={self.current_scope.depth}, scopes={len(self._scope_stack)})"


def create_global_symbol_table(globals_dict: Dict[str, AIFPLValue]) -> SymbolTable:
    """Create a symbol table pre-populated with global symbols.
    
    This is useful for the analyzer to have access to built-in constants
    like pi, e, j, etc.
    
    Args:
        globals_dict: Dictionary of global name -> value
        
    Returns:
        SymbolTable with globals added to root scope
    """
    table = SymbolTable()
    
    for name in globals_dict.keys():
        table.add_symbol(name, "global")
    
    return table
