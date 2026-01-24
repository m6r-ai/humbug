"""Tests for Symbol Table.

These tests verify that the symbol table correctly tracks variables across
lexical scopes during analysis.
"""

import pytest
from aifpl.aifpl_symbol_table import (
    SymbolInfo,
    Scope,
    SymbolTable,
    create_global_symbol_table,
)
from aifpl.aifpl_value import AIFPLNumber, AIFPLSymbol


class TestSymbolInfo:
    """Test SymbolInfo dataclass."""
    
    def test_create_symbol_info(self):
        """Test creating a SymbolInfo."""
        info = SymbolInfo(
            name="x",
            symbol_type="local",
            scope_depth=1,
            var_index=0
        )
        
        assert info.name == "x"
        assert info.symbol_type == "local"
        assert info.scope_depth == 1
        assert info.var_index == 0
        assert not info.is_mutable
    
    def test_symbol_info_with_definition(self):
        """Test SymbolInfo with definition location."""
        ast_node = AIFPLSymbol("x")
        info = SymbolInfo(
            name="x",
            symbol_type="parameter",
            scope_depth=2,
            var_index=0,
            defined_at=ast_node
        )
        
        assert info.defined_at == ast_node
    
    def test_repr(self):
        """Test string representation."""
        info = SymbolInfo(
            name="foo",
            symbol_type="local",
            scope_depth=1,
            var_index=3
        )
        
        repr_str = repr(info)
        assert "foo" in repr_str
        assert "local" in repr_str
        assert "depth=1" in repr_str
        assert "idx=3" in repr_str


class TestScope:
    """Test Scope class."""
    
    def test_create_root_scope(self):
        """Test creating a root scope."""
        scope = Scope(parent=None, depth=0)
        
        assert scope.parent is None
        assert scope.depth == 0
        assert len(scope.symbols) == 0
        assert len(scope.children) == 0
    
    def test_add_symbol(self):
        """Test adding a symbol to a scope."""
        scope = Scope(parent=None, depth=0)
        
        info = scope.add_symbol("x", "local")
        
        assert info.name == "x"
        assert info.symbol_type == "local"
        assert info.var_index == 0
        assert "x" in scope.symbols
    
    def test_add_multiple_symbols(self):
        """Test adding multiple symbols."""
        scope = Scope(parent=None, depth=0)
        
        info1 = scope.add_symbol("x", "local")
        info2 = scope.add_symbol("y", "local")
        info3 = scope.add_symbol("z", "local")
        
        assert info1.var_index == 0
        assert info2.var_index == 1
        assert info3.var_index == 2
        assert len(scope.symbols) == 3
    
    def test_lookup_local(self):
        """Test looking up a symbol in the same scope."""
        scope = Scope(parent=None, depth=0)
        scope.add_symbol("x", "local")
        
        info = scope.lookup("x")
        
        assert info is not None
        assert info.name == "x"
    
    def test_lookup_not_found(self):
        """Test looking up a non-existent symbol."""
        scope = Scope(parent=None, depth=0)
        
        info = scope.lookup("x")
        
        assert info is None
    
    def test_lookup_in_parent(self):
        """Test looking up a symbol in parent scope."""
        parent = Scope(parent=None, depth=0)
        parent.add_symbol("x", "local")
        
        child = Scope(parent=parent, depth=1)
        
        info = child.lookup("x")
        
        assert info is not None
        assert info.name == "x"
    
    def test_shadowing(self):
        """Test variable shadowing."""
        parent = Scope(parent=None, depth=0)
        parent.add_symbol("x", "local")
        
        child = Scope(parent=parent, depth=1)
        child.add_symbol("x", "local")  # Shadow parent's x
        
        info = child.lookup("x")
        
        assert info is not None
        assert info.scope_depth == 1  # Should get child's x
    
    def test_lookup_local_only(self):
        """Test lookup_local doesn't search parent."""
        parent = Scope(parent=None, depth=0)
        parent.add_symbol("x", "local")
        
        child = Scope(parent=parent, depth=1)
        
        info = child.lookup_local("x")
        
        assert info is None  # Should not find parent's x
    
    def test_get_all_symbols(self):
        """Test getting all symbols in a scope."""
        scope = Scope(parent=None, depth=0)
        scope.add_symbol("x", "local")
        scope.add_symbol("y", "local")
        scope.add_symbol("z", "local")
        
        symbols = scope.get_all_symbols()
        
        assert len(symbols) == 3
        names = {s.name for s in symbols}
        assert names == {"x", "y", "z"}


class TestSymbolTable:
    """Test SymbolTable class."""
    
    def test_create_symbol_table(self):
        """Test creating a symbol table."""
        table = SymbolTable()
        
        assert table.root_scope is not None
        assert table.current_scope == table.root_scope
        assert table.get_current_depth() == 0
    
    def test_add_symbol_to_root(self):
        """Test adding a symbol to root scope."""
        table = SymbolTable()
        
        info = table.add_symbol("pi", "global")
        
        assert info.name == "pi"
        assert info.scope_depth == 0
    
    def test_enter_scope(self):
        """Test entering a new scope."""
        table = SymbolTable()
        
        new_scope = table.enter_scope()
        
        assert table.current_scope == new_scope
        assert new_scope.parent == table.root_scope
        assert table.get_current_depth() == 1
    
    def test_exit_scope(self):
        """Test exiting a scope."""
        table = SymbolTable()
        
        table.enter_scope()
        exited = table.exit_scope()
        
        assert table.current_scope == table.root_scope
        assert table.get_current_depth() == 0
    
    def test_cannot_exit_root_scope(self):
        """Test that exiting root scope raises error."""
        table = SymbolTable()
        
        with pytest.raises(RuntimeError, match="Cannot exit root scope"):
            table.exit_scope()
    
    def test_nested_scopes(self):
        """Test multiple nested scopes."""
        table = SymbolTable()
        
        # Root scope
        table.add_symbol("global_var", "global")
        
        # First level
        table.enter_scope()
        table.add_symbol("outer_var", "local")
        
        # Second level
        table.enter_scope()
        table.add_symbol("inner_var", "local")
        
        assert table.get_current_depth() == 2
        
        # Exit back to first level
        table.exit_scope()
        assert table.get_current_depth() == 1
        
        # Exit back to root
        table.exit_scope()
        assert table.get_current_depth() == 0
    
    def test_resolve_in_current_scope(self):
        """Test resolving a symbol in current scope."""
        table = SymbolTable()
        table.add_symbol("x", "local")
        
        info = table.resolve("x")
        
        assert info is not None
        assert info.name == "x"
    
    def test_resolve_in_parent_scope(self):
        """Test resolving a symbol from parent scope."""
        table = SymbolTable()
        table.add_symbol("x", "global")
        
        table.enter_scope()
        info = table.resolve("x")
        
        assert info is not None
        assert info.name == "x"
        assert info.scope_depth == 0
    
    def test_resolve_not_found(self):
        """Test resolving a non-existent symbol."""
        table = SymbolTable()
        
        info = table.resolve("nonexistent")
        
        assert info is None
    
    def test_resolve_local_only(self):
        """Test resolve_local doesn't search parent."""
        table = SymbolTable()
        table.add_symbol("x", "global")
        
        table.enter_scope()
        info = table.resolve_local("x")
        
        assert info is None
    
    def test_shadowing_in_symbol_table(self):
        """Test variable shadowing across scopes."""
        table = SymbolTable()
        
        # Add x in root scope
        table.add_symbol("x", "global")
        
        # Enter new scope and shadow x
        table.enter_scope()
        table.add_symbol("x", "local")
        
        info = table.resolve("x")
        
        assert info is not None
        assert info.scope_depth == 1  # Should get the local x
    
    def test_calculate_variable_offset_current_scope(self):
        """Test calculating offset for variable in current scope."""
        table = SymbolTable()
        table.enter_scope()
        table.add_symbol("x", "local")
        table.add_symbol("y", "local")
        
        depth, index = table.calculate_variable_offset("y")
        
        assert depth == 0  # Current scope
        assert index == 1  # Second variable
    
    def test_calculate_variable_offset_parent_scope(self):
        """Test calculating offset for variable in parent scope."""
        table = SymbolTable()
        
        # Add variable in root
        table.enter_scope()
        table.add_symbol("x", "local")
        
        # Enter child scope
        table.enter_scope()
        table.add_symbol("y", "local")
        
        # Look up x from child scope
        depth, index = table.calculate_variable_offset("x")
        
        assert depth == 1  # One scope up
        assert index == 0  # First variable in that scope
    
    def test_calculate_variable_offset_not_found(self):
        """Test calculating offset for non-existent variable."""
        table = SymbolTable()
        
        with pytest.raises(ValueError, match="not found"):
            table.calculate_variable_offset("nonexistent")
    
    def test_get_scope_stack(self):
        """Test getting the scope stack."""
        table = SymbolTable()
        
        table.enter_scope()
        table.enter_scope()
        
        stack = table.get_scope_stack()
        
        assert len(stack) == 3  # Root + 2 child scopes
        assert stack[0] == table.root_scope
        assert stack[-1] == table.current_scope
    
    def test_get_all_symbols_in_scope(self):
        """Test getting all symbols in current scope."""
        table = SymbolTable()
        table.add_symbol("x", "global")
        table.add_symbol("y", "global")
        
        symbols = table.get_all_symbols_in_scope()
        
        assert len(symbols) == 2
        names = {s.name for s in symbols}
        assert names == {"x", "y"}
    
    def test_dump(self):
        """Test dumping symbol table for debugging."""
        table = SymbolTable()
        table.add_symbol("global_var", "global")
        
        table.enter_scope()
        table.add_symbol("local_var", "local")
        
        dump = table.dump()
        
        assert "global_var" in dump
        assert "local_var" in dump


class TestComplexScenarios:
    """Test complex symbol table scenarios."""
    
    def test_function_with_parameters_and_locals(self):
        """Test symbol table for a function with parameters and locals."""
        table = SymbolTable()
        
        # Function scope
        table.enter_scope()
        
        # Add parameters
        param_x = table.add_symbol("x", "parameter")
        param_y = table.add_symbol("y", "parameter")
        
        # Add locals
        local_z = table.add_symbol("z", "local")
        
        assert param_x.var_index == 0
        assert param_y.var_index == 1
        assert local_z.var_index == 2
        
        # Resolve them
        assert table.resolve("x") == param_x
        assert table.resolve("y") == param_y
        assert table.resolve("z") == local_z
    
    def test_nested_functions(self):
        """Test symbol table for nested functions."""
        table = SymbolTable()
        
        # Outer function
        table.enter_scope()
        table.add_symbol("x", "parameter")
        outer_local = table.add_symbol("y", "local")
        
        # Inner function
        table.enter_scope()
        table.add_symbol("z", "parameter")
        
        # Inner function can see outer's variables
        info_x = table.resolve("x")
        info_y = table.resolve("y")
        
        assert info_x is not None
        assert info_x.scope_depth == 1
        assert info_y == outer_local
    
    def test_let_binding_simulation(self):
        """Test symbol table for let bindings."""
        table = SymbolTable()
        
        # Outer scope
        table.add_symbol("pi", "global")
        
        # Let expression scope
        table.enter_scope()
        table.add_symbol("x", "local")
        table.add_symbol("y", "local")
        
        # Body of let can see bindings
        assert table.resolve("x") is not None
        assert table.resolve("y") is not None
        assert table.resolve("pi") is not None
        
        # Exit let scope
        table.exit_scope()
        
        # Bindings no longer visible
        assert table.resolve("x") is None
        assert table.resolve("y") is None
        assert table.resolve("pi") is not None  # Global still visible
    
    def test_recursive_function_simulation(self):
        """Test symbol table for recursive function."""
        table = SymbolTable()
        
        # Let binding for recursive function
        table.enter_scope()
        fact_binding = table.add_symbol("fact", "local")
        
        # Function scope (lambda)
        table.enter_scope()
        table.add_symbol("n", "parameter")
        
        # Inside function, can reference 'fact' (self-reference)
        info = table.resolve("fact")
        
        assert info == fact_binding
        assert info.scope_depth == 1  # From parent scope
    
    def test_multiple_sibling_scopes(self):
        """Test multiple sibling scopes (e.g., if branches)."""
        table = SymbolTable()
        
        # Outer scope
        table.add_symbol("x", "global")
        
        # Then branch
        table.enter_scope()
        table.add_symbol("then_var", "local")
        then_scope = table.current_scope
        table.exit_scope()
        
        # Else branch (sibling of then)
        table.enter_scope()
        table.add_symbol("else_var", "local")
        else_scope = table.current_scope
        table.exit_scope()
        
        # Both are children of root
        assert then_scope in table.root_scope.children
        assert else_scope in table.root_scope.children
        assert then_scope != else_scope


class TestGlobalSymbolTable:
    """Test create_global_symbol_table helper."""
    
    def test_create_with_globals(self):
        """Test creating symbol table with globals."""
        globals_dict = {
            "pi": AIFPLNumber(3.14159),
            "e": AIFPLNumber(2.71828),
        }
        
        table = create_global_symbol_table(globals_dict)
        
        assert table.resolve("pi") is not None
        assert table.resolve("e") is not None
    
    def test_globals_have_correct_type(self):
        """Test that globals have correct symbol_type."""
        globals_dict = {
            "pi": AIFPLNumber(3.14159),
        }
        
        table = create_global_symbol_table(globals_dict)
        info = table.resolve("pi")
        
        assert info is not None
        assert info.symbol_type == "global"
        assert info.scope_depth == 0
    
    def test_empty_globals(self):
        """Test creating symbol table with no globals."""
        table = create_global_symbol_table({})
        
        assert table.root_scope is not None
        assert len(table.root_scope.symbols) == 0


class TestEdgeCases:
    """Test edge cases and error conditions."""
    
    def test_same_name_different_scopes(self):
        """Test same variable name in different scopes."""
        table = SymbolTable()
        
        table.enter_scope()
        info1 = table.add_symbol("x", "local")
        table.exit_scope()
        
        table.enter_scope()
        info2 = table.add_symbol("x", "local")
        table.exit_scope()
        
        # Different scopes, same name - should have same indices and values
        assert info1.var_index == info2.var_index
        assert info1.name == info2.name
        # They're equal by value (dataclass comparison), which is correct
    
    def test_deep_nesting(self):
        """Test deeply nested scopes."""
        table = SymbolTable()
        
        # Create 10 levels of nesting
        for i in range(10):
            table.enter_scope()
            table.add_symbol(f"var{i}", "local")
        
        assert table.get_current_depth() == 10
        
        # Can resolve variables from all levels
        for i in range(10):
            info = table.resolve(f"var{i}")
            assert info is not None
        
        # Exit all scopes
        for i in range(10):
            table.exit_scope()
        
        assert table.get_current_depth() == 0
    
    def test_variable_index_resets_per_scope(self):
        """Test that variable indices reset in each scope."""
        table = SymbolTable()
        
        # First scope
        table.enter_scope()
        info1 = table.add_symbol("x", "local")
        info2 = table.add_symbol("y", "local")
        table.exit_scope()
        
        # Second scope (sibling)
        table.enter_scope()
        info3 = table.add_symbol("a", "local")
        info4 = table.add_symbol("b", "local")
        table.exit_scope()
        
        # Indices should reset
        assert info1.var_index == 0
        assert info2.var_index == 1
        assert info3.var_index == 0  # Resets
        assert info4.var_index == 1
