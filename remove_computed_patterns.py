#!/usr/bin/env python3
"""Remove computed pattern functionality to make pattern matching purely structural."""

def remove_computed_patterns():
    """Remove the computed pattern logic from _try_match_pattern."""
    
    # Read the current file
    with open('src/aifpl/aifpl_evaluator.py', 'r') as f:
        content = f.read()
    
    # Find and replace the _try_match_pattern method to remove computed pattern logic
    start_marker = "    def _try_match_pattern("
    end_marker = "    def _try_match_list_pattern("
    
    start_pos = content.find(start_marker)
    end_pos = content.find(end_marker)
    
    if start_pos == -1 or end_pos == -1:
        raise ValueError("Could not find _try_match_pattern method boundaries")
    
    # The simplified method without computed pattern support
    new_method = '''    def _try_match_pattern(
        self,
        pattern: AIFPLValue,
        value: AIFPLValue,
        env: AIFPLEnvironment
    ) -> tuple[bool, AIFPLEnvironment] | None:
        """
        Try to match a pattern against a value.

        Args:
            pattern: Pattern to match
            value: Value to match against
            env: Current environment

        Returns:
            (True, new_env_with_bindings) if match succeeds, None if no match
        """
        # LITERAL PATTERNS - Phase 1
        if isinstance(pattern, (AIFPLNumber, AIFPLString, AIFPLBoolean)):
            if self._aifpl_equal(pattern, value):
                return (True, env)
            else:
                return None

        # VARIABLE PATTERNS - Phase 1  
        if isinstance(pattern, AIFPLSymbol):
            if pattern.name == "_":  # Wildcard - always matches, no binding
                return (True, env)
            else:
                # Variable binding - bind the symbol to the value
                new_env = env.define(pattern.name, value)
                return (True, new_env)

        # LIST PATTERNS - Phase 1 + Phase 2 (purely structural)
        if isinstance(pattern, AIFPLList):
            return self._try_match_list_pattern(pattern, value, env)

        # Pattern type not supported
        return None

'''
    
    # Replace the method
    content = content[:start_pos] + new_method + content[end_pos:]
    
    # Also remove the _is_computed_literal_pattern method since it's no longer needed
    computed_method_start = content.find("    def _is_computed_literal_pattern(")
    if computed_method_start != -1:
        # Find the end of this method (start of next method)
        next_method_start = content.find("\n    def ", computed_method_start + 1)
        if next_method_start != -1:
            # Remove the entire method including the blank line before it
            method_actual_start = content.rfind("\n", 0, computed_method_start)
            content = content[:method_actual_start] + content[next_method_start:]
        else:
            print("Warning: Could not find end of _is_computed_literal_pattern method")
    
    # Write the fixed content back
    with open('src/aifpl/aifpl_evaluator.py', 'w') as f:
        f.write(content)
    
    print("Removed computed pattern functionality - pattern matching is now purely structural!")

if __name__ == "__main__":
    remove_computed_patterns()