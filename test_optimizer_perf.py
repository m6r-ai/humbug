#!/usr/bin/env python3
"""Quick test to verify optimizer still works correctly after optimization."""

from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_desugarer import AIFPLDesugarer
from aifpl.aifpl_ast_optimizer import ASTOptimizer

def test_constant_folding():
    """Test that constant folding still works."""
    
    test_cases = [
        ("(+ 1 2)", "3"),
        ("(* 2 3)", "6"),
        ("(+ (* 2 3) (* 4 5))", "26"),
        ("(< 5 10)", "#t"),
        ("(and #t #t)", "#t"),
        ("(if #t 42 99)", "42"),
        ("(if #f 42 99)", "99"),
    ]
    
    parser = AIFPLParser()
    desugarer = AIFPLDesugarer()
    optimizer = ASTOptimizer()
    
    for expr_str, expected in test_cases:
        print(f"Testing: {expr_str}")
        
        # Parse and desugar
        parsed = parser.parse(expr_str)
        desugared = desugarer.desugar(parsed)
        
        # Optimize
        optimized = optimizer.optimize(desugared)
        
        # Check result
        result = optimized.describe()
        print(f"  Result: {result}")
        print(f"  Expected: {expected}")
        
        if result == expected:
            print("  ✓ PASS")
        else:
            print("  ✗ FAIL")
        print()

if __name__ == "__main__":
    test_constant_folding()
