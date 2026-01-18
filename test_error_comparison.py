#!/usr/bin/env python3
"""Compare error messages between interpreter and bytecode VM."""

import sys
sys.path.insert(0, '/Users/dave/github/m6r/humbug/src')

from aifpl.aifpl import AIFPL
from aifpl.aifpl_tokenizer import AIFPLTokenizer
from aifpl.aifpl_parser import AIFPLParser
from aifpl.aifpl_compiler import AIFPLCompiler
from aifpl.aifpl_vm import AIFPLVM
from aifpl.aifpl_evaluator import AIFPLEvaluator

def test_error(code_str, description):
    print(f"\n{'='*60}")
    print(f"Test: {description}")
    print(f"Code: {code_str}")
    print(f"{'='*60}")
    
    # Test with interpreter
    print("\n--- INTERPRETER ---")
    aifpl = AIFPL()
    try:
        aifpl.evaluate(code_str)
    except Exception as e:
        print(str(e))
    
    # Test with bytecode VM
    print("\n--- BYTECODE VM ---")
    try:
        tokens = AIFPLTokenizer().tokenize(code_str)
        ast = AIFPLParser(tokens, code_str).parse()
        code = AIFPLCompiler().compile(ast)
        
        evaluator = AIFPLEvaluator()
        vm = AIFPLVM(evaluator)
        vm.set_globals({**evaluator.CONSTANTS, **evaluator._builtin_functions})
        
        result = vm.execute(code)
    except Exception as e:
        print(str(e))

# Run tests
test_error("(/ 10 0)", "Division by zero")
test_error("undefined-var", "Undefined variable")
test_error("(first ())", "Empty list")
test_error("((lambda (x y) (+ x y)) 5)", "Function arity mismatch")
test_error('(+ 1 "hello")', "Type error in arithmetic")
test_error("(cons 1 2)", "Type error in cons")
