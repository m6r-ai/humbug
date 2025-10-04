# AIFPL Evaluator Missing Test Coverage

## Overview

The `aifpl_evaluator.py` file currently has **93% test coverage** with **32 missing statements** and **24 partial coverage statements**. This document outlines all the specific areas that need additional test coverage to reach 100%.

## Current Coverage Stats
- **Total statements:** 515
- **Statements run:** 483
- **Missing statements:** 32
- **Partial coverage:** 24
- **Current coverage:** 93%

## Missing Coverage Areas

### 1. Deep Recursion/Stack Overflow Handling

**Lines 112-119:** Maximum depth exceeded error handling
```python
if depth > self.max_depth:
    stack_trace = self.call_stack.format_stack_trace()
    raise AIFPLEvalError(
        message=f"Expression too deeply nested (max depth: {self.max_depth})",
        context=f"Call stack:\n{stack_trace}",
        suggestion="Reduce nesting depth or increase max_depth limit",
        example="Instead of deep nesting, use let bindings: (let ((x (+ 1 2))) (+ x 3))"
    )
```

**Test needed:** Create a test that creates an expression nested deeper than the max_depth limit (default 100) to trigger this error path.

### 2. Exception Handling in Main Evaluate Method

**Lines 135-137:** AIFPLEvalError re-raising (partial coverage)
```python
except AIFPLEvalError:
    # Re-raise AIFPL errors as-is
    raise
```

**Lines 139-146:** Generic exception wrapping
```python
except Exception as e:
    # Wrap other exceptions with context
    stack_trace = self.call_stack.format_stack_trace()
    raise AIFPLEvalError(
        message=f"Unexpected error during evaluation: {e}",
        context=f"Call stack:\n{stack_trace}",
        suggestion="This is an internal error - please report this issue"
    ) from e
```

**Tests needed:** 
- Create a test that triggers a non-AIFPLEvalError exception during evaluation
- Verify that AIFPLEvalError exceptions are properly re-raised

### 3. Invalid Expression Type Handling

**Lines 216-221:** Invalid expression type error
```python
raise AIFPLEvalError(
    message=f"Invalid expression type: {type(expr).__name__}",
    received=f"Expression: {self.format_result(expr)}",
    expected="Number, string, boolean, symbol, list, or function",
    suggestion="Check that your expression is properly formatted"
)
```

**Test needed:** Create a test with an invalid/unsupported expression type that would reach this error path.

### 4. Quote Form Validation

**Lines 240-247:** Quote with wrong number of arguments
```python
if quote_list.length() != 2:
    raise AIFPLEvalError(
        message="Quote expression has wrong number of arguments",
        received=f"Got {quote_list.length() - 1} arguments: {self.format_result(quote_list)}",
        expected="Exactly 1 argument",
        example="(quote expr) or 'expr",
        suggestion="Quote takes exactly one expression to quote"
    )
```

**Test needed:** Test quote expressions with 0 arguments `(quote)` and multiple arguments `(quote a b c)`.

### 5. Lambda Form Edge Cases

**Lines 290-299:** Single parameter without parentheses handling
```python
if not isinstance(param_expr, AIFPLSymbol):
    raise AIFPLEvalError(
        message="Lambda parameter list must contain symbols",
        received=f"Parameter list: {self.format_result(param_expr)} ({param_expr.type_name()})",
        expected="List of symbols: (param1 param2 ...)",
        example="(lambda (x y z) (+ x y z))",
        suggestion="Parameters should be unquoted variable names"
    )
```

**Test needed:** Test lambda with invalid single parameter like `(lambda 123 body)` or `(lambda "param" body)`.

### 6. Let Form Validation Edge Cases

**Lines 364-371:** Non-list binding expression
```python
if not isinstance(binding_expr, AIFPLList):
    raise AIFPLEvalError(
        message="Let binding list must be a list",
        received=f"Binding list: {self.format_result(binding_expr)} ({binding_expr.type_name()})",
        expected="List of bindings: ((var1 val1) (var2 val2) ...)",
        example="(let ((x 5) (y (* x 2))) (+ x y))",
        suggestion="Wrap bindings in parentheses: ((var val) (var val) ...)"
    )
```

**Lines 375-382:** Non-list individual binding
```python
if not isinstance(binding, AIFPLList):
    raise AIFPLEvalError(
        message=f"Let binding {i+1} must be a list",
        received=f"Binding {i+1}: {self.format_result(binding)} ({binding.type_name()})",
        expected="List with variable and value: (var val)",
        example="Correct: (x 5)\nIncorrect: x or \"x\"",
        suggestion="Wrap each binding in parentheses: (variable value)"
    )
```

**Tests needed:** 
- Test let with non-list binding structure: `(let 123 body)`
- Test let with non-list individual bindings: `(let (x (y 2)) body)`

### 7. Function Call Edge Cases

**Lines 524-531:** Empty list function call
```python
if current_call.is_empty():
    raise AIFPLEvalError(
        message="Cannot call empty list",
        received="Empty list: ()",
        expected="Function call: (function-name arg1 arg2 ...)",
        example="(+ 1 2 3) or (map (lambda (x) (* x 2)) (list 1 2 3))",
        suggestion="Put function name as first element of list"
    )
```

**Test needed:** Test attempting to evaluate an empty list as a function call.

### 8. Builtin Function Error Handling

**Lines 643-648:** Invalid function type in _call_function
```python
raise AIFPLEvalError(
    message="Cannot call non-function value",
    received=f"Trying to call: {func.type_name()}",
    expected="Function (builtin or lambda)",
    suggestion="This should not happen - please report this error"
)
```

**Test needed:** This appears to be unreachable code - verify by attempting to create a scenario where a non-function reaches this point.

### 9. Tail Call Detection Edge Cases

**Lines 765-771:** Deep recursion in tail detection
```python
if depth > self.max_depth:
    stack_trace = self.call_stack.format_stack_trace()
    raise AIFPLEvalError(
        message=f"Expression too deeply nested (max depth: {self.max_depth})",
        context=f"Call stack:\n{stack_trace}",
        suggestion="Reduce nesting depth or increase max_depth limit"
    )
```

**Lines 783-794:** Symbol lookup errors in tail detection
```python
except AIFPLEvalError as e:
    # Add more context to symbol lookup errors
    stack_trace = self.call_stack.format_stack_trace()
    available_vars = env.get_available_bindings()
    raise AIFPLEvalError(
        message=f"Undefined variable: '{expr.name}'",
        context=f"Available variables: {', '.join(sorted(available_vars)[:10])}"
            "{'...' if len(available_vars) > 10 else ''}",
        suggestion=f"Check spelling or define '{expr.name}' in a let binding",
        example=f"(let (({expr.name} some-value)) ...)"
    ) from e
```

**Lines 800, 806:** Empty list and quote form in tail detection
```python
if expr.is_empty():
    return expr

if self._is_symbol_with_name(first_elem, 'quote'):
    return self._evaluate_quote_form(expr, env, depth + 1)
```

**Lines 839-844:** Invalid expression type in tail detection
```python
raise AIFPLEvalError(
    message=f"Invalid expression type: {type(expr).__name__}",
    received=f"Expression: {self.format_result(expr)}",
    expected="Number, string, boolean, symbol, list, or function",
    suggestion="Check that your expression is properly formatted"
)
```

**Tests needed:**
- Test tail call optimization with deeply nested recursive calls
- Test undefined variable access in tail call context
- Test empty list evaluation in tail call context
- Test quote form in tail call context

### 10. Higher-Order Function Edge Cases

**Lines 925-932:** Non-function in higher-order context
```python
if not isinstance(func_value, (AIFPLFunction, AIFPLBuiltinFunction)):
    raise AIFPLEvalError(
        message="Cannot call non-function value in higher-order context",
        received=f"Trying to call: {self.format_result(func_value)} ({func_value.type_name()})",
        expected="Function (builtin or lambda)",
        example="(map (lambda (x) (* x 2)) (list 1 2 3))",
        suggestion="Provide a function as the first argument to higher-order functions"
    )
```

**Lines 937-943:** Unexpected tail call in higher-order context
```python
if isinstance(result, AIFPLTailCall):
    # This shouldn't happen in higher-order contexts, but handle it gracefully
    raise AIFPLEvalError(
        message="Unexpected tail call in higher-order function context",
        context="This is an internal error",
        suggestion="Please report this issue"
    )
```

**Tests needed:**
- Test higher-order functions (map, filter, etc.) with non-function arguments
- Investigate if tail call scenario in higher-order context is reachable

### 11. Range Function Edge Cases

**Lines 1178-1194:** Range parameter validation for 3-argument form
```python
if not isinstance(start_val, AIFPLNumber):
    # Error handling for start parameter
if not isinstance(end_val, AIFPLNumber):
    # Error handling for end parameter
```

**Test needed:** Test 3-argument range form with invalid parameter types: `(range "start" 10 2)`

### 12. Integer Validation Helper

**Lines 1382-1389:** Non-integer value error
```python
if not isinstance(value, AIFPLNumber) or not value.is_integer():
    raise AIFPLEvalError(
        message=f"Function '{function_name}' requires integer arguments",
        received=f"Got: {self.format_result(value)} ({value.type_name()})",
        expected="Integer number",
        example=f"({function_name} 1 5) not ({function_name} 1.5 5)",
        suggestion="Use whole numbers without decimal points"
    )
```

**Test needed:** Test range function with non-integer values to trigger this helper method.

### 13. Builtin Function Display

**Lines 1458-1460:** Builtin function formatting
```python
if isinstance(result, AIFPLBuiltinFunction):
    # Format builtin functions
    return f"<builtin {result.name}>"
```

**Test needed:** Test that evaluating a builtin function reference (without calling it) formats correctly.

### 14. String Escaping Edge Cases

**Lines 1472-1473, 1481-1482, 1484-1485:** String escape sequences
```python
elif char == '\\':
    result.append('\\\\')
elif char == '\r':
    result.append('\\r')
elif ord(char) < 32:  # Other control characters
    result.append(f'\\u{ord(char):04x}')
```

**Tests needed:** Test string formatting with:
- Backslash characters
- Carriage return characters  
- Other control characters (ASCII < 32)

### 15. Call Chain Management

**Lines 711-712:** Call chain cleanup in lambda functions
```python
if self.call_chain and self.call_chain[-1] is func:
    self.call_chain.pop()
```

**Test needed:** Test recursive lambda function calls to ensure proper call chain management.

## Partial Coverage Areas

Several lines have partial coverage, meaning they were executed but not all conditional branches were taken. These need tests for the untaken branches:

- Line 122: Environment creation condition
- Line 189: List type checking  
- Line 284: Parameter list type checking
- Line 524: Empty call list checking
- Line 640: Function type checking in _call_function
- Line 765: Depth checking in tail detection
- Line 797: List type checking in tail detection
- Line 799: Empty list checking in tail detection
- Line 805: Quote symbol checking in tail detection
- Line 925: Function type checking in higher-order context
- Line 937: Tail call checking in higher-order context
- Line 1178, 1187: Parameter type checking in range function
- Line 1382: Integer validation checking
- Line 1458: Builtin function type checking

## Implementation Strategy

### Phase 1: Error Condition Tests
1. Create tests for all error conditions and edge cases listed above
2. Focus on triggering the specific error paths that are currently uncovered
3. Verify error messages and exception types are correct

### Phase 2: Partial Coverage Branch Tests  
1. Create tests that exercise the untaken branches in partial coverage lines
2. Ensure both true and false conditions are tested for all conditionals
3. Test edge cases around type checking and validation

### Phase 3: Integration Tests
1. Create comprehensive integration tests that exercise multiple components together
2. Test complex nested expressions that might trigger multiple code paths
3. Test error recovery and exception propagation

## Test File Recommendations

Create these test files to organize the missing coverage:

1. `test_aifpl_evaluator_errors.py` - All error condition tests
2. `test_aifpl_evaluator_edge_cases.py` - Edge cases and boundary conditions  
3. `test_aifpl_evaluator_recursion.py` - Deep recursion and tail call tests
4. `test_aifpl_evaluator_formatting.py` - Result formatting and display tests

## Success Criteria

- Achieve 100% line coverage on `aifpl_evaluator.py`
- All error conditions properly tested with expected exception types and messages
- All conditional branches covered (both true and false paths)
- Integration tests verify complex evaluation scenarios work correctly