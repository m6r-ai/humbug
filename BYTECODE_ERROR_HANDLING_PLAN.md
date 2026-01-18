# AIFPL Bytecode Error Handling - Implementation Plan

## Goal
Achieve error message parity between the bytecode VM and the tree-walking interpreter, with identical error messages wherever possible.

## Current State Analysis

### Interpreter Error Handling Features
1. **Structured error messages** using `AIFPLEvalError` with:
   - `message`: Core error description
   - `context`: Additional context information
   - `expected`: What was expected
   - `received`: What was actually received
   - `suggestion`: Suggestion for fixing the error
   - `example`: Example of correct usage

2. **Detailed error messages for**:
   - Undefined variables (with available bindings)
   - Unknown functions (with suggestions)
   - Type mismatches (with type names)
   - Arity mismatches (with parameter info)
   - Division by zero
   - Index out of range
   - Empty list operations
   - Invalid conversions
   - Higher-order function errors (with element context)

3. **Error context tracking**:
   - Call stack tracking
   - Current expression context
   - Binding availability
   - Function name and parameters

### Current VM Error Handling Gaps
1. **Generic error messages**: Most errors are simple strings without context
2. **No call stack tracking**: No frame information in errors
3. **Missing type information**: Errors don't show what types were received
4. **No suggestions**: No helpful hints or examples
5. **No arity details**: Function arity errors don't show expected vs received
6. **No element context**: Higher-order function errors don't show which element failed

## Implementation Strategy

### Phase 1: Infrastructure (Foundation)
- [ ] Add call stack tracking to VM
- [ ] Add expression context tracking
- [ ] Add helper methods for formatting values
- [ ] Add helper methods for creating detailed errors

### Phase 2: Core Operations (High Priority)
- [ ] Arithmetic operations (+, -, *, /, //, %, **)
- [ ] Comparison operations (=, !=, <, >, <=, >=)
- [ ] Division by zero errors
- [ ] Type mismatch errors with type names

### Phase 3: Variables and Functions (High Priority)
- [ ] Undefined variable errors (with available bindings)
- [ ] Unknown function errors (with suggestions)
- [ ] Function arity mismatch errors (with parameter details)
- [ ] Non-function call errors

### Phase 4: List Operations (Medium Priority)
- [ ] Empty list access errors (first, rest, last)
- [ ] List type errors (cons, append, etc.)
- [ ] Index out of range errors (list-ref)
- [ ] List operation arity errors

### Phase 5: String Operations (Medium Priority)
- [ ] String type errors
- [ ] String index out of range
- [ ] String conversion errors
- [ ] String operation arity errors

### Phase 6: Higher-Order Functions (Medium Priority)
- [ ] Map/filter/fold errors with element context
- [ ] Predicate return type errors
- [ ] Function argument type errors

### Phase 7: Alist Operations (Low Priority)
- [ ] Alist type errors
- [ ] Key not found errors
- [ ] Alist operation arity errors

### Phase 8: Special Forms (Low Priority)
- [ ] If condition type errors
- [ ] Let binding errors
- [ ] Lambda errors

## Detailed Error Message Specifications

### 1. Arithmetic Operations

#### Current VM Error:
```python
raise AIFPLEvalError("+ requires numbers, got {type(arg).__name__}")
```

#### Target Interpreter Error:
```python
raise AIFPLEvalError(
    message="Arithmetic operation requires numbers",
    received=f"Argument {i+1}: {self.format_result(arg)} ({arg.type_name()})",
    expected="Number (integer, float, or complex)",
    example="(+ 1 2 3) or (+ 1.5 2.5)",
    suggestion="All arguments to arithmetic operations must be numbers"
)
```

### 2. Undefined Variable

#### Current VM Error:
```python
raise AIFPLEvalError(f"Undefined global variable: '{name}'")
```

#### Target Interpreter Error:
```python
available_vars = env.get_available_bindings()
raise AIFPLEvalError(
    message=f"Undefined variable: '{name}'",
    context=f"Available variables: {', '.join(sorted(available_vars)[:10])}...",
    suggestion=f"Check spelling or define '{name}' in a let binding",
    example=f"(let (({name} some-value)) ...)"
)
```

### 3. Function Arity Mismatch

#### Current VM Error:
```python
raise AIFPLEvalError(f"Function expects {code.param_count} arguments, got {len(args)}")
```

#### Target Interpreter Error:
```python
param_list = ", ".join(func.parameters) if func.parameters else "(no parameters)"
arg_list = ", ".join(self.format_result(arg) for arg in args) if args else "(no arguments)"

raise AIFPLEvalError(
    message=f"Function '{func.name}' expects {len(func.parameters)} arguments, got {len(args)}",
    received=f"Arguments provided: {arg_list}",
    expected=f"Parameters expected: {param_list}",
    example=(f"({func.name} {' '.join(['arg' + str(i+1) for i in range(len(func.parameters))])})"
        if func.parameters else f"({func.name})"),
    suggestion=f"Provide exactly {len(func.parameters)} argument{'s' if len(func.parameters) != 1 else ''}"
)
```

### 4. Division by Zero

#### Current VM Error:
```python
raise AIFPLEvalError("Division by zero")
```

#### Target Interpreter Error:
```python
raise AIFPLEvalError(
    message="Division by zero",
    received=f"Attempting to divide {self.format_result(a)} by 0",
    expected="Non-zero divisor",
    example="(/ 10 2) not (/ 10 0)",
    suggestion="Check that divisor is not zero"
)
```

### 5. Empty List Access

#### Current VM Error:
```python
raise AIFPLEvalError("first called on empty list")
```

#### Target Interpreter Error:
```python
raise AIFPLEvalError(
    message="Cannot get first element of empty list",
    received="Empty list: ()",
    expected="Non-empty list",
    example="(first (list 1 2 3)) → 1",
    suggestion="Check that list is not empty before calling first"
)
```

### 6. Higher-Order Function Errors

#### Current VM Error:
```python
raise AIFPLEvalError("filter predicate must return boolean")
```

#### Target Interpreter Error:
```python
raise AIFPLEvalError(
    message=f"Filter predicate must return boolean at element {i+1}",
    received=f"Element {i+1}: {self.format_result(item)}, Predicate returned: {self.format_result(pred_result)} ({pred_result.type_name()})",
    expected="Boolean value (#t or #f)",
    example="(filter (lambda (x) (> x 0)) (list -1 2 -3 4))",
    suggestion="Predicate function should use comparison operators"
)
```

## Implementation Checklist

### Helper Methods to Add
- [ ] `_format_result(value: AIFPLValue) -> str` - Format value for error messages
- [ ] `_get_available_globals() -> List[str]` - Get list of available global names
- [ ] `_get_function_name(func: AIFPLValue) -> str` - Get function name for errors
- [ ] `_create_call_stack_trace() -> str` - Format call stack for errors

### VM Class Enhancements
- [ ] Add `call_stack` tracking (list of frame info)
- [ ] Add `current_expression` tracking
- [ ] Add `message_builder` instance

### Bytecode Enhancements
- [ ] Add source position tracking to instructions (optional, for future)
- [ ] Add function name tracking to code objects

## Testing Strategy

1. **Create error test suite for bytecode**:
   - Copy `test_errors.py` structure
   - Create `test_bytecode_errors.py`
   - Test each error category

2. **Compare error messages**:
   - Run same error-triggering code through both interpreter and VM
   - Assert error messages match (or are equivalent)

3. **Test error message components**:
   - Verify `message` field matches
   - Verify `context` is present and helpful
   - Verify `suggestion` is present
   - Verify `example` is present

## Success Criteria

1. ✅ All error tests from `test_errors.py` pass with bytecode VM
2. ✅ Error messages contain same information as interpreter
3. ✅ Error messages are equally helpful for debugging
4. ✅ No regression in existing bytecode tests
5. ✅ Call stack information is available in errors

## Notes

- Some error messages may need minor differences due to VM architecture
- Focus on information quality over exact string matching
- Prioritize errors that users are most likely to encounter
- Consider adding VM-specific error context (e.g., instruction pointer, frame depth)

## Timeline Estimate

- Phase 1 (Infrastructure): 2-3 hours
- Phase 2 (Core Operations): 2-3 hours
- Phase 3 (Variables/Functions): 2-3 hours
- Phase 4 (Lists): 1-2 hours
- Phase 5 (Strings): 1-2 hours
- Phase 6 (Higher-Order): 2-3 hours
- Phase 7 (Alists): 1 hour
- Phase 8 (Special Forms): 1-2 hours
- Testing & Refinement: 2-3 hours

**Total: 14-22 hours**

---

*Document created: 2026-01-17*
*Status: Planning*
