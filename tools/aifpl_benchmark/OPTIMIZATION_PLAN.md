# AIFPL Heavyweight Processing Optimization Plan

## Context
Planning to use AIFPL for **heavyweight data processing** - long-running computations where every microsecond counts.

## Critical Bottlenecks for Heavy Processing

### 1. 🔥 **Environment Operations** (HIGHEST IMPACT)

**Current Implementation:**
```python
def define(self, name: str, value: AIFPLValue) -> 'AIFPLEnvironment':
    new_bindings = {**self.bindings, name: value}  # ← FULL DICT COPY
    return AIFPLEnvironment(new_bindings, self.parent, self.name)
```

**Problem:** Every variable binding copies the entire dictionary.
- Function with 10 params = 10 dict copies
- Nested `let` with 5 bindings = 5 dict copies
- Deep recursion = thousands of copies

**Impact on Heavy Processing:**
- Processing 1M items with `map`: millions of environment operations
- Deep recursive algorithms: O(n) copies per level
- Large data transformations: environment overhead dominates

**Solution 1: Batch Binding** (Quick Win)
```python
def define_many(self, bindings: Dict[str, AIFPLValue]) -> 'AIFPLEnvironment':
    new_bindings = {**self.bindings, **bindings}
    return AIFPLEnvironment(new_bindings, self.parent, self.name)

# In function calls:
param_bindings = dict(zip(func.parameters, arg_values))
func_env = func.closure_environment.define_many(param_bindings)
```

**Solution 2: Persistent Data Structures** (Better)
```python
from pyrsistent import pmap

class AIFPLEnvironment:
    bindings: PMap[str, AIFPLValue]  # O(log n) updates vs O(n)
    
    def define(self, name: str, value: AIFPLValue):
        new_bindings = self.bindings.set(name, value)  # O(log n)
        return AIFPLEnvironment(new_bindings, self.parent, self.name)
```

**Expected Speedup:** 2-5x for recursive/functional code

---

### 2. 🔥 **Function Call Overhead** (HIGH IMPACT)

**Current Implementation:**
```python
def _call_lambda_function(self, func, arg_values, env, depth):
    # Check arity
    if len(arg_values) != len(func.parameters):
        # ... detailed error message construction
    
    # Create environment (with multiple copies)
    func_env = AIFPLEnvironment(...)
    for param, arg in zip(func.parameters, arg_values):
        func_env = func_env.define(param, arg)
    
    # Push call stack
    self.call_stack.push(...)
    param_bindings = {}
    for param, arg_value in zip(func.parameters, arg_values):
        param_bindings[param] = arg_value
    
    # Track call chain
    self.call_chain.append(func)
    
    # Evaluate
    result = self._evaluate_expression_with_tail_detection(...)
    
    # Cleanup
    self.call_chain.pop()
    self.call_stack.pop()
```

**Problem:** Every function call has massive overhead:
- Arity checking
- Error message building (even when not needed)
- Multiple environment copies
- Call stack management
- Call chain tracking

**Impact:** For `(map f (range 1 1000000))`, you call `f` 1M times with full overhead each time.

**Solutions:**

**A. Fast Path for Simple Functions**
```python
def _call_lambda_function_fast(self, func, arg_values):
    """Fast path for simple functions (no error checking)."""
    # Skip arity check if we know it's correct
    # Skip call stack if not debugging
    # Use batch binding
    param_bindings = dict(zip(func.parameters, arg_values))
    func_env = func.closure_environment.define_many(param_bindings)
    return self._evaluate_expression(func.body, func_env, depth)
```

**B. Inline Built-in Operations**
```python
# In higher-order functions, detect built-in ops
if isinstance(func_value, AIFPLBuiltinFunction):
    if func_value.name == '+':
        # Direct call, skip machinery
        return func_value.native_impl(arg_values, env, depth)
```

**Expected Speedup:** 2-3x for functional code with many small function calls

---

### 3. 🔥 **List Operations** (HIGH IMPACT)

**Current Implementation:**
```python
@dataclass(frozen=True)
class AIFPLList(AIFPLValue):
    elements: Tuple[AIFPLValue, ...]  # ← Immutable tuple
    
    def cons(self, element: AIFPLValue) -> 'AIFPLList':
        return AIFPLList((element,) + self.elements)  # ← Creates new tuple
    
    def append_list(self, other: 'AIFPLList') -> 'AIFPLList':
        return AIFPLList(self.elements + other.elements)  # ← Creates new tuple
```

**Problem:** Every list operation creates a new tuple.
- `cons` is O(n) - should be O(1)
- `append` is O(n+m) - could be better with persistent structures
- Building large lists is quadratic

**Impact:**
```python
(fold cons () (range 1 100000))  # O(n²) - builds list one element at a time
```

**Solution: Persistent Vector**
```python
from pyrsistent import pvector

class AIFPLList:
    elements: PVector[AIFPLValue]  # O(log n) operations
    
    def cons(self, element):
        return AIFPLList(self.elements.cons(element))  # O(log n)
```

**Expected Speedup:** 5-10x for list-heavy code

---

### 4. 🟡 **Type Checking** (MODERATE IMPACT)

**Current Pattern:**
```python
def _evaluate_expression(self, expr, env, depth):
    if isinstance(expr, (AIFPLNumber, AIFPLString, AIFPLBoolean, ...)):
        return expr
    if isinstance(expr, AIFPLSymbol):
        return env.lookup(expr.name)
    if isinstance(expr, AIFPLList):
        # ...
```

**Problem:** `isinstance()` with tuples is slower than single checks.

**Solution: Type Tags**
```python
class AIFPLValue:
    TYPE_NUMBER = 1
    TYPE_SYMBOL = 4
    # ...

def _evaluate_expression(self, expr, env, depth):
    tag = expr.type_tag()  # Integer attribute access
    if tag <= 3:  # Numbers, strings, booleans
        return expr
    if tag == 4:  # Symbol
        return env.lookup(expr.name)
```

**Expected Speedup:** 1.2-1.5x (small but consistent)

---

### 5. 🟡 **Repeated Symbol Lookups** (MODERATE IMPACT)

**Current Implementation:**
```python
def lookup(self, name: str) -> AIFPLValue:
    if name in self.bindings:
        value = self.bindings[name]
        if isinstance(value, AIFPLRecursivePlaceholder):
            return value.get_resolved_value()
        return value
    if self.parent is not None:
        return self.parent.lookup(name)  # ← Recursive chain walk
    raise AIFPLEvalError(...)
```

**Problem:** Looking up variables in nested scopes walks the chain every time.

**Impact:** In a deeply nested function that references outer variables:
```python
(let ((a 1))
  (let ((b 2))
    (let ((c 3))
      (map (lambda (x) (+ a b c x)) (range 1 1000000)))))
```
Each lambda call looks up `a`, `b`, `c` - walking the environment chain 3M times.

**Solution: Lexical Addressing**
Compile variable references to (depth, offset) pairs at parse time.
```python
# Instead of: lookup("a")
# Use: get_at_depth(2, 0)  # 2 levels up, first binding
```

**Expected Speedup:** 1.5-2x for code with many variable references

---

### 6. 🔥 **No Bytecode Compilation** (MASSIVE IMPACT - Advanced)

**Current:** Tree-walking interpreter evaluates AST directly.

**Problem:** Every node is visited, type-checked, dispatched at runtime.

**Solution: Compile to Bytecode**
```python
# Compile once:
bytecode = compile_to_bytecode(parsed_expr)

# Execute many times:
result = vm.execute(bytecode)

# Bytecode example:
[
    (LOAD_CONST, 1),
    (LOAD_CONST, 2),
    (LOAD_CONST, 3),
    (CALL_BUILTIN, '+', 3),
]
```

**Benefits:**
- No type checking during execution
- No recursive evaluation overhead
- Can optimize bytecode (constant folding, etc.)
- Enables JIT compilation later

**Expected Speedup:** 5-20x for complex computations

**Effort:** High (2-3 weeks)

---

## Recommended Implementation Order

### Phase 1: Quick Wins (1 week)
1. **Batch environment binding** - Easy, 1.5-2x speedup
2. **Type tags** - Easy, 1.2-1.5x speedup
3. **Fast path for function calls** - Moderate, 1.5-2x speedup

**Combined: ~3-5x speedup with minimal changes**

### Phase 2: Data Structures (1-2 weeks)
4. **Persistent data structures** (pyrsistent)
   - Environments: 2-3x speedup
   - Lists: 5-10x for list-heavy code
   
**Combined with Phase 1: ~5-15x speedup**

### Phase 3: Advanced (3-4 weeks)
5. **Lexical addressing** - 1.5-2x additional
6. **Bytecode compilation** - 5-10x additional

**Total potential: 25-100x speedup for heavyweight processing**

---

## Specific Optimizations for Heavy Processing

### A. Optimize Higher-Order Functions

**Current `map` implementation:**
```python
def _builtin_map_special(self, args, env, depth):
    func_expr, list_expr = args
    list_value = self._evaluate_expression(list_expr, env, depth + 1)
    
    result_elements = []
    for i, item in enumerate(list_value.elements):
        # Full function call machinery for each item
        item_result = self._call_function_with_evaluated_args(func_expr, [item], env, depth + 1)
        result_elements.append(item_result)
    
    return AIFPLList(tuple(result_elements))
```

**Optimized version:**
```python
def _builtin_map_special(self, args, env, depth):
    func_expr, list_expr = args
    list_value = self._evaluate_expression(list_expr, env, depth + 1)
    func_value = self._evaluate_expression(func_expr, env, depth + 1)
    
    # Fast path for built-ins
    if isinstance(func_value, AIFPLBuiltinFunction):
        result_elements = [
            func_value.native_impl([item], env, depth + 1)
            for item in list_value.elements
        ]
        return AIFPLList(tuple(result_elements))
    
    # Fast path for lambdas
    if isinstance(func_value, AIFPLFunction):
        result_elements = []
        for item in list_value.elements:
            # Skip call stack, use fast binding
            param_bindings = {func_value.parameters[0]: item}
            func_env = func_value.closure_environment.define_many(param_bindings)
            result = self._evaluate_expression(func_value.body, func_env, depth + 1)
            result_elements.append(result)
        return AIFPLList(tuple(result_elements))
```

### B. Optimize `fold`

**Current:** Full function call overhead per iteration.

**Optimized:** Accumulator stays in register, minimal environment allocation.

### C. Optimize `range`

**Current:** Creates list of AIFPLNumber objects.

**Optimized:** Lazy range object that generates numbers on demand (for use with `map`/`filter`).

---

## Profiling Strategy

Before implementing, profile real heavyweight workloads:

```python
import cProfile
import pstats

def profile_heavy_workload():
    aifpl = AIFPL()
    
    # Test 1: Deep recursion
    aifpl.evaluate("""
        (let ((factorial (lambda (n)
                           (if (<= n 1)
                               1
                               (* n (factorial (- n 1)))))))
          (factorial 1000))
    """)
    
    # Test 2: Large list processing
    aifpl.evaluate("(fold + 0 (map (lambda (x) (* x x)) (range 1 10000)))")
    
    # Test 3: Nested data structures
    aifpl.evaluate("""
        (let ((data (map (lambda (i) (alist ("id" i) ("value" (* i i)))) (range 1 1000))))
          (fold + 0 (map (lambda (item) (alist-get item "value")) data)))
    """)

profiler = cProfile.Profile()
profiler.enable()
profile_heavy_workload()
profiler.disable()

stats = pstats.Stats(profiler)
stats.sort_stats('cumulative')
stats.print_stats(30)
```

This will show you **exactly** where time is spent.

---

## Expected Results for Heavy Processing

| Workload | Current | Phase 1 | Phase 2 | Phase 3 |
|----------|---------|---------|---------|---------|
| Deep recursion (1000 levels) | 1.0x | 3x | 8x | 15x |
| Large list map (100k items) | 1.0x | 4x | 20x | 50x |
| Nested data (1000 alists) | 1.0x | 3x | 10x | 30x |
| Complex fold operations | 1.0x | 5x | 15x | 40x |

---

## My Recommendation

1. **Start with Phase 1** (1 week, 3-5x speedup) - Proves value quickly
2. **Profile real workloads** - See where time actually goes
3. **Implement Phase 2** if needed (2 weeks, 5-15x total)
4. **Consider Phase 3** only if you need 50-100x speedup

The biggest wins for heavyweight processing are:
- **Environment operations** (currently O(n), should be O(log n))
- **Function call overhead** (too much machinery)
- **List operations** (immutable tuples are slow for large data)

Want me to implement Phase 1 optimizations?
