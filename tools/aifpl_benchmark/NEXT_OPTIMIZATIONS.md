# Next Optimization Steps - Prioritized Action Plan

## Current Status

✅ **Phase 1 Partially Complete:**
- Batch environment binding (`define_many`) - DONE
- Type tags for fast dispatch - DONE  
- Fast paths for map/filter/fold - DONE

📊 **Results:** 2.0-2.4x speedup for higher-order functions, 1.3x for recursion/let

🎯 **Target:** 3-5x overall speedup

## Top 3 Bottlenecks (from profiling)

### 1. 🔥 Debug Overhead in Recursion (17% of runtime)
**Problem:** Building repr strings for call stack on every function call
**Fix Time:** 2 days
**Impact:** 1.3-1.5x for recursive workloads

### 2. 🔥 Dependency Analysis in Let (50% of let eval time)
**Problem:** Running Tarjan's SCC algorithm even for simple non-recursive bindings
**Fix Time:** 1 day
**Impact:** 1.5-2x for let bindings specifically

### 3. 🔥 Environment Lookup (10-12% of runtime)
**Problem:** Walking environment chain on every variable reference
**Fix Time:** 2 days
**Impact:** 1.2-1.3x across all workloads

## Recommended Implementation Path

### 🚀 Quick Wins (1 week) - Get to 3-4x

#### Day 1-2: Remove Debug Overhead
```python
# In aifpl_evaluator.py
class AIFPLEvaluator:
    def __init__(self, debug_mode: bool = False, ...):
        self.debug_mode = debug_mode
        
    def _call_lambda_function(self, func, arg_values, env, depth):
        if not self.debug_mode:
            # Fast path: skip call stack, repr, call chain
            param_bindings = dict(zip(func.parameters, arg_values))
            func_env = func.closure_environment.define_many(param_bindings)
            return self._evaluate_expression(func.body, func_env, depth)
        
        # Slow path with full debugging (existing code)
        self.call_stack.push(...)
        self.call_chain.append(func)
        try:
            # ... existing code ...
        finally:
            self.call_chain.pop()
            self.call_stack.pop()
```

**Files to modify:**
- `src/aifpl/aifpl_evaluator.py`: Add debug_mode parameter and fast path
- `src/aifpl/aifpl.py`: Pass debug_mode=False by default

**Expected:** Tail Recursive Sum: 1.870ms → 1.3ms (1.4x)

---

#### Day 3: Skip Dependency Analysis for Simple Let

```python
# In aifpl_evaluator.py
def _evaluate_let_form(self, let_list, env, depth, in_tail_position=False):
    # ... parse bindings ...
    
    # Quick check: does any binding reference itself or later bindings?
    has_recursion = self._has_recursive_bindings(bindings)
    
    if not has_recursion:
        # Fast path: simple sequential evaluation
        current_env = env
        for name, expr in bindings:
            value = self._evaluate_expression(expr, current_env, depth + 1)
            current_env = current_env.define(name, value)
        body_result = self._evaluate_expression(body, current_env, depth)
        return body_result
    
    # Slow path: full dependency analysis (existing code)
    analyzer = AIFPLDependencyAnalyzer()
    binding_groups = analyzer.analyze_let_bindings(bindings)
    # ... existing code ...

def _has_recursive_bindings(self, bindings):
    """Quick check if bindings are recursive."""
    bound_names = {name for name, _ in bindings}
    for name, expr in bindings:
        free_vars = self._get_free_variables_quick(expr)
        if bound_names & free_vars:  # Any overlap?
            return True
    return False

def _get_free_variables_quick(self, expr):
    """Quick free variable extraction without full analysis."""
    if isinstance(expr, AIFPLSymbol):
        return {expr.name}
    if isinstance(expr, AIFPLList):
        # Skip lambda/let bodies (they create new scope)
        if expr.elements and isinstance(expr.elements[0], AIFPLSymbol):
            if expr.elements[0].name in ('lambda', 'let'):
                return set()
        # Recursively collect from all elements
        result = set()
        for elem in expr.elements:
            result |= self._get_free_variables_quick(elem)
        return result
    return set()
```

**Files to modify:**
- `src/aifpl/aifpl_evaluator.py`: Add fast path for non-recursive let

**Expected:** Let with Many Bindings: 0.159ms → 0.10ms (1.6x)

---

#### Day 4-5: Add Environment Lookup Cache

```python
# In aifpl_environment.py
@dataclass(frozen=True)
class AIFPLEnvironment:
    bindings: Dict[str, AIFPLValue] = field(default_factory=dict)
    parent: 'AIFPLEnvironment | None' = None
    name: str = "anonymous"
    _cache: Dict[str, AIFPLValue] = field(default_factory=dict, init=False, repr=False, compare=False)
    
    def lookup(self, name: str) -> AIFPLValue:
        # Check cache first
        if name in self._cache:
            return self._cache[name]
        
        # Original lookup logic
        if name in self.bindings:
            value = self.bindings[name]
            if isinstance(value, AIFPLRecursivePlaceholder):
                value = value.get_resolved_value()
            # Cache the result
            object.__setattr__(self, '_cache', {**self._cache, name: value})
            return value
        
        if self.parent is not None:
            value = self.parent.lookup(name)
            # Cache the result
            object.__setattr__(self, '_cache', {**self._cache, name: value})
            return value
        
        raise AIFPLEvalError(f"Undefined variable: '{name}'...")
    
    def define(self, name: str, value: AIFPLValue) -> 'AIFPLEnvironment':
        new_bindings = {**self.bindings, name: value}
        # Don't copy cache to new environment (it will build its own)
        return AIFPLEnvironment(new_bindings, self.parent, self.name)
```

**Files to modify:**
- `src/aifpl/aifpl_environment.py`: Add caching to lookup

**Expected:** 10-15% improvement across all workloads (1.1-1.15x)

---

### 📊 Expected Results After Quick Wins

| Workload | Current | After Quick Wins | Total vs Baseline |
|----------|---------|------------------|-------------------|
| Map (100) | 0.442ms | 0.35ms | **3.0x** |
| Filter (100) | 0.445ms | 0.35ms | **3.0x** |
| Fold (100) | 0.137ms | 0.11ms | **2.4x** |
| Let (10 bindings) | 0.159ms | 0.10ms | **2.1x** |
| Tail Recursion | 1.870ms | 1.15ms | **2.0x** |
| Map+Fold | 0.530ms | 0.40ms | **3.0x** |

**Average: 2.5-3x vs baseline** (up from 1.8x currently)

---

## Phase 2: Persistent Data Structures (2-3 weeks) - Get to 5-10x

### Week 1: Environment with pyrsistent

```python
from pyrsistent import pmap

@dataclass(frozen=True)
class AIFPLEnvironment:
    bindings: PMap[str, AIFPLValue] = field(default_factory=pmap)
    
    def define(self, name: str, value: AIFPLValue) -> 'AIFPLEnvironment':
        # O(log n) instead of O(n)
        new_bindings = self.bindings.set(name, value)
        return AIFPLEnvironment(new_bindings, self.parent, self.name)
    
    def define_many(self, new_bindings: Dict[str, AIFPLValue]) -> 'AIFPLEnvironment':
        # O(m log n) instead of O(n + m)
        result = self.bindings
        for k, v in new_bindings.items():
            result = result.set(k, v)
        return AIFPLEnvironment(result, self.parent, self.name)
```

**Expected:** 2-3x for environment-heavy workloads

---

### Week 2-3: Lists with pyrsistent

```python
from pyrsistent import pvector

@dataclass(frozen=True)
class AIFPLList(AIFPLValue):
    elements: PVector[AIFPLValue] = field(default_factory=pvector)
    
    def cons(self, element: AIFPLValue) -> 'AIFPLList':
        # O(log n) instead of O(n)
        return AIFPLList(self.elements.cons(element))
    
    def append_list(self, other: 'AIFPLList') -> 'AIFPLList':
        # O(log n) instead of O(n + m)
        return AIFPLList(self.elements.extend(other.elements))
```

**Expected:** 5-10x for large list operations

---

## Testing Strategy

After each optimization:

```bash
# Run benchmarks and compare
python benchmark.py --save after_optimization.json
python benchmark.py --compare final_optimized.json

# Ensure no regressions
cd ../../src/aifpl
pytest

# Profile to verify improvement
cd ../../tools/aifpl_benchmark
python profile_detailed.py > profile_after.txt
```

---

## Priority Decision Matrix

| Optimization | Effort | Impact | Priority |
|--------------|--------|--------|----------|
| Remove debug overhead | 2 days | 1.3x recursion | **HIGH** |
| Skip let analysis | 1 day | 1.5x let | **HIGH** |
| Environment cache | 2 days | 1.15x all | **MEDIUM** |
| Pyrsistent env | 1 week | 2-3x env ops | **MEDIUM** |
| Pyrsistent lists | 1 week | 5-10x lists | **LOW** (only for large lists) |

---

## Recommended Timeline

**This Week (5 days):**
- ✅ Day 1-2: Remove debug overhead
- ✅ Day 3: Skip let dependency analysis  
- ✅ Day 4-5: Environment lookup cache

**Next Week (5 days):**
- Integrate pyrsistent for environments
- Update all environment operations
- Benchmark and verify 4-5x total improvement

**Week After (optional):**
- Pyrsistent for lists (if needed for large data)
- Target: 8-10x for list-heavy workloads

---

## Success Criteria

**Minimum (end of this week):**
- [ ] Map (100): 0.35ms or better (3x vs baseline)
- [ ] Tail Recursion: 1.2ms or better (2x vs baseline)
- [ ] Let (10): 0.10ms or better (2x vs baseline)
- [ ] All tests passing
- [ ] No performance regressions in other benchmarks

**Stretch (end of next week):**
- [ ] Map (100): 0.25ms or better (4x vs baseline)
- [ ] Overall average: 4-5x vs baseline
- [ ] Pyrsistent integration complete

---

## Questions to Answer

1. **Do we need debug mode in production?**
   - If no → Remove all debug overhead permanently
   - If yes → Add flag and fast path

2. **How common are recursive let bindings?**
   - If rare → Simple check is sufficient
   - If common → Keep full analysis

3. **What's the target use case?**
   - Small data (< 100 elements) → Quick wins sufficient
   - Large data (> 1000 elements) → Need Phase 2

4. **Performance vs maintainability?**
   - Maximum speed → All optimizations
   - Code clarity → Just quick wins

---

## Next Action

**Recommended:** Start with Day 1-2 (remove debug overhead) as it's:
- High impact (1.3x for recursion)
- Low risk (doesn't change semantics)
- Easy to test (just add flag)
- Reversible (can toggle debug mode)

Would you like me to implement this first optimization?
