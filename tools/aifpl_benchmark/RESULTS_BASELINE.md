# AIFPL Baseline Performance Results

## Summary

Initial baseline measurements taken on: 2026-01-09

**Key Findings:**
- Average operation time: **1.796ms**
- Slowest: Fibonacci (15) at **41.964ms** (expected - exponential algorithm)
- Fastest: Simple Addition at **0.095ms**

## Performance Characteristics

### Hot Spots (from profiling)

1. **Environment operations** (`define`) - Called 1046 times for just 10 simple benchmarks
   - Every variable binding creates a new environment
   - Dict copying overhead

2. **Type checking** (`isinstance`) - Called 725 times
   - Happens on every node evaluation
   - Multiple checks per node

3. **Function evaluation** (`_evaluate_expression`) - Core evaluation loop
   - 142 calls (recursive)
   - Main interpreter overhead

### Category Performance

| Category | Avg Time | Notes |
|----------|----------|-------|
| Arithmetic | ~0.11ms | Fast - minimal overhead |
| Functions | ~0.12ms | Slight overhead for lambda |
| Recursion | ~15ms | Fibonacci dominates, but factorial/tail-sum reasonable |
| Lists | ~0.13ms | Small lists are fine |
| Higher-order | ~0.96ms | **Bottleneck** - function call overhead multiplied |
| Let bindings | ~0.19ms | **Bottleneck** - environment overhead visible |
| Strings | ~0.10ms | Fast |
| Alists | ~0.13ms | Fast - O(1) lookups working |
| Complex | ~1.31ms | Combines all overhead |

## Identified Bottlenecks

### 1. Higher-Order Functions (CRITICAL)
- **Map (100 elements)**: 1.071ms
- **Filter (100 elements)**: 1.042ms
- **Map + Fold Pipeline**: 1.216ms

**Analysis**: Each element processed requires full function call machinery. For 100 elements, that's 100x the overhead.

**Target**: 3-5x improvement with fast path optimization

### 2. Let Bindings (HIGH)
- **Let with Many Bindings (10)**: 0.207ms
- **Simple Let (2 bindings)**: 0.126ms

**Analysis**: ~0.04ms per binding overhead. Environment copying visible.

**Target**: 2-3x improvement with batch binding

### 3. Recursion (MODERATE)
- **Tail Recursive Sum (100)**: 2.362ms
- **Factorial (10)**: 0.407ms

**Analysis**: TCO working, but environment overhead still present. Each recursive call creates new environment.

**Target**: 2-3x improvement with batch binding + fast path

### 4. List Operations (LOW PRIORITY)
- **Cons Building (10 elements)**: 0.175ms

**Analysis**: Acceptable for small lists. Would become problematic for large data structures.

**Target**: 5-10x improvement with persistent structures (for large lists)

## Optimization Priority

Based on these results, recommended optimization order:

1. **Batch Environment Binding** - Affects let, functions, recursion
   - Expected: 2-3x improvement in let/recursion benchmarks
   - Effort: Low (1 day)

2. **Fast Path for Higher-Order Functions** - Affects map/filter/fold
   - Expected: 3-5x improvement in higher-order benchmarks
   - Effort: Moderate (2-3 days)

3. **Type Tag Dispatch** - Affects everything
   - Expected: 1.2-1.5x across-the-board improvement
   - Effort: Low (1 day)

4. **Persistent Data Structures** - Affects lists (when large)
   - Expected: 5-10x for large list operations
   - Effort: Moderate (2-3 days)
   - Priority: Lower (current lists are fast enough for small data)

## Expected Results After Phase 1

After implementing batch binding + fast path + type tags:

| Benchmark | Current | Target | Expected Speedup |
|-----------|---------|--------|------------------|
| Map (100 elements) | 1.071ms | ~0.25ms | 4x |
| Filter (100 elements) | 1.042ms | ~0.25ms | 4x |
| Fold (100 elements) | 0.269ms | ~0.10ms | 2.5x |
| Let with Many Bindings | 0.207ms | ~0.08ms | 2.5x |
| Tail Recursive Sum | 2.362ms | ~0.90ms | 2.5x |
| Map + Fold Pipeline | 1.216ms | ~0.35ms | 3.5x |

**Overall target: 3-5x improvement for heavyweight processing workloads**

## How to Use These Results

1. **Before optimizing**: Run `python benchmark.py --save pre_optimization.json`
2. **Implement optimization**
3. **After optimizing**: Run `python benchmark.py --compare pre_optimization.json`
4. **Verify improvements** in target categories
5. **Ensure no regressions** in other categories

## Next Steps

1. Implement batch environment binding
2. Re-run benchmarks and compare with this baseline
3. Verify 2-3x improvement in let/recursion benchmarks
4. Proceed to next optimization based on profiling results
