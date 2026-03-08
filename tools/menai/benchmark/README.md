# Menai Performance Benchmark Suite

Comprehensive performance testing for the Menai interpreter. Measures performance across various workload patterns to identify bottlenecks and track optimization improvements over time.

## Quick Start

```bash
# Run all benchmarks
python benchmark.py

# Run quick benchmarks only (faster iteration)
python benchmark.py --quick

# Run with profiling to see hotspots
python benchmark.py --profile

# Save baseline for future comparison
python benchmark.py --save baseline.json

# Compare current performance with baseline
python benchmark.py --compare baseline.json
```

## Benchmark Categories

### Arithmetic Operations
- Simple addition, nested operations, complex math
- Tests basic interpreter overhead and numeric operations

### Function Calls
- Lambda creation and invocation
- Multiple arguments, nested calls
- Tests function call overhead and environment management

### Recursion
- Factorial, Fibonacci, tail-recursive sum
- Tests stack management and tail call optimization
- **Critical for identifying environment/call overhead**

### List Operations
- List creation, append, reverse, cons building
- Tests immutable data structure performance
- **Critical bottleneck area for large data processing**

### Higher-Order Functions
- Map, filter, fold operations
- Pipeline compositions
- **Critical for functional programming workloads**
- Tests function call overhead at scale

### Let Bindings
- Simple, nested, many bindings, recursive
- Tests environment creation and variable lookup
- **Critical bottleneck area**

### String Operations
- String concatenation and manipulation
- Tests string handling efficiency

### Alist Operations
- Creation, get, set, merge
- Tests O(1) lookup claims
- Important for data structure workloads

### Complex Real-World Scenarios
- Data processing pipelines
- Nested transformations
- **Representative of actual heavy processing workloads**

## Output Format

### Standard Output
```
Running 30 benchmarks...
================================================================================
[1/30] Simple Addition... ✓ 0.045ms (±0.003ms)
[2/30] Nested Arithmetic... ✓ 0.123ms (±0.008ms)
...

================================================================================
BENCHMARK RESULTS
================================================================================

ARITHMETIC
--------------------------------------------------------------------------------
Benchmark                                Mean         Median       Ops/sec
--------------------------------------------------------------------------------
Simple Addition                          0.045ms      0.044ms      22222.2
Nested Arithmetic                        0.123ms      0.121ms      8130.1
...

SUMMARY
================================================================================
Total benchmarks: 30
Total time: 12.34s
Average operation time: 0.411ms

Slowest: Fibonacci (15) (45.234ms)
Fastest: Simple Addition (0.045ms)
```

### JSON Output
Results are saved as JSON for programmatic analysis:
```json
{
  "timestamp": "2024-01-15T10:30:00.123456",
  "python_version": "3.11.0",
  "menai_version": "1.0",
  "results": [
    {
      "name": "Simple Addition",
      "category": "arithmetic",
      "expression": "(+ 1 2 3 4 5)",
      "iterations": 1000,
      "total_time": 0.045,
      "mean_time": 0.000045,
      "median_time": 0.000044,
      "min_time": 0.000042,
      "max_time": 0.000051,
      "std_dev": 0.000003,
      "ops_per_sec": 22222.2
    }
  ]
}
```

## Comparison Mode

Compare current performance with a baseline:

```bash
# Create baseline before optimization
python benchmark.py --save baseline.json

# ... make optimizations ...

# Compare new performance
python benchmark.py --compare baseline.json
```

Output:
```
================================================================================
COMPARISON WITH BASELINE: baseline.json
================================================================================
Benchmark                                Current      Baseline     Change
--------------------------------------------------------------------------------
Simple Addition                          0.045ms      0.050ms      ↑ 10.0%
Map (100 elements)                       5.234ms      15.123ms     ↑ 65.4%
...

SUMMARY
================================================================================
Improvements (15):
  Map (100 elements): 65.4% faster
  Fold (100 elements): 58.2% faster
  ...

Overall: 35.2% faster on average
```

## Profiling Mode

Identify hotspots in the code:

```bash
python benchmark.py --profile
```

Shows top 30 functions by cumulative time:
```
================================================================================
PROFILING RESULTS (Top 30 functions by cumulative time)
================================================================================
   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
     5000    0.234    0.000    1.456    0.000 menai_evaluator.py:123(_evaluate_expression)
     3200    0.156    0.000    0.987    0.000 menai_environment.py:45(define)
     ...
```

## Usage in CI/CD

Track performance over time:

```bash
#!/bin/bash
# run_benchmark.sh

# Run benchmarks and save with timestamp
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
python benchmark.py --save "results_${TIMESTAMP}.json"

# Compare with main branch baseline
if [ -f "baseline_main.json" ]; then
    python benchmark.py --compare baseline_main.json
fi

# Alert if performance degrades by >10%
# ... parse JSON and check ...
```

## Interpreting Results

### What to Look For

1. **High std_dev**: Indicates inconsistent performance, may need more iterations
2. **Low ops/sec**: Identifies slow operations
3. **Category patterns**:
   - Slow "recursion" → Environment/call overhead
   - Slow "lists" → Data structure overhead
   - Slow "higher-order" → Function call overhead

### Expected Bottlenecks (Current Implementation)

Based on code analysis:

1. **Environment operations** (let bindings, function calls)
   - Every `define()` copies entire dict
   - Look at "Let with Many Bindings" and "Recursive" benchmarks

2. **List operations** (cons, append)
   - Creates new tuples on every operation
   - Look at "Cons Building" and large list benchmarks

3. **Function call overhead**
   - Full machinery for every call
   - Look at "Map (100 elements)" vs "Map (10 elements)"

4. **Higher-order functions**
   - Combines function call + list overhead
   - Look at "Map + Fold Pipeline"

### After Optimization

Track improvements in specific categories:
- Batch binding → "Let with Many Bindings" should improve
- Persistent structures → "Cons Building" should improve dramatically
- Fast path → "Map (100 elements)" should improve significantly

## Extending the Benchmark Suite

Add new benchmarks in `benchmark.py`:

```python
Benchmark(
    "Your Benchmark Name",
    "category",
    "(your menai expression)",
    iterations=100  # Adjust based on speed
),
```

Categories: arithmetic, functions, recursion, lists, higher-order, let, strings, dicts, complex

## Tips

- Use `--quick` during development for fast iteration
- Use `--profile` to find hotspots before optimizing
- Save baseline before major changes
- Run full suite before committing optimizations
- Check that optimizations don't break functionality (run tests first!)

## Performance Targets

After implementing optimizations:

| Category | Current | Target | Via |
|----------|---------|--------|-----|
| Recursion | 1.0x | 3-5x | Batch binding, fast path |
| Lists (large) | 1.0x | 5-10x | Persistent structures |
| Higher-order | 1.0x | 3-5x | Fast path, batch binding |
| Let bindings | 1.0x | 2-3x | Batch binding |
| Overall | 1.0x | 3-5x | Combined optimizations |
