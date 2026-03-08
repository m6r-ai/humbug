# Menai Bytecode Analyzer

A data-driven tool for quantifying the impact of compiler optimizations on Menai bytecode generation.

## Purpose

This tool helps measure and visualize the improvements from compiler optimizations by:

1. **Comparing bytecode** - Shows before/after bytecode with and without optimizations
2. **Quantifying improvements** - Measures instruction reduction, constant elimination, etc.
3. **Identifying opportunities** - Highlights where optimizations have the most impact
4. **Tracking progress** - Provides metrics to track optimization improvements over time

## Features

- **Instruction Analysis** - Count and categorize all bytecode instructions
- **Resource Tracking** - Monitor constants, names, code objects, and local variables
- **Complexity Metrics** - Calculate cyclomatic complexity and other code metrics
- **Comparison Reports** - Side-by-side comparison of optimized vs unoptimized code
- **JSON Output** - Machine-readable format for automated analysis
- **Batch Processing** - Analyze multiple files and generate summary statistics
- **Disassembly View** - Show actual bytecode instructions for detailed inspection

## Installation

No installation needed - just run from the tools directory:

```bash
cd tools/menai_bytecode_analyzer
python bytecode_analyzer.py --help
```

## Usage

### Analyze a Single Expression

```bash
python bytecode_analyzer.py --expr "(+ (* 2 3) (* 4 5))"
```

**Output:**
```
============================================================
                 BYTECODE COMPARISON REPORT
============================================================

                          SUMMARY
------------------------------------------------------------
  Instructions Eliminated:      3 ( 25.0%)
  Constants Eliminated:         2
  Load Ops Eliminated:          2
  Arithmetic Ops Eliminated:    1

[... detailed statistics ...]
```

### Analyze a File

```bash
python bytecode_analyzer.py --file examples/config.menai
```

### Show Disassembled Bytecode

```bash
python bytecode_analyzer.py --expr "(+ 1 2)" --disassemble
```

This shows the actual bytecode instructions before and after optimization.

### Generate JSON Report

```bash
python bytecode_analyzer.py --expr "(+ 1 2)" --json > report.json
```

**JSON Format:**
```json
{
  "unoptimized": {
    "total_instructions": 8,
    "constants": 2,
    "instruction_counts": {
      "LOAD_CONST": 2,
      "CALL_BUILTIN": 1,
      ...
    }
  },
  "optimized": {
    "total_instructions": 5,
    "constants": 1,
    ...
  },
  "improvements": {
    "instruction_reduction": 3,
    "instruction_reduction_pct": 37.5,
    ...
  }
}
```

### Batch Analysis

Analyze multiple files and generate a summary:

```bash
python bytecode_analyzer.py --batch examples/*.menai
```

**Output:**
```
============================================================
                  BATCH ANALYSIS SUMMARY
============================================================

Analyzed 10 files:

  config.menai
    Instructions:   45 →   32 ( 28.9% reduction)
  math.menai
    Instructions:   67 →   51 ( 23.9% reduction)
  ...

Overall:
  Instructions:  450 →  360 ( 20.0% reduction)
```

## Test Cases

The `test_cases.menai` file contains redictic examples of code patterns where constant folding provides benefits:

1. Simple arithmetic constants
2. Nested arithmetic
3. Mathematical constants
4. Unit conversions
5. Bit operations
6. Boolean logic
7. Configuration objects
8. Algorithm parameters
9. Range calculations
10. Comparison operations

Run the test suite:

```bash
python bytecode_analyzer.py --file test_cases.menai
```

## Metrics Explained

### Instruction Count
Total number of bytecode instructions. Lower is better - fewer instructions mean faster execution.

### Constant Count
Number of constants in the constant pool. Constant folding reduces this by pre-computing values.

### Load Operations
Instructions that load values onto the stack (LOAD_CONST, LOAD_VAR, etc.). Optimizations reduce redundant loads.

### Arithmetic Operations
Calls to arithmetic builtins (+, -, *, /, etc.). Constant folding eliminates these when operands are known.

### Cyclomatic Complexity
Measure of code complexity based on control flow branches. Lower complexity means simpler code.

### Instruction Reduction %
Percentage decrease in total instructions. This is the primary metric for optimization impact.

## Integration with Benchmarking

Use this tool alongside `menai_benchmark` to correlate bytecode improvements with runtime performance:

1. **Before optimization:**
   ```bash
   python bytecode_analyzer.py --file mycode.menai --json > before.json
   python ../menai_benchmark/benchmark.py --file mycode.menai > before_perf.txt
   ```

2. **After optimization:**
   ```bash
   python bytecode_analyzer.py --file mycode.menai --json > after.json
   python ../menai_benchmark/benchmark.py --file mycode.menai > after_perf.txt
   ```

3. **Compare:**
   - Bytecode reduction (from JSON)
   - Runtime improvement (from benchmark)
   - Correlation between bytecode size and performance

## Expected Results

Based on typical Menai code patterns:

| Code Type | Expected Instruction Reduction | Expected Performance Gain |
|-----------|-------------------------------|---------------------------|
| Configuration-heavy | 20-30% | 15-25% |
| Mathematical | 15-25% | 10-20% |
| Data processing | 10-15% | 5-15% |
| Business logic | 5-10% | 3-8% |

## Development Workflow

When implementing new optimizations:

1. **Baseline** - Run analyzer on test cases before implementing
2. **Implement** - Add optimization to compiler
3. **Measure** - Run analyzer again to see impact
4. **Validate** - Ensure instruction reduction matches expectations
5. **Benchmark** - Confirm runtime performance improvement
6. **Document** - Record metrics for future reference

## Future Enhancements

Potential additions to this tool:

- [ ] Visual graphs of bytecode size over time
- [ ] Heatmap showing which optimizations help most
- [ ] Regression detection (optimizations making things worse)
- [ ] Integration with CI/CD for automated tracking
- [ ] Comparison of multiple optimization strategies
- [ ] Cost/benefit analysis (compilation time vs runtime gain)

## Examples

### Example 1: Simple Constant Folding

**Input:**
```menai
(+ (* 2 3) (* 4 5))
```

**Without optimization:**
```
LOAD_CONST 2
LOAD_CONST 3
CALL_BUILTIN * 2
LOAD_CONST 4
LOAD_CONST 5
CALL_BUILTIN * 2
CALL_BUILTIN + 2
RETURN
```
8 instructions, 4 constants

**With optimization:**
```
LOAD_CONST 26
RETURN
```
2 instructions, 1 constant

**Improvement:** 75% instruction reduction

### Example 2: Configuration Constants

**Input:**
```menai
(let ((buffer-size (* 1024 1024))
      (timeout-ms (* 1000 30)))
  (+ buffer-size timeout-ms))
```

**Without optimization:** ~15 instructions
**With optimization:** ~5 instructions
**Improvement:** ~67% reduction

## Contributing

When adding new optimization passes:

1. Add test cases to `test_cases.menai`
2. Run analyzer before and after
3. Document expected improvements
4. Update this README with results

## License

Same as Menai project.
