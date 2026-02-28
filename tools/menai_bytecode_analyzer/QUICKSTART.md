# Quick Start Guide

## 5-Minute Setup

1. **Navigate to the tool:**
   ```bash
   cd tools/menai_bytecode_analyzer
   ```

2. **Test a simple expression:**
   ```bash
   python bytecode_analyzer.py --expr "(+ (* 2 3) (* 4 5))"
   ```

3. **View the results** - You'll see instruction counts before and after optimization.

## Common Workflows

### Workflow 1: Quick Check

Test if an optimization is working:

```bash
python bytecode_analyzer.py --expr "(+ 1 2)"
```

Look for: `Instructions Eliminated: X`

### Workflow 2: Analyze Test Suite

Run the full test suite:

```bash
python bytecode_analyzer.py --file test_cases.menai
```

### Workflow 3: Generate Report

Create a comprehensive report:

```bash
python generate_report.py --test-file test_cases.menai --output optimization_report.md
```

Open `optimization_report.md` to see:
- Summary statistics
- Per-test-case breakdown
- Visualization of improvements
- Conclusions

### Workflow 4: Compare Specific Code

Create a file with your code:

```bash
cat > mycode.menai << 'EOF'
(let ((x (* 10 20))
      (y (+ 5 5)))
  (+ x y))
EOF

python bytecode_analyzer.py --file mycode.menai --disassemble
```

This shows the actual bytecode instructions.

### Workflow 5: Batch Analysis

Analyze multiple files:

```bash
python bytecode_analyzer.py --batch examples/*.menai
```

## Understanding the Output

### Key Metrics

**Instructions Eliminated**: Number of bytecode instructions removed by optimization
- Higher is better
- 20%+ is excellent
- 10-20% is good
- <10% is moderate

**Constants Eliminated**: Number of constant values that were folded
- Each eliminated constant saves memory and load time

**Load Ops Eliminated**: Number of LOAD_CONST instructions removed
- Direct indicator of optimization effectiveness

### Example Output

```
============================================================
                 BYTECODE COMPARISON REPORT
============================================================

                          SUMMARY
------------------------------------------------------------
  Instructions Eliminated:      5 ( 38.5%)
  Constants Eliminated:         2
  Load Ops Eliminated:          4
  Arithmetic Ops Eliminated:    2
```

This means:
- ✅ 38.5% fewer instructions (excellent!)
- ✅ 2 constants computed at compile time
- ✅ 4 fewer loads from constant pool
- ✅ 2 arithmetic operations eliminated

## What to Measure

### Before Implementing Optimization

1. Run analyzer on test suite:
   ```bash
   python generate_report.py --output before_optimization.md
   ```

2. Note the baseline metrics

### After Implementing Optimization

1. Run analyzer again:
   ```bash
   python generate_report.py --output after_optimization.md
   ```

2. Compare the reports:
   - Did instruction count decrease?
   - Which test cases improved most?
   - Are there any regressions?

### Validating Correctness

Always verify optimizations don't change behavior:

```bash
# Test that results are identical
python -c "
from menai.menai import Menai
menai = Menai()
expr = '(+ (* 2 3) (* 4 5))'
result = menai.evaluate(expr)
print(f'Result: {result}')
assert result == 26, 'Optimization changed behavior!'
print('✓ Correctness verified')
"
```

## Integration with Benchmarking

Combine bytecode analysis with runtime benchmarks:

```bash
# 1. Analyze bytecode
python bytecode_analyzer.py --file mycode.menai --json > bytecode.json

# 2. Benchmark performance
cd ../menai_benchmark
python benchmark.py --file ../menai_bytecode_analyzer/mycode.menai > perf.txt

# 3. Compare
# - X% bytecode reduction → Y% performance gain
# - Correlation shows optimization effectiveness
```

## Troubleshooting

### "No optimization detected"

If you see 0% instruction reduction:
- Check that the code actually has constant expressions
- Verify optimization flag is enabled in compiler (when implemented)
- Some code patterns may not benefit from current optimizations

### "Unexpected increase in instructions"

If optimized code has MORE instructions:
- This is a regression - the optimization is buggy
- File a bug report with the test case
- Disable that optimization until fixed

### "Can't import menai"

Make sure you're running from the correct directory:
```bash
cd tools/menai_bytecode_analyzer
python bytecode_analyzer.py ...
```

## Next Steps

1. **Baseline**: Run analyzer on current codebase
2. **Implement**: Add constant folding optimization
3. **Measure**: Run analyzer again
4. **Validate**: Ensure 10-20% instruction reduction
5. **Benchmark**: Confirm runtime improvement
6. **Document**: Save reports for future reference

## Tips

- Start with simple test cases
- Focus on high-impact optimizations first
- Always validate correctness
- Track metrics over time
- Celebrate wins! 🎉

## Questions?

See the full README.md for detailed documentation.
