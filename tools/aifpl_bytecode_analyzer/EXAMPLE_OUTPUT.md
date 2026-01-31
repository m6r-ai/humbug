# Example Output from Bytecode Analyzer

This document shows what the tool outputs look like, so you know what to expect.

## Example 1: Simple Constant Folding

**Input:**
```aifpl
(+ (* 2 3) (* 4 5))
```

**Command:**
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
  Instructions Eliminated:      5 ( 62.5%)
  Constants Eliminated:         4
  Load Ops Eliminated:          4
  Arithmetic Ops Eliminated:    2

============================================================
                   UNOPTIMIZED BYTECODE
============================================================

Overall Statistics:
  Total Instructions:           8
  Constants:                    4
  Names:                        1
  Code Objects:                 0
  Max Locals:                   0
  Cyclomatic Complexity:        1

Instruction Breakdown:
  Load Operations:              4
  Store Operations:             0
  Arithmetic Operations:        3
  Control Flow:                 0
  Function Operations:          3

Instruction Frequency:
  LOAD_CONST                    4
  CALL_BUILTIN                  3
  RETURN                        1

============================================================
                    OPTIMIZED BYTECODE
============================================================

Overall Statistics:
  Total Instructions:           3
  Constants:                    0
  Names:                        0
  Code Objects:                 0
  Max Locals:                   0
  Cyclomatic Complexity:        1

Instruction Breakdown:
  Load Operations:              0
  Store Operations:             0
  Arithmetic Operations:        1
  Control Flow:                 0
  Function Operations:          1

Instruction Frequency:
  LOAD_CONST                    1
  RETURN                        1

                    IMPROVEMENT ANALYSIS
------------------------------------------------------------
  ✓ Reduced instruction count by 62.5%
  ✓ Eliminated 4 constants
  ✓ Eliminated 4 load operations
  ✓ Eliminated 2 arithmetic operations

============================================================
```

**Interpretation:**
- 62.5% fewer instructions!
- All intermediate values computed at compile time
- Final result (26) loaded as single constant

---

## Example 2: Configuration Constants

**Input:**
```aifpl
(let ((buffer-size (* 1024 1024))
      (timeout-ms (* 1000 30)))
  (+ buffer-size timeout-ms))
```

**Output Summary:**
```
SUMMARY
------------------------------------------------------------
  Instructions Eliminated:     10 ( 45.5%)
  Constants Eliminated:         4
  Load Ops Eliminated:          4
  Arithmetic Ops Eliminated:    2

IMPROVEMENT ANALYSIS
------------------------------------------------------------
  ✓ Reduced instruction count by 45.5%
  ✓ Eliminated 4 constants
  ✓ Eliminated 4 load operations
  ✓ Eliminated 2 arithmetic operations
```

**Interpretation:**
- Configuration values pre-computed
- Runtime just loads final values
- Nearly 50% reduction in bytecode

---

## Example 3: JSON Output

**Command:**
```bash
python bytecode_analyzer.py --expr "(+ 1 2)" --json
```

**Output:**
```json
{
  "unoptimized": {
    "total_instructions": 5,
    "constants": 2,
    "names": 1,
    "code_objects": 0,
    "max_locals": 0,
    "cyclomatic_complexity": 1,
    "instruction_counts": {
      "LOAD_CONST": 2,
      "CALL_BUILTIN": 1,
      "RETURN": 1
    }
  },
  "optimized": {
    "total_instructions": 2,
    "constants": 1,
    "names": 0,
    "code_objects": 0,
    "max_locals": 0,
    "cyclomatic_complexity": 1,
    "instruction_counts": {
      "LOAD_CONST": 1,
      "RETURN": 1
    }
  },
  "improvements": {
    "instruction_reduction": 3,
    "instruction_reduction_pct": 60.0,
    "constant_reduction": 1,
    "load_reduction": 1,
    "arithmetic_reduction": 1
  }
}
```

**Use case:** Parse this JSON in scripts for automated analysis

---

## Example 4: Disassembly View

**Command:**
```bash
python bytecode_analyzer.py --expr "(+ 1 2)" --disassemble
```

**Output:**
```
============================================================
              DISASSEMBLY (Unoptimized)
============================================================

CodeObject: <module>
  Parameters: 0
  Locals: 0
  Constants: [1, 2]
  Names: ['+']
  Instructions:
      0: LOAD_NAME 0          ; Load '+'
      1: LOAD_CONST 0         ; Load 1
      2: LOAD_CONST 1         ; Load 2
      3: CALL_BUILTIN 0 2     ; Call + with 2 args
      4: RETURN               ; Return result

============================================================
               DISASSEMBLY (Optimized)
============================================================

CodeObject: <module>
  Parameters: 0
  Locals: 0
  Constants: [3]
  Names: []
  Instructions:
      0: LOAD_CONST 0         ; Load 3
      1: RETURN               ; Return result
```

**Interpretation:**
- Unoptimized: 5 instructions
- Optimized: 2 instructions
- Result pre-computed: (+ 1 2) → 3

---

## Example 5: Batch Analysis

**Command:**
```bash
python bytecode_analyzer.py --batch test_cases/*.aifpl
```

**Output:**
```
============================================================
                  BATCH ANALYSIS SUMMARY
============================================================

Analyzed 10 files:

  simple_arithmetic.aifpl
    Instructions:   45 →   32 ( 28.9% reduction)
  nested_arithmetic.aifpl
    Instructions:   67 →   51 ( 23.9% reduction)
  math_constants.aifpl
    Instructions:   89 →   62 ( 30.3% reduction)
  unit_conversions.aifpl
    Instructions:   54 →   38 ( 29.6% reduction)
  bit_operations.aifpl
    Instructions:   38 →   26 ( 31.6% reduction)
  boolean_logic.aifpl
    Instructions:   42 →   35 ( 16.7% reduction)
  configuration.aifpl
    Instructions:   78 →   52 ( 33.3% reduction)
  algorithm_params.aifpl
    Instructions:   56 →   41 ( 26.8% reduction)
  range_calculations.aifpl
    Instructions:   49 →   37 ( 24.5% reduction)
  comparisons.aifpl
    Instructions:   44 →   32 ( 27.3% reduction)

Overall:
  Instructions:  562 →  406 ( 27.8% reduction)
============================================================
```

**Interpretation:**
- Consistent 20-30% improvement across test cases
- Overall: nearly 28% instruction reduction
- Configuration code benefits most (33.3%)

---

## Example 6: Comprehensive Report

**Command:**
```bash
python generate_report.py --test-file test_cases.aifpl --output report.md
```

**Output:** (excerpt from generated markdown)

```markdown
# AIFPL Optimization Impact Report

Generated: 2024-01-15 14:30:00

## Summary

- **Test Cases:** 10
- **Total Instructions (Before):** 562
- **Total Instructions (After):** 406
- **Total Reduction:** 156 (27.8%)
- **Average Reduction:** 27.3%

## Detailed Results

| Test Case | Instructions Before | Instructions After | Reduction |
|-----------|--------------------:|-------------------:|----------:|
| Simple Arithmetic | 45 | 32 | 28.9% |
| Nested Arithmetic | 67 | 51 | 23.9% |
| Math Constants | 89 | 62 | 30.3% |
| Unit Conversions | 54 | 38 | 29.6% |
| Bit Operations | 38 | 26 | 31.6% |
| Boolean Logic | 42 | 35 | 16.7% |
| Configuration | 78 | 52 | 33.3% |
| Algorithm Params | 56 | 41 | 26.8% |
| Range Calculations | 49 | 37 | 24.5% |
| Comparisons | 44 | 32 | 27.3% |

## Instruction Reduction Visualization

```
Simple Arithmetic              ██████████████ 28.9%
Nested Arithmetic              ███████████ 23.9%
Math Constants                 ███████████████ 30.3%
Unit Conversions               ██████████████ 29.6%
Bit Operations                 ███████████████ 31.6%
Boolean Logic                  ████████ 16.7%
Configuration                  ████████████████ 33.3%
Algorithm Params               █████████████ 26.8%
Range Calculations             ████████████ 24.5%
Comparisons                    █████████████ 27.3%
```

## Conclusions

✓ **Excellent impact**: 27.3% average instruction reduction

**Best optimization:** Configuration (33.3% reduction)
**Least optimization:** Boolean Logic (16.7% reduction)
```

---

## What to Look For

### Good Signs ✓
- Instruction reduction > 20%
- Constants eliminated
- Load operations reduced
- Arithmetic operations eliminated

### Warning Signs ⚠️
- Instruction count increased
- More constants than before
- Complexity increased
- Unexpected behavior

### Action Items
- If reduction < 10%: Look for more optimization opportunities
- If reduction > 30%: Great! Document and share
- If negative: Bug! Investigate immediately
- If varies widely: Some patterns benefit more than others

---

## Using These Results

1. **Development:** Quick feedback on optimization effectiveness
2. **Code Review:** Quantify impact of changes
3. **Documentation:** Show concrete improvements
4. **Planning:** Prioritize high-impact optimizations
5. **Validation:** Ensure no regressions

Remember: These are **static** measurements. Always validate with runtime benchmarks!
