# Menai Parenthesis Balance Checker

A standalone tool for validating parenthesis balance in Menai files. Provides detailed depth tracking and error reporting to help debug structural issues quickly.

## Features

- ✅ Fast parenthesis balance validation
- ✅ Line-by-line depth tracking
- ✅ Form type annotations (lambda, let, letrec, if, match)
- ✅ Automatic error context display
- ✅ Line range filtering for large files
- ✅ Robust handling of strings, comments, and complex literals
- ✅ Full depth chart shown by default
- ✅ Clear exit codes for CI/CD integration

## Installation

No installation needed! The tool uses the Menai lexer from `src/menai`.

## Usage

### Basic Check

```bash
python -m tools.menai_checker.checker file.menai
```

**Output (default - shows full depth chart):**
```
✓ Parentheses balanced in file.menai
  Total: 5 opens, 5 closes
  Maximum depth: 3

Line | Depth | Code
-----|-------|--------------------------------------------------
   1 |     0 | ; Example file
   2 |     2 | (let ((x 5)
   3 |     1 |       (y 10))
   4 |     0 |   (+ x y))
✓ Parentheses balanced in file.menai
  Total: 505 opens, 505 closes
  Maximum depth: 8
```

**Output when unbalanced:**
```
✗ Parentheses UNBALANCED in file.menai
  Total: 506 opens, 505 closes
  Missing 1 closing parenthesis

Line | Depth | Code
-----|-------|-----------------------------------------------------
 395 |   4   |     (list "forward-pass" forward-pass)
 396 |   4   |     (list "backward-pass" backward-pass)
 397 |   3   |   )))
 398 |   2   | )
 399 |   1   | )
 400 |   0   | )
 401 |  -1   | )  <-- ERROR: Extra closing parenthesis (depth went negative)

Unmatched closing parenthesis at line 401
```

### Check Specific Line Range

Useful for large files - only display depth chart for specific lines:

```bash
# Lines 100 to 200
python -m tools.menai_checker.checker file.menai -l 100-200

# From line 100 to end
python -m tools.menai_checker.checker file.menai -l 100-

# From start to line 200
python -m tools.menai_checker.checker file.menai -l -200
```

### Limit Line Width

Truncate long lines for better readability:

```bash
python -m tools.menai_checker.checker file.menai --max-width 80
```

Lines longer than 80 characters will be truncated with "...".

### Show Full Depth Chart

By default, the depth chart only shows lines around errors. To see the entire file:

```bash
python -m tools.menai_checker.checker file.menai --show-all
```

### Combined Options

```bash
# Check specific range with annotations
python -m tools.menai_checker.checker file.menai -l 100-200 -a

# Quick summary check
python -m tools.menai_checker.checker file.menai -s
```

### ANSI Color Output

Use `-c` or `--color` to enable colorized output:

```bash
python -m tools.menai_checker.checker file.menai -a -c
```

**Color features:**

1. **Matching paren colors** - Parentheses are colored based on their depth level, making it easy to visually match opening and closing parens:
   - Depth 0: Cyan
   - Depth 1: Yellow  
   - Depth 2: Green
   - Depth 3: Magenta
   - Depth 4: Blue
   - Depth 5: Red
   - (cycles through palette for deeper nesting)

2. **Form annotations** - Shown with `<--` arrow pointing back to what's being closed. Annotations are colored to match the paren they're closing, making it easy to visually trace which opening paren each annotation refers to.

3. **Line numbers** - Color matches the depth at the start of the line (the currently open paren context):
   - Shows which nesting level each line belongs to
   - Same color as the paren that opened that context
   - Makes it easy to see which lines are inside which forms
   - Dim color for depth 0 (top level)

4. **Errors** - Shown in bright red for visibility

**Why this matters:** The semantic color scheme means colors have meaning - you can visually trace from a closing annotation back to its opening paren by matching colors, and see at a glance which context each line belongs to!

## Exit Codes

- `0` - Parentheses are balanced
- `1` - Parentheses are unbalanced
- `2` - Error (file not found, invalid arguments, etc.)

## Examples

### Example 1: Balanced File

```bash
$ python -m tools.menai_checker.checker examples/balanced.menai
✓ Parentheses balanced in balanced.menai
  Total: 42 opens, 42 closes
  Maximum depth: 5
```

### Example 2: Missing Closing Paren

```bash
$ python -m tools.menai_checker.checker examples/missing_close.menai
✗ Parentheses UNBALANCED in missing_close.menai
  Total: 43 opens, 42 closes
  Missing 1 closing parenthesis

Line | Depth | Code
-----|-------|-----------------------------------------------------
  38 |   3   |     (+ x y)
  39 |   2   |   )
  40 |   1   | )
  41 |   1   |   <-- ERROR: Missing 1 closing parenthesis

Unclosed expressions at end of file
```

### Example 3: With Annotations

```bash
$ python -m tools.menai_checker.checker scheduling.menai -a -l 395-402
✓ Parentheses balanced in scheduling.menai
  Total: 502 opens, 502 closes
  Maximum depth: 25

Line | Depth | Code
-----|-------|-----------------------------------------------------
 395 |     5 |     (list "calculate-slack" calculate-slack)
 396 |     5 |     (list "identify-critical-path" identify-critical-path)
 397 |     5 |     (list "schedule-project" schedule-project)
 398 |     4 |   )
 399 |     4 |
 400 |     1 |   )))  <-- closes letrec, closes let
 401 |     0 | )  <-- closes lambda
 402 |     0 |
```

## How It Works

1. **Lexing**: Uses the robust Menai lexer to parse the file, correctly handling:
   - String literals with embedded parens: `"(hello)"`
   - Comments with parens: `; (comment)`
   - Complex numbers: `3+4j`
   - All Menai literal types

2. **Depth Tracking**: Tracks parenthesis depth at each line:
   - Opening paren `(` increases depth by 1
   - Closing paren `)` decreases depth by 1

3. **Error Detection**:
   - **Negative depth**: More closing parens than opening parens
   - **Non-zero final depth**: Unclosed expressions at end of file

4. **Context Display**: Automatically shows ±5 lines around errors for context

## Integration

### Pre-commit Hook

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
for file in $(git diff --cached --name-only --diff-filter=ACM | grep '\.menai$'); do
    python -m tools.menai_checker.checker "$file"
    if [ $? -ne 0 ]; then
        echo "Parenthesis balance check failed for $file"
        exit 1
    fi
done
```

### CI/CD

```yaml
# .github/workflows/check-menai.yml
name: Check Menai Files
on: [push, pull_request]
jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Check Menai parentheses
        run: |
          for file in $(find . -name "*.menai"); do
            python -m tools.menai_checker.checker "$file" || exit 1
          done
```

## Troubleshooting

### "Module not found" Error

Make sure you're running from the project root directory, or adjust your `PYTHONPATH`:

```bash
export PYTHONPATH="${PYTHONPATH}:$(pwd)/src"
python -m tools.menai_checker.checker file.menai
```

### Lexer Errors

If the lexer reports errors (invalid syntax, bad escape sequences, etc.), fix those first before checking paren balance.

## Limitations

- Only checks parenthesis balance, not semantic correctness
- Does not validate Menai form structure (e.g., `let` bindings)
- Requires valid Menai tokens (strings must be properly escaped, etc.)

For full validation, use the Menai parser/evaluator.

## Future Enhancements

Potential features for future versions:

- Find matching paren pairs (interactive mode)
- Structure outline (show high-level nesting)
- Validate Menai forms (let, lambda, etc.)
- JSON output for tool integration
- Watch mode for continuous checking
- Editor integration (LSP)

## License

Same as parent project.
