# AIFPL Pretty-Printer

A command-line tool for formatting AIFPL source code with comment preservation.

## Installation

No installation required - just run the script directly:

```bash
# Make it executable (Unix/Mac)
chmod +x tools/aifpl_pretty_print/aifpl_pretty_print.py

# Or run with Python
python tools/aifpl_pretty_print/aifpl_pretty_print.py
```

## Usage

### Basic Usage

```bash
# Format a file and print to stdout
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl

# Format and save to a new file
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl -o formatted.aifpl

# Format in-place (overwrites original)
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl --in-place
```

### From stdin

```bash
# Pipe code through the formatter
echo "(let ((x 5)(y 10)) (+ x y))" | python tools/aifpl_pretty_print/aifpl_pretty_print.py -

# Format code from clipboard (Mac)
pbpaste | python tools/aifpl_pretty_print/aifpl_pretty_print.py -
```

### Custom Options

```bash
# Use 4-space indentation
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl --indent 4

# Set maximum line width
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl --width 100

# Adjust compact threshold
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl --compact-threshold 80

# Change comment spacing
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl --comment-spacing 4
```

### Check Mode

Check if a file is already formatted (useful for CI/CD):

```bash
python tools/aifpl_pretty_print/aifpl_pretty_print.py myfile.aifpl --check
# Exit code 0 if formatted, 1 if needs formatting
```

## Options

| Option | Default | Description |
|--------|---------|-------------|
| `input` | - | Input file (use "-" for stdin) |
| `-o, --output` | stdout | Output file |
| `-i, --in-place` | false | Format file in-place |
| `--indent` | 2 | Spaces per indentation level |
| `--width` | 80 | Maximum line width hint |
| `--compact-threshold` | 60 | Max length for compact format |
| `--comment-spacing` | 2 | Spaces before end-of-line comments |
| `--check` | false | Check if already formatted |

## Features

### Comment Preservation

- **End-of-line comments** stay inline with configurable spacing
- **Standalone comments** get proper spacing with blank lines
- **Comment blocks** are preserved with original content

Example:
```aifpl
; Input
(let ((x 5);value
(y 10));another
(+ x y))

; Output
(let ((x 5)  ; value
      (y 10))  ; another
  (+ x y))
```

### Smart Formatting

- **Compact format** for short, simple expressions
- **Multi-line format** for complex expressions
- **Proper alignment** for bindings in let/letrec
- **Special handling** for lambda, if, match

Example:
```aifpl
; Input
(letrec ((factorial (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))) (factorial 5))

; Output
(letrec ((factorial (lambda (n)
                      (if (<= n 1)
                        1
                        (* n (factorial (- n 1)))))))
  (factorial 5))
```

### Binding Forms

All binding forms (`let`, `let*`, `letrec`) are properly aligned:

```aifpl
(let ((x 5)
      (y 10)
      (z 15))
  (+ x y z))

(let* ((x 5)
       (y (* x 2))
       (z (+ x y)))
  (list x y z))

(letrec ((even? (lambda (n)
                  (or (= n 0)
                      (odd? (- n 1)))))

         (odd? (lambda (n)
                 (and (!= n 0)
                      (even? (- n 1))))))
  (even? 10))
```

## Library Usage

You can also use the pretty-printer as a library:

```python
from aifpl.aifpl_pretty_printer import pretty_print, FormatOptions

# Basic usage
code = "(let ((x 5)(y 10)) (+ x y))"
formatted = pretty_print(code)
print(formatted)

# With custom options
options = FormatOptions(
    max_line_width=100,
    indent_size=4,
    compact_threshold=80,
    comment_spacing=2
)
formatted = pretty_print(code, options)
```

## Examples

### Format all AIFPL files in a directory

```bash
# Unix/Mac
find . -name "*.aifpl" -exec python tools/aifpl_pretty_print/aifpl_pretty_print.py {} --in-place \;

# Or with a loop
for file in *.aifpl; do
    python tools/aifpl_pretty_print/aifpl_pretty_print.py "$file" --in-place
done
```

### Use in a pre-commit hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

for file in $(git diff --cached --name-only --diff-filter=ACM | grep '\.aifpl$'); do
    python tools/aifpl_pretty_print/aifpl_pretty_print.py "$file" --check
    if [ $? -ne 0 ]; then
        echo "Error: $file needs formatting"
        echo "Run: python tools/aifpl_pretty_print/aifpl_pretty_print.py $file --in-place"
        exit 1
    fi
done
```

## Implementation Details

The pretty-printer works at the token level to preserve comments:

1. Lexer runs with `preserve_comments=True` to emit comment tokens
2. Formatter processes tokens and applies formatting rules
3. End-of-line comments detected by comparing line numbers
4. Output cleaned up (trailing whitespace, excessive blank lines)

## Limitations

- Does not reflow or wrap long comments
- Does not preserve user's specific indentation choices within expressions
- Nested lists always formatted multi-line (no compact nested lists)

## Future Enhancements

- Configurable style presets (compact, expanded, etc.)
- Smart line breaking for long expressions
- Preservation of intentional blank lines
- Custom formatting rules per special form
- Editor integration plugins
