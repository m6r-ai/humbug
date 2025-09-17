# AIFPL (AI Functional Programming Language)

AIFPL is a mathematical expression language with LISP-like S-expression syntax designed for AI tool integration.

## Features

- **S-expression syntax**: `(operator arg1 arg2 ...)`
- **Mathematical operations**: Arithmetic, trigonometry, logarithms, bitwise operations
- **Number formats**: Integers, floats, complex numbers, hex (0xFF), binary (0b1010), octal (0o755)
- **Constants**: `pi`, `e`, `j` (imaginary unit)
- **Type promotion**: Automatic promotion from int → float → complex
- **Result simplification**: Complex numbers with negligible imaginary parts become real

## Package Structure

```
src/aifpl/
├── __init__.py              # Package exports
├── aifpl.py                 # Main AIFPL class (public API)
├── aifpl_error.py           # Exception classes
├── aifpl_token.py           # Token types and definitions
├── aifpl_tokenizer.py       # Tokenizer implementation
├── aifpl_parser.py          # Parser and AST definitions
└── aifpl_evaluator.py       # Expression evaluator
```

## Usage

### Basic Usage

```python
from aifpl import AIFPL

calculator = AIFPL()
result = calculator.evaluate("(+ 1 2 3)")  # Returns: 6
```

### With Configuration

```python
calculator = AIFPL(max_depth=200, imaginary_tolerance=1e-12)
result = calculator.evaluate("(sin (* pi 0.5))")  # Returns: 1
```

### Error Handling

```python
from aifpl import AIFPL, AIFPLError

calculator = AIFPL()
try:
    result = calculator.evaluate("(+ 1 2")  # Missing closing paren
except AIFPLError as e:
    print(f"AIFPL error: {e}")
```

## Supported Operations

### Arithmetic
- `(+ 1 2 3)` → `6`
- `(- 10 3)` → `7`
- `(* 2 3 4)` → `24`
- `(/ 12 3)` → `4`
- `(// 7 3)` → `2` (floor division)
- `(% 7 3)` → `1` (modulo)
- `(** 2 3)` → `8` (exponentiation)

### Mathematical Functions
- `(sin (* pi 0.5))` → `1`
- `(cos 0)` → `1`
- `(log e)` → `1`
- `(sqrt 16)` → `4`
- `(abs -5)` → `5`

### Bitwise Operations
- `(bit-or 5 3)` → `7`
- `(bit-and 7 3)` → `3`
- `(bit-xor 5 3)` → `6`
- `(bit-not 5)` → `-6`

### Base Conversion
- `(hex 255)` → `"0xff"`
- `(bin 10)` → `"0b1010"`
- `(oct 8)` → `"0o10"`

### Complex Numbers
- `(complex 3 4)` → `(3+4j)`
- `(+ 1 (* 2 j))` → `(1+2j)`
- `(real (complex 3 4))` → `3` (extract real part)
- `(imag (complex 3 4))` → `4` (extract imaginary part)
- `(real 5)` → `5` (real part of real number)
- `(imag 5)` → `0` (imaginary part of real number)

### Complex Number Operations
The `real` and `imag` functions extract components from any numeric value:

**Real Part Extraction:**
```lisp
(real 42)              ; → 42
(real 3.14)            ; → 3.14
(real (complex 3 4))   ; → 3
(real j)               ; → 0
```

**Imaginary Part Extraction:**
```lisp
(imag 42)              ; → 0
(imag 3.14)            ; → 0
(imag (complex 3 4))   ; → 4
(imag j)               ; → 1
```

**With Expressions:**
```lisp
(real (+ (complex 1 2) (complex 3 4)))  ; → 4
(imag (sqrt -1))                        ; → 1
(real (* j j))                          ; → -1
```

**Type Behavior:**
- Returns most specific type (int when possible, float when necessary)
- Respects `imaginary_tolerance` setting for tiny imaginary parts
- `(imag (complex 3.0 1e-15))` → `0` (if below tolerance)

## Design Principles

1. **Independence**: No dependencies on other application packages
2. **Simplicity**: Direct S-expression evaluation without over-engineering
3. **Type Safety**: Comprehensive type hints throughout
4. **Error Handling**: Detailed error messages with position information
5. **Performance**: Efficient direct evaluation of nested structures

## Exception Hierarchy

- `AIFPLError` - Base exception
  - `AIFPLTokenError` - Tokenization errors
  - `AIFPLParseError` - Parsing errors  
  - `AIFPLEvalError` - Evaluation errors