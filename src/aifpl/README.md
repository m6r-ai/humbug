# AIFPL (AI Functional Programming Language)

AIFPL is a mathematical expression language with LISP-like S-expression syntax designed for AI tool integration. It supports mathematical calculations, string manipulation, boolean operations, and list processing.

## Features

- **S-expression syntax**: `(operator arg1 arg2 ...)`
- **Mathematical operations**: Arithmetic, trigonometry, logarithms, bitwise operations
- **String operations**: Manipulation, searching, conversion with full UTF-8 support
- **Boolean operations**: Logic operations with strict type checking
- **List operations**: Construction, manipulation, and conversion with heterogeneous support
- **Number formats**: Integers, floats, complex numbers, hex (0xFF), binary (0b1010), octal (0o755)
- **String literals**: `"hello world"` with escape sequences
- **Boolean literals**: `#t` (true) and `#f` (false)
- **Constants**: `pi`, `e`, `j` (imaginary unit), `true`, `false`
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

tool = AIFPL()
result = tool.evaluate("(+ 1 2 3)")  # Returns: 6
```

### With Configuration

```python
tool = AIFPL(max_depth=200, imaginary_tolerance=1e-12)
result = tool.evaluate("(sin (* pi 0.5))")  # Returns: 1
```

### Formatted Output

```python
# Get results with LISP-style formatting
result_str = tool.evaluate_and_format("(list 1 2 3)")  # Returns: "(1 2 3)"
```

### Error Handling

```python
from aifpl import AIFPL, AIFPLError

tool = AIFPL()
try:
    result = tool.evaluate("(+ 1 2")  # Missing closing paren
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

### Comparison Operations
- `(= 1 1)` → `#t`
- `(= 1 2)` → `#f`
- `(< 1 2)` → `#t`
- `(> 3 2)` → `#t`
- `(<= 1 1)` → `#t`
- `(>= 2 1)` → `#t`

### Boolean Operations
- `(and #t #f)` → `#f`
- `(and #t #t)` → `#t`
- `(or #t #f)` → `#t`
- `(or #f #f)` → `#f`
- `(not #t)` → `#f`
- `(not #f)` → `#t`

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

### List Operations

#### List Construction and Manipulation
```lisp
(list 1 2 3)                          ; → (1 2 3)
(list "a" "b" "c")                     ; → ("a" "b" "c")
(list 1 "hello" #t)                    ; → (1 "hello" #t) [mixed types]
(list)                                 ; → () [empty list]
(cons 1 (list 2 3))                    ; → (1 2 3) [prepend]
(append (list 1 2) (list 3 4))         ; → (1 2 3 4) [concatenate]
(reverse (list 1 2 3))                 ; → (3 2 1)
```

#### List Access and Properties
```lisp
(first (list 1 2 3))                  ; → 1
(rest (list 1 2 3))                   ; → (2 3)
(list-ref (list "a" "b" "c") 1)       ; → "b" (0-indexed)
(length (list 1 2 3))                 ; → 3
(null? (list))                        ; → #t
(null? (list 1))                      ; → #f
(list? (list 1 2))                    ; → #t
(list? "hello")                       ; → #f
(member? 2 (list 1 2 3))              ; → #t
(member? 5 (list 1 2 3))              ; → #f
```

#### List Equality
```lisp
(= (list 1 2) (list 1 2))             ; → #t
(= (list 1 2) (list 1 3))             ; → #f
(= (list 1 2) (list 1 2 3))           ; → #f
```

### String Operations

#### String Construction and Conversion
```lisp
(string-append "hello" " " "world")   ; → "hello world"
(number->string 42)                   ; → "42"
(number->string 3.14)                 ; → "3.14"
(string->number "42")                 ; → 42
(string->number "3.14")               ; → 3.14
```

#### String Information and Access
```lisp
(string-length "hello")               ; → 5
(string-ref "hello" 1)                ; → "e" (character at index 1)
(substring "hello" 1 4)               ; → "ell" (start=1, end=4 exclusive)
```

#### String Manipulation
```lisp
(string-upcase "hello")               ; → "HELLO"
(string-downcase "HELLO")             ; → "hello"
```

#### String Predicates
```lisp
(string-contains? "hello world" "world")  ; → #t
(string-prefix? "hello" "he")             ; → #t
(string-suffix? "hello" "lo")             ; → #t
(string=? "hello" "hello")                ; → #t
(string=? "hello" "world")                ; → #f
```

### String-List Integration

#### String-List Conversion
```lisp
(string->list "hello")                ; → ("h" "e" "l" "l" "o")
(list->string (list "h" "e" "l" "l" "o"))  ; → "hello"
```

#### String Splitting and Joining
```lisp
(string-split "name,age,city" ",")    ; → ("name" "age" "city")
(string-split "hello world" " ")      ; → ("hello" "world")
(string-join (list "hello" "world") " ")  ; → "hello world"
(string-join (list "a" "b" "c") ",") ; → "a,b,c"
```

### String Literals and Escape Sequences

String literals use double quotes and support escape sequences:

```lisp
"hello world"                         ; Basic string
"She said \"Hello!\""                 ; Escaped quotes
"Line 1\nLine 2"                      ; Newline
"Column 1\tColumn 2"                  ; Tab
"Path\\to\\file"                      ; Backslash
"Unicode: \u03B1\u03B2\u03B3"         ; Greek letters αβγ
```

**Supported escape sequences:**
- `\"` → literal quote
- `\\` → literal backslash
- `\n` → newline
- `\t` → tab
- `\r` → carriage return
- `\uXXXX` → Unicode code point (4 hex digits)

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

## Type System

AIFPL has a strict type system with the following types:

- **Numbers**: `int`, `float`, `complex` (with automatic promotion)
- **Strings**: UTF-8 strings with no automatic conversion
- **Booleans**: `#t` and `#f` with no automatic conversion
- **Lists**: Heterogeneous collections supporting any element type

### Type Promotion Rules

1. **Numeric promotion**: `int → float → complex`
2. **No cross-type operations**: Strings, booleans, and lists don't mix with numbers
3. **Explicit conversion**: Use conversion functions when needed
4. **List heterogeneity**: Lists can contain mixed types

### Examples of Type Strictness

```lisp
; Valid - same types
(+ 1 2 3)                             ; → 6 (all integers)
(string-append "hello" " " "world")   ; → "hello world" (all strings)
(and #t #f #t)                        ; → #f (all booleans)

; Valid - numeric promotion
(+ 1 2.5)                             ; → 3.5 (int promoted to float)
(* 2 (complex 1 1))                   ; → (2+2j) (promoted to complex)

; Valid - heterogeneous lists
(list 1 "hello" #t)                   ; → (1 "hello" #t)
(append (list 1 2) (list "a" "b"))    ; → (1 2 "a" "b")

; Valid - list equality
(= (list 1 2) (list 1 2))             ; → #t

; Invalid - type mismatch
(+ 1 "hello")                         ; Error: cannot add number and string
(and #t 1)                            ; Error: 'and' requires boolean arguments
(string-length 42)                    ; Error: string-length requires string
(+ (list 1 2))                        ; Error: cannot add list
(< (list 1) (list 2))                 ; Error: cannot compare lists (only = works)

; Valid - explicit conversion
(string-append "Count: " (number->string 42))  ; → "Count: 42"
(+ 5 (string->number "10"))                    ; → 15
(list->string (string->list "hello"))          ; → "hello"
```

### List Type Rules

1. **Mixed types allowed**: `(list 1 "hi" #t)` is valid
2. **No arithmetic operations**: `(+ (list 1 2))` is an error
3. **Only equality comparison**: `(= (list 1) (list 1))` works, `(< (list 1) (list 2))` doesn't
4. **Type-specific functions**: List functions require lists, string functions require strings
5. **Explicit conversion**: Use `string->list`, `list->string` for conversions

## Common Usage Patterns

### String Processing
```lisp
; Split CSV data and process
(string-split "name,age,city" ",")              ; → ("name" "age" "city")
(first (string-split "John,25,NYC" ","))        ; → "John"
(length (string-split "a,b,c,d" ","))           ; → 4

; Build strings from components
(string-join (list "hello" "world") " ")        ; → "hello world"
(string-join (reverse (string-split "a-b-c" "-")) "+")  ; → "c+b+a"
```

### Character-Level Processing
```lisp
; Convert to characters, process, convert back
(string->list "hello")                          ; → ("h" "e" "l" "l" "o")
(reverse (string->list "hello"))                ; → ("o" "l" "l" "e" "h")
(list->string (reverse (string->list "hello"))) ; → "olleh"
```

### Data Structure Manipulation
```lisp
; Build complex data structures
(list (list "name" "John") (list "age" 25))     ; → (("name" "John") ("age" 25))
(first (list (list 1 2) (list 3 4)))           ; → (1 2)
(rest (first (list (list 1 2 3) (list 4 5))))  ; → (2 3)
```

## Design Principles

1. **Independence**: No dependencies on other application packages
2. **Simplicity**: Direct S-expression evaluation without over-engineering
3. **Type Safety**: Comprehensive type hints and strict type checking
4. **Error Handling**: Detailed error messages with position information
5. **Performance**: Efficient direct evaluation of nested structures
6. **Functional Purity**: No side effects, deterministic results
7. **LISP Compatibility**: Following traditional LISP semantics where applicable

## Exception Hierarchy

- `AIFPLError` - Base exception
  - `AIFPLTokenError` - Tokenization errors
  - `AIFPLParseError` - Parsing errors  
  - `AIFPLEvalError` - Evaluation errors

## Advanced Usage

### Custom Configuration

```python
# Increase recursion depth for deeply nested expressions
tool = AIFPL(max_depth=500)

# Adjust tolerance for complex number simplification
tool = AIFPL(imaginary_tolerance=1e-15)
```

### Working with Results

```python
# Raw evaluation returns Python objects
result = tool.evaluate('(list 1 2 3)')
print(f"Result: {result}")  # Result: [1, 2, 3]
print(f"Type: {type(result)}")  # Type: <class 'list'>

# Formatted evaluation returns LISP-style strings
formatted = tool.evaluate_and_format('(list 1 2 3)')
print(f"Formatted: {formatted}")  # Formatted: (1 2 3)

# Boolean results
bool_result = tool.evaluate('(member? 2 (list 1 2 3))')
print(f"Boolean: {bool_result}")  # Boolean: True

formatted_bool = tool.evaluate_and_format('(member? 2 (list 1 2 3))')
print(f"Formatted Boolean: {formatted_bool}")  # Formatted Boolean: #t
```

### Error Handling Patterns

```python
from aifpl import AIFPL, AIFPLTokenError, AIFPLParseError, AIFPLEvalError

tool = AIFPL()

try:
    result = tool.evaluate(expression)
except AIFPLTokenError as e:
    print(f"Tokenization error: {e}")
except AIFPLParseError as e:
    print(f"Parsing error: {e}")
except AIFPLEvalError as e:
    print(f"Evaluation error: {e}")
except AIFPLError as e:
    print(f"General AIFPL error: {e}")
```

### Nested List Processing

```python
# Complex nested operations
nested_expr = '''
(string-join 
  (reverse 
    (string-split "hello,world,test" ",")) 
  " | ")
'''
result = tool.evaluate(nested_expr)  # → "test | world | hello"
```
