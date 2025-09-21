# AIFPL (AI Functional Programming Language)

AIFPL is a mathematical expression language with LISP-like S-expression syntax designed for AI tool integration. It supports mathematical calculations, string manipulation, boolean operations, list processing, conditional evaluation, and functional programming with lambda expressions and higher-order functions.

## Features

- **Pure list representation**: Everything is data - true homoiconicity like traditional Lisp
- **S-expression syntax**: `(operator arg1 arg2 ...)`
- **Mathematical operations**: Arithmetic, trigonometry, logarithms, bitwise operations
- **String operations**: Manipulation, searching, conversion with full UTF-8 support
- **Boolean operations**: Logic operations with strict type checking
- **List operations**: Construction, manipulation, and conversion with heterogeneous support
- **Conditional evaluation**: `if` expressions with lazy evaluation of branches
- **Lazy evaluation**: `and` and `or` perform lazy evaluation of operands
- **Lambda expressions**: Anonymous functions with lexical scoping and closures
- **Local bindings**: `let` expressions for sequential variable binding
- **Higher-order functions**: `map`, `filter`, `fold`, `range`, and more for functional programming
- **Tail call optimization**: Automatic optimization for recursive and mutually recursive functions
- **Type predicates**: Built-in functions to check value types
- **Number formats**: Integers, floats, complex numbers, hex (0xFF), binary (0b1010), octal (0o755)
- **String literals**: `"hello world"` with escape sequences
- **Boolean literals**: `#t` (true) and `#f` (false)
- **Constants**: `pi`, `e`, `j` (imaginary unit), `true`, `false`
- **Type promotion**: Automatic promotion from int → float → complex
- **Result simplification**: Complex numbers with negligible imaginary parts become real
- **Comments**: `;` introduces comments

## Architecture

AIFPL uses a **pure list representation** for all code, following traditional Lisp philosophy:

- **Everything is data**: Code and data have identical representation (`AIFPLValue` objects)
- **No special AST nodes**: Lambda expressions, let expressions, and function calls are all just lists
- **Homoiconic**: The same data structures represent both code and data
- **Simple and consistent**: One unified representation for all expressions

### Package Structure

```
src/aifpl/
├── __init__.py              # Package exports
├── aifpl.py                 # Main AIFPL class (public API)
├── aifpl_error.py           # Exception classes
├── aifpl_token.py           # Token types and definitions
├── aifpl_tokenizer.py       # Tokenizer implementation
├── aifpl_parser.py          # Parser (creates AIFPLValue objects)
├── aifpl_evaluator.py       # Expression evaluator
├── aifpl_environment.py     # Environment and function management
├── aifpl_value.py           # Value hierarchy (AIFPLValue types)
└── aifpl_dependency_analyzer.py  # Let binding dependency analysis
```

### Core Types

- **AIFPLValue**: Base class for all values (numbers, strings, booleans, symbols, lists, functions)

## Usage

### Basic Usage

```python
from aifpl import AIFPL

tool = AIFPL()
result = tool.evaluate("(+ 1 2 3)")  # Returns: 6
```

### With Configuration

```python
tool = AIFPL(max_depth=200, floating_point_tolerance=1e-12)
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

### Lambda Expressions and Anonymous Functions

Lambda expressions create anonymous functions with lexical scoping and closure support:

#### Basic Lambda Syntax
```aifpl
(lambda (param1 param2 ...) body)
```

#### Simple Lambda Examples
```aifpl
; Square function
((lambda (x) (* x x)) 5)                    ; → 25

; Add two numbers
((lambda (x y) (+ x y)) 3 4)                ; → 7

; String manipulation
((lambda (s) (string-upcase s)) "hello")     ; → "HELLO"

; Conditional logic
((lambda (x) (if (> x 0) "positive" "non-positive")) -3)  ; → "non-positive"
```

#### Lambda Functions as Values
```aifpl
; Lambda functions are first-class values
(let ((square (lambda (x) (* x x))))
  (square 6))                                ; → 36

; Functions in lists
(let ((ops (list (lambda (x) (* x 2)) (lambda (x) (+ x 1)))))
  ((first ops) 5))                          ; → 10

; Higher-order function usage
(map (lambda (x) (* x x)) (list 1 2 3 4))   ; → (1 4 9 16)
```

#### Closures and Lexical Scoping
```aifpl
; Closures capture their environment
(let ((multiplier 10))
  (let ((times-ten (lambda (x) (* x multiplier))))
    (times-ten 5)))                         ; → 50

; Nested closures
(let ((outer-var 100))
  (let ((make-adder (lambda (n) (lambda (x) (+ x n outer-var)))))
    (let ((add-five (make-adder 5)))
      (add-five 3))))                       ; → 108

; Function factories
(let ((make-multiplier (lambda (factor) (lambda (x) (* x factor)))))
  (let ((double (make-multiplier 2))
        (triple (make-multiplier 3)))
    (list (double 4) (triple 4))))          ; → (8 12)
```

#### Recursive Lambda Functions
```aifpl
; Factorial with tail recursion (automatically optimized)
(let ((factorial (lambda (n acc)
                   (if (<= n 1)
                       acc
                       (factorial (- n 1) (* n acc))))))
  (factorial 5 1))                          ; → 120

; List sum with recursion
(let ((sum-list (lambda (lst acc)
                  (if (null? lst)
                      acc
                      (sum-list (rest lst) (+ acc (first lst)))))))
  (sum-list (list 1 2 3 4 5) 0))           ; → 15

; Fibonacci with mutual recursion
(let ((fib-helper (lambda (n a b)
                    (if (<= n 0)
                        a
                        (fib-helper (- n 1) b (+ a b))))))
  (let ((fibonacci (lambda (n) (fib-helper n 0 1))))
    (fibonacci 10)))                        ; → 55
```

### Local Variable Binding with Let

Let expressions create local variable bindings with sequential evaluation and lexical scoping:

#### Basic Let Syntax
```aifpl
(let ((var1 val1) (var2 val2) ...) body)
```

#### Simple Let Examples
```aifpl
; Basic binding
(let ((x 5)) (+ x 10))                      ; → 15

; Multiple bindings
(let ((x 3) (y 4)) (+ (* x x) (* y y)))     ; → 25

; String operations
(let ((name "Alice") (age 30))
  (string-append name " is " (number->string age)))  ; → "Alice is 30"
```

#### Sequential Binding
```aifpl
; Later variables can reference earlier ones
(let ((x 5) (y (* x 2))) (+ x y))           ; → 15

; Building complex expressions
(let ((base 10)
      (squared (* base base))
      (cubed (* squared base)))
  (list base squared cubed))                ; → (10 100 1000)

; String processing chain
(let ((text "Hello World")
      (lower (string-downcase text))
      (words (string-split lower " ")))
  (string-join (reverse words) "-"))        ; → "world-hello"
```

#### Binding Functions
```aifpl
; Bind lambda functions to names
(let ((double (lambda (x) (* x 2)))
      (triple (lambda (x) (* x 3))))
  (+ (double 4) (triple 5)))                ; → 23

; Compose functions
(let ((add-one (lambda (x) (+ x 1)))
      (square (lambda (x) (* x x)))
      (compose (lambda (f g) (lambda (x) (f (g x))))))
  (let ((square-then-add (compose add-one square)))
    (square-then-add 4)))                   ; → 17

; Recursive function binding
(let ((countdown (lambda (n)
                   (if (<= n 0)
                       (list)
                       (cons n (countdown (- n 1)))))))
  (countdown 5))                            ; → (5 4 3 2 1)
```

#### Nested Let Expressions
```aifpl
; Nested scoping
(let ((x 10))
  (let ((x 20) (y x))  ; inner x shadows outer, y gets outer x value
    (+ x y)))                               ; → 30

; Complex nested computation
(let ((data (list 1 2 3 4 5)))
  (let ((squared (map (lambda (x) (* x x)) data))
        (sum-fn (lambda (lst) (fold + 0 lst))))
    (let ((total (sum-fn squared))
          (average (/ total (length squared))))
      (list "sum" total "average" average))))  ; → ("sum" 55 "average" 11)
```

### Higher-Order Functions

AIFPL provides powerful higher-order functions for functional programming patterns:

#### Map - Transform Each Element
```aifpl
; Basic mapping
(map (lambda (x) (* x 2)) (list 1 2 3 4))   ; → (2 4 6 8)

; String transformations
(map string-upcase (list "hello" "world"))  ; → ("HELLO" "WORLD")

; Complex transformations
(map (lambda (x) (+ (* x x) 1)) (list 1 2 3))  ; → (2 5 10)

; Nested list processing
(map (lambda (pair) (+ (first pair) (first (rest pair))))
     (list (list 1 2) (list 3 4) (list 5 6)))  ; → (3 7 11)
```

#### Filter - Select Elements by Predicate
```aifpl
; Basic filtering
(filter (lambda (x) (> x 0)) (list -1 2 -3 4))  ; → (2 4)

; String filtering
(filter (lambda (s) (> (string-length s) 3))
        (list "hi" "hello" "a" "world"))    ; → ("hello" "world")

; Complex predicates
(filter (lambda (x) (= (% x 2) 0)) (range 1 11))  ; → (2 4 6 8 10)

; Boolean combinations
(filter (lambda (x) (and (> x 5) (< x 15)))
        (range 1 20))                       ; → (6 7 8 9 10 11 12 13 14)
```

#### Fold - Accumulate Results
```aifpl
; Sum all elements
(fold + 0 (list 1 2 3 4 5))                ; → 15

; Product of elements
(fold * 1 (list 1 2 3 4))                  ; → 24

; String concatenation
(fold string-append "" (list "Hello" " " "World"))  ; → "Hello World"

; Build reverse list
(fold (lambda (acc x) (cons x acc)) (list) (list 1 2 3))  ; → (3 2 1)

; Find maximum
(fold (lambda (acc x) (if (> x acc) x acc)) 0 (list 3 7 2 9 1))  ; → 9

; Complex accumulation
(fold (lambda (acc x) (+ acc (* x x))) 0 (list 1 2 3 4))  ; → 30
```

#### Range - Generate Numeric Sequences
```aifpl
; Basic ranges
(range 1 5)                                 ; → (1 2 3 4)
(range 0 10 2)                             ; → (0 2 4 6 8)
(range 10 0 -1)                            ; → (10 9 8 7 6 5 4 3 2 1)

; Use with other functions
(map (lambda (x) (* x x)) (range 1 6))      ; → (1 4 9 16 25)
(filter (lambda (x) (= (% x 3) 0)) (range 1 21))  ; → (3 6 9 12 15 18)

; Generate data
(map (lambda (i) (list "item" i)) (range 1 4))  ; → (("item" 1) ("item" 2) ("item" 3))
```

#### Find - Locate First Matching Element
```aifpl
; Find first positive number
(find (lambda (x) (> x 0)) (list -1 -2 3 4))  ; → 3

; Find first long string
(find (lambda (s) (> (string-length s) 4))
      (list "hi" "hello" "a"))              ; → "hello"

; Returns #f if not found
(find (lambda (x) (> x 10)) (list 1 2 3))  ; → #f

; Complex search
(find (lambda (pair) (= (first pair) "name"))
      (list (list "age" 25) (list "name" "John")))  ; → ("name" "John")
```

#### Any? and All? - Boolean Predicates
```aifpl
; Check if any element matches
(any? (lambda (x) (> x 5)) (list 1 3 7 2))  ; → #t
(any? (lambda (x) (< x 0)) (list 1 2 3))   ; → #f

; Check if all elements match
(all? (lambda (x) (> x 0)) (list 1 2 3))   ; → #t
(all? (lambda (x) (> x 5)) (list 1 3 7))   ; → #f

; String validation
(all? (lambda (s) (> (string-length s) 0))
      (list "hello" "world" "test"))        ; → #t

(any? (lambda (s) (string-contains? s "@"))
      (list "user@email.com" "plain-text")) ; → #t
```

#### Take and Drop - List Slicing
```aifpl
; Take first n elements
(take 3 (list 1 2 3 4 5))                  ; → (1 2 3)
(take 2 (list "a" "b" "c" "d"))            ; → ("a" "b")

; Drop first n elements
(drop 2 (list 1 2 3 4 5))                  ; → (3 4 5)
(drop 3 (list "a" "b" "c" "d"))            ; → ("d")

; Safe operations (don't error on out-of-bounds)
(take 10 (list 1 2 3))                     ; → (1 2 3)
(drop 10 (list 1 2 3))                     ; → ()

; Combine with other operations
(take 3 (filter (lambda (x) (> x 0)) (list -1 2 -3 4 5 6)))  ; → (2 4 5)
```

### Functional Programming Patterns

#### Data Transformation Pipelines
```aifpl
; Process CSV-like data
(let ((data (list "john,25" "alice,30" "bob,22")))
  (map (lambda (row)
         (let ((parts (string-split row ",")))
           (list (first parts) (string->number (first (rest parts))))))
       data))                               ; → (("john" 25) ("alice" 30) ("bob" 22))

; Multi-step data processing
(let ((numbers (range 1 21)))
  (let ((evens (filter (lambda (x) (= (% x 2) 0)) numbers))
        (squares (map (lambda (x) (* x x)) evens))
        (sum (fold + 0 squares)))
    sum))                                   ; → 1540

; String processing pipeline
(let ((text "The Quick Brown Fox"))
  (let ((words (string-split (string-downcase text) " "))
        (long-words (filter (lambda (w) (> (string-length w) 3)) words))
        (capitalized (map string-upcase long-words)))
    (string-join capitalized "-")))         ; → "QUICK-BROWN"
```

#### Function Composition
```aifpl
; Compose two functions
(let ((compose (lambda (f g) (lambda (x) (f (g x))))))
  (let ((add-one (lambda (x) (+ x 1)))
        (double (lambda (x) (* x 2))))
    (let ((double-then-add (compose add-one double)))
      (double-then-add 5))))                ; → 11

; Chain multiple transformations
(let ((pipe (lambda (value . functions)
              (fold (lambda (acc f) (f acc)) value functions))))
  (let ((add-ten (lambda (x) (+ x 10)))
        (multiply-by-three (lambda (x) (* x 3)))
        (subtract-five (lambda (x) (- x 5))))
    (pipe 2 add-ten multiply-by-three subtract-five)))  ; → 31
```

#### Recursive Data Processing
```aifpl
; Process nested lists recursively
(let ((deep-sum (lambda (lst)
                  (if (null? lst)
                      0
                      (let ((head (first lst))
                            (tail (rest lst)))
                        (+ (if (list? head)
                               (deep-sum head)
                               head)
                           (deep-sum tail)))))))
  (deep-sum (list 1 (list 2 3) 4)))        ; → 10

; Tree traversal patterns
(let ((count-leaves (lambda (tree)
                      (if (list? tree)
                          (fold + 0 (map count-leaves tree))
                          1))))
  (count-leaves (list 1 (list 2 (list 3 4)) 5)))  ; → 5
```

#### Validation and Error Handling
```aifpl
; Validate all items in a list
(let ((validate-positive (lambda (nums)
                          (if (all? (lambda (x) (> x 0)) nums)
                              nums
                              "error: negative numbers found"))))
  (list (validate-positive (list 1 2 3))
        (validate-positive (list 1 -2 3))))  ; → ((1 2 3) "error: negative numbers found")

; Safe operations with defaults
(let ((safe-divide (lambda (x y)
                     (if (= y 0)
                         "undefined"
                         (/ x y)))))
  (map (lambda (pair) (safe-divide (first pair) (first (rest pair))))
       (list (list 10 2) (list 15 3) (list 8 0))))  ; → (5 5 "undefined")
```

### Conditional Operations

AIFPL supports conditional evaluation with lazy evaluation of branches:

```aifpl
(if condition then-expr else-expr)
```

#### Basic Examples:
```aifpl
(if (> 5 3) "greater" "less")         ; → "greater"
(if (= 1 2) (+ 1 1) (* 2 2))          ; → 4
(if #t "true branch" "false branch")  ; → "true branch"
(if #f "true branch" "false branch")  ; → "false branch"
```

#### Lazy Evaluation (Key Feature):
```aifpl
(if #t 42 (/ 1 0))                    ; → 42 (no division by zero error)
(if #f (undefined-symbol) "safe")     ; → "safe" (no undefined symbol error)
(if (> x 0) (/ 100 x) "undefined")    ; Safe division
```

#### Practical Examples:
```aifpl
; Safe list operations
(if (null? my-list) "empty" (first my-list))

; Data validation
(if (string-contains? email "@") "valid email" "invalid email")

; Conditional string building
(if (> count 1)
    (string-append (number->string count) " items")
    "1 item")

; List processing
(if (member? "target" search-list)
    (string-append "Found at position " (number->string (position "target" search-list)))
    "Not found")
```

#### Nested Conditionals:
```aifpl
(if (> x 0)
    (if (> x 10) "big positive" "small positive")
    (if (< x -10) "big negative" "small negative or zero"))
```

#### Type Requirements:
- Condition must be a boolean (`#t` or `#f`)
- Both then-expr and else-expr are required
- Only the chosen branch is evaluated (lazy evaluation)

### Arithmetic
- `(+ 1 2 3)` → `6`
- `(- 10 3)` → `7`
- `(* 2 3 4)` → `24`
- `(/ 12 3)` → `4`
- `(// 7 3)` → `2` (floor division)
- `(% 7 3)` → `1` (modulo)
- `(** 2 3)` → `8` (exponentiation)

### Comparison Operations

#### Equality and Inequality
```aifpl
; Equality - all values must be equal
(= 1 1)                               ; → #t
(= 1 2)                               ; → #f
(= 1 1 1)                             ; → #t (all equal)
(= 1 2 3)                             ; → #f (not all equal)

; Inequality - any values not equal
(!= 1 2)                              ; → #t (any inequality)
(!= 1 1)                              ; → #f (all equal)
(!= 1 2 3)                            ; → #t (not all equal)
(!= 1 1 1)                            ; → #f (all equal)
```

#### Numeric Comparisons
```aifpl
; Comparison chains (all must satisfy the relationship)
(< 1 2)                               ; → #t
(< 1 2 3)                             ; → #t (1 < 2 < 3)
(< 1 3 2)                             ; → #f (3 is not < 2)

(> 3 2)                               ; → #t
(> 3 2 1)                             ; → #t (3 > 2 > 1)

(<= 1 1)                              ; → #t
(<= 1 2 2)                            ; → #t (1 ≤ 2 ≤ 2)

(>= 2 1)                              ; → #t
(>= 3 2 2)                            ; → #t (3 ≥ 2 ≥ 2)
```

#### Mixed Type Equality
```aifpl
; Numeric types can be compared for equality
(= 1 1.0)                             ; → #t (int/float equivalence)
(= 2 (+ 1.0 1.0))                     ; → #t (numeric equivalence)

; Different non-numeric types are never equal
(= "hello" 5)                         ; → #f
(= #t 1)                              ; → #f
(= (list 1 2) "12")                   ; → #f

; Lists are compared element by element
(= (list 1 2) (list 1 2))             ; → #t
(= (list 1 2) (list 1 3))             ; → #f
```

### Boolean Operations
- `(and #t #f)` → `#f`
- `(and #t #t)` → `#t`
- `(or #t #f)` → `#t`
- `(or #f #f)` → `#f`
- `(not #t)` → `#f`
- `(not #f)` → `#t`

### Type Predicates

AIFPL provides comprehensive type checking functions:

#### Basic Type Predicates
```aifpl
; Number type checking (excludes booleans)
(number? 42)                          ; → #t
(number? 3.14)                        ; → #t
(number? (+ 1 j))                     ; → #t (complex)
(number? #t)                          ; → #f (booleans are not numbers)
(number? "42")                        ; → #f

; Specific numeric type checking
(integer? 42)                         ; → #t
(integer? 3.14)                       ; → #f
(integer? #t)                         ; → #f (booleans excluded)

(float? 3.14)                         ; → #t
(float? 42)                           ; → #f
(float? (/ 1 2))                      ; → #t (division produces float)

(complex? (+ 1 j))                    ; → #t
(complex? 42)                         ; → #f
(complex? 3.14)                       ; → #f
```

#### Other Type Predicates
```aifpl
; String type checking
(string? "hello")                     ; → #t
(string? 42)                          ; → #f
(string? (number->string 42))         ; → #t

; Boolean type checking
(boolean? #t)                         ; → #t
(boolean? #f)                         ; → #t
(boolean? 1)                          ; → #f
(boolean? 0)                          ; → #f

; Function type checking
(function? (lambda (x) (* x 2)))      ; → #t
(function? +)                         ; → #f (built-in operators are not functions)
(function? "hello")                   ; → #f

; List type checking (already existed)
(list? (list 1 2 3))                 ; → #t
(list? "hello")                       ; → #f
(list? ())                            ; → #t (empty list)
```

#### Type Predicate Usage Patterns
```aifpl
; Conditional type handling
(let ((process-value (lambda (x)
                       (if (number? x)
                           (* x 2)
                           (if (string? x)
                               (string-upcase x)
                               "unknown type")))))
  (list (process-value 5)             ; → 10
        (process-value "hello")       ; → "HELLO"
        (process-value #t)))          ; → "unknown type"

; Type validation
(let ((validate-numbers (lambda (lst)
                          (if (all? number? lst)
                              (fold + 0 lst)
                              "error: non-numeric values found"))))
  (list (validate-numbers (list 1 2 3))      ; → 6
        (validate-numbers (list 1 "x" 3))))  ; → "error: non-numeric values found"

; Polymorphic operations
(let ((safe-length (lambda (x)
                     (if (string? x)
                         (string-length x)
                         (if (list? x)
                             (length x)
                             "not a sequence")))))
  (list (safe-length "hello")         ; → 5
        (safe-length (list 1 2 3))    ; → 3
        (safe-length 42)))            ; → "not a sequence"
```

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
```aifpl
(list 1 2 3)                           ; → (1 2 3)
(list "a" "b" "c")                     ; → ("a" "b" "c")
(list 1 "hello" #t)                    ; → (1 "hello" #t) [mixed types]
(list)                                 ; → () [empty list]
(cons 1 (list 2 3))                    ; → (1 2 3) [prepend]
(append (list 1 2) (list 3 4))         ; → (1 2 3 4) [concatenate]
(reverse (list 1 2 3))                 ; → (3 2 1)
```

#### List Access and Properties
```aifpl
(first (list 1 2 3))                  ; → 1
(rest (list 1 2 3))                   ; → (2 3)
(last (list 1 2 3))                   ; → 3
(list-ref (list "a" "b" "c") 1)       ; → "b" (0-indexed)
(length (list 1 2 3))                 ; → 3
(null? (list))                        ; → #t
(null? (list 1))                      ; → #f
(list? (list 1 2))                    ; → #t
(list? "hello")                       ; → #f
(member? 2 (list 1 2 3))              ; → #t
(member? 5 (list 1 2 3))              ; → #f
```

#### List Utilities
```aifpl
; Remove all occurrences of an element
(remove 2 (list 1 2 3 2 4))           ; → (1 3 4)
(remove "x" (list "a" "x" "b" "x"))   ; → ("a" "b")
(remove 5 (list 1 2 3))               ; → (1 2 3) (no change if not found)

; Find index of first occurrence
(position 2 (list 1 2 3))             ; → 1 (0-based index)
(position "b" (list "a" "b" "c"))     ; → 1
(position 5 (list 1 2 3))             ; → #f (not found, consistent with find)
(position 2 (list 1 2 3 2))           ; → 1 (first occurrence)
```

#### List Utility Usage Patterns
```aifpl
; Clean and process data
(let ((data (list 1 -2 3 -4 5))
      (negatives (list -2 -4)))
  (let ((cleaned (fold (lambda (acc neg) (remove neg acc)) data negatives))
        (doubled (map (lambda (x) (* x 2)) cleaned)))
    doubled))                          ; → (2 6 10)

; Find and replace pattern
(let ((find-and-process (lambda (item lst)
                          (let ((pos (position item lst)))
                            (if pos
                                (string-append "Found " (number->string item) 
                                             " at position " (number->string pos))
                                "Not found")))))
  (list (find-and-process 3 (list 1 2 3 4))    ; → "Found 3 at position 2"
        (find-and-process 5 (list 1 2 3 4))))  ; → "Not found"

; List difference using remove
(let ((list-difference (lambda (list1 list2)
                         (fold remove list1 list2))))
  (list-difference (list 1 2 3 4 5) (list 2 4)))  ; → (1 3 5)
```

#### List Equality
```aifpl
(= (list 1 2) (list 1 2))             ; → #t
(= (list 1 2) (list 1 3))             ; → #f
(= (list 1 2) (list 1 2 3))           ; → #f
```

### String Operations

#### String Construction and Conversion
```aifpl
(string-append "hello" " " "world")   ; → "hello world"
(number->string 42)                   ; → "42"
(number->string 3.14)                 ; → "3.14"
(string->number "42")                 ; → 42
(string->number "3.14")               ; → 3.14
```

#### String Information and Access
```aifpl
(string-length "hello")               ; → 5
(string-ref "hello" 1)                ; → "e" (character at index 1)
(substring "hello" 1 4)               ; → "ell" (start=1, end=4 exclusive)
```

#### String Manipulation
```aifpl
(string-upcase "hello")               ; → "HELLO"
(string-downcase "HELLO")             ; → "hello"
(string-trim "  hello  ")             ; → "hello"
(string-replace "hello world" "world" "AIFPL")  ; → "hello AIFPL"
(string-replace "banana" "a" "o")     ; → "bonono" (non-overlapping)
(string-replace "test" "x" "y")       ; → "test" (no change if not found)
```

#### String Manipulation Usage Patterns
```aifpl
; Clean and normalize text
(let ((normalize-text (lambda (text)
                        (let ((trimmed (string-trim text))
                              (lower (string-downcase trimmed)))
                          (string-replace lower " " "-")))))
  (normalize-text "  Hello World  "))  ; → "hello-world"

; Template replacement
(let ((template "Hello {name}, you have {count} messages")
      (replace-template (lambda (tmpl name count)
                          (let ((with-name (string-replace tmpl "{name}" name)))
                            (string-replace with-name "{count}" (number->string count))))))
  (replace-template template "Alice" 5))  ; → "Hello Alice, you have 5 messages"

; Multiple replacements
(let ((clean-text (lambda (text)
                     (let ((step1 (string-replace text "bad" "good"))
                           (step2 (string-replace step1 "ugly" "pretty")))
                       (string-replace step2 "wrong" "right")))))
  (clean-text "bad ugly wrong text"))     ; → "good pretty right text"
```

#### String Predicates
```aifpl
(string-contains? "hello world" "world")  ; → #t
(string-prefix? "hello" "he")             ; → #t
(string-suffix? "hello" "lo")             ; → #t
(string=? "hello" "hello")                ; → #t
(string=? "hello" "world")                ; → #f
```

### String-List Integration

#### String-List Conversion
```aifpl
(string->list "hello")                ; → ("h" "e" "l" "l" "o")
(list->string (list "h" "e" "l" "l" "o"))  ; → "hello"
```

#### String Splitting and Joining
```aifpl
(string-split "name,age,city" ",")    ; → ("name" "age" "city")
(string-split "hello world" " ")      ; → ("hello" "world")
(string-join (list "hello" "world") " ")  ; → "hello world"
(string-join (list "a" "b" "c") ",") ; → "a,b,c"
```

### String Literals and Escape Sequences

String literals use double quotes and support escape sequences:

```aifpl
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
```aifpl
(real 42)              ; → 42
(real 3.14)            ; → 3.14
(real (complex 3 4))   ; → 3
(real j)               ; → 0
```

**Imaginary Part Extraction:**
```aifpl
(imag 42)              ; → 0
(imag 3.14)            ; → 0
(imag (complex 3 4))   ; → 4
(imag j)               ; → 1
```

**With Expressions:**
```aifpl
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
- **Functions**: First-class lambda functions with lexical scoping

### Type Promotion Rules

1. **Numeric promotion**: `int → float → complex`
2. **No cross-type operations**: Strings, booleans, and lists don't mix with numbers
3. **Explicit conversion**: Use conversion functions when needed
4. **List heterogeneity**: Lists can contain mixed types
5. **Function values**: Functions are first-class values that can be stored and passed

### Examples of Type Strictness

```aifpl
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

; Valid - conditionals with boolean conditions
(if #t "yes" "no")                    ; → "yes"
(if (> 5 3) 42 0)                     ; → 42

; Valid - functions as values
(let ((f (lambda (x) (* x 2)))) (f 5))  ; → 10
(list (lambda (x) (+ x 1)) (lambda (x) (* x 2)))  ; → (<lambda (x)> <lambda (x)>)

; Valid - type checking with predicates
(if (number? x) (* x 2) "not a number")
(filter string? (list 1 "hello" #t "world"))  ; → ("hello" "world")

; Invalid - type mismatch
(+ 1 "hello")                         ; Error: cannot add number and string
(and #t 1)                            ; Error: 'and' requires boolean arguments
(string-length 42)                    ; Error: string-length requires string
(+ (list 1 2))                        ; Error: cannot add list
(< (list 1) (list 2))                 ; Error: cannot compare lists (only = works)
(if 1 "yes" "no")                     ; Error: condition must be boolean

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

### Function Type Rules

1. **First-class values**: Functions can be passed, returned, stored in lists
2. **Lexical scoping**: Functions access variables from their definition environment
3. **Closures**: Functions capture their environment when created
4. **Arity checking**: Function calls must provide exact number of parameters
5. **Identity equality**: Each lambda creates a unique function object

## Pure List Representation Benefits

The pure list approach provides several advantages:

1. **True homoiconicity**: Code and data have identical representation
2. **Simpler architecture**: Only `AIFPLValue` types, no special AST nodes
3. **Traditional Lisp semantics**: Everything is data, following Lisp philosophy
4. **Easier to understand**: One consistent representation for all expressions
5. **Future extensibility**: Natural foundation for features like macros
6. **Reduced complexity**: Fewer types, simpler parser, more straightforward evaluator

### Internal Representation Examples

```aifpl
; Lambda expression: (lambda (x) (* x x))
; Represented as: AIFPLList([AIFPLSymbol("lambda"), AIFPLList([AIFPLSymbol("x")]), AIFPLList([...])])

; Let expression: (let ((x 5)) (+ x 1))
; Represented as: AIFPLList([AIFPLSymbol("let"), AIFPLList([AIFPLList([...])]), ...])

; Function call: (+ 1 2 3)
; Represented as: AIFPLList([AIFPLSymbol("+"), AIFPLNumber(1), AIFPLNumber(2), AIFPLNumber(3)])
```

The evaluator recognizes special forms by examining the first element of lists, maintaining the traditional Lisp approach where syntax is determined by structure, not by special types.

## Common Usage Patterns

### Functional Data Processing
```aifpl
; Process list of numbers
(let ((numbers (list 1 2 3 4 5 6 7 8 9 10)))
  (let ((evens (filter (lambda (x) (= (% x 2) 0)) numbers))
        (doubled (map (lambda (x) (* x 2)) evens))
        (sum (fold + 0 doubled)))
    sum))                                   ; → 60

; Text processing pipeline
(let ((text "The quick brown fox jumps"))
  (let ((words (string-split text " "))
        (long-words (filter (lambda (w) (> (string-length w) 3)) words))
        (upper-words (map string-upcase long-words)))
    (string-join upper-words "-")))         ; → "QUICK-BROWN-JUMPS"

; Data validation and transformation
(let ((validate-and-double (lambda (nums)
                            (if (all? (lambda (x) (> x 0)) nums)
                                (map (lambda (x) (* x 2)) nums)
                                "error: negative numbers"))))
  (list (validate-and-double (list 1 2 3))
        (validate-and-double (list 1 -2 3))))  ; → ((2 4 6) "error: negative numbers")
```

### Conditional Processing
```aifpl
; Safe division
(if (= divisor 0) "undefined" (/ dividend divisor))

; Safe list access
(if (null? my-list) "empty" (first my-list))

; Data validation
(if (string-contains? input "@")
    (string-append "Email: " input)
    "Invalid email format")

; Conditional list processing
(if (> (length items) 0)
    (string-join items ", ")
    "No items")

; Nested conditionals for complex logic
(if (> temperature 30)
    "hot"
    (if (> temperature 20) "warm" "cold"))
```

### Function Composition and Higher-Order Patterns
```aifpl
; Create reusable transformations
(let ((process-data (lambda (data)
                      (let ((clean (filter (lambda (x) (> x 0)) data))
                            (transformed (map (lambda (x) (+ (* x x) 1)) clean))
                            (limited (take 5 transformed)))
                        limited))))
  (process-data (list -1 2 3 -4 5 6 7 8)))  ; → (5 10 26 37 50)

; Build complex predicates
(let ((in-range (lambda (min max)
                  (lambda (x) (and (>= x min) (<= x max)))))
      (is-even (lambda (x) (= (% x 2) 0))))
  (let ((even-in-range (lambda (x) (and (is-even x) ((in-range 10 50) x)))))
    (filter even-in-range (range 1 60))))   ; → (10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50)

; Recursive data structures
(let ((tree-sum (lambda (tree)
                  (if (list? tree)
                      (fold + 0 (map tree-sum tree))
                      tree))))
  (tree-sum (list 1 (list 2 3) (list (list 4 5) 6))))  ; → 21
```

### Type-Safe Programming Patterns
```aifpl
; Polymorphic function with type checking
(let ((safe-process (lambda (value)
                      (if (number? value)
                          (* value value)
                          (if (string? value)
                              (string-upcase value)
                              (if (list? value)
                                  (length value)
                                  "unknown type"))))))
  (map safe-process (list 5 "hello" (list 1 2 3) #t)))  ; → (25 "HELLO" 3 "unknown type")

; Type validation pipeline
(let ((validate-input (lambda (data)
                        (if (list? data)
                            (if (all? number? data)
                                (fold + 0 data)
                                "error: non-numeric data")
                            "error: not a list"))))
  (list (validate-input (list 1 2 3))        ; → 6
        (validate-input (list 1 "x" 3))      ; → "error: non-numeric data"
        (validate-input "not a list")))      ; → "error: not a list"

; Mixed type list processing
(let ((categorize (lambda (items)
                    (let ((numbers (filter number? items))
                          (strings (filter string? items))
                          (booleans (filter boolean? items)))
                      (list (list "numbers" numbers)
                            (list "strings" strings)
                            (list "booleans" booleans))))))
  (categorize (list 1 "hello" #t 3.14 "world" #f)))
; → (("numbers" (1 3.14)) ("strings" ("hello" "world")) ("booleans" (#t #f)))
```

### String Processing
```aifpl
; Split CSV data and process
(string-split "name,age,city" ",")              ; → ("name" "age" "city")
(first (string-split "John,25,NYC" ","))        ; → "John"
(length (string-split "a,b,c,d" ","))           ; → 4

; Build strings from components
(string-join (list "hello" "world") " ")        ; → "hello world"
(string-join (reverse (string-split "a-b-c" "-")) "+")  ; → "c+b+a"

; Conditional string processing
(if (> (string-length text) 10)
    (string-append (substring text 0 10) "...")
    text)

; Advanced text processing with new operators
(let ((clean-and-format (lambda (text)
                          (let ((trimmed (string-trim text))
                                (normalized (string-replace trimmed "  " " ")))
                            (string-upcase normalized)))))
  (clean-and-format "  hello    world  "))      ; → "HELLO  WORLD"
```

### Character-Level Processing
```aifpl
; Convert to characters, process, convert back
(string->list "hello")                          ; → ("h" "e" "l" "l" "o")
(reverse (string->list "hello"))                ; → ("o" "l" "l" "e" "h")
(list->string (reverse (string->list "hello"))) ; → "olleh"
```

### Data Structure Manipulation
```aifpl
; Build complex data structures
(list (list "name" "John") (list "age" 25))     ; → (("name" "John") ("age" 25))
(first (list (list 1 2) (list 3 4)))            ; → (1 2)
(rest (first (list (list 1 2 3) (list 4 5))))   ; → (2 3)

; Conditional data processing
(if (member? "error" status-list)
    (list "status" "error" "failed")
    (list "status" "ok" "success"))

; Advanced list manipulation with new operators
(let ((data (list 1 2 3 2 4 2 5))
      (target 2))
  (let ((cleaned (remove target data))
        (position-info (position target data)))
    (list "original" data 
          "cleaned" cleaned 
          "first-position" position-info)))
; → ("original" (1 2 3 2 4 2 5) "cleaned" (1 3 4 5) "first-position" 1)
```

## Advanced Features

### Tail Call Optimization

AIFPL automatically optimizes tail calls to prevent stack overflow in recursive functions:

```aifpl
; Factorial with tail recursion (automatically optimized)
(let ((factorial (lambda (n acc)
                   (if (<= n 1)
                       acc
                       (factorial (- n 1) (* n acc))))))
  (factorial 1000 1))                       ; Works with large numbers

; Mutual recursion is also optimized
(let ((is-even (lambda (n)
                 (if (= n 0) #t (is-odd (- n 1)))))
      (is-odd (lambda (n)
                (if (= n 0) #f (is-even (- n 1))))))
  (is-even 10000))                          ; → #t (no stack overflow)
```

### Lexical Scoping and Closures

Functions capture their lexical environment, creating closures:

```aifpl
; Counter closure
(let ((make-counter (lambda (start)
                      (let ((count start))
                        (lambda ()
                          (let ((current count))
                            (set! count (+ count 1))  ; Note: set! not implemented, this is conceptual
                            current))))))
  (let ((counter (make-counter 10)))
    ; Each call would increment the internal count
    counter))

; Function factories with captured environment
(let ((make-multiplier (lambda (factor)
                         (lambda (x) (* x factor)))))
  (let ((double (make-multiplier 2))
        (triple (make-multiplier 3)))
    (list (double 5) (triple 5))))         ; → (10 15)
```

### Error Handling Patterns

```aifpl
; Validation chains
(let ((validate-number (lambda (x)
                         (if (> x 0) x "error: not positive")))
      (validate-range (lambda (x)
                        (if (and (>= x 1) (<= x 100)) x "error: out of range"))))
  (let ((process (lambda (x)
                   (let ((step1 (validate-number x)))
                     (if (string? step1)
                         step1
                         (validate-range step1))))))
    (list (process 50) (process -5) (process 150))))  ; → (50 "error: not positive" "error: out of range")

; Safe operations with defaults
(let ((safe-head (lambda (lst default)
                   (if (null? lst) default (first lst))))
      (safe-tail (lambda (lst default)
                   (if (null? lst) default (last lst))))
      (safe-divide (lambda (x y default)
                     (if (= y 0) default (/ x y)))))
  (list (safe-head (list) "empty")
        (safe-tail (list 1 2 3) "empty")
        (safe-divide 10 0 "undefined")))    ; → ("empty" 3 "undefined")
```

## Design Principles

1. **Pure List Representation**: Everything is data, following traditional Lisp philosophy
2. **Functional Programming**: First-class functions, immutable data, no side effects
3. **Lexical Scoping**: Variables resolved in their definition environment
4. **Tail Call Optimization**: Automatic optimization for recursive patterns
5. **Type Safety**: Comprehensive type hints and strict type checking
6. **Error Handling**: Detailed error messages with position information
7. **Performance**: Efficient evaluation with automatic optimizations
8. **LISP Compatibility**: Following traditional LISP semantics where applicable
9. **Lazy Evaluation**: Conditionals and boolean operators use lazy evaluation
10. **Independence**: No dependencies on external packages
11. **Simplicity**: Direct S-expression evaluation without over-engineering
12. **Homoiconicity**: Code and data use identical representations

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
tool = AIFPL(floating_point_tolerance=1e-15)
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

# Function results
func_result = tool.evaluate('(lambda (x) (* x x))')
print(f"Function: {func_result}")  # Function: <aifpl.aifpl_environment.AIFPLLambdaFunction object>

formatted_func = tool.evaluate_and_format('(lambda (x) (* x x))')
print(f"Formatted Function: {formatted_func}")  # Formatted Function: <lambda (x)>

# Boolean results
bool_result = tool.evaluate('(member? 2 (list 1 2 3))')
print(f"Boolean: {bool_result}")  # Boolean: True

formatted_bool = tool.evaluate_and_format('(member? 2 (list 1 2 3))')
print(f"Formatted Boolean: {formatted_bool}")  # Formatted Boolean: #t

# New operators results
inequality_result = tool.evaluate('(!= 1 2 3)')
print(f"Inequality: {inequality_result}")  # Inequality: True

type_check = tool.evaluate('(number? 42)')
print(f"Type check: {type_check}")  # Type check: True

position_result = tool.evaluate('(position "world" (list "hello" "world"))')
print(f"Position: {position_result}")  # Position: 1
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

### Complex Functional Processing

```python
# Complex nested operations with functional programming
functional_expr = '''
(let ((data (list 1 2 3 4 5 6 7 8 9 10)))
  (let ((process (lambda (nums)
                   (let ((evens (filter (lambda (x) (= (% x 2) 0)) nums))
                         (squares (map (lambda (x) (* x x)) evens))
                         (sum (fold + 0 squares)))
                     sum))))
    (process data)))
'''

# Higher-order function composition
composition_expr = '''
(let ((compose (lambda (f g) (lambda (x) (f (g x)))))
      (add-one (lambda (x) (+ x 1)))
      (double (lambda (x) (* x 2))))
  (let ((transform (compose add-one double)))
    (map transform (list 1 2 3 4 5))))
'''

# Recursive data processing with tail optimization
recursive_expr = '''
(let ((factorial (lambda (n acc)
                   (if (<= n 1)
                       acc
                       (factorial (- n 1) (* n acc))))))
  (factorial 20 1))
'''

# Using new operators for data processing
advanced_processing = '''
(let ((data (list "  hello  " "WORLD" "test" "  EXAMPLE  "))
      (clean-normalize (lambda (text)
                         (string-downcase (string-trim text)))))
  (let ((cleaned (map clean-normalize data))
        (filtered (remove "test" cleaned))
        (positions (map (lambda (item) 
                          (position item cleaned)) 
                       filtered)))
    (list "cleaned" cleaned "filtered" filtered "positions" positions)))
'''
```
