# Menai Modules

This directory contains reusable Menai modules that can be imported using the `(import "module-name")` syntax.

## Available Modules

### math_utils
Mathematical utility functions.

**Exports:**
- `square` - Square a number: `((dict-get math "square") 5)` → 25
- `cube` - Cube a number: `((dict-get math "cube") 3)` → 27
- `factorial` - Calculate factorial: `((dict-get math "factorial") 5)` → 120
- `abs` - Absolute value: `((dict-get math "abs") -5)` → 5
- `pow` - Power function: `((dict-get math "pow") 2 10)` → 1024

**Example:**
```menai
(let ((math (import "math_utils")))
  ((dict-get math "square") 7))  ; → 49
```

### string_utils
String utility functions.

**Exports:**
- `words` - Split string into words: `((dict-get str "words") "hello world")` → ("hello" "world")
- `lines` - Split string into lines
- `capitalize` - Capitalize first letter: `((dict-get str "capitalize") "hello")` → "Hello"
- `reverse` - Reverse a string: `((dict-get str "reverse") "hello")` → "olleh"
- `count-words` - Count words in string

**Example:**
```menai
(let ((str (import "string_utils")))
  ((dict-get str "capitalize") "hello world"))  ; → "Hello world"
```

## Creating Your Own Modules

1. Create a `.menai` file in this directory
2. Write Menai code that returns a value (typically an dict)
3. Import it using `(import "your-module-name")`

**Example module (my_module.menai):**
```menai
(let ((my-function (lambda (x) (+ x 1))))
  (dict (list "my-function" my-function)))
```

**Using it:**
```menai
(let ((mod (import "my_module")))
  ((dict-get mod "my-function") 5))  ; → 6
```

## Module Best Practices

- Use `let` or `letrec` to define private helper functions
- Export only the public API via the dict
- Use descriptive names for exported functions
- Add comments to document your module
- Keep modules focused on a single purpose
- Use subdirectories to organize related modules
