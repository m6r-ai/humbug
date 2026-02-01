# AIFPL Modules

This directory contains reusable AIFPL modules that can be imported using the `(import "module-name")` syntax.

## Available Modules

### math_utils
Mathematical utility functions.

**Exports:**
- `square` - Square a number: `((alist-get math "square") 5)` → 25
- `cube` - Cube a number: `((alist-get math "cube") 3)` → 27
- `factorial` - Calculate factorial: `((alist-get math "factorial") 5)` → 120
- `abs` - Absolute value: `((alist-get math "abs") -5)` → 5
- `pow` - Power function: `((alist-get math "pow") 2 10)` → 1024

**Example:**
```aifpl
(let ((math (import "math_utils")))
  ((alist-get math "square") 7))  ; → 49
```

### string_utils
String utility functions.

**Exports:**
- `words` - Split string into words: `((alist-get str "words") "hello world")` → ("hello" "world")
- `lines` - Split string into lines
- `capitalize` - Capitalize first letter: `((alist-get str "capitalize") "hello")` → "Hello"
- `reverse` - Reverse a string: `((alist-get str "reverse") "hello")` → "olleh"
- `count-words` - Count words in string

**Example:**
```aifpl
(let ((str (import "string_utils")))
  ((alist-get str "capitalize") "hello world"))  ; → "Hello world"
```

## Creating Your Own Modules

1. Create a `.aifpl` file in this directory
2. Write AIFPL code that returns a value (typically an alist)
3. Import it using `(import "your-module-name")`

**Example module (my_module.aifpl):**
```aifpl
(let ((my-function (lambda (x) (+ x 1))))
  (alist (list "my-function" my-function)))
```

**Using it:**
```aifpl
(let ((mod (import "my_module")))
  ((alist-get mod "my-function") 5))  ; → 6
```

## Module Best Practices

- Use `let` or `letrec` to define private helper functions
- Export only the public API via the alist
- Use descriptive names for exported functions
- Add comments to document your module
- Keep modules focused on a single purpose
- Use subdirectories to organize related modules
