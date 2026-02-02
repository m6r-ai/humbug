# Module System

AIFPL provides a module system for organizing code into reusable components. Modules enable code sharing and namespace organization while maintaining the language's pure functional semantics.

## Overview

- `(import "module-name")` → Load and return a module (special form)
- Module files must contain **exactly one expression** that evaluates to a value (typically an alist)
- The `.aifpl` extension is automatically added to module names
- Modules are parsed and cached before optimization passes run
- Module values are evaluated at runtime when the importing code executes
- Circular imports are detected and prevented with clear error messages
- Module search path defaults to `["."]` but can be configured

## Module File Structure

A module file must contain **exactly one expression** that evaluates to the module's exported value. The parser enforces this constraint - multiple top-level expressions will cause an error.

### Valid Module Example

```aifpl
; File: math_utils.aifpl
; This is VALID - single expression that returns an alist
(let ((square (lambda (x) (* x x)))
      (cube (lambda (x) (* x x x))))
  (alist
    (list "square" square)
    (list "cube" cube)))
```

### Invalid Module Example

```aifpl
; File: bad_module.aifpl
; This is INVALID - multiple top-level expressions
(let ((helper (lambda (x) (* x 2))))
  helper)

(alist (list "api" (lambda (x) x)))  ; ERROR: Parser error - unexpected token after complete expression
```

## Using Modules

Import modules using the `import` special form. The import expression returns the module's value, which can be bound to a variable or used directly.

### Basic Import

```aifpl
; Import returns the module value (typically an alist)
(let ((math (import "math_utils")))
  ((alist-get math "square") 5))     ; → 25

; Access multiple functions
(let ((math (import "math_utils")))
  (let ((sq ((alist-get math "square") 3))
        (cb ((alist-get math "cube") 2)))
    (+ sq cb)))                       ; → 17 (9 + 8)
```

### Import in Any Expression Context

Since `import` is a special form that's resolved during compilation, it can be used in any expression context:

```aifpl
; Conditional imports
(let ((math-lib (if use-advanced
                    (import "advanced_math")
                    (import "simple_math"))))
  ((alist-get math-lib "compute") data))

; Import in let binding
(let ((utils (import "string_utils"))
      (data "Hello World"))
  ((alist-get utils "process") data))
```

## Module Search Path

Modules are searched for in the configured module path directories. The default search path is `["."]` (current directory).

### Path Resolution

- Module names are relative to the search path directories
- The `.aifpl` extension is automatically appended
- Subdirectories are supported using forward slashes
- Absolute paths and relative path navigation (`./`, `../`) are rejected for security

```aifpl
; Searches for "math_utils.aifpl" in search path
(import "math_utils")

; Searches for "lib/validation.aifpl" in search path
(import "lib/validation")

; ERROR: Absolute paths not allowed
(import "/usr/lib/module")

; ERROR: Relative navigation not allowed
(import "../other/module")
```

### Configuring the Module Path

```python
from aifpl import AIFPL

# Create AIFPL instance with custom module path
tool = AIFPL(module_path=[".", "lib", "/usr/local/aifpl_modules"])

# Or update the module path later
tool.set_module_path([".", "my_modules", "shared_modules"])
```

## Module Compilation and Caching

### Compilation Process

1. **Parse**: Module file is lexed and parsed into an AST
2. **Semantic Analysis**: AST is type-checked and validated
3. **Module Resolution**: Any imports within the module are recursively resolved
4. **Cache**: The fully resolved AST is cached (before optimization)
5. **Optimization**: When the importing code runs, optimizations are applied across module boundaries

### Caching Behavior

- Modules are cached after first import
- Subsequent imports return the cached version (fast)
- Cache is per-AIFPL instance
- Changing the module path clears the cache

```python
tool = AIFPL()

# First import: module is parsed and cached
result1 = tool.evaluate('(import "math_utils")')

# Second import: returns cached version
result2 = tool.evaluate('(import "math_utils")')

# Clear cache manually if needed (e.g., during development)
tool.clear_module_cache()
```

## Circular Dependency Detection

AIFPL detects circular imports and raises an error with the full import chain:

### Example of Circular Dependency

```aifpl
; File: a.aifpl
(let ((b (import "b")))
  (alist (list "from-a" (lambda (x) ((alist-get b "from-b") x)))))

; File: b.aifpl  
(let ((a (import "a")))  ; ERROR: Circular import detected: a → b → a
  (alist (list "from-b" (lambda (x) ((alist-get a "from-a") x)))))
```

### Error Message

```
AIFPLCircularImportError: Circular import detected
Import chain: a → b → a
```

## Private Functions

Functions not included in the exported alist are private to the module:

```aifpl
; File: api_module.aifpl
(let ((private-helper (lambda (x) (* x 2)))      ; Private - not exported
      (public-double (lambda (x) (private-helper x)))  ; Public - uses private helper
      (public-square (lambda (x) (* x x))))       ; Public
  (alist
    (list "double" public-double)
    (list "square" public-square)))
; private-helper is not accessible to importers
```

## Module Examples

### Simple Utility Module

```aifpl
; File: string_utils.aifpl
(let ((trim-and-upper (lambda (s) (string-upcase (string-trim s))))
      (split-words (lambda (s) (string-split s " ")))
      (join-with-comma (lambda (lst) (string-join lst ", "))))
  (alist
    (list "trim-and-upper" trim-and-upper)
    (list "split-words" split-words)
    (list "join-with-comma" join-with-comma)))
```

Using the module:

```aifpl
(let ((utils (import "string_utils")))
  (let ((process (alist-get utils "trim-and-upper")))
    (process "  hello world  ")))     ; → "HELLO WORLD"
```

### Module with Nested Modules

```aifpl
; File: data_processing.aifpl
(let ((math (import "math_utils"))
      (strings (import "string_utils")))
  (let ((process-record (lambda (record)
                          (let ((value (first record))
                                (label (first (rest record))))
                            (let ((squared ((alist-get math "square") value))
                                  (upper ((alist-get strings "trim-and-upper") label)))
                              (list squared upper))))))
    (alist (list "process-record" process-record))))
```

### Configuration Module

```aifpl
; File: config.aifpl
(alist
  (list "database" (alist
                     (list "host" "localhost")
                     (list "port" 5432)
                     (list "name" "myapp")))
  (list "server" (alist
                   (list "port" 8080)
                   (list "workers" 4)))
  (list "features" (alist
                     (list "debug" #f)
                     (list "logging" #t))))
```

Using the configuration:

```aifpl
(let ((config (import "config")))
  (let ((db-config (alist-get config "database")))
    (alist-get db-config "host")))    ; → "localhost"
```

## Best Practices

### 1. Export Alists for Public APIs

```aifpl
; Good: Clear public API
(alist
  (list "function1" func1)
  (list "function2" func2))

; Avoid: Exporting single function (harder to extend)
(lambda (x) (* x 2))
```

### 2. Use Descriptive Module Names

```aifpl
; Good
(import "date_utils")
(import "validation")
(import "lib/http_client")

; Avoid
(import "utils")
(import "helpers")
(import "misc")
```

### 3. Keep Modules Focused

Each module should have a single, clear purpose:

```aifpl
; Good: Focused modules
(import "string_utils")
(import "math_utils")
(import "date_utils")

; Avoid: Kitchen-sink module
(import "utils")  ; Contains everything
```

### 4. Document Module Exports

```aifpl
; File: validation.aifpl
; Exports:
;   "email?" - (lambda (string) boolean) - Check if string is valid email
;   "phone?" - (lambda (string) boolean) - Check if string is valid phone
;   "url?" - (lambda (string) boolean) - Check if string is valid URL

(let ((email? (lambda (s) (string-contains? s "@")))
      (phone? (lambda (s) (>= (string-length s) 10)))
      (url? (lambda (s) (or (string-prefix? s "http://")
                            (string-prefix? s "https://")))))
  (alist
    (list "email?" email?)
    (list "phone?" phone?)
    (list "url?" url?)))
```

### 5. Handle Missing Modules Gracefully

```aifpl
; In application code, you might want to handle import errors
; (though AIFPL will raise AIFPLModuleNotFoundError)

; Document required modules in your main file
; Required modules:
;   - math_utils.aifpl
;   - string_utils.aifpl
;   - lib/validation.aifpl
```

## Module System Limitations

1. **No Dynamic Imports**: Module names must be string literals (cannot be computed at runtime)
2. **No Conditional Compilation**: All imports are resolved during compilation
3. **No Module Reloading**: Cached modules persist for the lifetime of the AIFPL instance
4. **No Version Management**: No built-in support for module versions or dependency resolution
5. **Single Expression**: Module files must contain exactly one expression

## Python API for Module Management

```python
from aifpl import AIFPL

# Create instance with custom module path
tool = AIFPL(module_path=[".", "lib", "/usr/local/aifpl_modules"])

# Evaluate code with imports
result = tool.evaluate('''
  (let ((math (import "math_utils")))
    ((alist-get math "square") 5))
''')

# Clear module cache (useful during development)
tool.clear_module_cache()

# Update module path (also clears cache)
tool.set_module_path([".", "new_modules"])

# Check current module path
print(tool.module_path)  # ['.' , 'new_modules']
```

## Error Messages

### Module Not Found

```
AIFPLModuleNotFoundError: Module 'math_utils' not found
Searched in: ['.', 'lib', '/usr/local/aifpl_modules']
Suggestion: Check module name spelling and ensure the file exists in the search path
```

### Circular Import

```
AIFPLCircularImportError: Circular import detected
Import chain: module_a → module_b → module_c → module_a
Suggestion: Refactor modules to break the circular dependency
```

### Invalid Module Path

```
AIFPLModuleError: Absolute module paths are not allowed: '/usr/lib/module'
Context: Module names must be relative to the module search path
Suggestion: Use a simple module name like 'calendar' or 'lib/validation'
```

### Multiple Expressions in Module

```
AIFPLParseError: Unexpected token after complete expression
Context: Module files must contain exactly one expression
Suggestion: Wrap multiple definitions in a single let or letrec expression
```
