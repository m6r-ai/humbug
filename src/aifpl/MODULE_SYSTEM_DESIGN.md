# AIFPL Module System Design

## Status: Design Proposal (Not Yet Implemented)

This document outlines a proposed module system for AIFPL that maintains the language's functional purity while enabling reusable libraries and code composition.

## Motivation

AIFPL is designed for **single-expression evaluation** with no side effects or mutable state. However, building real software requires:

- **Reusable libraries** - Calendar arithmetic, validation, data processing, etc.
- **Code organization** - Breaking large programs into manageable pieces
- **Dependency management** - One library using functions from another
- **Namespace management** - Avoiding name collisions

Currently, we work around this by using Python as a "backdoor module system":

```python
# Python manages the "module system"
calendar = bridge.evaluate_file("calendar.aifpl")  # Returns dict
validation = bridge.evaluate_file("validation.aifpl")

# Python passes modules back into AIFPL
result = bridge.evaluate_with_data(
    '((alist-get calendar "add-working-days") start 5 cal)',
    {"calendar": calendar, ...}
)
```

This works but has problems:
- Can't write pure-AIFPL programs that use multiple libraries
- Python glue code is essential infrastructure, not just convenience
- Mixing paradigms: functional evaluation + imperative module management
- No clear dependency declarations in AIFPL code

## Design Principles

1. **Maintain functional purity** - No side effects, no mutable state
2. **Deterministic evaluation** - Same input always produces same output
3. **Minimal syntax changes** - Build on existing AIFPL constructs
4. **Backward compatible** - Existing code continues to work
5. **Python controls I/O** - AIFPL evaluator doesn't do file operations directly
6. **Modules are values** - Can be passed around, composed, inspected

## Core Concept: Modules Are Alists

**Key insight:** We're already 90% there! Modules are just alists of functions.

```scheme
; calendar.aifpl - This IS a module!
(let (
  (add-working-days (lambda (date days calendar) ...))
  (working-days-between (lambda (start end calendar) ...))
  (internal-helper (lambda (x) ...)))  ; Private function
  
  ; The alist IS the module interface
  (alist
    (list "add-working-days" add-working-days)
    (list "working-days-between" working-days-between)))
    ; internal-helper not exported - it's private!
```

**What makes this elegant:**
- No new value types needed
- The alist naturally defines the public API
- Functions not in the alist are private (lexically scoped)
- Modules are first-class values (can be passed, stored, composed)
- Already works with current AIFPL!

## Proposed Addition: `import` Special Form

Add a single special form to load modules:

```scheme
(import "module-name")  ; Returns the alist from module-name.aifpl
```

### Example Usage

```scheme
; validation.aifpl
(let ((calendar (import "calendar")))  ; Load calendar module
  (let (
    (detect-cycles 
      (lambda (tasks deps)
        ; Use calendar functions
        (let ((start-date (get-start-date tasks))
              (end-date (get-end-date tasks)))
          ((alist-get calendar "working-days-between") 
           start-date end-date my-calendar))))
    
    (validate-task
      (lambda (task) ...)))
    
    ; Export validation functions
    (alist
      (list "detect-cycles" detect-cycles)
      (list "validate-task" validate-task))))
```

### Semantics

- `(import "module-name")` is a **special form** (like `quote`, `if`, `let`)
- Evaluates to the **alist** returned by evaluating `module-name.aifpl`
- Evaluation is **deterministic** - same file always produces same alist
- Modules are **cached** - each file evaluated only once per AIFPL instance
- Import resolution is **delegated to Python** - via module search path

## Implementation Architecture

### Python Side: Module Resolution

```python
class AIFPL:
    def __init__(self, module_path=None):
        """
        Initialize AIFPL with module search path.
        
        Args:
            module_path: List of directories to search for modules
                        Default: ["."] (current directory)
        """
        self.module_path = module_path or ["."]
        self.module_cache = {}  # module_name -> alist
    
    def resolve_module(self, module_name: str) -> str:
        """
        Find module file in search path.
        
        Args:
            module_name: Name like "calendar" or "lib/validation"
        
        Returns:
            Full path to module file
            
        Raises:
            ModuleNotFoundError: If module not found in search path
        """
        for directory in self.module_path:
            module_path = Path(directory) / f"{module_name}.aifpl"
            if module_path.exists():
                return str(module_path)
        
        raise ModuleNotFoundError(
            f"Module '{module_name}' not found in {self.module_path}"
        )
    
    def load_module(self, module_name: str):
        """
        Load and evaluate a module, with caching.
        
        Args:
            module_name: Name of module to load
            
        Returns:
            The alist returned by evaluating the module
        """
        # Check cache
        if module_name in self.module_cache:
            return self.module_cache[module_name]
        
        # Resolve to file path
        module_path = self.resolve_module(module_name)
        
        # Load and evaluate
        with open(module_path, 'r') as f:
            code = f.read()
        
        # Parse and evaluate (imports within this module will recurse)
        result = self.evaluate(code)
        
        # Cache the result
        self.module_cache[module_name] = result
        
        return result
```

### AIFPL Side: `import` Evaluation

```python
class AIFPLEvaluator:
    def evaluate(self, expr, constants, prelude):
        """Evaluate an AIFPL expression."""
        
        # ... existing code ...
        
        # Handle import special form
        if self._is_import(expr):
            return self._evaluate_import(expr)
        
        # ... rest of evaluation ...
    
    def _is_import(self, expr):
        """Check if expression is (import "module-name")."""
        return (isinstance(expr, AIFPLList) and 
                len(expr) == 2 and
                isinstance(expr[0], AIFPLSymbol) and
                expr[0].name == "import" and
                isinstance(expr[1], AIFPLString))
    
    def _evaluate_import(self, expr):
        """
        Evaluate (import "module-name").
        
        Returns the alist from the module.
        """
        module_name = expr[1].value  # Extract string from AIFPLString
        
        # Delegate to AIFPL instance to load the module
        # (AIFPL instance has module_path and cache)
        return self.aifpl_instance.load_module(module_name)
```

## Usage Examples

### Simple Library Usage

```scheme
; main.aifpl
(let ((calendar (import "calendar")))
  ((alist-get calendar "add-working-days") 
   "2025-01-15" 
   10 
   my-calendar))
```

### Composing Multiple Libraries

```scheme
; analysis.aifpl
(let ((calendar (import "calendar"))
      (validation (import "validation")))
  
  (let ((analyze-project
          (lambda (project)
            ; Validate first
            (let ((valid? ((alist-get validation "validate-project") project)))
              (if valid?
                  ; Then analyze dates
                  (let ((start (get-start project))
                        (end (get-end project)))
                    ((alist-get calendar "working-days-between") 
                     start end (get-calendar project)))
                  #f)))))
    
    ; Export
    (alist
      (list "analyze-project" analyze-project))))
```

### Nested Imports (Transitive Dependencies)

```scheme
; validation.aifpl - imports calendar
(let ((calendar (import "calendar")))
  (let ((validate-dates ...))
    (alist (list "validate-dates" validate-dates))))

; analysis.aifpl - imports validation (which imports calendar)
(let ((validation (import "validation")))
  ; validation already loaded calendar internally
  ; we don't need to import calendar again unless we use it directly
  (let ((analyze ...))
    (alist (list "analyze" analyze))))
```

### Private vs Public Functions

```scheme
; math-utils.aifpl
(let (
  ; Public functions
  (factorial (lambda (n) ...))
  (fibonacci (lambda (n) ...))
  
  ; Private helper - not exported
  (helper (lambda (x y) ...)))
  
  ; Only export public API
  (alist
    (list "factorial" factorial)
    (list "fibonacci" fibonacci)))
```

## Module Search Path

Modules are resolved using a search path (like Python's `sys.path` or Node's `node_modules`):

```python
# Default: current directory
aifpl = AIFPL()

# Custom search path
aifpl = AIFPL(module_path=[
    ".",           # Current directory
    "./lib",       # Local libraries
    "./vendor",    # Third-party libraries
    "/usr/local/lib/aifpl"  # System libraries
])
```

**Resolution rules:**
1. Search directories in order
2. First match wins
3. Relative imports: `"./utils"` starts from current file's directory
4. Absolute imports: `"calendar"` searches module_path

## Caching Strategy

**Per-AIFPL-instance caching:**
- Each `AIFPL()` instance has its own module cache
- Module loaded once per instance, reused for subsequent imports
- Deterministic: same file always produces same result
- Safe for concurrent evaluations (each gets its own instance)

**Why not global caching?**
- Different instances might have different module_paths
- Allows testing with mock modules
- Clearer lifecycle management

## Circular Dependencies

**Approach: Detect and error**

```python
class AIFPL:
    def __init__(self, module_path=None):
        self.module_path = module_path or ["."]
        self.module_cache = {}
        self.loading_stack = []  # Track currently-loading modules
    
    def load_module(self, module_name: str):
        # Check for circular dependency
        if module_name in self.loading_stack:
            cycle = self.loading_stack + [module_name]
            raise CircularImportError(
                f"Circular import detected: {' -> '.join(cycle)}"
            )
        
        # Check cache
        if module_name in self.module_cache:
            return self.module_cache[module_name]
        
        # Mark as loading
        self.loading_stack.append(module_name)
        
        try:
            # Load and evaluate
            result = self._do_load_module(module_name)
            self.module_cache[module_name] = result
            return result
        finally:
            # Always pop from stack
            self.loading_stack.pop()
```

**Why not allow circular imports?**
- Simpler implementation
- Easier to reason about
- Forces better design (break cycles with interfaces)
- Consistent with functional paradigm (no forward references)

## Error Handling

### Module Not Found

```
AIFPLModuleError: Module 'calendar' not found
  Searched in:
    - ./calendar.aifpl
    - ./lib/calendar.aifpl
    - /usr/local/lib/aifpl/calendar.aifpl
  
  Suggestion: Check module name spelling or add directory to module_path
```

### Circular Import

```
AIFPLCircularImportError: Circular import detected
  Import chain:
    validation.aifpl
    -> calendar.aifpl
    -> validation.aifpl (circular!)
  
  Suggestion: Break the cycle by extracting shared code to a third module
```

### Module Evaluation Error

```
AIFPLEvalError: Error evaluating module 'calendar'
  File: ./calendar.aifpl
  Line: 42
  Error: Undefined variable: 'add-one-day'
  
  Context: Module imports are evaluated in dependency order.
           Check that all functions are defined before use.
```

## Backward Compatibility

**Existing code continues to work:**

```scheme
; Old style - still valid!
(let (
  (add-working-days (lambda ...))
  (working-days-between (lambda ...)))
  
  (alist
    (list "add-working-days" add-working-days)
    (list "working-days-between" working-days-between)))
```

**Python bridge still works:**

```python
# Old style - still valid!
calendar = bridge.evaluate_file("calendar.aifpl")
result = bridge.evaluate_with_data(
    '((alist-get calendar "add-working-days") ...)',
    {"calendar": calendar}
)
```

**New style is opt-in:**

```scheme
; New style - use import
(let ((calendar (import "calendar")))
  ((alist-get calendar "add-working-days") ...))
```

## Future Enhancements

### Phase 2: Namespace Syntax Sugar

```scheme
; Instead of:
((alist-get calendar "add-working-days") start 5 cal)

; Could support:
(calendar:add-working-days start 5 cal)

; Or even:
(use calendar (add-working-days working-days-between))
(add-working-days start 5 cal)  ; No prefix needed
```

### Phase 3: Module Metadata

```scheme
; Module with metadata
(module calendar
  (version "1.0.0")
  (author "AIFPL Team")
  (dependencies ())
  
  (let (...)
    (alist ...)))
```

### Phase 4: Package Manager

```bash
# Install packages
aifpl install calendar-utils
aifpl install validation@2.1.0

# Packages stored in ~/.aifpl/packages/
# Automatically added to module_path
```

### Phase 5: Hot Reloading

```python
# For REPL/development
aifpl = AIFPL(module_path=[...])
aifpl.reload_module("calendar")  # Clear cache, re-evaluate
```

## Comparison with Other Languages

### Scheme/Racket Modules

```scheme
; Racket
(module calendar racket
  (provide add-working-days)
  (define (add-working-days ...) ...))

(require "calendar.rkt")
(add-working-days ...)
```

**AIFPL approach:**
- Simpler (no `provide`, just alist)
- More explicit (alist shows exports clearly)
- More flexible (alists are first-class values)

### Python Modules

```python
# Python
import calendar
calendar.add_working_days(...)

from calendar import add_working_days
add_working_days(...)
```

**AIFPL approach:**
- More explicit (import returns value)
- Functional (modules are immutable values)
- No implicit namespace pollution

### JavaScript Modules

```javascript
// ES6
import { addWorkingDays } from './calendar.js';
addWorkingDays(...);

// CommonJS
const calendar = require('./calendar');
calendar.addWorkingDays(...);
```

**AIFPL approach:**
- Similar to CommonJS (import returns value)
- More explicit (alist access)
- Functional (no module.exports mutation)

## Implementation Checklist

### Minimal Viable Module System

- [ ] Add `import` to parser as special form
- [ ] Add `module_path` parameter to `AIFPL.__init__`
- [ ] Implement `resolve_module()` - search path resolution
- [ ] Implement `load_module()` - load, evaluate, cache
- [ ] Add `loading_stack` for circular dependency detection
- [ ] Update evaluator to handle `import` expressions
- [ ] Add error classes: `ModuleNotFoundError`, `CircularImportError`
- [ ] Write tests for basic import
- [ ] Write tests for transitive imports
- [ ] Write tests for circular import detection
- [ ] Write tests for caching behavior
- [ ] Update documentation with import examples

### Nice to Have

- [ ] Relative imports (`"./utils"`)
- [ ] Module path environment variable (`AIFPL_PATH`)
- [ ] Better error messages with import chain context
- [ ] Module reload for development
- [ ] Import profiling/debugging
- [ ] Namespace syntax sugar (`module:function`)

## Open Questions

1. **Module file extension** - `.aifpl` or something else? `.aifpl-mod`?
2. **Relative imports** - How to handle `"./utils"` vs `"utils"`?
3. **Standard library** - Should we ship standard modules? Where?
4. **Versioning** - Do modules need version numbers? Probably not initially.
5. **Prelude interaction** - How do modules interact with the prelude (map, filter, etc.)?
6. **Cross-language modules** - Could a "module" be implemented in Python? Interesting but complex.

## Conclusion

This module system design:
- ✅ Maintains AIFPL's functional purity
- ✅ Enables reusable libraries
- ✅ Minimal syntax changes (just `import`)
- ✅ Backward compatible
- ✅ Builds on existing constructs (alists)
- ✅ Python controls I/O (module resolution)
- ✅ Deterministic evaluation
- ✅ First-class modules (alists are values)

The key insight: **Modules are just alists of functions**. We already have the data structure, we just need `import` to load them!

---

**Document Version:** 1.0  
**Date:** 2025-01-15  
**Status:** Design Proposal  
**Next Steps:** Review, refine, prototype implementation
