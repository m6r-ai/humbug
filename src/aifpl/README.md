# AIFPL (AI Functional Programming Language)

AIFPL is a pure functional programming language with Lisp-like S-expression syntax, designed
primarily for use as an AI tool. It is homoiconic, strictly typed, and has no side effects,
making it safe for AI tool integration.

> **Language reference**: AIFPL is designed to be used by AIs. The authoritative language
> reference is the AI tool description, which any AI assistant in this system can access
> directly. If you are a human user and want to know how a language feature works, just ask
> the AI.

## Features

- Pure functional: no side effects, immutable data
- Homoiconic: code and data share the same representation (S-expressions)
- Strict, runtime type system with no implicit coercion between numeric types
- Proper lists only (no cons cells or improper lists)
- Tail call optimization for recursive functions
- Pattern matching
- Association lists (alists) with O(1) lookup
- Module system
- Trace debugging support

## Architecture

AIFPL uses an optimizing compiler pipeline feeding into a bytecode VM:

```text
Source code
    │
    ▼
AIFPLLexer          – tokenization
    │
    ▼
AIFPLParser         – S-expression parsing to AST (AIFPLASTNode)
    │
    ▼
AIFPLSemanticAnalyzer – type checking, scope analysis, free variable detection
    │
    ▼
AIFPLDesugarer      – expand syntactic sugar (let, let*, letrec, quote, etc.)
    │
    ▼
AIFPLConstantFolder – constant folding optimization pass
    │
    ▼
AIFPLIRBuilder      – lower AST to IR (AIFPLIRNode)
    │
    ▼
AIFPLCodegen        – generate bytecode (AIFPLBytecode / CodeObject)
    │
    ▼
AIFPLVM             – stack-based bytecode virtual machine
```

Modules are resolved and cached before optimization passes run, allowing
cross-module optimizations.

## Implementation

- Python 3.10+, no external dependencies
- All runtime values are immutable frozen dataclasses (`AIFPLValue` hierarchy)
- Lists are backed by Python tuples
- Alists use a tuple of pairs with a hash-backed dict for O(1) lookup
- Tail calls are detected during compilation and optimized in the VM

## Python API

### Basic usage

```python
from aifpl import AIFPL

tool = AIFPL()
result = tool.evaluate("(integer+ 1 2 3)")  # Returns: AIFPLValue
```

### Configuration

```python
tool = AIFPL(
    max_depth=200,                          # Maximum call stack depth
    module_path=[".", "aifpl_modules"],     # Module search path
)
```

### Module management

```python
tool.clear_module_cache()                   # Clear cached modules
tool.set_module_path([".", "my_modules"])   # Update search path (clears cache)
```

### Trace watchers

```python
from aifpl import AIFPLStdoutTraceWatcher, AIFPLBufferingTraceWatcher

# Print traces to stdout
tool = AIFPL()
tool.add_trace_watcher(AIFPLStdoutTraceWatcher())

# Collect traces into a buffer
watcher = AIFPLBufferingTraceWatcher()
tool.add_trace_watcher(watcher)
tool.evaluate("...")
print(watcher.get_traces())
```

### Error handling

```python
from aifpl import AIFPL, AIFPLError, AIFPLParseError, AIFPLEvalError

tool = AIFPL()
try:
    result = tool.evaluate("(integer+ 1 2")
except AIFPLParseError as e:
    print(f"Parse error: {e}")
except AIFPLEvalError as e:
    print(f"Eval error: {e}")
except AIFPLError as e:
    print(f"AIFPL error: {e}")
```

### Value types

Runtime values are instances of the `AIFPLValue` hierarchy:

| Class           | AIFPL type  |
|-----------------|-------------|
| `AIFPLInteger`  | `integer`   |
| `AIFPLFloat`    | `float`     |
| `AIFPLComplex`  | `complex`   |
| `AIFPLString`   | `string`    |
| `AIFPLBoolean`  | `boolean`   |
| `AIFPLSymbol`   | `symbol`    |
| `AIFPLList`     | `list`      |
| `AIFPLAList`    | `alist`     |
| `AIFPLFunction` | `function`  |

## Module system

Modules are `.aifpl` files containing a single expression that evaluates to a value
(typically an alist of exported functions). They are imported with `(import "module-name")`.

## Development tools

The `tools/` directory contains utilities for working with AIFPL:

- `aifpl_benchmark/` – performance benchmarking
- `aifpl_bytecode_analyzer/` – bytecode inspection
- `aifpl_checker/` – static analysis
- `aifpl_disassembler/` – bytecode disassembly
- `aifpl_pretty_print/` – code formatting
