# Menai (AI Functional Programming Language)

Menai is a pure functional programming language with Lisp-like S-expression syntax, designed
primarily for use as an AI tool. It is homoiconic, strictly typed, and has no side effects,
making it safe for AI tool integration.

> **Language reference**: Menai is designed to be used by AIs. The authoritative language
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
- Dictionaries with O(1) lookup
- Module system
- Trace debugging support

## Architecture

Menai uses an optimizing compiler pipeline feeding into a bytecode VM:

```text
Source code
    │
    ▼
MenaiLexer             – tokenization
    │
    ▼
MenaiParser            – S-expression parsing to AST (MenaiASTNode)
    │
    ▼
MenaiSemanticAnalyzer  – type checking, scope analysis, free variable detection
    │
    ▼
MenaiDesugarer         – expand syntactic sugar (let, let*, letrec, quote, etc.)
    │
    ▼
MenaiASTConstantFolder – constant folding optimization pass
    │
    ▼
MenaiIRBuilder         – lower AST to IR (MenaiIRNode)
    │
    ▼
MenaiIROptimizer       – IR-level dead binding elimination (fixed-point loop)
    │
    ▼
MenaiCodegen           – generate bytecode (MenaiBytecode / CodeObject)
    │
    ▼
MenaiVM                – stack-based bytecode virtual machine
```

Modules are resolved and cached before optimization passes run, allowing
cross-module optimizations.

## Implementation

- Python 3.10+, no external dependencies
- All runtime values are immutable frozen dataclasses (`MenaiValue` hierarchy)
- Lists are backed by Python tuples
- Alists use a tuple of pairs with a hash-backed dict for O(1) lookup
- Tail calls are detected during compilation and optimized in the VM

## Python API

### Basic usage

```python
from menai import Menai

tool = Menai()
result = tool.evaluate("(integer+ 1 2 3)")  # Returns: MenaiValue
```

### Configuration

```python
tool = Menai(
    max_depth=200,                          # Maximum call stack depth
    module_path=[".", "menai_modules"],     # Module search path
)
```

### Module management

```python
tool.clear_module_cache()                   # Clear cached modules
tool.set_module_path([".", "my_modules"])   # Update search path (clears cache)
```

### Trace watchers

```python
from menai import MenaiStdoutTraceWatcher, MenaiBufferingTraceWatcher

# Print traces to stdout
tool = Menai()
tool.add_trace_watcher(MenaiStdoutTraceWatcher())

# Collect traces into a buffer
watcher = MenaiBufferingTraceWatcher()
tool.add_trace_watcher(watcher)
tool.evaluate("...")
print(watcher.get_traces())
```

### Error handling

```python
from menai import Menai, MenaiError, MenaiParseError, MenaiEvalError

tool = Menai()
try:
    result = tool.evaluate("(integer+ 1 2")
except MenaiParseError as e:
    print(f"Parse error: {e}")
except MenaiEvalError as e:
    print(f"Eval error: {e}")
except MenaiError as e:
    print(f"Menai error: {e}")
```

### Value types

Runtime values are instances of the `MenaiValue` hierarchy:

| Class           | Menai type  |
|-----------------|-------------|
| `MenaiInteger`  | `integer`   |
| `MenaiFloat`    | `float`     |
| `MenaiComplex`  | `complex`   |
| `MenaiString`   | `string`    |
| `MenaiBoolean`  | `boolean`   |
| `MenaiSymbol`   | `symbol`    |
| `MenaiList`     | `list`      |
| `MenaiDict`    | `dict`     |
| `MenaiFunction` | `function`  |

## Module system

Modules are `.menai` files containing a single expression that evaluates to a value
(typically an dict of exported functions). They are imported with `(import "module-name")`.

## Development tools

The `tools/` directory contains utilities for working with Menai:

- `menai_benchmark/` – performance benchmarking
- `menai_bytecode_analyzer/` – bytecode inspection
- `menai_checker/` – static analysis
- `menai_disassembler/` – bytecode disassembly
- `menai_pretty_print/` – code formatting
