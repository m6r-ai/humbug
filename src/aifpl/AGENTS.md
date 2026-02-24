# AGENTS.md - AIFPL Source Directory

## Purpose

This directory contains the complete implementation of AIFPL (AI Functional Programming
Language) — a pure functional language designed for use as an AI tool. This guide is
intended to help an AI navigate the design and assist with evolving the language.

## Authoritative Language Reference

The authoritative description of AIFPL's language semantics, operators, and built-in
functions is the **AI tool description** (available to any AI in this system via the
`help` tool). Do not rely on README.md for language semantics — it describes the
implementation architecture only. When in doubt about language behaviour, use the
AIFPL tool directly to test.

## Compiler Pipeline

The pipeline is orchestrated by `aifpl_compiler.py`, which chains the passes in this order:

```
AIFPLLexer                  aifpl_lexer.py
    ↓
AIFPLParser                 aifpl_parser.py
    ↓
AIFPLSemanticAnalyzer       aifpl_semantic_analyzer.py
    ↓
AIFPLModuleResolver         aifpl_module_resolver.py
    ↓  ← module ASTs are cached here (before optimization)
AIFPLDesugarer              aifpl_desugarer.py
    ↓
AIFPLConstantFolder         aifpl_constant_folder.py   (AST optimization pass)
    ↓
AIFPLIRBuilder              aifpl_ir_builder.py
    ↓
AIFPLCodeGen                aifpl_codegen.py
    ↓
AIFPLVM                     aifpl_vm.py
```

Key design point: module resolution happens **before** desugaring and optimization,
so modules are compiled to resolved ASTs and cached. When the importing code runs,
optimizations are applied across module boundaries.

## File-by-File Guide

| File | Role | Size |
|------|------|------|
| `aifpl.py` | Public API (`AIFPL` class). Also contains `_PRELUDE_SOURCE` — AIFPL source for variadic built-ins | Large |
| `aifpl_compiler.py` | Pipeline orchestrator — read this first to understand the flow | Small |
| `aifpl_lexer.py` | Tokenizer | Medium |
| `aifpl_parser.py` | S-expression parser → `AIFPLASTNode` tree | Medium |
| `aifpl_semantic_analyzer.py` | Scope analysis, arity checking, free variable detection | Large |
| `aifpl_module_resolver.py` | Resolves `import` forms, detects circular dependencies | Small |
| `aifpl_desugarer.py` | Expands all syntactic sugar: `let`, `let*`, `letrec`, `quote`, `match`, etc. → canonical form | Very large |
| `aifpl_constant_folder.py` | AST-level constant folding optimization pass | Very large |
| `aifpl_optimization_pass.py` | Base class for AST optimization passes | Tiny |
| `aifpl_ir.py` | IR dataclasses (`AIFPLIRExpr` union type) — the compilation plan | Small |
| `aifpl_ir_builder.py` | Lowers desugared AST → IR. Resolves variable addressing, tail call detection | Large |
| `aifpl_codegen.py` | Lowers IR → `CodeObject` bytecode | Medium |
| `aifpl_bytecode.py` | `Opcode` enum, `Instruction`, `CodeObject`, `BUILTIN_OPCODE_MAP` | Medium |
| `aifpl_bytecode_validator.py` | Validates bytecode correctness | Large |
| `aifpl_vm.py` | Stack-based bytecode VM. Executes `CodeObject`. Handles TCO, closures, trace | Very large |
| `aifpl_value.py` | All runtime value types (`AIFPLValue` hierarchy) | Medium |
| `aifpl_ast.py` | AST node types (`AIFPLASTNode` hierarchy) | Small |
| `aifpl_token.py` | Token type definitions | Tiny |
| `aifpl_builtin_registry.py` | Arity table for all builtins; generates bytecode stubs for fixed-arity builtins | Medium |
| `aifpl_error.py` | Exception hierarchy with structured error messages | Small |
| `aifpl_trace.py` | Trace watcher implementations | Small |
| `aifpl_pretty_printer.py` | AST pretty printer | Medium |
| `aifpl_dependency_analyzer.py` | Analyses binding group dependencies for `letrec` ordering | Small |

## The Prelude

Many built-in functions that appear variadic (e.g. `integer+`, `float*`, `string-append`,
`list`, `append`, `alist`, all typed comparison operators) are **implemented in AIFPL
itself** as lambdas in `_PRELUDE_SOURCE` inside `aifpl.py`. They delegate to the
fixed 2-argument opcode versions internally.

This is the split:
- **Fixed-arity builtins** → implemented as a bytecode stub in `AIFPLBuiltinRegistry`
  using a single opcode from `BUILTIN_OPCODE_MAP` in `aifpl_bytecode.py`
- **Variadic builtins** → implemented as AIFPL lambdas in `_PRELUDE_SOURCE` in `aifpl.py`,
  which call the fixed-arity opcode versions

`AIFPLBuiltinRegistry.ARITY_TABLE` is the single source of truth for arity of all
builtins and is consumed by both the semantic analyser and the registry itself.

## Adding a New Built-in Function

1. Add an opcode to the `Opcode` enum in `aifpl_bytecode.py`
2. Add the opcode → arity mapping to `BUILTIN_OPCODE_MAP` in `aifpl_bytecode.py`
3. Implement the opcode in `aifpl_vm.py`
4. Add the arity entry to `ARITY_TABLE` in `aifpl_builtin_registry.py`
5. If variadic: add a prelude lambda to `_PRELUDE_SOURCE` in `aifpl.py` and add the
   name to `prelude_names` in `AIFPLBuiltinRegistry.create_builtin_function_objects()`
6. Update the tool description to document the new function
7. Add tests in `tests/aifpl/`

## Adding a New Special Form

Special forms (things that are not regular function calls) are handled in multiple places:

1. **Desugarer** (`aifpl_desugarer.py`) — if the form needs to be expanded into simpler
   forms before IR building
2. **IR builder** (`aifpl_ir_builder.py`) — add a new `AIFPLIRXxx` dataclass in
   `aifpl_ir.py` and handle it in the IR builder
3. **Codegen** (`aifpl_codegen.py`) — generate bytecode for the new IR node
4. **Semantic analyser** (`aifpl_semantic_analyzer.py`) — if the form has scope or
   arity implications that need early checking
5. Update the tool description

## Value Types

All runtime values are **immutable frozen dataclasses** inheriting from `AIFPLValue`
in `aifpl_value.py`. The full hierarchy:

- `AIFPLInteger` — Python `int`
- `AIFPLFloat` — Python `float`
- `AIFPLComplex` — Python `complex`
- `AIFPLString` — Python `str`
- `AIFPLBoolean` — Python `bool`
- `AIFPLSymbol` — interned name (used for quoted symbols)
- `AIFPLList` — backed by a Python `tuple` (proper lists only, no cons cells)
- `AIFPLAList` — tuple of `(key, value)` pairs + hash-backed dict for O(1) lookup
- `AIFPLFunction` — compiled lambda or builtin stub; carries `CodeObject`, captured
  values, and variadic flag

## Key Design Decisions

- **Proper lists only**: `AIFPLList` is backed by a Python tuple. There are no cons
  cells or improper lists. `cons` requires the second argument to be a list.
- **Strict typing**: No implicit coercion between numeric types. All operators are
  type-specific (e.g. `integer+`, `float*`).
- **Tail call optimization**: Detected in `aifpl_ir_builder.py` (sets `is_tail_call`
  on `AIFPLIRCall`), implemented in `aifpl_vm.py` via `TAIL_CALL` opcode.
- **Closures**: Free variables are detected in `aifpl_semantic_analyzer.py` and
  captured via `MAKE_CLOSURE` opcode in the VM.
- **Homoiconicity**: Quoted expressions produce `AIFPLList`/`AIFPLSymbol` values
  identical in structure to the AST they represent.

## Tests

Tests mirror the source structure under `tests/aifpl/`. When making changes, run the
test suite to verify correctness. The `tools/` directory contains additional utilities:
- `aifpl_disassembler/` — inspect generated bytecode
- `aifpl_bytecode_analyzer/` — analyse bytecode patterns
- `aifpl_checker/` — static analysis
- `aifpl_benchmark/` — performance testing
