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

Many built-in functions that appear variadic (e.g. `integer+`, `float*`, `string-concat`,
`list`, `list-concat`, `alist`, all typed comparison operators) are **implemented in AIFPL
itself** as lambdas in `_PRELUDE_SOURCE` inside `aifpl.py`. They fold over the
fixed binary-opcode versions internally.

There are three categories of builtin:

- **Fixed-arity builtins** → implemented as a bytecode stub in `AIFPLBuiltinRegistry`
  using a single opcode from `BUILTIN_OPCODE_MAP` in `aifpl_bytecode.py`. The stub is
  a two-instruction `CodeObject` (`<opcode>` + `RETURN`) used when the builtin is passed
  as a first-class value.
- **Variadic builtins** → implemented as AIFPL lambdas in `_PRELUDE_SOURCE` in `aifpl.py`,
  which call the fixed binary-arity opcode versions. These names are listed in
  `prelude_names` inside `AIFPLBuiltinRegistry.create_builtin_function_objects()` and are
  skipped by the registry so the prelude's compiled lambdas take effect instead.
- **Optional-argument builtins** → a small set of builtins accept fewer arguments than
  their underlying opcode requires. The codegen (`aifpl_codegen.py`) synthesises the
  missing argument inline when emitting a direct call; the prelude supplies a wrapper
  lambda for first-class use. The affected builtins and their synthesised defaults are:

  | Builtin | Optional arg | Synthesised default |
  |---------|-------------|---------------------|
  | `range` | `step` | `1` (integer constant) |
  | `string-slice` | `end` | `(string-length str)` — re-evaluates the string arg |
  | `string->list` | `delimiter` | `""` (empty string → split into characters) |
  | `list-slice` | `end` | `(list-length lst)` — re-evaluates the list arg |
  | `list->string` | `separator` | `""` (empty string → concatenate without separator) |
  | `alist-get` | `default` | `#f` |

`AIFPLBuiltinRegistry.BUILTIN_OPCODE_ARITIES` is the arity table for **opcode-backed
builtins only** and is consumed by both the semantic analyser and the registry itself.
Pure-AIFPL prelude functions (`list-map`, `list-filter`, `list-fold`, `list-zip`, `list-unzip`, `list-find`, `list-any?`,
`list-all?`, etc.) are **not** in this table and must **not** be added — the registry asserts
that every entry has a corresponding `BUILTIN_OPCODE_MAP` entry, so adding a prelude-only
name will cause an assertion failure at startup. Prelude-only functions have their arity
enforced at runtime by the lambda itself, exactly like any user-defined function.

## Variable Addressing and `LOAD_NAME`

The IR builder resolves all variable references to one of three addressing modes:

- **`LOAD_VAR index`** — lexically-addressed local variable in the current frame.
  Used for all user-defined bindings (`let`, `let*`, `letrec`, lambda parameters).
- **`LOAD_PARENT_VAR index depth`** — lexically-addressed variable in an enclosing
  frame at `depth` levels up. Used for free variables captured from outer scopes
  (closures). Free variables are detected by the semantic analyser and captured via
  `MAKE_CLOSURE` at the call site.
- **`LOAD_NAME name_index`** — name-table lookup, used **only for global builtins**
  that are referenced as first-class values (i.e. not called directly with the correct
  fixed arity). When the codegen sees a direct call to a known builtin at the right
  arity it emits the primitive opcode directly; `LOAD_NAME` is emitted when the
  builtin name appears as a variable reference (e.g. passed to `list-map` or `list-fold`).
  The name table is populated from `AIFPLBuiltinRegistry` by the VM at startup.

## Design Decisions — Clarifications

- **No `cond` form**: Deliberate omission. `match` covers all multi-branch conditional
  use cases and is more expressive. Use nested `if` for simple two-branch conditions.
- **`integer-` vs `integer-neg`**: Both perform unary negation. `integer-` is the
  multi-arity subtraction operator that also handles the unary case (1 arg → negate);
  `integer-neg` is the dedicated fixed-arity (1, 1) unary opcode. They are equivalent
  for single-argument calls. Prefer `integer-neg` when unary negation is the intent.
- **`symbol` type**: Symbols are produced only by `quote`. There is no `symbol->string`
  or `string->symbol` conversion by design — symbols exist to support homoiconicity
  (code-as-data), not as a general-purpose key type. Use strings for alist keys.
- **Tail call optimization**: TCO is detected in `aifpl_ir_builder.py` (sets
  `is_tail_call` on `AIFPLIRCall`) and implemented via the `TAIL_CALL` opcode in the
  VM. It is correctly propagated through `let`/`let*`/`letrec` bodies, `if` branches,
  and `match` arms — anywhere the body expression is in tail position.
- **Self-recursive tail calls**: In addition to the general `TAIL_CALL` mechanism,
  direct self-recursive calls (a function calling itself) are further optimised: the
  IR builder sets `is_tail_recursive` on the `AIFPLIRCall`, and the codegen emits a
  plain `JUMP 0` (back to the start of the function) instead of `TAIL_CALL`, avoiding
  even the overhead of a new frame setup.

## Adding a New Built-in Function

1. Add an opcode to the `Opcode` enum in `aifpl_bytecode.py`
2. Add the opcode → arity mapping to `BUILTIN_OPCODE_MAP` in `aifpl_bytecode.py`
3. Implement the opcode in `aifpl_vm.py`
4. Add the arity entry to `BUILTIN_OPCODE_ARITIES` in `aifpl_builtin_registry.py`
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
