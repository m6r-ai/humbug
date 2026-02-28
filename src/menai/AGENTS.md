# AGENTS.md - Menai Source Directory

## Purpose

This directory contains the complete implementation of Menai (AI Functional Programming
Language) — a pure functional language designed for use as an AI tool. This guide is
intended to help an AI navigate the design and assist with evolving the language.

## Authoritative Language Reference

The authoritative description of Menai's language semantics, operators, and built-in
functions is the **AI tool description** (available to any AI in this system via the
`help` tool). Do not rely on README.md for language semantics — it describes the
implementation architecture only. When in doubt about language behaviour, use the
Menai tool directly to test.

## Compiler Pipeline

The pipeline is orchestrated by `menai_compiler.py`, which chains the passes in this order:

```
MenaiLexer                  menai_lexer.py
    ↓
MenaiParser                 menai_parser.py
    ↓
MenaiSemanticAnalyzer       menai_semantic_analyzer.py
    ↓
MenaiModuleResolver         menai_module_resolver.py
    ↓  ← module ASTs are cached here (before optimization)
MenaiDesugarer              menai_desugarer.py
    ↓
MenaiASTConstantFolder      menai_ast_constant_folder.py   (AST optimization pass)
    ↓
MenaiIRBuilder              menai_ir_builder.py
    ↓
MenaiCodeGen                menai_codegen.py
    ↓
MenaiVM                     menai_vm.py
```

Key design point: module resolution happens **before** desugaring and optimization,
so modules are compiled to resolved ASTs and cached. When the importing code runs,
optimizations are applied across module boundaries.

## File-by-File Guide

| File | Role | Size |
|------|------|------|
| `menai.py` | Public API (`Menai` class). Also contains `_PRELUDE_SOURCE` — Menai source for variadic built-ins | Large |
| `menai_ast_optimization_pass.py` | Base class for AST optimization passes | Tiny |
| `menai_compiler.py` | Pipeline orchestrator — read this first to understand the flow | Small |
| `menai_lexer.py` | Tokenizer | Medium |
| `menai_parser.py` | S-expression parser → `MenaiASTNode` tree | Medium |
| `menai_semantic_analyzer.py` | Scope analysis, arity checking, free variable detection | Large |
| `menai_module_resolver.py` | Resolves `import` forms, detects circular dependencies | Small |
| `menai_desugarer.py` | Expands all syntactic sugar: `let`, `let*`, `letrec`, `quote`, `match`, etc. → canonical form | Very large |
| `menai_constant_folder.py` | AST-level constant folding optimization pass | Very large |
| `menai_ir.py` | IR dataclasses (`MenaiIRExpr` union type) — the compilation plan | Small |
| `menai_ir_builder.py` | Lowers desugared AST → IR. Resolves variable addressing, tail call detection | Large |
| `menai_ir_optimization_pass.py` | Base class for IR optimization passes | Tiny |
| `menai_codegen.py` | Lowers IR → `CodeObject` bytecode | Medium |
| `menai_bytecode.py` | `Opcode` enum, `Instruction`, `CodeObject`, `BUILTIN_OPCODE_MAP` | Medium |
| `menai_bytecode_validator.py` | Validates bytecode correctness | Large |
| `menai_vm.py` | Stack-based bytecode VM. Executes `CodeObject`. Handles TCO, closures, trace | Very large |
| `menai_value.py` | All runtime value types (`MenaiValue` hierarchy) | Medium |
| `menai_ast.py` | AST node types (`MenaiASTNode` hierarchy) | Small |
| `menai_token.py` | Token type definitions | Tiny |
| `menai_builtin_registry.py` | Arity table for all builtins; generates bytecode stubs for fixed-arity builtins | Medium |
| `menai_error.py` | Exception hierarchy with structured error messages | Small |
| `menai_trace.py` | Trace watcher implementations | Small |
| `menai_pretty_printer.py` | AST pretty printer | Medium |
| `menai_dependency_analyzer.py` | Analyses binding group dependencies for `letrec` ordering | Small |

## The Prelude

Many built-in functions that appear variadic (e.g. `integer+`, `float*`, `string-concat`,
`list`, `list-concat`, `dict`, all typed comparison operators) are **implemented in Menai
itself** as lambdas in `_PRELUDE_SOURCE` inside `menai.py`. They fold over the
fixed binary-opcode versions internally.

There are three categories of builtin:

- **Fixed-arity builtins** → implemented as a bytecode stub in `MenaiBuiltinRegistry`
  using a single opcode from `BUILTIN_OPCODE_MAP` in `menai_bytecode.py`. The stub is
  a two-instruction `CodeObject` (`<opcode>` + `RETURN`) used when the builtin is passed
  as a first-class value.
- **Variadic builtins** → implemented as Menai lambdas in `_PRELUDE_SOURCE` in `menai.py`,
  which call the fixed binary-arity opcode versions. These names are listed in
  `prelude_names` inside `MenaiBuiltinRegistry.create_builtin_function_objects()` and are
  skipped by the registry so the prelude's compiled lambdas take effect instead.
- **Optional-argument builtins** → a small set of builtins accept fewer arguments than
  their underlying opcode requires. The codegen (`menai_codegen.py`) synthesises the
  missing argument inline when emitting a direct call; the prelude supplies a wrapper
  lambda for first-class use. The affected builtins and their synthesised defaults are:

  | Builtin | Optional arg | Synthesised default |
  |---------|-------------|---------------------|
  | `range` | `step` | `1` (integer constant) |
  | `string-slice` | `end` | `(string-length str)` — re-evaluates the string arg |
  | `string->list` | `delimiter` | `""` (empty string → split into characters) |
  | `list-slice` | `end` | `(list-length lst)` — re-evaluates the list arg |
  | `list->string` | `separator` | `""` (empty string → concatenate without separator) |
  | `dict-get` | `default` | `#f` |

`MenaiBuiltinRegistry.BUILTIN_OPCODE_ARITIES` is the arity table for **opcode-backed
builtins only** and is consumed by both the semantic analyser and the registry itself.
Pure-Menai prelude functions (`map-list`, `filter-list`, `fold-list`, `zip-list`, `find-list`, `any-list?`,
`all-list?`, etc.) are **not** in this table and must **not** be added — the registry asserts
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
  builtin name appears as a variable reference (e.g. passed to `map-list` or `fold-list`).
  The name table is populated from `MenaiBuiltinRegistry` by the VM at startup.

## Design Decisions — Clarifications

- **No `cond` form**: Deliberate omission. `match` covers all multi-branch conditional
  use cases and is more expressive. Use nested `if` for simple two-branch conditions.
- **`integer-` vs `integer-neg`**: Both perform unary negation. `integer-` is the
  multi-arity subtraction operator that also handles the unary case (1 arg → negate);
  `integer-neg` is the dedicated fixed-arity (1, 1) unary opcode. They are equivalent
  for single-argument calls. Prefer `integer-neg` when unary negation is the intent.
- **`symbol` type**: Symbols are produced only by `quote`. There is no `symbol->string`
  or `string->symbol` conversion by design — symbols exist to support homoiconicity
  (code-as-data), not as a general-purpose key type. Use strings for dict keys.
- **Tail call optimization**: TCO is detected in `menai_ir_builder.py` (sets
  `is_tail_call` on `MenaiIRCall`) and implemented via the `TAIL_CALL` opcode in the
  VM. It is correctly propagated through `let`/`let*`/`letrec` bodies, `if` branches,
  and `match` arms — anywhere the body expression is in tail position.
- **Self-recursive tail calls**: In addition to the general `TAIL_CALL` mechanism,
  direct self-recursive calls (a function calling itself) are further optimised: the
  IR builder sets `is_tail_recursive` on the `MenaiIRCall`, and the codegen emits a
  plain `JUMP 0` (back to the start of the function) instead of `TAIL_CALL`, avoiding
  even the overhead of a new frame setup.

## Adding a New Built-in Function

1. Add an opcode to the `Opcode` enum in `menai_bytecode.py`
2. Add the opcode → arity mapping to `BUILTIN_OPCODE_MAP` in `menai_bytecode.py`
3. Implement the opcode in `menai_vm.py`
4. Add the arity entry to `BUILTIN_OPCODE_ARITIES` in `menai_builtin_registry.py`
5. If variadic: add a prelude lambda to `_PRELUDE_SOURCE` in `menai.py` and add the
   name to `prelude_names` in `MenaiBuiltinRegistry.create_builtin_function_objects()`
6. Update the tool description to document the new function
7. Add tests in `tests/menai/`

## Adding a New Special Form

Special forms (things that are not regular function calls) are handled in multiple places:

1. **Desugarer** (`menai_desugarer.py`) — if the form needs to be expanded into simpler
   forms before IR building
2. **IR builder** (`menai_ir_builder.py`) — add a new `MenaiIRXxx` dataclass in
   `menai_ir.py` and handle it in the IR builder
3. **Codegen** (`menai_codegen.py`) — generate bytecode for the new IR node
4. **Semantic analyser** (`menai_semantic_analyzer.py`) — if the form has scope or
   arity implications that need early checking
5. Update the tool description

## Value Types

All runtime values are **immutable frozen dataclasses** inheriting from `MenaiValue`
in `menai_value.py`. The full hierarchy:

- `MenaiInteger` — Python `int`
- `MenaiFloat` — Python `float`
- `MenaiComplex` — Python `complex`
- `MenaiString` — Python `str`
- `MenaiBoolean` — Python `bool`
- `MenaiSymbol` — interned name (used for quoted symbols)
- `MenaiList` — backed by a Python `tuple` (proper lists only, no cons cells)
- `MenaiDict` — tuple of `(key, value)` pairs + hash-backed dict for O(1) lookup
- `MenaiFunction` — compiled lambda or builtin stub; carries `CodeObject`, captured
  values, and variadic flag

## Key Design Decisions

- **Proper lists only**: `MenaiList` is backed by a Python tuple. There are no cons
  cells or improper lists. `cons` requires the second argument to be a list.
- **Strict typing**: No implicit coercion between numeric types. All operators are
  type-specific (e.g. `integer+`, `float*`).
- **Tail call optimization**: Detected in `menai_ir_builder.py` (sets `is_tail_call`
  on `MenaiIRCall`), implemented in `menai_vm.py` via `TAIL_CALL` opcode.
- **Closures**: Free variables are detected in `menai_semantic_analyzer.py` and
  captured via `MAKE_CLOSURE` opcode in the VM.
- **Homoiconicity**: Quoted expressions produce `MenaiList`/`MenaiSymbol` values
  identical in structure to the AST they represent.

## Tests

Tests mirror the source structure under `tests/menai/`. When making changes, run the
test suite to verify correctness. The `tools/` directory contains additional utilities:
- `menai_disassembler/` — inspect generated bytecode
- `menai_bytecode_analyzer/` — analyse bytecode patterns
- `menai_checker/` — static analysis
- `menai_benchmark/` — performance testing
