# AGENTS.md - Menai Source Directory

## Purpose

This document exists to convey design intent, non-obvious invariants, and architectural
decisions that cannot be read directly from the code.  It is a guide for AI agents
working on this codebase.

### What this document is NOT

This document does NOT describe what the code currently does in detail.  It does not
reproduce pipeline diagrams, file-by-file role tables, pass-order lists, or any other
information that is already expressed clearly in the source.  That kind of content becomes
a maintenance liability: it drifts out of date as the code evolves and then actively
misleads the next reader.

If you update the code, DO NOT add derived technical descriptions here.  If you feel
the urge to document how something works, put that documentation in the source file itself
(module docstring, class docstring, inline comment) where it will be read alongside the
code it describes and is more likely to be kept correct.

This document should only grow when there is a genuine design decision, constraint, or
non-obvious invariant to record that cannot be expressed in the code itself.

## Where to start

- Pipeline: read `menai_compiler.py` — it is the authoritative, always-current
  description of the compilation pipeline and pass order.
- Language semantics: use the AI tool description (available via the `help` tool).
  Do not rely on README.md for semantics. Do NOT assume that because it looks a bit like
  Scheme or Lisp that it's actually the same.
- Individual passes: each source file has a module-level docstring that describes
  what that pass does, its invariants, and its position in the pipeline.

## APIs

Many of the internal APIs are non-obvious.  DO NOT attempt to guess what they might be.
If you need to use an API read the source code to understand it first.

## Architectural invariants

These are constraints that must hold across the whole compiler.  They are recorded here
because they span multiple files and are easy to violate accidentally.

### The IR tree is immutable — passes return new trees

No IR optimisation pass may mutate its input tree in place.  Each pass receives an IR tree
and returns a new one, along with a boolean indicating whether anything changed.  The
pass manager uses that flag to drive the fixed-point loop.

The reason: immutability makes passes composable and makes bugs easier to isolate.  A pass
that mutates its input can corrupt the tree in ways that only manifest later in an
unrelated pass.

### Menai is pure — dead code elimination is always safe

Because Menai has no side effects, any expression whose result is never used can be
discarded unconditionally.  Optimisation passes may rely on this without checking for
side effects.

### `letrec` reaching the IR builder is always a genuine mutually-recursive group

The desugarer guarantees that by the time `letrec` reaches the IR builder, every
`letrec` is a single strongly-connected component of mutually-recursive bindings.
Non-recursive bindings are hoisted to `let` forms.

However, not every binding in a `letrec` group is necessarily a lambda.  A
non-lambda binding (e.g. `(letrec ((x (list (lambda () x)))) x)`) can appear in a
`letrec` group when its RHS contains a nested lambda that closes over the binding
name — the dependency analyzer sees a cycle and correctly keeps it in `letrec`.
The IR builder and both codegens handle this.  The CFG builder handles it via a
dedicated Phase 2b / Phase 3b in `_build_letrec`: non-lambda binding values are
evaluated after all sibling lambda closures exist (so nested lambdas can capture
them), and any nested lambdas with sibling captures are patched afterward.

IR passes downstream of the IR builder may not assume all `letrec` bindings are lambdas.

### The prelude and the builtin registry must stay consistent

There are two categories of builtin that must not be confused:

- Opcode-backed builtins have an entry in `BUILTIN_OPCODE_ARITIES` in
  `menai_builtin_registry.py`.  The registry asserts that every entry in this table has a
  corresponding opcode in `BUILTIN_OPCODE_MAP`.  Adding a name here without an opcode
  will cause an assertion failure at startup.
- Prelude-only functions (e.g. `map-list`, `filter-list`, `fold-list`) are implemented
  as Menai lambdas in `_PRELUDE_SOURCE` in `menai.py`.  They MUST NOT be added to
  `BUILTIN_OPCODE_ARITIES`.

## Design decisions

These are decisions that might otherwise look like oversights or invite "improvement".

### No `cond` form

Deliberate omission.  `match` covers all multi-branch conditional use cases and is more
expressive.  Do not add `cond`.

### Symbols are not strings

`symbol` values are produced only by `quote` and exist solely to support homoiconicity
(code-as-data).

### Proper lists only

`MenaiList` is backed by a native C array (`PyObject **elements`, `Py_ssize_t length`).  There are no cons cells and no improper lists.
`cons` requires its second argument to be a list.  This is intentional: improper lists
add complexity for minimal benefit in a language without pattern-matched list destructuring
at the cons-cell level.

### Strict numeric typing

There is no implicit coercion between `integer`, `float`, and `complex`.  All arithmetic
operators are type-specific (e.g. `integer+`, `float*`).  This is intentional.

## Tools

You can find tools related to Menai in tools/menai.

## VM implementation

We currently have a legacy Python VM implementation but we're replacing it with a C version.  When implementing any new
functionality, consider that the aim is to remove all python code from the C-based VM.

### C formatting

Do NOT use lines of characters in comments.  E.g. never use something like:

```c
/* --------------------------------
 * This is a bad comment - don't do this!
 * -------------------------------- */
```

For single line comments put the open and close of the comment on the same line:

```c
/* This is a good single line comment */
```

For multi-line comments the open and close go on their own lines:

```c
/*
 * This is a great multiline comment.
 * Where we have more than one line of text.
 */
```

Do NOT use excess whitespace to line up things on adjacent lines.  E.g. never do:

```c
int x_with_long_name = 0;
iny y                = 1;
```

Instead do:

```c
int x_with_long_name = 0;
int y = 1;
```

Do NOT put code on the same line after an opening brace.  E.g. never do:

```c
if (foo) { something(); }
```

Instead do:

```c
if (foo) {
    something();
}
```
