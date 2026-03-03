# AGENTS.md - Menai Source Directory

## Purpose

This document exists to convey **design intent, non-obvious invariants, and architectural
decisions** that cannot be read directly from the code.  It is a guide for AI agents
working on this codebase.

### What this document is NOT

This document does **not** describe what the code currently does in detail.  It does not
reproduce pipeline diagrams, file-by-file role tables, pass-order lists, or any other
information that is already expressed clearly in the source.  That kind of content becomes
a maintenance liability: it drifts out of date as the code evolves and then actively
misleads the next reader.

**If you update the code, do not add derived technical descriptions here.**  If you feel
the urge to document how something works, put that documentation in the source file itself
(module docstring, class docstring, inline comment) where it will be read alongside the
code it describes and is more likely to be kept correct.

This document should only grow when there is a genuine design decision, constraint, or
non-obvious invariant to record that cannot be expressed in the code itself.

## Where to start

- **Pipeline**: read `menai_compiler.py` — it is the authoritative, always-current
  description of the compilation pipeline and pass order.
- **Language semantics**: use the AI tool description (available via the `help` tool).
  Do not rely on README.md for semantics.  When in doubt, test with the Menai tool directly.
- **Individual passes**: each source file has a module-level docstring that describes
  what that pass does, its invariants, and its position in the pipeline.

## Architectural invariants

These are constraints that must hold across the whole compiler.  They are recorded here
because they span multiple files and are easy to violate accidentally.

### Variables are symbolic until the final addressing pass

All IR transformation passes — including the closure converter, lambda lifter, and all
optimisation passes — work exclusively with **symbolic** `MenaiIRVariable` nodes
(`depth=-1, index=-1`).  The single final addressing pass resolves all variables and
allocates all slots immediately before code generation.

**No pass upstream of the addresser may read or depend on `depth` or `index`.**  Any pass
that introduces new variable references must emit them with `depth=-1, index=-1`.

The reason: keeping variables symbolic means every transformation pass can freely
restructure the IR tree (reorder bindings, inline expressions, introduce new let-bindings)
without ever producing stale or incorrect addresses.  A single clean addressing pass at
the end is far simpler and more reliable than trying to maintain correct addresses
incrementally through a sequence of tree rewrites.

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

### `letrec` reaching the IR builder is always a genuine mutually-recursive lambda group

The desugarer guarantees that by the time `letrec` reaches the IR builder, every
`letrec` is a single fully-mutually-recursive group of lambdas.  Non-recursive bindings
and non-lambda bindings have been hoisted to `let` forms.  IR passes may rely on this
invariant.

### The prelude and the builtin registry must stay consistent

There are two categories of builtin that must not be confused:

- **Opcode-backed builtins** have an entry in `BUILTIN_OPCODE_ARITIES` in
  `menai_builtin_registry.py`.  The registry asserts that every entry in this table has a
  corresponding opcode in `BUILTIN_OPCODE_MAP`.  Adding a name here without an opcode
  will cause an assertion failure at startup.
- **Prelude-only functions** (e.g. `map-list`, `filter-list`, `fold-list`) are implemented
  as Menai lambdas in `_PRELUDE_SOURCE` in `menai.py`.  They must **not** be added to
  `BUILTIN_OPCODE_ARITIES`.

## Design decisions

These are decisions that might otherwise look like oversights or invite "improvement".

### No `cond` form

Deliberate omission.  `match` covers all multi-branch conditional use cases and is more
expressive.  Do not add `cond`.

### Symbols are not strings

`symbol` values are produced only by `quote` and exist solely to support homoiconicity
(code-as-data).  There is intentionally no `symbol->string` or `string->symbol`
conversion.  Use strings for dict keys and general-purpose identifiers.

### Proper lists only

`MenaiList` is backed by a Python tuple.  There are no cons cells and no improper lists.
`cons` requires its second argument to be a list.  This is intentional: improper lists
add complexity for minimal benefit in a language without pattern-matched list destructuring
at the cons-cell level.

### Strict numeric typing

There is no implicit coercion between `integer`, `float`, and `complex`.  All arithmetic
operators are type-specific (e.g. `integer+`, `float*`).  This is intentional.
