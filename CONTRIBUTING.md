# Contributing to Humbug

Thank you for your interest in contributing. This document covers what you need to know
before submitting changes.

## Prerequisites

Follow the **Developer installation** steps in the [README](./README.md) to get a working
development environment before making any changes.

## Verifying your changes

Before submitting any change, run the full suite of static analysis tools:

```bash
python -m tools.code_checker
```

All checks must pass cleanly. This runs:

- **Dependency checker** — enforces the acyclic inter-module dependency graph defined in
  `dependency-rules.yaml`. No module may import from another that is not explicitly listed
  as a dependency, and no circular dependencies are permitted.
- **mypy** — full static type checking across `src/`. All code must be fully typed.
- **pylint** — linting across `src/`. The codebase is held to a 10.00/10 rating.

## Coding standards

Consistency is more important than improvement. A change that makes one part of the
codebase "better" but leaves it inconsistent with the rest is not acceptable. If you
want to improve a pattern or convention, that improvement must be applied uniformly
across the entire codebase — not just in the files you happen to be touching. The
burden for identifying and resolving any inconsistency a change introduces rests
entirely with the contributor.

- No file-level docstrings — they go stale quickly as code evolves.
- No block comments delimiting sections within a file. Functions and classes have
  docstrings; grouping comments just add clutter.
- Use modern Python: `type | None` instead of `Optional`, no `@property` (use simple
  getter methods instead).
- Use builtins (`dict`, `list`, `set`, `tuple`, `type`, `frozenset`) and
  `collections.abc` (`Callable`, `Awaitable`, `AsyncGenerator`, `Generator`,
  `Iterator`, `Sequence`, `Coroutine`) instead of legacy `typing.Dict`, `typing.List`, etc.
- Tests must reflect correct and desired behaviour. Never write or patch a test to mask
  broken implementation logic — if the logic is wrong, the test must fail.
- Test docstrings describe expected behaviour only. They must not reference historical
  bugs, previously broken behaviour, or implementation details of past fixes. A test is
  a specification, not a changelog.
- When restructuring code, do not remove existing comments. If a comment appears wrong,
  flag it rather than silently deleting it.

## Dependency rules

Humbug has only 4 external runtime dependencies beyond the Python standard library
(PySide6, qasync, aiohttp, certifi). This is a core design principle and the bar for
adding a new one is extremely high. If you think a new external dependency is genuinely
necessary, raise it for discussion on Discord before writing any code that relies on it.
Do not add new external dependencies without explicit agreement.

The dependency graph between modules must remain acyclic. When adding a new import:

1. Check `dependency-rules.yaml` to see if the dependency is already declared.
2. If not, consider whether the dependency is appropriate — does it violate the acyclic
   constraint or couple modules that should remain independent?
3. If appropriate, add it to `dependency-rules.yaml` before running the checker.

## Getting involved

Join the [Discord server](https://discord.gg/GZhJ7ZtgwN) if you want to discuss ideas or
coordinate on larger changes before writing code.
