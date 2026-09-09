# ADR-0005: Desktop UI toolkit — Qt

Date: 2026-08-27  
Status: Accepted

## Context

Humbug needed a desktop UI toolkit that could deliver a rich, native-feeling experience on Windows, macOS, and Linux from
one codebase (ADR-0002), while respecting the minimal-dependency constraint (ADR-0004).

## Decision

The desktop frontend is built with Qt, via the PySide6 Python bindings.

- Qt provides the native widgets, layout, and rendering needed for a rich desktop experience, with mature cross-platform support for all three target operating systems.

- PySide6 is one of the three permitted third-party runtime dependencies, alongside qasync, which bridges Qt's event loop and Python's asyncio so conversations and tools run on a single event loop.

- Qt code is confined to the desktop frontend.
  The frontend-agnostic core modules carry no Qt dependencies, keeping the UI replaceable in principle.

## Alternatives considered

- Electron was rejected because it is too bloated: a bundled browser runtime is incompatible with the minimal-dependency principle and inflates distribution size and memory use.

- A text user interface was rejected because it cannot present a rich-enough experience for the interactions Humbug requires.

## Consequences

### Positive

- A rich desktop experience — tabs, columns, syntax highlighting, rendered markdown, side-by-side diffs — on all three platforms from one codebase.

- Qt's maturity provides widgets, accessibility, and platform integration that would otherwise have to be built or adopted piecemeal.

- qasync lets the entire application share one asyncio event loop, keeping AI conversations, tools, and UI responsive within a single concurrency model.

### Negative

- PySide6 is the largest dependency in the project, both in distribution size and in API surface; it is the one place where the minimal-dependency principle is most strained.

- Qt is a lasting constraint: the desktop frontend is written against its idioms, and moving away from it would be a rewrite.

- Qt-specific failure modes (for example, object lifetime issues between Python and the underlying C++ objects) require discipline and established patterns to avoid.
