# ADR-0004: Minimal dependencies

Date: 2026-08-27  
Status: Accepted

## Context

Every third-party dependency is also supply-chain surface.
With it we inherit another party's release process, security vulnerabilities, transitive dependency graph, and upgrade treadmill.
This is particularly tricky for a tool whose users include regulated environments.

Additionally, Humbug is intended to be heavily developed using itself, and third-party dependencies mean it's very hard for AI models
to understand APIs and capabilities because they are often not transparent.

## Decision

Runtime dependencies are limited to three third-party packages beyond the Python standard library (certifi, PySide6, and qasync).

Sibling projects under the Humbug umbrella may also be used (currently Menai).

The constraint is mechanically enforced by the `dependency_checker` tool.
Each module must explicitly declare the external packages it may use, unused declared dependencies are flagged, and the internal module dependency graph must remain acyclic.
The checker runs as part of the standard code-quality gate, so violations block completion rather than awaiting review.

## Alternatives considered

- Established libraries per need (HTTP clients, PDF/DOCX parsers, Markdown engines, syntax highlighters) were rejected because they risk introducing the problems described above.
- Vendoring third-party source code was rejected because it is hard to keep compliant/coherent with the system design.

## Consequences

### Positive

- A small, auditable supply chain and a system readable from one repository.
- Any part can be replaced, because the interfaces are owned rather than adapted to.
- AIs can read and modify every layer, which compounds the bootstrapping advantage.
- Dependency upgrades are rare, deliberate events rather than continuous churn.
- Implementations do just what they need and nothing else (YAGNI principle).

### Negative

- Maintenance and edge cases for everything in-house are Humbug's responsibility — parser quirks, format corners, proxy tunnelling, platform differences.
- Feature parity with mature libraries lags.
  Some obscure format corners remain unsupported.
