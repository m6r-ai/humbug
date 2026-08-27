# Architecture Decision Records (ADRs)

This directory contains ADRs for the Humbug project. Each ADR records a significant
architectural or design decision: the context, the decision, the alternatives that
were considered, and the consequences.

## Relationship to other documents

- **AGENTS.md** — points contributors here for the definitive index of decisions
  and the conventions for writing new ADRs.
- **blueprint.md** — captures the project's purpose and core principles. ADRs
  record specific decisions that follow from those principles.
- **CONTRIBUTING.md** — points contributors here for design context before
  proposing changes.

## How to write an ADR

Use the next available four-digit number. Copy the structure of an existing ADR:

```markdown
# ADR-NNNN: Title

Date: YYYY-MM-DD  
Status: Accepted

## Context

What problem were we solving? What forces were at play?

## Decision

What did we decide?

## Alternatives considered

What else was on the table, and why did we reject it?

## Consequences

### Positive

What benefits does this decision bring?

### Negative

What costs, risks, or constraints does this decision impose?
```

### Status values

- **Proposed** — decision is under discussion, not yet final.
- **Accepted** — decision is final and implemented (or being implemented).
- **Superseded by ADR-XXXX** — replaced by a later decision.
- **Deprecated** — no longer relevant, but not superseded by a specific ADR.

### Principles

- An ADR records **context, alternatives, and reasoning** — the things that are
  not in the code. Do not reproduce implementation details that can be read from
  the source.
- An ADR is written when the decision is **settled**, not while it is still under
  discussion. Use a working document for proposals, then create the ADR once the
  approach is confirmed.
- Once accepted, an ADR is not updated to reflect implementation details. If the
  decision itself changes, write a new ADR that supersedes the old one.

## Index

| Number | Title | Status |
|--------|-------|--------|
| [0001](0001-conversation-as-primary-mode.md) | Conversation as the primary mode of operation | Accepted |
| [0002](0002-os-neutrality.md) | OS neutrality — one experience on Windows, macOS, and Linux | Accepted |
| [0003](0003-local-first-data-ownership.md) | Local-first data ownership | Accepted |
| [0004](0004-minimal-dependencies.md) | Minimal dependencies | Accepted |
| [0005](0005-desktop-ui-toolkit-qt.md) | Desktop UI toolkit — Qt | Accepted |
| [0006](0006-humans-and-ais-are-equal-actors.md) | Humans and AIs are equal actors | Accepted |
