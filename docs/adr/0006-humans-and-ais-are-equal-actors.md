# ADR-0006: Humans and AIs are equal actors

Date: 2026-08-27  
Status: Accepted

## Context

Humbug is built for collaboration between humans and AIs, not for assistance.
Conceptually, the model is similar to the idea of pair programming.

Dominant patterns in current systems make the AI a second-class actor in one of two ways:

- It is embedded as an assistant inside human-driven tools

- It is run headlessly through a separate API surface that the human never sees.

Considerations:

- Conversation as the primary mode of operation (ADR-0001) only works if the AI can act on the same environment the human works in.

- Transparency requires the human to see AI actions in the same place as everything else, not through a separate channel.

- Auditability requires the actions of both actors to be recorded together.

## Decision

Humans and AIs are equal actors in Humbug.

- There is no separate "AI mode".
  The same environment serves both: the same tabs, editors, terminals, previews, version-control views, and files.

- All tools, including the GUI, are designed to be usable by both.
  The AI orchestrates the same UI the human uses (e.g. opening and arranging tabs, editing buffers, running terminals, navigating previews, etc.) through the same underlying contexts and registry.

- Equality is about capability and standing, not identical permissions.
  The safety model remains asymmetric by design: AI actions with side effects require human approval, and Humbug-internal state stays inaccessible to the AI.

## Alternatives considered

- A separate AI surface (a headless agent API alongside a human-only GUI) was rejected because it splits the environment in two.
  The human cannot see what the AI is doing in the same place, every capability must be built twice, and the two surfaces drift apart.

- AI as an assistant inside human-driven tools was rejected because it caps the AI at second-class standing.
  It can react, but it cannot drive its own workflows or share the same tools on equal terms.

## Consequences

### Positive

- One implementation per capability serves both actors.
  The document converter, for example, exists as both a user tool and an AI tool over the same core.

- Transparency is a by-product of equality: AI actions appear in the same UI, with the same visibility, as human actions.

- The AI can orchestrate the full workspace, enabling agentic workflows without a separate automation layer.

- A single set of contexts, tools, and underlying models keeps the codebase smaller and comprehensible.

### Negative

- Every capability must be designed for both audiences; parity is an ongoing effort rather than a finished state.

- Equality of capability demands explicit asymmetry of permission: the approval system, mindspace scoping, and AI-inaccessible internal state exist precisely because the AI's actions are not pre-trusted.

- The UI must be drivable programmatically as well as manually, which constrains how frontend components are built.
