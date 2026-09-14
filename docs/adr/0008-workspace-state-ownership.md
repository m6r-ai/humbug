# ADR-0008: mindspace state ownership — layout, content, and view

Date: 2026-09-13  
Status: Accepted

## Context

A mindspace in Humbug contains a set of open contexts — editors, conversations,
terminals, previews, diffs, and others — arranged into columns, with one context
focused.  This state must be preserved across session boundaries, and it must be
preserved when a context is moved between columns during a session.

Humbug's architecture separates the backend model of the mindspace from
any particular frontend.  The `ContextRegistry` tracks open contexts and emits
events; a frontend subscribes to those events and renders them.  The desktop
frontend (`TabManager`) is one such projection.  A headless or remote frontend is
anticipated.

This separation only holds if the state that defines the mindspace lives in the
frontend-agnostic layer.  But mindspace state is not homogeneous.  It contains
three kinds of information with different owners:

- Layout: which contexts are open, their type, path, title, column, focus,
  and ephemerality.  This is meaningful with or without a frontend.

- Content: the identity of the thing a context represents: a file path, a
  terminal command, a conversation transcript.  This is also meaningful without a
  frontend, and a headless frontend must be able to reconstruct it.

- View: cursor positions, scroll offsets, find-widget state, message
  expansion state.  This is meaningful only to a view.  It has no representation
  in a headless frontend, but a graphical frontend wants to preserve it.

Conflating these three kinds of state in a single structure forces a frontend to
participate in state that is not its concern, and prevents a headless frontend
from restoring a session at all.

Separately, moving a context between columns is sometimes implemented by
destroying and recreating the frontend's representation of it.  This requires
carrying live, non-serialisable state — in-memory buffers, process handles —
across the gap.  This is a frontend concern and is not mindspace state.

## Decision

mindspace state is divided into three layers, each with a single owner.

Layout is owned by the `ContextRegistry`.  The registry is the source of
truth for which contexts are open, their type, path, title, column, focus, and
ephemerality.  Frontends observe layout through registry events and do not hold
authoritative copies of it.

Content is owned by the frontend-agnostic context models.  Each context type
has a model (`EditorContext`, `ConversationContext`, `TerminalContext`, and so on)
that can serialise its own content state and be reconstructed from it without a
frontend.  A context type is reconstructable if and only if its model can be
reconstructed.

View is owned by the frontend.  View state is carried as an opaque
`frontend_state` blob keyed by context id.  The registry stores and returns this
blob but never interprets it.  A graphical frontend populates it from its own view
state; a headless frontend leaves it empty.  The blob is not namespaced; if a
second frontend requires isolation, that is a later decision.

Session persistence is a registry operation.  Saving a session produces a single
structure containing, for each context, its layout, its content state, and its
frontend state.  Restoring a session reconstructs each context model, registers
it, and re-opens the context, emitting `OPENED` so that any subscribed frontend
renders it.  A frontend does not participate in session persistence beyond
supplying and consuming the `frontend_state` blob.

Moving a context between columns is a frontend operation.  A frontend that
recreates its representation of a context during a move is responsible for
carrying any live state across the move itself.  Such state is not mindspace state
and does not enter the persistence format.

The persisted session format reflects this division directly: layout, content
state, and frontend state are stored as distinct parts of each context's entry.

## Alternatives considered

- A single flat state structure owned by the frontend.  This is the simplest
  to implement for a single frontend, but it makes the frontend authoritative over
  layout, which inverts the model/projection relationship.  A headless frontend
  could not restore a session, because session state would only be reachable
  through frontend components.

- View state owned by the registry.  Placing cursor positions and scroll
  offsets in the frontend-agnostic layer would put view-specific data in a layer
  that has no view.  The data would be meaningless to a headless frontend, and the
  model would accumulate frontend concerns.

- A structured view-state provider protocol.  A typed protocol for view state
  would formalise the interface between a context and a frontend's view state.
  This was rejected as over-engineering: view state is broad across context types
  and largely uniform in shape, and a protocol would add indirection without
  adding capability.  An opaque blob achieves the same separation with less
  machinery.

- Namespacing the frontend state blob.  Namespacing would isolate frontends
  from each other's view state.  This was deferred: with a single frontend there
  is nothing to isolate, and the added structure would be speculative.

## Consequences

### Positive

- The registry is the genuine source of truth for mindspace layout, and a frontend
  is a projection of it.

- A headless or remote frontend can save and restore a full session without a
  graphical toolkit, supplying no view state.

- View state is preserved across both session boundaries and column moves through
  the same mechanism, without the registry needing to understand it.

- Content state is defined per context type, so a new context type is
  reconstructable by implementing its model, independent of any frontend.

### Negative

- The registry stores an opaque `frontend_state` blob it does not interpret.  This
  is a deliberate concession: the registry remains frontend-agnostic while
  allowing frontends to preserve view state it cannot model.

- View state round-trips only if the frontend supplies it.  A frontend that fails
  to populate `frontend_state` for a context type will silently lose that type's
  view state across sessions.  This is not caught by the type system.

- The boundary between content and view state must be applied correctly by context
  authors.  State that is view-specific but placed in a context model would leak a
  frontend concern into the frontend-agnostic layer; state that is content but
  placed in the frontend blob would be lost to a headless frontend.
