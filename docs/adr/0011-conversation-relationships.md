# ADR-0011: Conversation relationships and delegated-session authorisation

Date: 2026-09-25  
Status: Accepted

## Context

Conversations in Humbug are not independent. A conversation can delegate a task
to a child conversation, and a conversation can be forked from another. These
relationships form a directed acyclic graph over the conversation files in a
mindspace: delegation edges are recorded explicitly when a child is created, and
fork edges are inferred from message history shared between files.

Several parts of the system need to reason about this graph. The conversations
panel presents it as a tree, so that a delegated child appears beneath the
conversation that spawned it and a shared child appears beneath each of its
parents. Operations that move or delete a conversation must walk the graph to
decide which descendants travel with it and which are shared with conversations
outside the operation. And the delegate tool must be able to answer a specific
question: was a given conversation delegated by the conversation now asking to
resume it?

That last question is a security question. A delegated conversation is resumed
by passing its session identifier to the delegate tool. Session identifiers are
mindspace-relative file paths, so knowing one is enough to name it. Without a
check, any conversation could resume any other conversation's session, reading
context it was never given and injecting instructions into a conversation it does
not own. The relationship between conversations is therefore not merely a display
concern; it is the basis for an authorisation decision.

The model of conversation relationships must be available to the parts of the
system that need it, including frontend-agnostic ones. The delegate tool is
frontend-agnostic and has no access to any graphical layer, so the model cannot
live in a frontend.

## Decision

Conversation relationships are modelled by a single frontend-agnostic component,
`ConversationDag`, which reads the conversation files in a mindspace and exposes
the resulting graph: the children of a conversation, the parents of a
conversation, the roots, the fork edges, and the set of conversations that travel
with a given conversation under a move or delete.

The model is derived entirely from the conversation files themselves. It holds no
separate index and introduces no new persisted state; the files are the source of
truth for their own parentage. It is pure and synchronous, with no dependency on
a frontend, a file watcher, or any event mechanism. Callers that need to react to
changes in the graph are responsible for observing the filesystem and rebuilding
or updating the model as appropriate.

A delegated conversation may be resumed only by the conversation that delegated
it. The delegate tool resolves the parents of the target conversation through
`ConversationDag` and permits resumption only when the requesting conversation is
one of them. The check fails closed: a request that cannot establish the
relationship is denied. The session identifier names a conversation; it does not
grant access to it.

## Alternatives considered

- **Treat the session identifier as a capability.** Possession of a session
  identifier would be sufficient to resume the conversation. This is simpler, but
  it makes the identifier a bearer token: any leak, log entry, or guess grants
  full access to the conversation's context, and there is no way to distinguish
  the conversation that delegated the session from any other. The relationship
  between conversations is already recorded, so the stronger check costs little.

- **Place the model in a frontend.** The graph would be built and owned by the
  component that displays it. This satisfies the display and file-operation needs
  but leaves the delegate tool without access to the relationships it must check,
  forcing either a dependency from the frontend-agnostic tool onto a frontend or a
  second, divergent implementation of the same graph.

- **Place the model in the mindspace.** The mindspace already owns the
  conversations directory and could host the graph alongside its other state. This
  was rejected to keep the mindspace focused on mindspace-level concerns and to
  keep the graph independently testable, in the same way other self-contained
  models are kept separate.

- **Reimplement the relationship resolution inside the delegate tool.** The tool
  could read the conversation files directly and resolve parents itself. This
  duplicates logic that the rest of the system also needs and risks the two
  implementations disagreeing about what a relationship is.

## Consequences

### Positive

- One component defines what a conversation relationship is, so the tree view,
  the file operations, and the delegate tool all agree on the graph.

- The delegate tool enforces session ownership without depending on a frontend,
  preserving the frontend-agnostic boundary.

- The model is pure and derived from the files, so it is straightforward to test
  and introduces no new state to keep consistent.

- Resuming a session is authorised by a recorded relationship rather than by
  possession of an identifier, so a leaked or guessed identifier does not grant
  access to a conversation.

### Negative

- The delegate tool acquires a dependency on the on-disk conversation layout: it
  must be able to resolve the graph to authorise a resumption. A conversation
  whose parentage cannot be resolved — a root conversation, or one whose parent
  has been removed — cannot be resumed by anyone, which is the intended
  fail-closed behaviour but is stricter than a caller might expect.

- A conversation may have more than one parent when its parent was forked after
  the child was created. The authorisation check admits any of them. This is
  consistent with how the graph treats shared children elsewhere, but it means
  "the conversation that delegated it" can be more than one conversation.

- The model reflects the filesystem as of the last read. A caller that resumes a
  session immediately after a conversation file changes may act on a slightly
  stale graph. The window is small and the failure mode is a denied resumption,
  not an unauthorised one.
