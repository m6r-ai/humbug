# ADR-0019: Conversation identity — a UUID in the transcript, distinct from its path

Date: 2026-11-03  
Status: Accepted

## Context

A conversation is identified by its transcript file path. ADR-0011 records that session
identifiers are mindspace-relative file paths, and ADR-0017 stores that path as the log context
entry that attributes a record to a conversation. Nothing else identifies a conversation.

That works while the path is stable, and it is not. Three problems follow.

**Renaming loses attribution.** A conversation can be renamed, so the path recorded on existing
log entries stops resolving. The entries remain, pointing at a name that no longer exists, with
nothing recording what it became. The same applies to a conversation moved between directories.
An audit trail whose referent can be renamed away is weaker than it appears. Duplication is the
mirror image: a copy is a new conversation that is byte-identical to its source, so content alone
cannot distinguish the two.

**Fork provenance is inferred rather than recorded.** `ConversationDag` detects a fork edge by
finding message IDs shared between two transcript files, taking the last shared message as the
fork point. This is sound for display, which is what it was built for, but it is a heuristic: two
conversations that coincidentally share a message become linked, and a conversation that happens
to contain a copy of another's history is indistinguishable from a genuine fork. Delegation is
different — a delegated conversation records its parent explicitly by message and tool call — but
even that requires locating a conversation by searching for the message it references, so
answering "which conversation is my parent?" depends on scanning files rather than on identity.

**New features are forced onto the same foundation.** The file change journal (ADR-0018) must
attribute a mutation to the conversation that caused it, in a field that an authorisation
decision may later depend on. Building that on a mutable path would inherit the defect in the
one place it matters most.

## Decision

**Identity and location are separate concepts.** A conversation has a UUID, which is its
identity, and a path, which is where its transcript currently lives. The UUID never changes. The
path may change whenever the user likes and nothing depends on its value beyond opening the file.

**The UUID is assigned when the conversation is created**, is opaque, and is derived from
neither the path nor the content. It is stored in the transcript's metadata alongside the
existing format version and parent reference. A transcript that predates this decision is
assigned one when it is next opened, and that assignment is persisted, so migration is lazy and
requires no pass over the conversations directory.

**Rename and move preserve the UUID.** They change the locator and nothing else. Today they change
the identity: `AITranscriptConversation.set_path` retargets the conversation identifier to the new
path, so every log entry written before a rename refers to a conversation that no longer exists
under that name.

**Copying a conversation produces a new identity.** A duplicate is a new line of work, not the
same one in two places, so it receives a new UUID and records the conversation it was copied from.
This has to be an operation rather than a file copy. Duplication is currently `shutil.copy2` of
the transcript in the conversation sidebar, which would copy the identifier along with the
content, leaving two transcripts that claim to be the same conversation. Because the transcript
handler already owns the format, it should own duplication too, so that every frontend gets a new
identity rather than each having to remember to.

**A fork records its origin rather than having it inferred.** There is no fork command: a fork
arises when a conversation is duplicated, and `ConversationDag` currently detects the relationship
by finding message IDs shared between the two files. The duplication operation records the origin
UUID and the fork point explicitly, so provenance becomes a recorded fact. The inference in
`ConversationDag` becomes unnecessary for anything that needs to be certain — it may remain for
display breadth, but no decision may rest on it.

**Only delegation confers authority.** A delegated conversation records its parent, and that edge
is the sole relationship that grants one conversation any right over another — the existing
resumption check in ADR-0011 is the model. Fork provenance is recorded and displayed and is
explicitly not an authorisation input: a fork is a new line of work and inherits no rights over
its ancestor's history, even though it begins with a copy of its content.

**The UUID is what gets recorded by anything that needs to attribute activity.** The journal
records it (ADR-0018), and the interaction log should record it in place of the path, so that the
two stores agree about who did what. Paths already written to the log remain path-form; a reader
must tolerate both, and old entries are left as they are rather than rewritten.

**The UUID is an identifier, not a credential.** It is visible in a file the user can edit, so it
must never be used as proof of anything. It names a conversation; the delegation edge is what
authorises acting on one. A transcript whose identifier is missing or malformed is assigned a new
one and the event is logged, rather than being rejected or silently trusted.

## Alternatives considered

- **Keep the path as the identifier.** No new state, and consistent with ADR-0011's view of what
  a session identifier is. Rejected because rename destroys every attribution made before it, and
  the journal would inherit the defect in the field it most depends on.

- **Hash the message history to derive identity.** Derived from content, so a fork would collide
  with its ancestor exactly where identity must differ, and the identity of every conversation
  would change as the conversation grew.

- **Derive identity from creation time and name.** Cheap and mostly unique, but it is the path
  problem wearing a different hat: a copy carries the same components, and the value changes if
  the derivation is ever adjusted.

- **Record identity in a separate index file, keyed by path.** Would leave transcripts untouched,
  at the price of a second file that can disagree with the data, needing repair whenever a
  transcript is moved outside Humbug. The transcript is the source of truth for its own identity,
  as it already is for its own parentage.

- **Continue inferring forks from shared messages, and treat that as identity.** Rejected: a
  heuristic is adequate for laying out a tree and inadequate for deciding whether one conversation
  may act on another's work.

- **Introduce the UUID for the journal only, leaving the log on paths.** Avoids a log migration,
  but leaves two identifiers for the same thing, disagreeing after every rename, and leaves the
  audit trail's attribution defect in place.

- **Treat the UUID as a bearer credential, as an API session token would be.** Superficially
  convenient for session resumption. Rejected for the reason ADR-0011 gives: an identifier stored
  in a user-editable file must not grant access on its own, and the relationship between
  conversations is already recorded.

## Consequences

### Positive

- Attribution survives rename and move, so logged activity and journalled mutations remain
  traceable to the conversation that caused them.
- The log and the journal can share one identifier, so "who did this?" has one answer across both
  stores rather than two that drift apart after a rename.
- Fork provenance becomes a recorded fact, and delegation remains the only relationship that
  grants authority, so authorisation does not rest on an inference.
- The delegate tool's ownership check can identify the parent exactly instead of resolving it by
  locating a referenced message.
- Identity is independent of where a file sits, so future features that need to attribute activity
  do not each have to invent a way to name a conversation.

### Negative

- Transcripts without an identifier are assigned one only when opened, so a conversation never
  opened since the change has no identity to be attributed to. Activity before that first open
  cannot reference it.
- Duplication must change in the same release. Until it assigns a new identity, copying a
  conversation produces two transcripts claiming the same UUID, which is worse than the current
  positional behaviour because the ambiguity is silent rather than obvious. A transcript copied
  outside Humbug, by hand or by a file manager, carries its source's identifier for the same
  reason, and no mechanism in Humbug can prevent that.
- The identifier is opaque, so debugging and support need a way to map an identifier back to a
  path, which means exposing that mapping in the places a user or developer inspects.
- Log entries written before this decision remain path-attributed, so readers and any future
  integrity pass must tolerate two forms in one store, and the log's history is only attributable
  by identity from the point the change takes effect.
- Renaming no longer changes a conversation's identity, which is the intent, but it removes the
  accidental signal that a rename used to provide about when a conversation's activity was
  recorded.
- One more field must be preserved by every future writer of a transcript. A writer that
  reconstructs metadata rather than updating it would silently re-identify a conversation, and
  nothing would notice until attribution pointed somewhere unexpected.
