# ADR-0018: File change journal — reversible file mutations inside the mindspace

Date: 2026-11-03  
Status: Accepted

## Context

Humbug gates persistent state changes behind per-action approval (ADR-0007) because an
overwritten file cannot be restored. The reasoning is sound, but the cost scales with the
length of a task: an agentic task that writes forty files requires forty approvals, each
blocking the conversation until the human responds. The human becomes the rate limit on every
long task, and sees the work one fragment at a time with no view of the whole.

The same gate is also applied unevenly. A file the *user* removes is moved into
`.humbug/trash/` with a manifest recording its original location, and can be restored from the
trash panel. A file the *AI* removes is unlinked outright, after a prompt that states it cannot
be recovered. A file either actor overwrites is unrecoverable by either. The recovery mechanism
exists and is attached to one actor only.

Prevention is weaker than it appears for destructive changes. A prompt cannot undo an
overwrite, while a recorded pre-image can. Recovery complements prevention rather than
replacing it, and it separates the risk of loss from the approval gate — which is the
precondition for ever reducing approval friction without weakening safety.

ADR-0017 established the interaction log as an append-only audit trail that is never truncated,
and set out that deletion from it must be explicit, visible and archival. The journal needs the
opposite freedom: a content-addressed store has to reclaim what nothing references. The two
therefore cannot share a store. What they share is the principle — append-only, plain text, and
nothing removed silently.

Attributing a mutation to the conversation that caused it requires an identity for a
conversation that survives being renamed. There is none today. A conversation's identifier is its
transcript path (ADR-0011), which is what ADR-0017 records in the log's context, so renaming a
conversation leaves its logged activity pointing at a name that no longer exists. A journal built
on the same foundation would inherit that defect in the one field an authorisation decision might
later depend on.

## Decision

A journal records the pre-image of file mutations inside the mindspace, so that those mutations
can be reversed.

**Scope of the guarantee.** The journal covers file mutations made through Humbug's own
interfaces: the AI filesystem tool, the editor's save path, and the desktop file operations
(rename, move, duplicate, create, delete). It does not cover terminal command effects, changes
outside the mindspace boundary, git operations, or the consequences of content that has already
been read, executed or committed. Reversing a file is not reversing an effect. This limitation
is the promise, and it must be stated where users will meet it rather than implied away.

**Directories are covered only where the trash already holds them.** A user removing an empty
directory moves it into the trash, and that removal is recorded as a directory: the entry says the
path existed and had no content to capture, and reversal recreates it from the trash. Because the
record carries no per-file detail, the same mechanism would restore a whole tree if a caller ever
trashed one. Directory moves and creations are not recorded, and neither is the AI tool's
`remove directory`, which unlinks an empty directory with no bin to put it in. Recursive capture of
a directory move — one group per operation, a depth limit, and a rule for files above the capture
threshold inside the tree — is deliberately not attempted.

**Directories are covered only where the trash already holds them.** A user removing an empty
directory moves it into the trash, and that removal is recorded as a directory: the entry says the
path existed and had no content to capture, and reversal recreates it from the trash. Because the
record carries no per-file detail, the same mechanism would restore a whole tree if a caller ever
trashed one. Directory moves and creations are not recorded, and neither is the AI tool's
`remove directory`, which unlinks an empty directory with no bin to put it in. Recursive capture of
a directory move — one group per operation, a depth limit, and a rule for files above the capture
threshold inside the tree — is deliberately not attempted.

**Every actor is covered.** A mutation is journalled regardless of who made it. The journal is a
record of what happened to the mindspace, not of what the AI did to it, so a window of history
can be undone as a whole and the human's own recent work is protected by the same mechanism as
the AI's. The actor is recorded as a label, for display and for the changes view; it does not
determine which entries may be reversed.

**Approval policy is unchanged.** This ADR adds recovery and relaxes no gate. Every operation
that prompts today continues to prompt. Reducing friction is a separate decision that this
journal makes defensible rather than speculative.

**Capture from disk, not from intent.** The pre-image is the affected path's content read
immediately before the mutation. It is not derived from tool call arguments or caller
declarations, because neither reliably describes the write: `transform_file` produces content
that never enters the context window (ADR-0013), `write_file` writes a temporary file and
renames it, and a move has two paths. One helper performs capture for every writer rather than
each deciding for itself, so a new writer joins by calling it and not by inventing a policy.

**Blob storage.** Contents are stored under `.humbug/journal/blobs/`, named by SHA-256 digest and
fanned out one level on the first two hexadecimal characters. Blobs hold raw bytes. The fan-out
is a layout decision rather than an optimisation: it bounds the cost of a sweep, keeps any one
directory small, and is expensive to change once blobs exist.

**Index.** Mutations are recorded in append-only JSON Lines segments under `.humbug/journal/`,
rotated past a byte cap and named after the sequence number of their first record, following the
format ADR-0017 established. A record carries a format version, a sequence number, a timestamp,
the mindspace-relative path, the operation, the location of the pre-image, the post-image hash,
the actor, and the identifier of the conversation whose activity caused the mutation. The index
holds hashes and locators; the blobs hold content.

**Identity is a UUID, not a path.** A journal entry names its conversation by a UUID assigned
when the conversation is created, stored in the transcript, and preserved when the conversation
is renamed or moved. Paths are not usable for this: they change under rename, and the interaction
log's existing path-based attribution (ADR-0017) already loses its referent when a conversation
is renamed. A fork receives a new UUID and records the conversation it was forked from; that
provenance is displayed and is not an authorisation input. Delegation edges remain the only
relationship that confers authority between conversations.

The journal records the identifier and interprets nothing. Which conversations may act on which
others' history — including a conversation acting on a delegated child's, and an AI session
helping a user undo work belonging to a different conversation — is a question about a tool, not
about the store. Keeping it out of the journal means the journal's format does not change when
that policy does, and it leaves one component, not two, answering questions about conversation
relationships.

**Removal.** A removal is journalled with no post-image, and its content is kept wherever the
removal has already put it. A file removed by the user is moved into the trash, which is both the
cheapest possible capture and the right home for content the user deliberately discarded; a file
removed by the AI has no bin, so its content is stored as a blob. Reversal consults the entry's
recorded pre-image location, so the index carries one vocabulary for removals and a reversal
restores a file whoever removed it. Restoring from a trash location delegates to the existing
restore path rather than reimplementing it.

Content the trash holds is not copied into the journal. Copying it would leave two stores holding
the same bytes and disagreeing about what is recoverable: emptying the trash would destroy one
copy while the journal kept the other, so an action the user understands as destroying deleted
content would silently fail to. With one copy, emptying the trash makes those removals
unrevertable, and the interface reports them as such. The journal records the trash entry's name
so that a removal can be found again and a reversal can check the content is still there.

The trash holds two unrelated kinds of item: content the user removed, which is within the
journal's reach, and Humbug's own state such as conversation transcripts, which the `.humbug/`
exclusion keeps out of the journal entirely. That they share a directory and a manifest is a
storage convenience, not a claim that they are the same thing.

**Separate from the interaction log.** The log witnesses that an action happened; the journal
makes its effect undoable. They differ in granularity — messages written for a human to read
versus machine-generated bulk — in lifecycle, since the log is never truncated while blobs must
be reclaimed, and in risk exposure, since an append-only string is a far smaller surface than a
crash-safe paired write plus reclamation. A shared store would hold two contradictory retention
invariants and would let a defect in the newer subsystem damage the audit trail.

**Write ordering.** The blob is written and flushed before the index record is appended. A crash
between the two leaves an unreferenced blob, which is harmless and reclaimable. The reverse order
would leave a record pointing at content that does not exist, which is not.

**A mutation that does not happen is cancelled, not deleted.** The record has to be written before
the mutation, because the pre-image can only be read while the old content is still there. If the
mutation then fails, the record describes something that did not occur, and the changes view would
report it and offer to reverse it. The index is append-only, so the record is withdrawn by
appending a void record that names the sequence number it cancels; reads return live records only.
A mutation is therefore recorded, attempted, and cancelled on failure — one rule for every writer,
so the journal never claims a change that did not happen while keeping the audit property that
records are only ever added.

**Failure.** A capture failure does not abort the mutation, following ADR-0017's rule that
recording must never break the action being recorded. It is not silent either: the failure is
logged at ERROR and the affected file is reported in the changes view as not revertable. A
mutation that was not journalled must never appear to be covered.

**Reversal is a query, not an inverse.** A set of entries is reversed per file, not as a global
last-in-first-out unwinding. For each affected file its entries are walked backwards and the
file is restored to the pre-image of the earliest entry being reversed. Each file therefore
lands on a state that genuinely existed, and a conflict on one file does not prevent the safe
reversal of the rest.

**Divergence is a conflict.** Before a file is restored, its current hash is compared with the
recorded post-image hash of the newest entry being reversed. A mismatch means the file has
changed since in a way the journal cannot account for — a terminal command, an external tool, a
running process — and the file is reported as a conflict rather than overwritten. Because
mutations made through Humbug are journalled whatever their actor, divergence is never about
authorship and never about one actor's work outranking another's.

**Reversal scope is temporal.** A file may be reversed individually, or every entry within a time
window together. Time is how a person describes what they want undone, and a window legitimately
spans several conversations, delegated children, and their own edits. Ordering is the index's
sequence, never a file timestamp, which a process may set arbitrarily. The window is resolved to
a concrete set of entries before the preview is shown and exactly that set is applied, so entries
cannot fall out of the window while the human is deciding.

In practice the useful scope is anchored to a record rather than a clock: "restore the mindspace
to this change" reverses exactly the entries after one the user chose, and the per-path replay
leaves each affected file holding what the newest record at or before it produced. That is the
state the mindspace was in at that moment, described by pointing at something visible rather than
by naming a duration, and it needs no window arithmetic on a wall clock that may be wrong.

In practice the useful scope is anchored to a record rather than a clock: "restore the mindspace
to this change" reverses exactly the entries after one the user chose, and the per-path replay
leaves each affected file holding what the newest record at or before it produced. That is the
state the mindspace was in at that moment, described by pointing at something visible rather than
by naming a duration, and it needs no window arithmetic on a wall clock that may be wrong.

**Retention.** The journal is not capped by age. Age-based expiry would make the reachable
horizon a promise the store cannot keep; with no expiry the horizon is simply the oldest entry,
which the interface reports as a fact. A size ceiling exists as a guard against disk exhaustion,
warns before it reclaims anything, and never evicts silently. Discarding history is a policy
decision for the user on the terms ADR-0017 sets out — explicit, visible and archival — except
where the user asks to forget content for privacy, which is destructive by definition and must
say so. Files above a size threshold are recorded as metadata-only entries, reported as changed
but not revertable, so that one large binary cannot dominate the store. The threshold is 1 MiB,
which was measured rather than guessed: on the Humbug mindspace it captures 96% of files while
holding a complete first capture to about 43 MiB, and leaves 38 metadata-only entries rather than
a long tail. The figures are recorded in `docs/mindspace-tree-scan-measurement.md`.

Retention is addressed by record as well as by time: a caller discards everything up to a sequence
number it has chosen, or all of it. That is what a person means by "forget this and everything
before it", and a time window is a convenience resolved to the same cutoff.

Retention is addressed by record as well as by time: a caller discards everything up to a sequence
number it has chosen, or all of it. That is what a person means by "forget this and everything
before it", and a time window is a convenience resolved to the same cutoff.

**The store is unreachable by tools.** The journal lives inside `.humbug/`, which every tool path
resolver refuses, so no tool can read the store or invoke a reversal by naming a journal path. Any
tool that exposes journal operations is a deliberate, narrow exception: it exposes operations —
list entries for a set of conversations and paths, read a prior version, request a restoration —
and never accepts or returns a journal path, so the exclusion is not widened into a general read
of `.humbug/`. Such a tool is designed separately, and a restoration it requests is a persistent
change requiring user approval like any other.

**A view reads what is new, not the whole index.** Contiguous sequence numbers make a record's byte
position within a segment estimable, so a view that already holds the history it shows can ask for
the records after the last one it saw and read only the tail of the newest segment. Measured on a
20,000-entry journal, reading one appended record costs 0.1 ms against 120 ms for a whole-index
read, which is the difference between a panel that can be open during a long task and one that
cannot. What cannot be applied incrementally — a purge, or a cancellation of a record already
displayed — reloads, because the alternative is a listing that quietly disagrees with the store.

**The trash is unchanged.** The trash remains the user's bin, with its own retention and its own
browsing interface, and a user's removal continues to land there. The journal records that the
removal happened and where the content went; it does not change how the trash behaves, and it
does not cause agent activity to accumulate in a bin the user must learn to ignore.

## Alternatives considered

- **Auto-approve writes that do not overwrite.** Overwrites are the common case, so this
  addresses a minority of prompts, and it removes the gate on file creation, which is the usual
  route by which hooks, `.pth` files and CI workflows are planted. It buys little and costs real
  exposure.

- **Journal only AI mutations.** Narrower, and sufficient for the immediate motivation, but it
  makes the reach of "undo the last thirty minutes" depend on who happened to act, so a task the
  human and the AI worked on together can only be partially unwound. Capturing every actor is
  the same mechanism with fewer rules and no actor to reason about at reversal time.

- **Journal only AI mutations, and filter reversal by actor.** The most complicated of the three
  options and the only one that needs the human to understand a ranking between their own work
  and the AI's. Rejected: the divergence check already protects changes the journal cannot
  account for, so actor-based filtering adds policy without adding safety.

- **Exclude user removals from the journal and point at the trash instead.** Would avoid two
  stores participating in one restoration, but leaves a gap in the one thing a temporal undo is
  for: a file deleted during the window would not come back. Recording the removal with a
  locator, and delegating restoration to the trash, keeps one restoration path and one uniform
  entry vocabulary.

- **Copy removed content into the journal's blob store, so reversal never depends on the trash.**
   Simpler for the reverser, which would need no second store. Rejected because it stores the same
   bytes twice and makes the two stores disagree: emptying the trash would destroy one copy while
   the journal silently kept another, so the user's only means of reclaiming deleted content would
   not do what it appears to do.

- **Store the interaction log as the journal index.** Avoids a second append-only mechanism, but
  the log must never delete anything while a blob store must reclaim unreferenced content, so a
  shared store would hold contradictory invariants. The log is also message-shaped and
  human-readable, and should not depend on a new subsystem for its integrity.

- **Store binary deltas against the previous version instead of whole contents.** Smaller, but it
  conflates detection with storage and makes every restoration depend on an unbroken chain of
  prior versions, so a single damaged entry would poison every later restoration. Content
  addressing also deduplicates identical writes without any extra machinery.

- **Copy the whole tree at each checkpoint.** O(tree) in both time and space per checkpoint, and
  it cannot say what changed, so it cannot tell the user what changed either.

- **Use git as the storage engine.** A mindspace is not a repository and may contain several,
  partial ones, or none. Committing to the user's repositories would violate the principle that
  their content follows their rules, and stash-like manipulation would leave the working tree in
  a state they did not choose.

- **Record the transcript path as the conversation identifier.** Consistent with ADR-0017 and
  needing no new state, but a rename severs the link and a fork is indistinguishable from a copy.
  The field exists to attribute a mutation, so it must not change when a file is renamed.

- **Derive lineage from shared message history.** Fork edges are inferred that way in
  `ConversationDag` because display can tolerate a heuristic. Authorisation cannot: two
  conversations that coincidentally share a message would become linked, and a fork would inherit
  rights over its ancestor's history.

- **Let the journal decide which conversations may act on which entries.** Would put policy in the
  store, so a change in that policy would change the on-disk format, and would create a second
  answer to a question `ConversationDag` already answers for session resumption.

- **SQLite for the index.** Standard library, and it would give indexes and atomic multi-row
  updates, but the access patterns are append and sequential scan. ADR-0017's reasoning applies
  unchanged: a binary file is harder to inspect, copy and verify, and it would introduce a second
  storage idiom to a codebase whose conversations, settings and usage are all plain JSON.

- **Reverse by unwinding all entries in reverse order.** Simpler to state, but one conflict makes
  the whole operation all-or-nothing, where a mixed outcome is safe as long as each file ends on a
  state that existed.

- **Force a reversal over a diverging change.** Would discard work the journal never saw and
  cannot restore, which is the loss the journal exists to prevent. Divergence is reported and
  never forced: there is no override, and adding one would be a separate decision about
  accepting that loss knowingly.

- **Hash-chain the entries for tamper evidence now.** The sequence ordering a chain needs is
  already present, so a chain can be added later without changing the format. It is a separate
  decision about what the chain should cover, including whether the interaction log joins it.

## Consequences

### Positive

- A file mutation inside the mindspace can be reversed, whoever made it, which is a stronger
  guarantee for destructive changes than the approval that preceded them.
- AI removals become recoverable, retiring the claim in the delete prompt that they cannot be.
  That string is a prompt to the user and must be corrected with this change.
- A window of history can be undone as a whole, including the human's own edits, so a task that
  went wrong can be rewound rather than picked apart one file at a time.
- Content hashes in the index make a mutation verifiable rather than merely asserted, and are the
  natural basis for the tamper evidence the blueprint describes.
- Approval friction can later be reduced on evidence rather than faith: the journal's coverage
  defines exactly which operations could safely be relaxed, and which must keep prompting.
- The reach of undo is a reported fact rather than a configured promise, so the interface cannot
  claim more than the store holds.
- Emptying the trash has one consequence: the removals it held become unrevertable, and are
  reported as such rather than restoring something else or failing obscurely.
- Attribution is by an identifier that survives rename, so a mutation stays traceable to the
  conversation that caused it and can later be scoped by a policy that changes without touching
  the store.

### Negative

- The journal grows with history rather than with tree size. Deduplication helps repeated
  identical writes but not iterative editing, so heavily edited files accumulate distinct blobs
  indefinitely unless the user purges.
- Retention is the user's decision, so a mindspace nobody tends grows without limit. The store
  warns when it passes its size ceiling and offers the purge, but it never reclaims anything on
  its own; the warning is only as good as the interface that shows it.
- Reclaiming history is irreversible, and the segment that straddles the cutoff is rewritten to
  drop the records before it. That is the one operation that does not merely append, and it is
  confined to a segment replaced atomically under a user's explicit request.
- Reversibility does not reduce the cost of a wrong direction. A task that goes forty writes down
  a mistaken path can be tidied up, but the tokens, the time and any executed effects are already
  spent. Recovery complements early correction; it does not replace it.
- Capturing human edits places the human's work-in-progress in the store alongside the AI's. That
  is the intent, but it means the journal holds content the user may not think of as recorded, and
  the privacy purge has to be discoverable for that reason.
- The journal depends on conversation identity work that does not exist yet. Until a transcript
  carries a UUID and rename preserves it, the journal cannot populate the attribution field, and
  the log will continue to attribute by path, so the two stores will disagree about a
  conversation's identity until the log is migrated.
- Nothing in this ADR gives an AI access to its own history. An AI session can only inspect or
  reverse mutations through a tool, which is deliberately deferred, so an agentic task that
  overwrites a file it cannot reconstruct still needs the human until that tool exists.
- Every writer must capture a pre-image. A writer that is missed fails open and silently, so the
  list of writers must be revisited whenever the interface grows, and a periodic scan is needed to
  report changes no entry accounts for.
- Capture reads content before every mutation, so writing a large file now costs a read it did not
  cost before. The per-file threshold bounds the worst case but not the common one.
- The journal is a second place where file state lives, and must be excluded from Humbug's planned
  internal versioning of `.humbug/`, for the reason ADR-0017 gives for the log: snapshotting a
  version store on every internal commit is not viable.
- Divergence is a normal outcome rather than an error, so the interface must present per-file
  outcomes with the reason a file was skipped, which is more surface than a single succeed-or-fail
  message.
- A reversed file may already have been read, executed or committed. The journal cannot know, and
  users may over-trust it unless the scope of the guarantee is stated where they will see it.
- A removal the trash holds is revertable only while the trash keeps it, so the reach of undo for
  deletions is bounded by the user's own decision to empty the bin rather than by the journal.
- The trash holds both user content and Humbug's own state, so one directory serves two purposes
  with different guarantees. The distinction is invisible in the store and has to be held in the
  code that writes to it.
