# ADR-0021: One home per removal — the trash keeps conversations, the journal keeps everything else

Date: 2026-11-06  
Status: Accepted

## Context

ADR-0018 gave the mindspace a change journal that records the pre-image of every file mutation made
through Humbug, and closed the asymmetry between actors that motivated it: a mutation is recorded
whether the AI or the human made it. It did not close a second asymmetry, in the *capture*.

A file the user removes from the Files, Preview or VCS panel is moved into `.humbug/trash/`, which
is where its content survives. A file the AI removes is copied into `.humbug/journal/blobs/`. The
two captures differ in every property a user could care about. One moves, the other copies. One is
listed in the Trash panel, the other in the Changes panel. One is destroyed by Empty Trash, the
other by forgetting journalled history. One keeps the file under its own name, the other under a
digest.

ADR-0018 saw the seam and accepted it as a convenience: *"The trash holds two unrelated kinds of
item ... That they share a directory and a manifest is a storage convenience, not a claim that they
are the same thing."* The convenience has been paid for since. The Trash panel lists rows it cannot
open, because it lists files as well as conversation transcripts. A single delete has two recoveries,
and which one is offered depends on which panel the user happened to have open. The rule a reader of
the code must learn is "a removal goes to the trash, unless the AI did it".

One of the two stores has work the other cannot do. Conversation transcripts live under
`.humbug/conversations/`, which the mindspace boundary and the `.humbug/` exclusion keep out of the
journal entirely and deliberately: that exclusion is what stops the AI reaching its own audit trail,
and a conversation is Humbug's own state rather than the user's content. Deleting a conversation
therefore needs a bin, and will always need one.

So each store has one thing only it can do, and one thing it currently does twice.

## Decision

The trash handles conversations. The journal handles every other removal.

**The choice is the boundary the journal already draws.** `Mindspace.journal_target()` answers, for
any path, whether the journal covers it, from the two rules that already exist: inside the mindspace,
outside `.humbug/`. A removal asks it. A path it rejects is Humbug's own state from the conversations
directory, and goes to the trash. A path it accepts is user content, and is journalled. Neither store
re-tests the other's rule, so the choice cannot drift from the scope that reversal and the change
watcher already use.

**Every journalled removal captures its content, whatever the actor.** A file removed from a panel is
recorded with its pre-image in the journal's blob store and then removed — the path the AI's
filesystem tool already takes. One removal rule, one capture, one restoration.

**An empty directory is reversible without a bin.** A directory has no content to capture, so
reversal recreates it where the record says it existed rather than restoring it from somewhere else.
Deleting an empty folder therefore stays reversible once the trash stops holding it. This also
brings the AI's `remove directory` operation into the journal, which ADR-0018 left outside it
because it had no bin to put anything in: an actor is not a reason for a directory removal to be
recoverable by one and not the other, and with the bin gone there is no longer a distinction to
preserve.

**Empty Trash and Delete Forever describe the trash.** They destroy what the bin holds, which is now
conversations, and they say so. They make no claim about the journal, and the journal makes no claim
about them. The two stores keep the opposite retention rules ADR-0018 gave them, and neither depends
on the other for a promise it makes.

**The trash's two-purpose history is not rewritten.** A mindspace created by a development build may
hold a file removal recorded under the old rule, naming a trash entry that reversal restores from.
Those records stay readable and reversible; only new records stop naming the trash.

This ADR supersedes the **Removal** section of ADR-0018, and its rejection of *"Copy removed content
into the journal's blob store, so reversal never depends on the trash"*. The rest of ADR-0018 —
scope, capture from disk, blob storage, index format, identity, write ordering, and the separation
from the interaction log — stands unchanged.

## Alternatives considered

- **Leave both captures in place and document the seam.** The status quo, and the cheapest option.
  Rejected because the cost is not one-time: every reader of the removal path has to learn an
  actor-dependent rule, and the Trash panel's row that cannot be opened is a visible symptom rather
  than a hidden inefficiency.

- **Unify on the trash: every removal moves into it, the AI's included.** This is the mirror image of
  the decision above and would keep the cheapest possible capture, the file under its own name, and a
  bin the user already knows. Rejected because agent activity would then accumulate in that bin,
  which ADR-0018 already identified as a cost: the user would have to learn to ignore the panel that
  is supposed to be their recovery. It would also make the journal's reach depend on the bin's
  contents — emptying the trash would silently shorten the window a restore covers, which is the one
  thing a temporal undo must not have.

- **Journal everything, conversations included.** Impossible while `.humbug/` is excluded from the
  journal, and that exclusion must not be weakened. The journal is reachable by an AI tool through
  reversal; the audit trail must not be.

- **Make the Trash panel a filtered view over the journal.** Would unify the picture without changing
  where content is stored. Rejected because the panel must list conversations too, so it would read
  two stores anyway, and would then have to explain that the file rows it shows are destroyed by a
  purge in a different panel. It unifies the presentation, not the model.

- **Make Empty Trash also purge the journal's captured deletions.** One button for "get rid of deleted
  content". Rejected: it makes one store's retention action reach into the other's, which is the
  conflation of two opposite retention invariants that ADR-0018 kept the stores apart to prevent.

- **Keep journalling the trash locator and also move the file there.** Two copies of the same bytes
  and two answers to "is this recoverable" — the alternative ADR-0018 already disposed of, and not
  revisited here.

## Consequences

### Positive

- One removal rule, one capture per store, one place content is kept and one place it is restored
  from.
- The human and the AI take the same removal path, completing the convergence ADR-0018 began.
- The Trash panel lists only what it can act on, and the Changes panel lists everything a revert
  would touch.
- The actor stops being a reason for two behaviours, so a new panel that can delete a file has one
  rule to follow rather than a choice to make.

### Negative

- **Emptying the trash no longer destroys a deleted file.** The manual currently promises that Delete
  Forever cannot be undone. For files that becomes untrue, and the promise that replaces it is
  weaker and further away: what destroys a deleted file's content is forgetting the journalled
  history that holds it. That is a different panel, and a coarser action — an interval of history
  rather than one item, because the journal reclaims by span. The user's means of reclaiming disk
  still exists, but it moves, and it becomes less precise.
- **A delete stops being free.** Moving into the trash is O(1); capturing to blobs reads and writes
  the whole file. Deleting a large file becomes proportional to its size and counts against the
  journal's size guard, so a habit of deleting large files will make that guard fire. ADR-0018
  described the trash as "the cheapest possible capture", and that was accurate.
- **Discoverability.** A user who deletes a file and looks for it in the Trash panel will not find it.
  It appears in the panel that is described as showing *changes*, which is where a removal now
  belongs, but it is a thing to be taught rather than assumed.
- **Every deletion's recoverability now depends on the journal's retention rather than the bin's**, so
  a user who forgets journalled history destroys more than they used to with the same action.
- **The trash still needs a retention decision of its own.** It has no automatic expiry for
  conversation content, and this ADR does not give it one.
