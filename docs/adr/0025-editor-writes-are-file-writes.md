# ADR-0025: An editor write is a file write

Date: 2026-11-08  
Status: Accepted

## Context

ADR-0007 divided tool state into persistent and ephemeral, and put the approval gate where
ephemeral state becomes persistent. Its illustration was the editor: `apply_diff` and `transform`
changed the in-memory buffer and asked nothing, while `save_file` wrote the buffer to disk and
asked. The reasoning was that a buffer change cannot be evaluated before it is applied, so gating
both steps would cost two prompts for one change.

Two things have since happened, and both were preconditions that ADR-0007 named rather than
properties it could rely on.

The first is that a write is now recoverable. ADR-0018's change journal records the pre-image of
every mutation inside the mindspace, and ADR-0022 lets a user approve a whole class of journalled
changes for the rest of a turn. The prompt no longer has to exist because the write cannot be
undone; it exists to let the user see the change before it happens, and that job is now shared with
the record.

The second is sharper, and it makes ADR-0007's premise false in some mindspaces. The editor's auto
backup setting copies the whole buffer to disk on a timer. So an AI's buffer change did not stay
ephemeral: it reached the filesystem without an approval, under a name nothing journalled, and
appeared in the Files panel and the Changes panel as a change nothing had made. The sentence the
tool rested on — that a buffer change is "not yet a change to your files" — was true only where a
setting was off.

There is also the plain operational cost. The tool told the model, in three separate descriptions,
that it must call `save_file` afterwards to persist its work. Models forgot, often enough that the
reminder is the reason this ADR exists. A reminder that is needed because an operation is
half-finished is a description of a defect, not a mitigation of one.

## Decision

`apply_diff` and `transform` write the file. Each is a single operation that changes the buffer and
saves it, and the approval gate moves onto it.

**The approval shows the file change, not the requested one.** The write carries the whole buffer,
so the diff the user is asked about is the whole buffer's difference from the file, which is also
what the journal records the pre-image of. A buffer that already held unsaved work — the user's, or
an earlier change never written — makes that difference larger than the change being requested, and
the approval then says so in words as well as in the diff.

**A change that is not written is not left in the buffer.** If the user refuses, or the write
fails, the change is undone by applying its inverse through the same path, so the buffer is left as
it was found and edits made to it in the meantime are left alone. A refused operation that left the
user's editing surface altered would be no refusal at all.

**Where the write cannot happen, the operation does not start.** A tab showing a read-only view of
an earlier version, and a tab with no file path, are refused before anything is changed. The first
can never be written back; the second can only be given a path by a person, at a file dialog.

**The classification stays the file's.** An editor tab may hold a file outside the mindspace, which
the journal does not record, so the request is classified from `Mindspace.journalled_path()` exactly
as the save was. A change to such a file keeps asking, even under a standing approval.

**`save_file` is removed.** Its only purpose was to persist what the other operations had left in
the buffer. With both operations writing, there is nothing left to persist, and two ways to write an
editor buffer is one more than the system needs.

**The editor writes two things, and both have a home.** The file, journalled. And a backup of the
buffer, which is Humbug's own working state and now lives in `.humbug/backups/` for every tab,
saved files included, rather than beside the file being edited. Nothing the editor writes lands in
the user's tree outside the journal.

## Alternatives considered

- **Keep the gate at a separate save, and make the instruction to save more prominent.** The status
  quo, and the problem being solved: the reminder is what gets ignored. Strengthening it treats a
  missing step as an attention failure when it is a design one.

- **Auto-save the buffer silently, without asking.** This is the failure the journal makes
  survivable but not acceptable: the user would learn about the change from the Changes panel
  afterwards, and every change to a file the journal does not record would still have to ask, so the
  silent and the prompted cases would be arbitrary.

- **Ask before applying, using the requested diff.** Saves nothing the approval already has to say:
  the write carries the whole buffer, so a hunk that is shown alone understates the change whenever
  the buffer was already dirty. It also leaves the user judging a change they cannot see, which is
  the objection ADR-0007 raised against gating the buffer in the first place.

- **Keep `save_file` for untitled tabs, retries and saving someone else's work.** Each of the first
  two is a real path but not a reason to keep an operation: a failed write is retried by repeating
  the change, and an untitled tab must be named by a person, which the AI cannot do. The third is not
  the tool's business. An operation kept for two rare paths is an operation the model will reach for
  by habit.

- **Refuse the write when the buffer holds unsaved work, rather than showing it.** Safer in the
  narrow sense and worse in the common one: a user who has typed in a file and then asks the AI to
  change it would be blocked, with no way forward except saving by hand first. Showing the whole
  change and naming the extra work lets them decide.

- **Leave the backup beside the file.** The half-migrated state this ADR tidies: `.humbug/backups`
  was introduced for untitled tabs and never adopted for saved ones. A backup beside the file is user
  content to every other part of Humbug — listed, searchable, scanned, reported as an unattributed
  change — when it is only ever a crash copy.

## Consequences

### Positive

- One operation, one approval, one journal entry, and no step to forget. The instruction to save is
  gone from the tool's descriptions because the thing it asked for no longer exists.
- The user approves what is actually written, and what is journalled is what they approved.
- A refused or failed write leaves the buffer unchanged, so the tool's atomicity claim covers the
  buffer as well as the file.
- Editor content no longer reaches the disk by any route other than the journalled one, so the
  approval model's claim about buffers is true whatever the backup setting is.
- Backups moved out of the user's tree, so `git status`, the Files panel, search and the change
  watcher see only content that is content.

### Negative

- Every AI edit to a file asks, where a buffer change used to be free. On a long task this is a
  prompt per edit unless the user arms the standing approval, which requires the file to be inside
  the mindspace.
- An approval shown is an approval that can be stale: the user may edit the buffer while the card is
  on screen, and the write then includes those edits even though the diff they read did not.
- The write commits the buffer's other unsaved work as well, which is reversible but is more than
  the AI asked for. The approval names it, and a user who does not read the card is still surprised
  by it.
- Undoing the change on refusal costs a second application of the diff machinery, so the buffer is
  briefly in the changed state before it is put back. If the user edited the changed region in the
  meantime the inverse may not apply, and the buffer keeps a change that was refused — reported to
  the AI, and repaired by a person.
- Removing `save_file` removes the only way an AI can persist a buffer it did not change. Nothing
  needed it, but a future front end that lets an AI edit a buffer without a file behind it would
  have to introduce the operation again.
