# ADR-0023: Journalling a creation, so a directory can be reversed

Date: 2026-11-07  
Status: Accepted

## Context

ADR-0018 made the journal the record of every file mutation made through Humbug, and stated the
rule that keeps it honest: "Every writer must capture a pre-image. A writer that is missed fails
open and silently." Two writers were missed — the git tool writing `diff`/`show` output to a file,
and the document converter writing the image folder beside a converted document — and the
filesystem tool's `create_directory` recorded nothing because there was nothing for it to capture.

A created **file** was already covered, if only in principle. `JournalEntry.existed_before` records
that a path was absent, and reversal removes what the creation put there. A created **directory**
had no such record: a directory has no content, so no writer could produce an entry describing one,
and reversal would have had nothing to tell it to remove a directory rather than unlink a file.

This mattered twice over. It left the least dangerous write in the tool set — creating a folder —
as the one operation that always prompted, even under a standing approval that covered everything
around it. And it left the document converter half-recorded: reversing a conversion to Markdown
restored the document and left its images orphaned in a folder nothing knew about.

## Decision

An entry records whether the path is a directory **after** the mutation, as well as before it.

**One field, symmetric with the one already there.** `post_image_is_directory` joins
`pre_image_is_directory`. An entry now describes a mutation by what was at the path before and what
is there after, which is what lets reversal choose between unlinking a file and removing a
directory without inspecting the tree and guessing. Both fields default to false, so every record
written before this ADR reads unchanged.

**A creation is recorded, not inferred.** `FileJournal.record_creation()` writes an entry with no
pre-image and no post-image digest, because a creation has neither. The path is still inspected
rather than assumed: the record says `existed_before` according to what is actually on disk, so a
creation recorded for a path that unexpectedly exists reads as a write and reversal restores what
is there instead of deleting it.

**A directory is removed only when it is empty.** Reversal removes a created directory with `rmdir`,
never recursively. Anything inside it was put there by later mutations that this reversal is not
undoing, and removing them would discard work the journal cannot restore. A directory that is no
longer empty is reported as failed, with the reason, rather than emptied.

**A path is reverted before the paths beneath it.** Reversing a creation that made a chain of
folders — `create_directory` with `create_parents` — records each folder it made. Unwinding those
records requires the deepest path to be handled first, or the parent's removal fails on the child
the reversal is about to remove. Reversal now orders paths deepest-first, which changes nothing for
independent paths and makes a parent/child pair behave.

**The writers that were missed now record.** The git tool records the output file it writes and
attributes it to the conversation that asked, which also lets the write be covered by a standing
approval. The document converter records the image folder as a creation and each image as a write,
before either happens, so a conversion to Markdown or HTML is reversible in whole rather than in
part. That needed one seam: `extract_images_to_sidecar()` takes an optional callback that is given
each path — and the bytes about to be written, or `None` when it is the folder itself — immediately
before the write, because a pre-image can only be captured while the old content is still there.

**What is still not covered: a populated directory move.** An empty directory can now be journalled
at both ends and its move reversed. A directory holding files cannot, because its contents are not
captured, and ADR-0018 deliberately declined recursive capture of a directory move. Such a move
keeps its own prompt, and its authorization text says the contents will be lost.

## Alternatives considered

- **A separate `JournalOperation.CREATE`.** Symmetric with `DELETE`, and tempting. Rejected because
  the operation alone cannot say whether a directory or a file was created — the removal of a
  directory already uses `DELETE` with `pre_image_is_directory` — so the new operation would have
  needed the new field anyway, and the index would then hold two vocabularies for the same thing.

- **Infer the kind from the filesystem when reversing.** Fewer fields, and no format change. But
  reversal would be deciding what a record meant from what the tree happens to hold now, which is
  the same guessing the divergence check exists to avoid, and a directory replaced by a file since
  the record would be silently misread.

- **Record only the leaf directory of a `create_parents` chain.** Simpler, and what the tool
  originally did by recording nothing at all. It reverses the visible part and leaves a trail of
  empty folders the journal does not account for, which is the same half-promise the document
  converter's sidecar had.

- **Remove a created directory recursively.** Would make reversal of a nested creation work in one
  step and would silently delete files that later mutations created and this reversal is not
  touching. Rejected on the principle that reversal never discards work the journal cannot restore.

- **Give the converter its own capture instead of a callback seam.** The converter knows the
  document IR, so it could walk it, derive the image names, record them and then call the writer.
  That duplicates the naming rule `extract_images_to_sidecar` owns and would drift from it. One
  callback, called at the moment of each write, keeps the rule in one place.

## Consequences

### Positive

- The least dangerous write in the tool set no longer asks for permission while deletions around it
  do not, and a standing approval covers a whole refactor without an interruption in the middle.
- Reversing a conversion to Markdown or HTML removes the images it extracted instead of leaving
  them behind.
- A git diff written to a file is reversible and attributed, so it no longer appears in the changes
  list as something that happened on its own.
- No writer in the AI's reach is now silent, which is what makes "the journal records every mutation
  made through Humbug" true rather than nearly true.

### Negative

- The entry format gained a field. Records written before it read as "not a directory", which is
  correct for all of them, but the format is now versioned by compatibility rather than by a
  version number, and a second such addition should probably come with one.
- Reversal orders paths deepest-first rather than in sequence order. The order is no longer what a
  reader of the index would predict, and the reason — a parent cannot be removed while a child is
  still in it — is only written down in the code and here.
- A created directory that has since gained content cannot be reversed, and the failure is reported
  per path. That is honest, but it means "undo this folder creation" can legitimately fail on a
  mindspace somebody kept working in.
- A move of a populated directory remains unjournalled, so the changes list still cannot describe
  it and the prompt still has to warn that contents will be lost.
- The converter records its sidecar writes through a callback, so a future format that writes
  something else beside the document has to be taught the same seam or it will be a silent writer
  again.
