# ADR-0017: Interaction log storage — append-only segments, no truncation

Date: 2026-10-29  
Status: Accepted

## Context

The mindspace interaction log is the audit trail described in the blueprint: a record of every
AI tool call, human action, approval and denial, and settings change, kept as an independent
witness that the AI cannot reach. It is the foundation for a future tamper-evident chain.

The implementation did not support that intent. The log was held as a single in-memory list,
capped at 100 entries, and rewritten in full to `.humbug/system.json` on every event. The cap
existed to keep the log tab responsive, because the tab builds one widget per entry with no
bounded window — a view problem that had been solved in the storage layer.

Three defects followed from that:

- **The record was destroyed.** Truncation happened when appending and again when loading, so
  the oldest entry was silently discarded on every event and any larger file was cut down the
  first time it was opened. At roughly 200 bytes per entry and a few thousand entries per working
  day, the "complete audit trail" retained a few minutes of activity, with no record that
  anything had been lost.
- **The log was not durable.** Reopening the file for writing truncated it before the new
  content was written, so an interrupted write left invalid JSON, and the loader responded to
  invalid JSON by discarding the entire history.
- **The cost of logging grew with the log.** Every event rewrote the whole file and triggered a
  full re-diff of the view, and a truncated write could only be detected by losing everything.

A rewrite also has no value for a tamper-evident log: a chain over a prefix that Humbug rewrites
on every event proves nothing about what it replaced.

## Decision

The interaction log is an append-only store, and the only limits on it are limits on a single
record and on what a caller reads.

**Records.** Each record is one JSON object on its own line in a `.jsonl` segment file. Appending
is a single write followed by `flush` and `fsync`, so the common case is O(1) rather than O(n),
and an interrupted write can only damage the partial final line, which is skipped on read. A
record carries a `sequence` number, a level, a timestamp, and content. `sequence` replaces the
UUID: it is assigned by the store, starts at 1, and is contiguous, so it identifies a record
across sessions and is simultaneously the record count. Reads are addressed by sequence.

**Context.** A record also carries an optional `context` object, a map of short string keys to
string values, omitted when empty. Content is written for a human reader; context is what makes
the record attributable, so that entries can be selected by the thing they relate to rather than
by parsing prose. The field that matters today is `conversation`, holding a conversation's
transcript path made mindspace-relative (the same identifier ADR-0011 calls a session
identifier). An entry records the conversation whose activity it belongs to: a prompt or
response belongs to the conversation that spoke, a tool call to the conversation that requested
it. Entries that are not part of any conversation's activity — settings changes, human file
management — carry no context. Unknown keys are preserved, so a reader that does not understand
a key ignores it, and records written before `context` existed read back with an empty context.

**Content bound.** Content longer than 4096 characters is truncated, and the omission is stated
in the record itself (`... [N characters omitted]`). Typical entries are around 200 bytes, so
this only affects the unbounded interpolations that exist at a handful of call sites — terminal
keystrokes, search patterns, exception text, paths. The payload of such an action is already
recorded in its conversation, so the audit record's job is to witness that the action happened,
when, and at which sequence. A silent truncation would be the same sin as a silent deletion, so
truncation must be visible; a maximum record size also guarantees that rotation always makes
progress.

**Segmentation.** A segment is rotated once it exceeds 4 MB, and rotation starts a new segment
rather than removing anything. At roughly 200 bytes per record that is about 20,000 records, or
some weeks of typical use; at 500 records a day the log grows by about 40 MB a year. Segments are
named after the sequence number of their first record, which makes the log self-describing and
means no second index file is needed that could fall out of step with the data. The segment size
is chosen so that operations which touch the whole history — export, a deep search, a future
integrity pass — advance in small steps, and so that a segment needing repair is small to copy.

**Retention.** Segments are never deleted. Deletion is a policy decision that must be explicit,
visible to the user, and archival rather than destructive, so it does not belong in a constant
inside the store.

**Failures.** A write failure is recorded on the store and logged at ERROR rather than raised,
because logging an interaction must never abort the action being recorded, and is not swallowed
either, because a gap in the audit trail is a failure the user must be able to see.

**Bounds belong to the view.** The log tab renders a window of the newest entries and reads only
the entries appended since the last load, discarding entries that fall off the front. That bound
is a property of the widget, not of the store.

Existing logs were discarded rather than migrated. They were at most a few tens of kilobytes,
contained only the last 100 events, and could not be recovered beyond that, so migration would
have preserved a fragment of a fragment.

## Alternatives considered

- **Keep the JSON array and write incrementally.** Appending inside a JSON array requires
  rewriting the closing bracket, so the file is still not append-only, and a crash can still
  corrupt the structure rather than one line.

- **Keep the entry cap but raise it.** Any fixed entry cap is an arbitrary silent deletion, and
  it would still be enforced by the renderer's cost rather than by what the user needs to keep.

- **Cap by entry count per segment instead of bytes.** Byte size is what actually bounds parse
  and scan cost, and it handles records of wildly different lengths; a count cap lets one
  pathological record dominate a segment.

- **A SQLite database.** It brings indexing and querying, but the log's access patterns are
  sequential append and sequential scan by sequence, and a single binary file is harder to
  inspect, copy, and verify than plain text. It would also add a second storage idiom to a
  codebase whose conversations, settings, and usage are all plain JSON files.

- **A sidecar index of segment byte offsets and counts.** It removes the need to scan on open,
  at the price of a second file that can disagree with the data. Contiguous sequence numbers and
  a tail read of the newest segment give the same answer without one.

- **Compress segments when they rotate.** At tens of megabytes a year the saving does not pay
  for decompression on the read path. Worth revisiting if a single mindspace passes about 1 GB.

- **A retention window with deletion.** Rejected on the grounds above; if it is ever wanted it
  must be a user-visible setting that archives rather than deletes.

## Consequences

### Positive

- The audit trail is complete: no event is discarded to make room for another, and nothing in
  the store has a length limit.
- Appending does not get slower as the log grows, and the write path cannot truncate what is
  already recorded.
- An interrupted write costs one record, not the history.
- Monotonic sequence numbers give records a stable identity across sessions and are the
  ordering a hash chain would later need, so the format does not have to change to support
  tamper evidence.
- Entries are attributable to a conversation without parsing their text, so the log can be
  read conversation by conversation and a future view can filter on that.
- The view bound is now explicit and local, so it can be raised, paged, or replaced by a
  virtualised list without touching storage.

### Negative

- The log grows without bound. A heavily used mindspace will accumulate hundreds of megabytes
  over years, and there is no automatic reclaiming short of deleting segments by hand.
- Reading across a segment boundary needs the segment list, so the store keeps a small in-memory
  index derived from filenames at open.
- `sequence` is only a reliable record count while the log is intact; a manually deleted segment
  or a corrupt record makes the count an upper bound.
- Rotation, `fsync` and truncation are policy that now lives in the store and must be respected
  by anything that writes to it.
- The log tab shows the newest page of entries rather than the whole log. It reports which range
  it is showing in the status bar, and find searches the whole log and loads the page holding a
  match, but browsing backwards page by page is a view-layer feature this ADR does not deliver.
- Find highlights are computed when a search runs, so entries appended afterwards are matched by
  the count but not highlighted until the next search.
- Humbug's planned internal versioning of `.humbug/` will have to treat an append-only log
  specially: snapshotting hundreds of megabytes on every internal commit is not viable, so the
  log will need excluding from snapshots or hard-linking them.
