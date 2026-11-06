# ADR-0020: Terminal command effects — checkpointing the mindspace so a restore covers them

Date: 2026-11-05  
Status: Accepted

## Context

ADR-0018 records the pre-image of every file mutation made through Humbug's own interfaces so
that it can be reversed. It does not see the effects of terminal commands, and it cannot see
anything the user does outside Humbug at all. A command typed into a terminal, run by the AI
through the terminal tool, or started as a background process from one can rewrite the mindspace
with nothing recorded, so the Changes panel offers a restore that is not the whole truth.

ADR-0018 is explicit that this must not be answered by routing terminal commands through the
approval gate: every gate stays where it is, and reducing friction is a separate decision. The
answer is to observe the mindspace, so that a restore either covers what a command did or says it
cannot.

Four facts shape what is possible.

**A scan finds state, not pre-images.** A scanner runs on a schedule that cannot know which files a
command will touch, so a scan taken after a change tells you which files changed and nothing about
what they held. What makes the guarantee reachable is that this is only true of a *single* scan: in
a series, the state observed at one checkpoint is the pre-image of the interval that follows it, so
nothing has to be predicted about a command that has not run yet.

**A command's duration is unknowable.** It may take seconds, minutes or hours. A single checkpoint
before it bounds nothing, because everything a command writes after its first mutation is
unrecorded. Any design that assumes a start and an end will be wrong about the case that matters.

**The terminal stack has no command lifecycle event.** `TerminalBase` is a PTY wrapper exposing
`start`, `read_data`, `write_data`, `is_running` and `terminate`. There is no prompt mark, no exit
status, and no notion of a command beginning or ending. What does exist is one chokepoint for each
direction: every byte written to a PTY passes through `write_data`, from both the human typing in a
terminal widget and the AI's terminal tool, and every byte returned passes through `read_data` in
the tab's read loop. Neither is a command boundary.

**A metadata walk is cheap and a content read is not.** Measured on a mindspace of about a thousand
files (`docs/mindspace-tree-scan-measurement.md`): 6.7 ms for a pruned metadata walk, 242 ms to read
and hash every file. Roughly 4.8 µs per entry, so a mindspace ten times larger walks in about half a
second.

## Decision

A checkpoint walks the mindspace tree collecting file metadata. Comparing it with the checkpoint
before it yields the set of paths that changed. Each of those changes is recorded in the change
journal as an entry, and can be reversed exactly as a mutation Humbug made itself can.

**Ordering is the index's sequence, never a timestamp.** Ordering is by when Humbug learned of a
change: for a mutation, the moment it recorded it; for a detection, the moment the walk ran. Both
are the same claim, and one sequence expresses it. A second store would have no order in common with
the journal's, so a merged listing could not honestly say which of two changes in one interval came
first.

**A detection is an ordinary entry.** It carries the path, whether the path existed before, the
digest before and after, and it is recorded against the actor `unattributed` — a statement that
nothing in Humbug made the change, rather than a guess at which process did. Everything ADR-0018
built then applies unchanged: per-path reversal, the divergence check, voiding a record whose
mutation did not happen, and a panel that lists entries without knowing what produced them.

**A checkpoint's stored content is the pre-image of the interval that follows it.** This is what
makes reversal reachable: the state observed at one checkpoint is what to restore to when the next
checkpoint finds the path different. Recording the observation is the whole mechanism; no scan has
to guess what a pending command will do.

**Content is captured for what changed, not for the whole tree.** The metadata tuple decides whether
a file has to be read, and a file is read only when its tuple moved. Measured over repeated
intervals: an idle tree flagged no path at all in eleven consecutive samples, and touching 300 files
cost 300 reads in the one interval containing the writes and none afterwards. The cost of a
checkpoint series therefore follows the content that changes and never the time that passes, which
is what makes a checkpoint per interval affordable for a command of unknown duration.

The tuple is a prefilter only. It decides whether to read, and is never the recorded ordering,
because a file timestamp is not evidence of when something happened (ADR-0018). Its strength is
platform-dependent, and the difference is recorded rather than assumed: on Unix `st_ctime` is inode
change time and userland cannot set it, so a rewrite with a restored `st_mtime` still moves it; on
Windows `st_ctime` is creation time and does not move on modification, so the prefilter degrades to
size and modification time there.

**One full capture per mindspace.** A baseline holding metadata alone cannot supply the pre-image of
the first change detected after it, so the first checkpoint reads and stores every file under the
capture threshold. That is a one-off cost of about 242 ms and 43 MiB on the measured tree, and it is
what lets a restore reach back to the moment the mindspace was first watched. It records no entries,
because nothing has changed yet.

**There is no size threshold, and this supersedes ADR-0018's.** That decision recorded a file above
1 MiB as metadata only, so that one large binary could not dominate the store. The consequence was
a change that could be seen and never undone, which asks the user to hold a distinction the
interface cannot show them. Content is now copied in chunks, so retaining a file costs the time to
read and write it once and no memory at all, and size no longer decides revertability. What
protects the store instead is the size guard and the purge: the journal warns as it grows and
reclaims nothing on its own, which is ADR-0018's retention rule unchanged.

A capture can still fail, and an entry whose content could not be stored is recorded with no
pre-image and reported as not revertable. That is a failure being reported rather than a policy
being applied.

**The interval is configurable, and also measured.** A slower machine, or one running antivirus on
every file open, needs a longer interval than a warm fast one. The scanner predicts the cost of its
own walk and lengthens the interval, reporting the shorter horizon, when a walk would take a
significant fraction of it. Configuration is the override; self-measurement is the default, so the
feature does not depend on a user knowing what number to choose.

**Checkpoints are triggered by activity, not by operation boundaries.** A checkpoint is taken when a
mindspace opens, when input is submitted to a terminal, on the interval while a terminal has produced
output since the last checkpoint, and shortly after output stops. An idle mindspace costs nothing.
Because the cadence follows activity rather than a command's start and end, it survives background
processes that outlive the command that started them.

The trigger at submission fires before the input reaches the PTY, and the quiescence trigger fires
only after output has actually stopped, because a command that has produced nothing yet looks exactly
like one that has finished. The consequence is a narrow and unavoidable blind spot: a very fast
command that completes between the submission checkpoint and the next interval is discovered at the
following checkpoint rather than before it.

**The baseline is persisted.** A checkpoint written only to memory cannot say anything about the time
Humbug was closed, which is the case users most often create: close Humbug, work in another editor,
come back. The baseline is therefore a file under `.humbug/`, written atomically, and reconciled when
the mindspace opens, so those changes are detected and journalled at the next checkpoint rather than
passing in silence. They are not attributed to the interval in which they were discovered.

That file has a single writer only because `MindspaceManager` holds an exclusive lock on the
mindspace for as long as it is open. The scanner depends on that, and the dependency is recorded
rather than assumed, in the same terms as the instance registry: the registry is advisory, the claim
is the decision.

**Retained content is declared, not inferred.** The baseline holds digests for files no entry refers
to, so the journal's sweep is told what the baseline still needs rather than being allowed to reclaim
it. The keep-set is the union of every live entry's digests and the digests the baseline declares.
Purging history through a sequence number still discards what it discarded before; it does not
silently destroy the state a later restore would land on.

**A change the journal already recorded is not recorded twice.** A file written through a Humbug
interface and then noticed by a walk is one change. The watcher suppresses a detection that an entry
added since its last checkpoint already accounts for, so the panel shows one row and the reversal
chain has one link. The cost is that a path written by Humbug and then also touched by something else
in the same interval is attributed to Humbug; the record is wrong in the direction that preserves a
reversible state, and distinguishing the two would mean attributing a change to a cause.

**Attribution is not attempted.** A checkpoint can say that a path differs from the checkpoint before
it, and that it began differing within a window. It cannot say what changed it. In particular it must
not name a command, an actor, or a process, and it must not infer a cause from "a command was running
during that interval": a build and an AI edit overlap constantly, so the inference would be wrong
exactly where it was most confident. This is the same refusal ADR-0018 makes about ranking one
actor's work above another's.

**The scanner prunes generated output, and by its own rule.** A coverage report, a cache or a build
directory is rewritten wholesale by one command and rebuilt from sources that are themselves watched,
so recording it adds no coverage and buries the changes a user would act on. Its prune set is its own
rather than `IGNORED_DIRS`: that set also decides what global search and the Quick Switcher show, and
search should not hide generated output a user might want to read while the change list must not fill
with it. Measured, pruning costs nothing in coverage — excluding the build caches removed 67% of the
tree and changed the paths read by 234 of 235, all of them the coverage report that one observed test
run had just rewritten.

This is a judgement about what belongs in a change list, not a performance decision, and the two are
worth keeping distinct: a large source tree is scanned in full whatever it contains.

**Deletions are reported as deletions.** Two path sets compared against a persisted baseline
distinguish new from removed without extra machinery. The caveat is stated rather than hidden: a
deletion inside a pruned directory is invisible, because the scanner never had the path.

**A checkpoint is a filesystem view, not a process view.** Nothing arranges for the tree to hold
still while it is walked, so a file being rewritten in place may be observed in a state that never
existed as a complete document. The scanner records what it saw and does not attempt to identify a
torn write, because it has no way to tell one from a genuinely odd file.

**What the interface says.** A restore covers every change that was observed, whoever or whatever
made it, and the confirmation says so rather than disclaiming changes made outside Humbug. What it
states instead is what is genuinely not covered: changes made since the most recent checkpoint, files
too large to capture, and anything that happened before the mindspace was first watched.

## Alternatives considered

- **Route terminal commands through the journal's approval gate.** Would make every effect recorded
  and reversible, and is rejected by ADR-0018: approval policy is unchanged and reducing friction is
  a separate decision. It also makes the human the rate limit on every task, which is the problem the
  journal exists to solve.

- **Detect and report changes without making them revertable.** The honest floor, and it answers "can
  I undo this" with no when the answer could be yes. The mechanism that makes them revertable costs
  nothing on an idle tree, so there is no reason to stop there.

- **Keep the scanner's results in a store of their own.** Would avoid two entry shapes in one index.
  Rejected because two stores have no order in common, so a merged listing cannot state which of two
  changes in one interval came first, and a restore would have to reconcile two accounts of the same
  path.

- **Record one bulk record per walk rather than one entry per changed path.** Cheaper in index size.
  Rejected because it cannot express "this path held X and now holds Y", which is the only shape the
  existing reversal, divergence and panel logic understand.

- **Snapshot the whole tree at every checkpoint.** Complete, and O(tree) in space per interval. With
  content addressing the bytes are only marginally worse, but it reads the whole tree every interval
  instead of only what moved, which the measurement shows is the difference between free and not.

- **Snapshot the tree before each command.** Requires predicting which files a command will touch,
  which is unknowable, and pays the full cost per command rather than per change.

- **Capture content only for files the journal already knows about.** Bounds the cost, and leaves
  every file a command creates — the commonest thing a build does — unrecoverable and arbitrary.

- **Detect and report only, with no persisted baseline.** Catches changes while Humbug is running,
  and says nothing about the case users create most often. That silence is indistinguishable from
  "nothing changed".

- **Output quiescence as the command boundary.** No shell configuration, works everywhere, and fires
  for human and AI input alike. Rejected as the sole trigger: it fires on an idle prompt, cannot tell
  a finished command from a paused one, and misjudges a long-running silent command. Retained as one
  trigger among several, where being wrong only costs a cheap walk.

- **Shell integration via prompt marks.** Exact, covers both actors, and gives boundaries and exit
  status. Deferred rather than rejected: it needs per-shell configuration, and a shell that has not
  been configured silently receives no checkpoints unless that is reported. It would sharpen the
  triggers this decision builds without changing what a checkpoint is.

- **Hook only AI-invoked commands.** Cheap and exactly attributable, since the tool knows which
  conversation asked. Rejected for two reasons: the human typing a destructive one-liner is the
  likeliest case, and the tool returns when the bytes are written to the PTY rather than when the
  command finishes, so it provides attribution but no boundary at all.

- **Scan only while a command is in flight.** The cheapest cadence, and it covers agentic work.
  Rejected because it misses the case the feature exists for: work done in another application, or
  while Humbug is closed, happens when no command is running.

- **Extend `IGNORED_DIRS` with the build directories.** One list instead of two. Rejected because that
  set is shared by global search and the Quick Switcher, so the scanner's prune set would silently
  change what those features show.

- **Correlate a change with a command known to be running.** Free, since the scanner knows when a
  command was in flight, and it would read like attribution. Rejected as a causal claim the data
  cannot support; it is most wrong where it is most confident.

- **Snapshot into a git repository or the user's repository.** Git is not available in every
  mindspace, a mindspace may contain several partial repositories, and committing to the user's
  repositories would violate the principle that their content follows their rules.

- **Report only at reversal time, as the existing divergence check does.** Requires no new store, and
  is what happens today. Rejected because the user learns what is not covered after asking for a
  restore, which is the moment the belief that they are returning to a known state has already been
  formed.

## Consequences

### Positive

- A change made by a terminal command, or by anything else outside Humbug, is reported and can be
  reversed, so restoring to an earlier change actually restores the mindspace.
- There is one order, one horizon, one reversal mechanism and one set of per-path outcomes, because
  there is one index.
- The cost follows the content that changes rather than the time that elapses, so a command of
  unknown duration is affordable, and an idle mindspace costs nothing.
- A background command, or work done in another application, is covered by the same mechanism as a
  foreground command.
- A detection inherits the divergence check for free: a file edited after detection is reported as a
  conflict rather than overwritten.
- The panel needs no second data source, no merged ordering and no second cursor.
- Nothing here changes approval policy, and nothing here makes a command prompt for permission.

### Negative

- The journal now grows with changes made outside Humbug, which is content the user may not think of
  as recorded. The privacy purge has to be discoverable for that reason, which ADR-0018 already notes
  for the human's own edits.
- The first checkpoint of a mindspace reads and stores the whole tree, which is a visible one-off cost
  on a large mindspace and must not be triggered while the user is waiting for something else.
- The reach of undo is always slightly behind: a change made since the most recent checkpoint is not
  yet recorded, so the guarantee is bounded by the interval.
- A detection's order is by discovery, so within one interval the true order of two changes is
  unknown, and the interface must not imply it.
- Two entry shapes share one index, so a reader of the index must not assume every entry describes
  something Humbug did.
- The store grows with the size of what changes rather than with the number of changes, so a large
  binary that is rewritten often costs its full size each time. The size guard warns; nothing
  reclaims automatically.
- A purge can reclaim content a baseline still needs, which makes some restores unrevertable; this is
  reported when it happens rather than prevented.
- The prefilter's strength varies by platform, and on Windows a rewrite that restores modification
  time is invisible to it. The content hash still catches it at capture time, and a metadata-only
  comparison does not.
- A deletion inside a pruned directory is invisible, and the interface cannot distinguish that from a
  deletion that never happened. The prune set is therefore a silent boundary: a file changed under a
  generated directory is reported nowhere, and the list must not be read as a complete account of
  what changed on disk.
- The scanner's persistent baseline is correct only because the mindspace is exclusively locked while
  open; anything that weakens that guarantee silently corrupts the baseline.
- Attribution being refused means the panel can say a file changed but never what changed it, which is
  less satisfying than users expect and has to be worded so it does not read as a defect.
