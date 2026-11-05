# Mindspace tree scan — measurement

Reference figures for the file change journal (ADR-0018) and the terminal checkpoint scanner.
They are recorded here so that neither ADR has to carry a performance claim it cannot substantiate.

## Method

A recursive walk collecting metadata only, with no file content read:

```text
{relpath: (st_ino, st_size, st_mtime_ns, st_ctime_ns, st_mode)}
```

Implemented with `os.scandir` and `DirEntry.stat(follow_symlinks=False)`, so no symlink is
followed and no directory reached through one is descended. Directories are descended only when
`S_ISDIR(mode)` is true and `S_ISLNK(mode)` is false. Five iterations, reporting the minimum and
median of the walk. The hash pass reads every selected file in 1 MiB chunks through SHA-256.

The pruned set was `{.git, .humbug, .venv, venv, __pycache__, node_modules, dist, build, htmlcov,
.mypy_cache, .pytest_cache}`. The unpruned pass applied no exclusions at all.

## Environment

| | |
|---|---|
| Mindspace | `~/github/m6r/humbug` |
| Platform | macOS, arm64 |
| Filesystem | APFS |
| Cache | warm |

## Results

| Measurement | Result |
|---|---|
| Pruned entries | 1,427 (1,027 files) |
| Pruned walk | 6.7 ms min, 6.8 ms median |
| Unpruned entries | 21,465 |
| Unpruned walk | 116.2 ms min, 118.4 ms median |
| Pruned total bytes | 168.4 MiB |
| Read plus SHA-256, all 1,027 files | 241.7 ms |

Cost per entry is about 4.8 µs, so a mindspace an order of magnitude larger would scan in
roughly half a second.

## Per-file capture threshold

| Threshold | Files captured | Bytes | Hash time |
|---|---|---|---|
| 256 KiB | 933 (91%) | 13.6 MiB | 26.1 ms |
| 1 MiB | 989 (96%) | 42.8 MiB | 32.2 ms |
| 4 MiB | 1,020 (99%) | 99.7 MiB | 55.7 ms |

38 files exceeded 1 MiB and accounted for 125.5 MiB of the 168.4 MiB total. They were GIF
screenshots, a `.docx`, and saved `.conv` transcripts. 887 files were 64 KiB or smaller.

## Prefilter hit rate

The figures above size a walk. They do not say how much content a scanner would have to read
after one, which is the number that decides whether periodic content capture is affordable. That
depends on how often the metadata tuple actually moves between checkpoints.

Method: sample the tree repeatedly at a 1.5–2 s interval, comparing each walk with the one before,
and count the paths whose tuple `(st_ino, st_size, st_mtime_ns, st_ctime_ns, st_mode)` moved or
appeared. A changed or new path is what a scanner would read; a removed path needs no read. Two
prune sets were walked against the same intervals — `IGNORED_DIRS` alone, and `IGNORED_DIRS` plus
`htmlcov`, `.mypy_cache` and `.pytest_cache`.

| Interval | Prune set | Files | Paths to read | Of which outside `src/` |
|---|---|---|---|---|
| Idle, 11 samples | either | 1,051 / 3,248 | 0 | 0 |
| `pytest tests/src/mindspace`, 20 samples | `IGNORED_DIRS` alone | 3,248 | 235 in 3 samples | 0 |
| `pytest tests/src/mindspace`, 20 samples | extended | 1,051 | 1 in 1 sample | 1 |
| `touch` on 300 source files, 7 samples | either | 1,051 / 3,248 | 300 once, then 0 | 0 |

Two results matter.

**An idle tree flags nothing at all, ever.** Eleven consecutive samples over 16 s produced zero
paths to read under both prune sets. The prefilter is not merely cheap on a quiet tree; content
capture during idle time is free, so a periodic checkpoint can run indefinitely while nothing is
happening.

**Writing files costs one read per distinct file written, not one per interval.** The mass-edit
case read 300 files in the interval that contained the writes and zero in the six intervals after
it. This is what makes an hours-long command affordable: the cost tracks the content the command
changes and never the time it takes, so a command that writes nothing for two hours costs six
thousand cheap walks and no reads.

## The prune set does not matter for scanning

Excluding `htmlcov`, `.mypy_cache` and `.pytest_cache` removed 67% of the tree — 2,197 of 3,248
files — and changed the number of paths a scanner would read by 234, all of them under `htmlcov`
and all appearing in the two intervals that straddled a coverage report being rewritten. After
those intervals the extended and unextended sets flagged identically.

## Conclusions

- A metadata scan is cheap enough to run around every terminal command rather than only at task
  boundaries, so the scanner does not have to weaken the recovery guarantee to stay out of the
  way.
- Pruning is worth about 17× on this tree, and the directories excluded are not all in
  `IGNORED_DIRS`: `htmlcov`, `.mypy_cache` and `.pytest_cache` were present and unpruned. A
  scanner needs a prune set of its own if it wants that saving — which would also change what
  global search and the Quick Switcher see, and so is a separate decision.
- Pruning buys walk time and, in the case of generated output, coverage. Excluding the build
  directories removed 67% of the tree and changed the paths a scanner would read by 234 of 235, and
  all 234 were coverage HTML the observed test run had just rewritten. Those are files no one would
  ever want to revert, so pruning them costs nothing and keeps them out of a change list.
- 1 MiB is the point where the threshold curve turns: it captures 96% of files while keeping a
  complete first capture to about 43 MiB and 32 ms, and leaves 38 metadata-only entries rather
  than a long tail of them.
- Periodic content capture is affordable. An idle tree needs no reads at all, and a tree being
  written needs one read per distinct file written whatever the interval, so the cost of a
  checkpoint series follows the content a command changes and not the duration of the command.

## Limits of these figures

Warm cache only; cold cost is I/O bound and would measure the machine rather than the design.
One mindspace of about a thousand files, on one filesystem. The per-entry cost extrapolates; the
totals do not.

The prefilter measurements above were taken later than the walk figures and on a slightly larger
tree: 1,051 files pruned rather than the 1,027 reported earlier, the difference being files added
to the project in between. The two sets are consistent to about 3%.

The prefilter figures depend on `st_ctime` moving when content changes, which holds on Unix and
does not hold on Windows, where `st_ctime` is creation time. On Windows the prefilter degrades to
size and modification time. None of these figures were taken on Windows, and a tree being watched
by antivirus would move the walk cost by an unmeasured factor.
