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

## Conclusions

- A metadata scan is cheap enough to run around every terminal command rather than only at task
  boundaries, so the scanner does not have to weaken the recovery guarantee to stay out of the
  way.
- Pruning is worth about 17× on this tree, and the directories excluded are not all in
  `IGNORED_DIRS`: `htmlcov`, `.mypy_cache` and `.pytest_cache` were present and unpruned. A
  scanner needs a prune set of its own unless `IGNORED_DIRS` is extended — which would also change
  what global search and the Quick Switcher see, and so is a separate decision.
- 1 MiB is the point where the threshold curve turns: it captures 96% of files while keeping a
  complete first capture to about 43 MiB and 32 ms, and leaves 38 metadata-only entries rather
  than a long tail of them.

## Limits of these figures

Warm cache only; cold cost is I/O bound and would measure the machine rather than the design.
One mindspace of about a thousand files, on one filesystem. The per-entry cost extrapolates; the
totals do not.
