# Working document: multiple instances and multiple mindspaces

Status: Implemented — agreed and built; an ADR is still to be written

This document records a design under discussion. Per `docs/adr/README.md`, an ADR is
written once the decision is settled; this document is the vehicle for settling it.

Decisions 1, 2, 3, 4, 6 and 7 are implemented.  Decision 5 remains open and was not
implemented.

## Problem

Humbug supports multiple mindspaces, but each running instance supports a single
mindspace. Two capabilities are missing:

1. **Opening another mindspace in a new window.** There is no way to launch a second
   instance from a running one, using the same launch configuration as the current
   instance.
2. **Propagating global settings changes between instances.** Global settings live in
   `~/.humbug/user-settings.json` and are shared by all instances, but a change made in
   one instance is invisible to the others until they restart.

## Current state

- `UserManager` (singleton, `src/desktop/user/user_manager.py`) owns global settings in
  `~/.humbug/user-settings.json`. It emits `settings_changed` and reinitialises AI
  backends.
- `MindspaceManager` (singleton, `src/desktop/mindspace/mindspace_manager.py`) owns the
  mindspace model and a home config at `~/.humbug/mindspace.json` holding `lastMindspace`
  and `recentMindspaces`.
- `src/desktop/__main__.py` is the entry point. It reads no launch arguments; it restores
  `lastMindspace` unconditionally.
- `FileWatcher` (`src/desktop/file_watcher/file_watcher.py`) is an existing polling
  singleton. Each `(path, callback)` registration keeps its own independent baseline.

## Decisions taken

### 1. Settings propagation: file watching with a revision counter

The shared settings file remains the single source of truth. Instances detect changes by
watching it, rather than by an IPC channel.

A 5 second latency budget was agreed as acceptable. This makes polling adequate and
removes the need for a cross-platform IPC layer (Unix domain sockets, named pipes, and
local TCP all differ across Linux, macOS, and Windows).

`user-settings.json` gains a monotonically increasing `revision` field. On detecting a
file change, an instance reloads only if the on-disk revision exceeds its own. This
settles the self-write question: the instance that performed the write sees its own
revision already current and takes no action.

Watching reuses the existing `FileWatcher` singleton. Its per-registration baseline is
what makes this work: re-registering after a deliberate write refreshes only that
watcher's baseline.

Poll interval: 1–2 seconds (well inside the 5 second budget).

### 2. Hot-applying all global settings

Every field of `UserSettings` was checked for hot-appliability.

| Field | Hot-appliable | Notes |
|---|---|---|
| `theme`, `custom_colors`, `saved_color_themes`, `active_custom_theme_name` | Yes | `StyleManager` setters already wired via `_on_user_settings_changed` |
| `font_size`, `font_ligatures` | Yes | Same path |
| `language` | Yes | `LanguageManager.set_language` emits `language_changed` |
| `external_file_allowlist`, `external_file_denylist`, `allow_external_file_access` | Yes, already | `_get_filesystem_access_settings()` reads settings fresh on every tool call |
| `ai_backends` | Yes | `update_backend_settings` rebuilds backends |
| `file_sort_order` | Yes | Already handled via sidebar models |
| `check_for_updates` | Startup only | Read once in `_run_startup_update_check`; hot-applying is harmless but inert |
| `onboarding_tour_status`, `onboarding_tour_version` | Requires the fix below | Read-modify-write race |

`check_for_updates` is documented as startup-only rather than presented as live.

### 3. `UserManager` must own mutation

This is a prerequisite, not an optional extra.

Several call sites mutate the settings object in place and then write the whole file:

```python
settings = self._user_manager.settings()
settings.onboarding_tour_status = status
settings.onboarding_tour_version = CURRENT_TOUR_VERSION
self._user_manager.update_settings(settings)
```

(`TourController._finish`, and the same shape in `MainWindow._on_theme_changed` and
elsewhere.)

With one instance this is safe. With several it is a lost-update race: two instances load
the file, each mutates its own copy, and the second write silently reverts the first
instance's change — including unrelated fields changed in between.

**Decision:** `UserManager` stops handing out a mutable `UserSettings` for in-place
editing. Updates are expressed as field-level changes which `UserManager` merges against
the current on-disk state before writing. Call sites such as `TourController._finish` are
updated accordingly.

### 4. Launch context

To launch a new instance with the same configuration, the current instance passes an
explicit launch context rather than relying on ambient process state.

Contents:

- executable
- argv (minus arguments already consumed)
- working directory
- target mindspace path

**No environment is propagated.** API keys are read from `~/.humbug/user-settings.json`,
which the child reads anyway. Propagating the environment would copy secrets into a
child process for no benefit. This must be stated explicitly so a future contributor does
not add `env=os.environ.copy()`.

The mechanism must work for both frozen builds (`.dmg`, `.exe`, `.AppImage`) and
development launches (`python -m desktop`). `sys.executable` is correct in both cases;
the argv differs.

### 5. Environment-variable API keys

`UserManager._initialize_ai_backends` consults `ANTHROPIC_API_KEY` and seven similar
variables, but only for backends not already enabled in saved settings, and only once at
startup.

This path is already vestigial and already buggy: a key sourced from the environment is
applied in memory but never persisted, so it silently disappears the first time anything
else triggers a save. It is also the only reason the launch context would ever need the
environment.

**Proposed:** remove the environment-variable block. This makes `user-settings.json` the
single unambiguous source of truth, removes the latent bug, and removes the last reason
the launch context would need the environment.

This is a user-visible behaviour change and is **not yet agreed**.

### 6. Instance registry and deduplication

Two instances must never share a mindspace. Both would hold `.humbug/` state — session
layout, conversations, and the audit log — and write to it concurrently. The audit log in
particular is intended as a tamper-evident witness; two writers undermine that.

Registry: one file per instance at `~/.humbug/instances/<pid>.json` containing:

- `pid`
- process start time
- mindspace path
- launch timestamp

Per-instance files avoid the write race entirely: each instance only ever writes its own
file. Written on startup and on mindspace change, removed on clean exit, stale entries
pruned by checking both pid liveness and process start time.

Process start time is recorded alongside the pid to defeat pid reuse: after a crash an
unrelated process could inherit the pid, and we would wrongly conclude the mindspace is
still open.

When a spawn is requested:

- No live instance owns the target mindspace → spawn a new instance with the launch context.
- A live instance owns it → show a message identifying the owning process. Do not spawn.

**Every mindspace-switching surface enforces this.**  A mindspace that another instance
has open is shown but disabled in the Recent Mindspaces menu and in the sidebar header
menu, so the user cannot select it.  The check itself lives in `_open_mindspace_path`,
the single gate every switching path goes through, so a stale menu cannot defeat it.

The two menus obtain the disabled state differently, because of when they are built.
The Recent Mindspaces menu is rebuilt on a timer and caches its contents, so the cache
key includes the set of mindspaces open elsewhere as well as the recent list.  The
sidebar header menu is built on demand when clicked, so it reads current state directly.

**Mindspace paths are compared by real path, not as strings.**  A mindspace path reaches
the manager in whatever form the user chose it — with or without a trailing separator,
through a symlink, or with different case on a case-insensitive filesystem.  Comparing
the raw strings treats those as different mindspaces, which has two visible effects: the
currently open mindspace is listed among the recent ones (and so appears as a switch
target rather than being excluded), and the same mindspace appears twice.  The
`_same_mindspace` helper in `MindspaceManager` normalises both sides with `os.path.realpath`
for every such comparison.  The stored path is left in the user's own form so that menus
and tooltips read naturally.

**The stored recent list keeps the currently open mindspace; the menu excludes it.**  The
two are different questions and must not be conflated.  `recentMindspaces` in the home
config records what the user has opened, and is the durable record.  `recent_mindspaces()`
excludes the mindspace open in this window when a menu is built, because switching to it
would be a no-op.  Excluding it at *write* time instead loses it permanently, and the case
where that bites is exactly the multi-window one: a second instance started with
`--mindspace P` has `P` as its current mindspace, and `P` is also the first instance's
`lastMindspace`, so writing the list would drop `P` — making the first instance
unreachable from the menu.  This was the cause of a mindspace disappearing from both
menus after a second window opened it.

**The new-window menu does not filter against the current mindspace itself.**  It relies on
`recent_mindspaces()` for that.  Filtering again would make its result depend on whether a
mindspace is open yet, which is transient during startup: the menu timer starts before the
deferred mindspace restore runs, so an early rebuild would cache a list built with no
mindspace open and then suppress the rebuild that should follow.

### 7. No cross-process window focusing

Focusing another process's window cannot be done from Qt alone; `raise_()` and
`activateWindow()` act only on the calling process's windows. Each platform differs:

- **macOS** — `NSRunningApplication.activateWithOptions_` via PyObjC. Requires a new
  dependency, and Apple increasingly restricts focus-stealing.
- **Windows** — `SetForegroundWindow` via `ctypes`, needing `AllowSetForegroundWindow`
  to work around deliberate focus-stealing restrictions.
- **Linux** — no reliable mechanism exists. There is no standard cross-desktop protocol,
  and Wayland forbids it by design.

**Decision:** detect and notify, do not focus. The user is told which process already has
the mindspace open. This satisfies the deduplication requirement, needs no platform code
and no new dependencies, and behaves identically on all three platforms.

Focusing remains a possible later addition. The registry is a prerequisite for it either
way, so the seam is clean: the registry answers "who owns mindspace X", and a future
`focus_instance(pid)` would sit behind it.

## Open questions

1. Whether to remove the environment-variable API key path (decision 5).

## Implementation notes

### Where the code lives

- `src/desktop/user/user_settings.py` — the `revision` field, its validation on load,
  and its increment on save.
- `src/desktop/user/user_manager.py` — `update_settings_fields`, `reload_if_changed`,
  `start_watching`, and the revision merge performed before every write.
- `src/desktop/user/instance_registry.py` — the registry of running instances.
- `src/desktop/user/launch_context.py` — building and spawning a launch context.
- `src/desktop/main_window.py` — the "Open Mindspace in New Window" action, the
  settings-changed handler, and registry updates on mindspace changes.
- `src/desktop/__main__.py` — the `--mindspace` argument.

### Deviations from the plan

**The poll interval is not specified by the settings watcher.**  `FileWatcher` is a
singleton whose poll interval is fixed at first construction, and other components
construct it with the default.  Passing a different interval from `UserManager` would
have been silently ignored depending on construction order, so the default (1 second)
is used.  It is well inside the agreed 5 second budget.

**Process start time is only available on Linux.**  The pid-reuse check compares the
process start time recorded in the registry against the current one, using
`/proc/<pid>`.  On macOS and Windows there is no portable equivalent without adding a
dependency, so the check degrades to pid existence there.  The failure mode is safe —
a reused pid is reported as a live instance, and the user is told to look for an
existing window rather than being given a second instance on the same mindspace — but
it is a real limitation of the non-Linux paths.

**The settings dialog applies changes through the signal.**  `MainWindow` previously
did not subscribe to `UserManager.settings_changed` at all; the dialog applied its own
changes directly.  The application logic was extracted into `_apply_user_settings`,
which is now reached both from the dialog (via the signal) and from a cross-instance
reload.  This was necessary: without it, a reload would update the settings object but
change nothing on screen.

**Two fields are documented as not fully hot-appliable.**  `check_for_updates` is read
once at startup, so a change to it takes effect on the next launch.  The onboarding
tour fields are read at startup and written once when the tour ends; they are
hot-appliable but have no visible effect until the next launch.

## Consequences

### Positive

- No IPC layer, so no platform-specific socket or pipe code.
- No new dependencies.
- The shared settings file remains the single source of truth.
- The `UserManager` mutation fix removes a class of silent data loss that exists
  independently of multi-instance use.
- Deduplication prevents two instances corrupting a shared mindspace.

### Negative

- Settings propagation is bounded by the poll interval, not instantaneous.
- `check_for_updates` cannot be hot-applied and is documented as startup-only.
- Every call site that currently mutates settings in place must be changed to the
  field-level update API.
- Pid reuse requires start-time comparison; the registry is slightly more complex than a
  bare pid list.
- A user who has a mindspace open elsewhere cannot switch to it from the dialog; they
  must find the window themselves.
