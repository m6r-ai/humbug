# ADR-0014: Multiple instances and multiple mindspaces

Date: 2026-10-09  
Status: Accepted

## Context

Humbug supports multiple mindspaces, but each running instance supports a single
mindspace. Two capabilities were missing.

First, there was no way to open another mindspace in a new window. A user working
across two mindspaces had to launch a second instance by hand, and there was no way
to launch it from a running instance using the same launch configuration.

Second, global settings changes did not propagate between instances. Global settings
live in `~/.humbug/user-settings.json` and are shared by all instances, but a change
made in one instance was invisible to the others until they restarted. A user with two
windows open could change the theme in one and see the other disagree with it.

A third constraint follows from the architecture rather than from either capability.
Two instances must never share a mindspace. Both would hold `.humbug/` state — session
layout, conversations, and the audit log — and write to it concurrently. The audit log
in particular is intended as a tamper-evident witness; two writers undermine that
guarantee, and the blueprint's auditability principle depends on it.

Several forces shaped the design. Humbug is OS-neutral (ADR-0002) and keeps
dependencies minimal (ADR-0004), so any mechanism that differs across Linux, macOS, and
Windows, or that requires a new dependency, carries a high cost. The shared settings
file is the single source of truth for global settings, and that should not change.
Finally, the `UserManager` settings API was not safe for concurrent writers
independently of this work.

## Decision

### Settings propagate by watching the shared file

The shared settings file remains the single source of truth. Instances detect changes
by watching it rather than by an IPC channel. A 5 second latency budget is acceptable,
which makes polling adequate and removes the need for a cross-platform IPC layer.

`user-settings.json` gains a monotonically increasing `revision` field. On detecting a
file change an instance reloads only if the on-disk revision exceeds its own, which
settles the self-write question: the instance that performed the write sees its own
revision already current and takes no action. Watching reuses the existing
`FileWatcher` singleton, whose per-registration baseline is what makes this work.

### All global settings are hot-applied except where documented otherwise

Every field of `UserSettings` was assessed for hot-appliability. Almost all apply
immediately: theme and font settings through `StyleManager`, language through
`LanguageManager`, filesystem access settings (already read fresh on every tool call),
AI backends, and file sort order. `check_for_updates` is read once at startup and is
documented as startup-only rather than presented as live. The onboarding tour fields
are hot-appliable but have no visible effect until the next launch.

Applying settings through the signal required extracting the application logic into
`_apply_user_settings`, reached both from the settings dialog and from a cross-instance
reload. Without this a reload would update the settings object but change nothing on
screen.

### `UserManager` owns mutation

`UserManager` stops handing out a mutable `UserSettings` for in-place editing. Updates
are expressed as field-level changes which `UserManager` merges against the current
on-disk state before writing.

This is a prerequisite, not an optional extra. Several call sites previously mutated
the settings object in place and then wrote the whole file. With one instance this is
safe; with several it is a lost-update race, where two instances each mutate their own
copy and the second write silently reverts the first instance's change, including
unrelated fields changed in between.

### Launch context is passed explicitly

To launch a new instance with the same configuration, the current instance passes an
explicit launch context: executable, argv (minus arguments already consumed), working
directory, and target mindspace path. The child is started with `--mindspace <path>` so
it opens the requested mindspace rather than restoring the last one used.

No environment is propagated. API keys are read from `~/.humbug/user-settings.json`,
which the child reads anyway, so propagating the environment would copy secrets into a
child process for no benefit. This is stated explicitly so that a future contributor
does not add `env=os.environ.copy()`.

The mechanism works for both frozen builds (`.dmg`, `.exe`, `.AppImage`) and
development launches (`python -m desktop`); `sys.executable` is correct in both cases
and the argv differs.

### A registry of running instances enforces one instance per mindspace

A registry holds one file per instance at `~/.humbug/instances/<pid>.json`, containing
pid, process start time, mindspace path, and launch timestamp. Per-instance files avoid
the write race entirely, since each instance only ever writes its own file. Entries are
written on startup and on mindspace change, removed on clean exit, and pruned by
checking both pid liveness and process start time. Process start time is recorded
alongside the pid to defeat pid reuse, where after a crash an unrelated process could
inherit the pid and be mistaken for a live instance.

When a spawn is requested, a new instance is spawned with the launch context only if no
live instance owns the target mindspace.

This is enforced by prevention, not by notification: every mindspace-switching surface
shows a mindspace that another instance has open but disables it, so the user cannot
select it in the first place. A mindspace open elsewhere is never offered as a switch
target. The check itself lives in the single gate every switching path goes through, so
a stale menu cannot defeat it. The two menus obtain the disabled state differently
because of when they are built: the Recent Mindspaces menu is rebuilt on a timer and
caches its contents, so its cache key includes the set of mindspaces open elsewhere; the
sidebar header menu is built on demand and reads current state directly.

Mindspace paths are compared by real path, not as strings. A path reaches the manager in
whatever form the user chose it — with or without a trailing separator, through a
symlink, or with different case on a case-insensitive filesystem. Comparing raw strings
treats those as different mindspaces, which both lists the currently open mindspace
among the recent ones and shows the same mindspace twice. A helper normalises both sides
with `os.path.realpath` for every such comparison; the stored path is left in the user's
own form so menus and tooltips read naturally.

The stored recent list keeps the currently open mindspace; the menu excludes it. These
are different questions. The home config records what the user has opened and is the
durable record; `recent_mindspaces()` excludes the mindspace open in this window when a
menu is built, because switching to it would be a no-op. Excluding it at write time
instead loses it permanently, and the case where that bites is exactly the multi-window
one: a second instance started with `--mindspace P` has `P` as its current mindspace,
and `P` is also the first instance's `lastMindspace`, so writing the list would drop
`P` and make the first instance unreachable from the menu.

The new-window menu does not filter against the current mindspace itself; it relies on
`recent_mindspaces()` for that. Filtering again would make its result depend on whether
a mindspace is open yet, which is transient during startup: the menu timer starts before
the deferred mindspace restore runs, so an early rebuild would cache a list built with
no mindspace open and then suppress the rebuild that should follow.

### Do not focus an existing instance's window

Focusing another process's window cannot be done from Qt alone; `raise()` and
`activateWindow()` act only on the calling process's windows. Each platform differs:
macOS needs `NSRunningApplication.activateWithOptions_` via PyObjC (a new dependency,
and Apple increasingly restricts focus-stealing); Windows needs `SetForegroundWindow`
via `ctypes` with `AllowSetForegroundWindow` to work around deliberate focus-stealing
restrictions; Linux has no reliable mechanism, as there is no standard cross-desktop
protocol and Wayland forbids it by design.

The decision is therefore not to focus at all. Deduplication does not depend on it: a
mindspace open in another instance is disabled in the menus and cannot be selected, so
there is nothing to focus. This needs no platform code and no new dependencies, and
behaves identically on all three platforms. Focusing remains a possible later addition;
the registry is a prerequisite for it either way, so the seam is clean: the registry
answers who owns mindspace X, and a future `focus_instance(pid)` would sit behind it.

### The environment-variable API key path is out of scope

`UserManager._initialize_ai_backends` consults `ANTHROPIC_API_KEY` and seven similar
variables, but only for backends not already enabled in saved settings, and only once
at startup. This path is already vestigial and already buggy: a key sourced from the
environment is applied in memory but never persisted, so it silently disappears the
first time anything else triggers a save.

Removing it would make `user-settings.json` the single unambiguous source of truth,
remove the latent bug, and remove the last reason the launch context would ever need the
environment. It is a user-visible behaviour change and was not agreed, so it is not part
of this decision. The environment is not propagated regardless, so the two are
independent.

## Alternatives considered

- **An IPC channel for settings propagation.** Unix domain sockets, named pipes, and
  local TCP all differ across Linux, macOS, and Windows, so an IPC layer would be
  platform-specific code in a project that is deliberately OS-neutral (ADR-0002). The
  agreed 5 second latency budget makes polling adequate, and file watching reuses an
  existing singleton rather than adding a subsystem.

- **Propagating the environment to the child process.** This would copy API keys and any
  other secrets in the parent's environment into every spawned instance. The child reads
  the same settings file the parent does, so there is nothing to gain.

- **Cross-process window focusing.** Rejected as described above: it needs a new
  dependency on macOS, `ctypes` and a focus-stealing workaround on Windows, and has no
  reliable mechanism at all on Linux. A platform-specific feature that works on two of
  three platforms would violate the OS-neutrality principle.

- **A single registry file listing all instances.** A shared file would reintroduce the
  write race this design exists to avoid, and would need locking across three platforms.
  One file per instance means each instance only ever writes its own file.

- **Excluding the current mindspace from the stored recent list at write time.** Simpler
  to read, but it permanently loses the mindspace in the multi-window case, as described
  above. The exclusion belongs at menu-build time, where it is a presentation concern.

- **Comparing mindspace paths as raw strings.** Simpler, but produces two visible bugs
  (the open mindspace listed as a switch target, and the same mindspace listed twice)
  for any user who reaches the same directory by a different route.

## Consequences

### Positive

- No IPC layer, so no platform-specific socket or pipe code.
- No new dependencies.
- The shared settings file remains the single source of truth.
- The `UserManager` mutation fix removes a class of silent data loss that exists
  independently of multi-instance use.
- Deduplication prevents two instances corrupting a shared mindspace, which protects the
  tamper-evident audit log.
- Not focusing an existing instance's window behaves identically on all three platforms,
  and the registry is the clean seam if focusing is added later.

### Negative

- Settings propagation is bounded by the poll interval, not instantaneous.
- `check_for_updates` cannot be hot-applied and is documented as startup-only.
- Every call site that previously mutated settings in place had to change to the
  field-level update API.
- Pid reuse requires start-time comparison, and the start time is only available
  portably on Linux via `/proc/<pid>`. On macOS and Windows the check degrades to pid
  existence. The failure mode is safe — a reused pid is reported as a live instance and
  the user is told to look for an existing window, rather than being given a second
  instance on the same mindspace — but it is a real limitation of the non-Linux paths.
- A user who has a mindspace open elsewhere cannot switch to it from the dialog; they
  must find the window themselves.
