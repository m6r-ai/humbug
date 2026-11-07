# Code Review: Mission Control / Autonomous Mode

**Branch:** `dev-autonomous-mode`
**Scope:** 21 modified + 19 added files (`src/mission/`, `src/desktop/mission_tab/`, `src/ai/`, docs, tests)
**Reviewer:** Mission lead (automated review)
**Date:** 2026-11-06

> **Status update (2026-11-06):** items 1, 2, 3, 4 and 8 below have been fixed. The
> findings are kept as written so the review remains a record of what was found. See
> "Fixes applied" at the end of this document for what changed and how it was verified.

---

## Verdict

This is a strong, well-structured piece of work. The separation between the front-end-agnostic
`src/mission/` service and the Qt `src/desktop/mission_tab/` projection follows the architecture the
blueprint already prescribes for the context registry, and the service genuinely holds no UI state.
The safety posture of the AI reviewer is the best part of the design: it fails closed on every path,
re-checks the auto-approve level *after* the review returns, and re-validates that the decision is
still the one the conversation is waiting on before acting. Test coverage is unusually good for a
feature this size — 75 tests pass, including the loop guard, interruption/resume, mode switching and
every auto-approve level.

Static analysis is clean: `tools.code_checker` reports 0 dependency violations, mypy passes on 482
files, pylint scores 10.00/10.

The blocking issues are not in the code — they are in the documentation trail and one ADR
numbering collision. Details below.

---

## Blocking

### 1. Two ADRs share the number 0019

`docs/adr/0019-autonomous-mode.md` and `docs/adr/0019-conversation-identity.md` both exist and both
claim "ADR-0019". This happened because the branch merged `dev` after `dev` had already taken 0019
for conversation identity.

Worse, `docs/adr/README.md` line 91 lists **only** `0019-conversation-identity.md`. The autonomous
mode ADR is therefore invisible from the index that `AGENTS.md` calls "the definitive index".

**Recommendation:** renumber the autonomous-mode ADR to `0021` (0020 is taken by
`terminal-command-effects.md`), rename the file, update its `# ADR-0019:` heading, add it to
`docs/adr/README.md`, and update the reference in `docs/user-manual/README.md` line 42 which
currently points at "ADR-0019 (autonomous mode)".

### 2. `docs/adr/0019-autonomous-mode.md` is not referenced from the code it governs

`src/mission/mission_service.py` contains no reference to the ADR. Every other significant
subsystem in this codebase cites its ADR from the module docstring or the relevant method
(compare `src/mindspace/`, which cites ADR-0017/0018/0020). Given that autonomous mode is the most
consequential behavioural change in the release — an AI that re-prompts itself — the decision record
should be discoverable from the code.

**Recommendation:** add the ADR reference to the `MissionService` class docstring.

---

## Should fix

### 3. `asyncio.get_event_loop()` is deprecated and will warn on 3.14

Three call sites: `mission_service.py:948`, `:1214`, `:1610`.

On Python 3.14 (which this repo runs — the test run reports `python 3.14.6`) `get_event_loop()`
emits a `DeprecationWarning` when there is no running loop, and the semantics of "get or create" are
being removed. The service is always driven from within a running loop, so
`asyncio.get_running_loop()` is both correct and future-proof.

**Recommendation:** replace all three with `asyncio.get_running_loop()`.

### 4. `_run_task` swallows exceptions from the tasks it starts

```python
def _run_task(self, coroutine: Any) -> None:
    task = asyncio.get_event_loop().create_task(coroutine)
    self._continue_tasks.add(task)
    task.add_done_callback(self._continue_tasks.discard)
```

`add_done_callback(self._continue_tasks.discard)` keeps the reference alive, which is right, but
nothing ever retrieves the task's exception. If `_continue_mission` or `stop_task`'s
`submit_message` raises, asyncio logs "Task exception was never retrieved" only at garbage
collection, with no mission context — and the mission is left in whatever state it was in, silently.

**Recommendation:** add a done-callback that inspects `task.exception()` and logs it with the
mission id, and records a `MissionActivityKind.ERROR` entry so the user sees it on the dashboard.

### 5. `stop_task` fails silently when the delegate cannot be identified

`stop_task` (`mission_service.py:1584`) matches the delegated conversation by comparing
`os.path.splitext(os.path.basename(task.agent.strip()))[0]` against the transcript's basename. If the
lead reports `agent` as an absolute path, a relative path with a different separator, or a bare
session id without the `.conv` extension, the match silently fails and the delegate is **not**
stopped — while the user is told the task was stopped.

The manual already acknowledges this as a limitation ("Stopping a task relies on the delegate being
identifiable"), so it is documented, but the silent failure is worth surfacing in the UI rather than
being indistinguishable from success.

**Recommendation:** when `task.agent` is set but no tracked conversation matches, record a
`TASK_STOPPED` activity entry with a detail noting the delegate could not be found, so the user knows
to stop it themselves.

### 6. `_mission_for_child` is O(n·m) on every sync

`mission_service.py:1443-1449` walks every tracked conversation and, for each, scans its entire
message history looking for the parent message id. `_sync()` runs on every context open, close and
title change, so with a long conversation and several delegates this becomes quadratic on every tab
activity.

**Recommendation:** build a `{message_id: mission_id}` index once per sync pass, or cache it keyed
on the parent message id.

### 7. `decisions()` is called inside `_can_continue` and `_status_for`, and rebuilds the list each time

`_can_continue` calls `self.decisions()`, which iterates `self._decisions`, calls
`pending_tool_authorization()` on each conversation, and sorts. `_status_for` calls it again, and
`snapshot()` calls it a third time. All three are on hot paths (every turn finish, every notify).

**Recommendation:** compute the live decision list once per notify cycle and pass it down, or accept
a `list[MissionDecision]` parameter in `_can_continue`/`_status_for`.

### 8. The reviewer's prompt truncation is applied inconsistently

`mission_reviewer.py:68` truncates `request.detail` to `_DETAIL_LIMIT` (6000) and appends
`[detail shortened]`. But `request.reason` and `request.objective` are not truncated inside
`_build_prompt` — the caller truncates `lead_update` to `_REVIEW_TEXT_LIMIT` (800) and each
instruction to 800, but `objective` and `reason` are passed through unbounded.

A mission objective is user-authored so its length is bounded in practice, but `reason` comes from
the tool call and is not. A tool that emits a very long reason could push the actual request detail
out of the model's attention, or exceed the context window and cause the review to fail (which fails
closed — acceptable, but wasteful).

**Recommendation:** truncate `objective` and `reason` in `_build_prompt` alongside `detail`.

### 9. `MissionConversationAuditor` does not record tool results

`mission_audit.py` records reasoning, AI responses, tool-call requests and user prompts. It does not
record tool *results*. A tab-based conversation records these (they arrive as `USER` messages with
`tool_results`, which the auditor explicitly skips at line 121).

The effect is that a mission lead's interaction log shows what it asked to do but not what came back
— so the audit trail cannot answer "did the file write succeed?" for a headless mission, while it can
for a tabbed one. Given the blueprint's emphasis on the log as "an independent witness", this
asymmetry is worth closing.

**Recommendation:** record tool results for headless conversations in the same form a tab uses, or
document the difference deliberately in the ADR.

---

## Nit

- `mission_service.py:41-43` — `_REJECT_REASON`, `_UNSURE_REASON`, `_UNSURE_MESSAGE` are module
  constants duplicating strings that also live in the conversation-tab code. If those strings ever
  diverge the two approval paths will report differently. Consider importing them from one place.
- `mission_service.py:606` — `mission.title = title[:_MAX_TITLE_LENGTH * 2]` allows 144 characters
  while `_initial_title` truncates to 72. The asymmetry is unexplained; a user-renamed mission can be
  twice as long as an auto-titled one, which may overflow the chip in the UI.
- `mission_service.py:1324` — `title = mission.title if mission.title_locked else (plan.title or mission.title)`
  is correct but the parenthesised `or` reads awkwardly; `plan.title or mission.title` alone would do.
- `mission.py:60` — `MissionTaskStatus.from_text` maps `"failed"` to `BLOCKED`. Semantically a failed
  task is not blocked on the user, and mapping it means a mission with a failed step will never be
  auto-continued (`_can_continue` returns False on any `BLOCKED`), which is probably not the intent.
  Consider a distinct `FAILED` status or mapping to `PENDING`.
- `mission_plan_parser.py:88` — tasks missing an id are numbered by *position*, but the loop uses
  `enumerate(raw_tasks, start=1)` on the unfiltered list. A plan whose first task has no title and
  whose second has no id gets id `"2"` while being the only task. Harmless, but ids become
  non-contiguous and `stop_task`'s `stopped_tasks` matching then works only by luck.
- `mission_store.py:63` — `load_all` sorts by `created_at`, a string. Records written with a
  non-ISO `createdAt` (or an empty one) will sort to the front. Low risk since only
  `create_mission` writes it.
- `mission_widget.py` — the file is 81KB. It is the largest single file in the feature and holds
  painting, layout and interaction. Consider splitting the painted sub-widgets into their own modules
  the way `mission_painted.py` already does for some of them.
- `mission_service_holder.py` — the singleton uses `hasattr(self, "_initialized")` rather than a
  class-level flag or `__init__` guard. It works, but it is a pattern not used elsewhere in the
  codebase; confirm it is intentional.

---

## What is done well

- **Fail-closed reviewer.** Every failure path in `MissionApprovalReviewer.review` returns
  `ReviewVerdict(False, ...)`. `parse_verdict` requires an exact lowercase `"approve"`. The level is
  re-checked after the verdict returns (`mission_service.py:1124`), so a level lowered mid-review
  cannot be bypassed.
- **Stale-decision protection.** `_take_decision` re-validates the tool-call id against what the
  conversation is actually waiting on, so a decision can never answer a different request. This is
  the subtle bug most implementations get wrong.
- **Loop guard.** The three-continues-without-progress rule is implemented with two counters
  (`_continues_without_progress`, `_completed_at_last_continue`) and correctly resets on user
  guidance and on a completed step. `AUTO_PAUSED` is recorded exactly once.
- **Interruption semantics.** `active` is cleared on a *successful* turn finish but deliberately left
  set on a failed turn, so a crash mid-turn is resumable. The reasoning is documented in a comment.
- **Mode exclusivity.** `_take_over_by_tab` stops the headless conversation before the tab adopts
  the transcript, with an explicit comment about never having two copies write the same file.
- **Prompt-injection framing.** The reviewer prompt delimits untrusted content with BEGIN/END markers
  and instructs the model to treat instructions inside as data — and to treat any instruction aimed
  at it as grounds to defer to the human.
- **Docs.** The manual chapter is honest about limitations in the way `AGENTS.md` asks for, and is
  correctly linked from `index.md` and mapped in `README.md`.

---

## Test results

```text
tests/src/mission/ + tests/src/desktop/mission_tab/
75 passed in 11.75s

tools.code_checker
  dependency checker  PASS
  mypy                PASS (482 files)
  pylint              PASS (10.00/10)
```

### Missing tests worth adding

1. `MissionStore._write_json` failure path — simulate an `OSError` and assert `MissionError` is raised
   and no `.mission-*.tmp` file is left behind.
2. `Mission.from_dict` with a corrupt record (missing `id`, `tasks` not a list, `stoppedTasks`
   containing a dict) — `load_all` should skip it and still return the good records.
3. `parse_mission_plan` with duplicate task ids — currently the later task silently wins in
   `stopped_tasks` matching.
4. `_apply_plan` when the lead reports a *shorter* plan — tasks are replaced wholesale, so a task the
   user had in progress disappears from the dashboard. Confirm this is intended.
5. The `_run_task` exception path (see Should-fix 4) — assert the failure is logged and surfaced.
6. `switch_to_autonomous` when the lead transcript file has been deleted — currently
   `_load_headless` returns `None` and the mission is left in `AUTONOMOUS` with no conversation.

---

## Suggested order of work

1. Fix the ADR numbering collision and add the ADR to the index (blocking, ~10 minutes).
2. `get_running_loop()` and `_run_task` error handling (correctness, low risk).
3. `_mission_for_child` index and the repeated `decisions()` calls (performance, low risk).
4. Reviewer prompt truncation and tool-result auditing (safety completeness).
5. Nits and additional tests as time allows.

---

## Fixes applied

The blocking items and the two lowest-risk code corrections were made in the same change as this
review.

### Blocking 1 — ADR renumbered to 0021

- `docs/adr/0019-autonomous-mode.md` moved to `docs/adr/0021-autonomous-mode.md`.
- Its heading changed from `# ADR-0019:` to `# ADR-0021:`.
- Added to `docs/adr/README.md` as row 0021 with status
  `Proposed — Pending Maintainer Review`.
- `docs/user-manual/README.md` line 42 now points at ADR-0021. Line 39 still correctly points at
  ADR-0019 (conversation identity).
- `docs/Humbug_Autonomous_Mode_Architecture_Assessment.md` line 4 now links to
  `adr/0021-autonomous-mode.md`.

`0019-conversation-identity.md` is untouched and remains the only 0019.

### Blocking 2 — ADR cited from the code

`MissionService`'s class docstring in `src/mission/mission_service.py` now ends with `See ADR-0021.`

### Should-fix 3 — `get_running_loop()`

All three `asyncio.get_event_loop()` calls replaced with `asyncio.get_running_loop()`
(`_consider_continuing`, `_schedule_sync`, `_run_task`). The service is only ever driven from inside a
running loop, so the weaker "get or create" semantics were never needed.

### Should-fix 4 — background task failures are no longer silent

`_run_task` now takes an optional `mission_id` and hands the finished task to a new
`_task_finished` callback, which:

- releases the strong reference (as before);
- returns quietly if the task was cancelled;
- otherwise logs the exception with `exc_info`, and — when the work belongs to a mission — appends a
  `MissionActivityKind.ERROR` entry to the activity feed and notifies listeners, so the failure is
  visible on the dashboard rather than only at garbage collection.

All four call sites now pass their mission id.

### Should-fix 8 — reviewer prompt bounds

`_build_prompt` now truncates `objective` (2000 chars) and `reason` (1000 chars) as it already did
`detail` (6000), each with a `[shortened]` marker so the reviewer can tell the text was cut. This
keeps a tool that emits a very long reason from crowding the request detail out of the reviewer's
attention or overrunning its context window.

### Verification

```text
tests/src/mission/ + tests/src/desktop/mission_tab/
79 passed in 7.13s          (was 75; +4 from the reviewer prompt changes)

tools.code_checker
  dependency checker  PASS
  mypy                PASS (483 files)
  pylint              PASS (10.00/10)
```

A search for `0019-autonomous-mode` across `docs/` now finds only this review document, which
records the original finding.

### Second round — should-fix 5, 6 and 7

Implemented by a delegated child agent and independently verified by a second child agent, which
returned **PASS** after reading the diff and re-running both checks itself.

**5 — `stop_task` no longer fails silently.** A `stopped_delegate` flag records whether a matching
open delegate was found. When `task.agent` is set but no tracked conversation matches, the
`TASK_STOPPED` activity detail now reads
`"<task_id> — its delegate could not be found, so it may still be running"`. When `agent` is empty
or matches, the detail stays the bare `task_id` as before. The task is always stopped; only the
delegate cancellation is best-effort. Covered by a new test,
`TestStoppingTasks::test_stopping_a_task_whose_delegate_is_not_open_says_so`, which would fail
against the old code.

**6 — `_mission_for_child` uses a per-sync index.** `_sync()` builds a `{message_id: mission_id}`
map once per pass and passes it in, so the method is a dict lookup instead of a rescan of every
tracked conversation's history per child. The index is a local, rebuilt on every sync — deliberately
not cached on `self`, since a stale index would misattribute conversations to the wrong mission.

**7 — `decisions()` computed once per operation.** `snapshot()` computes the list once and passes
it to `_status_for` and `_can_continue`, both of which accept an optional precomputed
`list[MissionDecision]` and fall back to computing it. Existing no-argument callers are unaffected
and the public `decisions()` signature is unchanged. The verifier confirmed there is no stale-list
risk: `snapshot()` is synchronous and the decision-consuming paths re-read live state.

**Also fixed from the verifier's report:**

- The new test called `asyncio.run(run())` twice, running the whole scenario twice. Reduced to one
  call.
- `_delegator_of` reintroduced the same quadratic scan fix 6 removed, and runs once per agent inside
  `snapshot()`. Replaced with a `_delegator_index()` built once per snapshot, matching the approach
  used for `_mission_for_child`. Its now-unused `mission_id` parameter was dropped.

### Verification (second round)

```text
tests/src/mission/ + tests/src/desktop/mission_tab/
93 passed in 6.38s          (was 79)

tools.code_checker
  dependency checker  PASS
  mypy                PASS
  pylint              PASS (10.00/10)
```

### Still outstanding

Should-fix 9 (headless conversations do not audit tool results), the eight nits, and the six
suggested tests are unchanged and still worth doing.

The verifier also noted that forks sharing a message id make the delegator index's choice
arbitrary (last writer wins, where the old code took the first match). Both are arbitrary in that
ambiguous case and no test covers it; judged immaterial.
