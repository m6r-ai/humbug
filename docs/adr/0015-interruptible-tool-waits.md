# ADR-0015: Interruptible tool waits

Date: 2026-10-20
Status: Accepted

## Context

Some AI tools wait.  The clock tool's `sleep` and `alarm` operations block for a
duration chosen by the AI, which may be minutes.  The AI chooses that duration to
anticipate how long an external action will take, but the action may finish early or
fail.  With a plain `asyncio.sleep`, the user has no way to cut the wait short: the
AI is idle until the full duration elapses, and the only recourse is to cancel the
whole conversation.

Tool approval already solves the inverse problem — pausing the AI until the user
decides.  It does so with an `asyncio.Future` that the UI resolves, and an inline
control shown inside the tool-call message.  An interruptible wait needs the same
shape: the tool proceeds on its own until either the wait completes or the user
intervenes.

## Decision

A tool that waits for more than a threshold (currently five seconds) delegates the
wait to the requester through `AIConversation.interruptible_sleep`.  The requester:

- runs the wait and a user-interrupt future concurrently, returning as soon as either
  completes;
- emits `TOOL_INTERRUPTIBLE_WAIT_STARTED` and `TOOL_INTERRUPTIBLE_WAIT_ENDED` so the
  frontend can show and remove a cancel control inside the executing tool-call message;
- exposes `interrupt_pending_wait`, which resolves the future when the user cancels.

Waits at or below the threshold use a plain sleep, so a short wait does not flash a
control on screen.

An interrupted wait is a normal, successful tool result.  The tool returns the current
time and states that the user interrupted the wait.  It is not an error: the AI
requested a wait, the user shortened it, and the AI can continue from the current
time.  The tool detects the requester's interruptible wait by duck typing, so the
clock tool keeps its dependency on `ai_tool` alone and remains usable with requesters
that do not offer the capability.

## Alternatives considered

- **Cancel the whole conversation.**  This already exists, but it aborts the AI's
  entire turn and discards in-progress work.  Interrupting a wait should let the AI
  continue with the result it was waiting for.

- **A modal dialog.**  A modal message box blocks the UI and prevents the user from
  doing anything else while the AI waits, which is worse than the wait itself.  The
  control is inline and non-modal, like the approval UI.

- **Surface interruption as an error result.**  An interrupted wait is an expected
  outcome that the user asked for, not a failure.  Reporting it as an error would
  invite the AI to retry the wait or apologise, which is not what the user wants.

- **Show the control for every wait.**  A sub-second wait would flash a control the
  user cannot meaningfully act on.  The threshold keeps the UI quiet for short waits.

## Consequences

### Positive

- The user can cut short a wait the AI over-estimated, without abandoning the turn.
- The mechanism reuses the established approval pattern: a future, two events, and an
  inline control in the tool-call message.
- Interruption is a first-class, successful result, so the AI continues naturally.

### Negative

- The tool framework now has a second user-interaction mechanism alongside approval,
  and tools must know to delegate their waits to the requester to benefit from it.
- The threshold is a heuristic.  A wait just under five seconds cannot be interrupted,
  and a wait just over it shows a control that may be dismissed almost immediately.
