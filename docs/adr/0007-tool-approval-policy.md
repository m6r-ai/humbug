# ADR-0007: Tool approval policy — persistent vs ephemeral state

Date: 2026-09-07  
Status: Accepted

## Context

AI tools in Humbug operate on two fundamentally different kinds of state:

- Persistent state: files on disk, git repositories, terminal processes. Changes survive beyond the tool call and may be difficult or impossible to undo.

- Ephemeral state: in-memory editor buffers, cursor positions, scroll positions, tab layout. Changes exist only in the current session and are discarded when the tab or session closes. They are also immediately visible and reversible in the UI.

The safety principle is that tools which change state require explicit user approval. But "change state" is underspecified: some state changes persist beyond the tool call and need a gate, while others are ephemeral and reversible in the UI with no need for pre-approval.

The editor tool illustrates the distinction. Two operations, `apply_diff` and `transform`, modify the in-memory buffer. A third, `save_file`, writes the buffer to disk. If the buffer modifications required approval, the user would face two approval prompts for a single logical change: one to modify the buffer, and another to persist it. The user cannot meaningfully evaluate a buffer modification without seeing it applied, so the first approval adds friction without adding safety.

## Decision

User approval is required for operations that can or definitely do modify persistent state. Operations that modify ephemeral state run without approval.

The dividing line is: does the change survive beyond the tool call without a separate, approval-gated persistence step?
If yes (the tool writes to disk, sends an HTTP request with side effects, executes a terminal command, etc.), approval is required.
If no (the tool modifies an in-memory buffer, moves a cursor, opens a tab, etc.), no approval is required.

Where an ephemeral change has a separate persistence step (e.g. editor `transform` modifies the buffer, while `save_file` persists it to disk), the approval gate lives at the persistence step, not at the ephemeral modification step.

## Alternatives considered

- Require approval for all state changes, ephemeral or persistent.  This doubles the approval burden for editor workflows (approve to modify the buffer, then approve again to save) without adding safety value, because the user cannot meaningfully evaluate a buffer modification without seeing it applied. It also creates inconsistency: `apply_diff` and `transform` modify the same buffer in the same way, so there is no principled reason to gate one and not the other.

- Require no approval for editor operations at all, including `save_file`.  This would eliminate the approval gate for writing to disk via the editor, which is a persistent state change. It removes the user's ability to course-correct before a file is overwritten.

- Make the distinction per-tool rather than per-state-type.   Each tool could decide its own approval policy. A single principle applied across all tools is more maintainable and less error-prone than leaving the classification to each tool author's judgement.

## Consequences

### Positive

- One clear principle determines whether an operation needs approval: does the change persist beyond the tool call without a separate approval-gated step?

- Editor workflows require only one approval (at `save_file`), not two.

- New operations can be classified by asking a single question, reducing the risk of inconsistency between operations that modify the same kind of state.

### Negative

- The AI can make large changes to an editor buffer without approval. The user sees them in the UI and can undo or refuse to save, but there is no pre-approval gate. This is acceptable because the change is ephemeral and the persistence step (`save_file`) is gated.

- The distinction between ephemeral and persistent state must be applied correctly by tool authors. A misclassification, such as treating a persistent change as ephemeral, would bypass the approval gate. Code review and the existing pattern (editor `apply_diff`/`transform` vs `save_file`) are the safeguards.
