# ADR-0012: Message edit and delete — deferred truncation and a single editing surface

Date: 2026-09-27  
Status: Accepted

## Context

The conversation UI historically offered two actions on a user message: an **edit** button and a **delete** button. Both truncated the conversation from that message onward, but they differed in two ways that made them confusing:

- **Where the revised prompt was authored.** Edit authored it inline in the message. Delete loaded the original text into the input box at the bottom of the conversation.
- **Whether resubmission was automatic.** Edit truncated and immediately resubmitted the edited text, invoking the AI without a further explicit user action. Delete truncated and left the text in the input box for the user to submit (or abandon) manually.

Neither button's name described what it did. "Delete" restored text to the input box, which is an editing behaviour. "Edit" re-ran the AI, which is not implied by the word "edit". The two actions were therefore near-duplicates with different, surprising side effects.

The inline editor was also a strict subset of the input box. It was a bare `MarkdownTextEdit` with syntax highlighting and Ctrl+Enter, whereas the input box is a full `ConversationMessage` subclass that additionally supports attachments, model display and settings, submit/stop button state, streaming awareness, the `modified` signal, height capping, and view-state persistence. Editing inline therefore offered a degraded experience and did not participate in the tab's state model.

Finally, the inline editing path had stopped working, and no tests covered it.

## Decision

There are two distinct actions on a user message, with clearly separated intent:

**Delete** removes this message and every subsequent message. The user confirms via a modal dialog before anything is removed. No text is restored to the input box.

**Edit from here** loads the message text into the input box, spotlighted and focused, and marks the message and every subsequent message as pending removal. Nothing is removed at this point. The pending messages are heavily greyed out but remain fully interactive (selectable, copyable, links clickable). The truncation happens only when the user submits. If the user cancels, clears the input box, or navigates away, the pending state is discarded and the conversation is unchanged.

The message being edited is distinguished by an amber border, separate from the spotlight colour used for focus and search navigation. This makes the edit target visually obvious without relying on the user interpreting the spotlight, which has other meanings elsewhere in the UI. Esc cancels a pending edit, taking precedence over the streaming-cancellation behaviour of Esc.

The inline editing surface is removed entirely. All editing happens in the input box, which is the single, fully capable editing surface.

A pending edit is not persisted across session restore: on restore the input box is empty and there is no pending edit target. Because nothing is removed until submit, no data is at risk from this choice.

## Alternatives considered

- **Keep both actions as they are, and repair the inline editor.** This retains two editing surfaces, one of which is permanently less capable than the other, and retains the surprising behaviour of re-invoking the AI on edit-confirm. It also means maintaining two code paths for the same logical operation.

- **Single action only, replacing both buttons.** This loses a genuine "throw it away" gesture. Without a delete action, the only way to discard a message would be to enter edit mode, clear the input box, and submit nothing, which is a poor way to express deletion.

- **Edit truncates immediately, with a modal confirmation.** This makes the destructive step happen before the user has committed to anything, so an accidental edit followed by abandonment still loses messages. Deferring truncation to submit time makes the operation recoverable and removes the need for a modal, because the greyed-out messages are themselves the warning.

- **Keep inline editing but make it a fully featured composer.** This would restore parity between the two surfaces but is a much larger effort and still leaves two editing surfaces to maintain and keep consistent.

## Consequences

### Positive

- Each action does exactly one thing, and its name describes that thing.

- Risk profiles are matched to the UI: delete is immediate and irreversible and is gated by a modal; edit is deferred and recoverable and needs no modal.

- The destructive step happens only on an action (submit) whose intent is unambiguous, so accidental edits cannot lose history.

- Cancellation is trivial and infallible: because the transcript is untouched until submit, cancelling is simply discarding presentation state rather than reconstructing deleted messages.

- Editing gains the full capability of the input box, including attachments, model display, button state, and view-state persistence.

- The inline editing code path is removed rather than maintained, in line with the project's YAGNI principle.

### Negative

- Editing is no longer spatially local to the message. The user edits at the bottom of the conversation rather than in place. This is accepted in exchange for a single, fully capable editing surface.

- The widget must track pending-edit state (the target message and the original text) and clear it on the various abandon paths. This is presentation state only, so an error in managing it can produce a stale highlight but cannot lose data.
