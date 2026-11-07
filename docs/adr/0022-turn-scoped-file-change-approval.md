# ADR-0022: A turn-scoped standing approval for journalled file changes

Date: 2026-11-07  
Status: Accepted

## Context

ADR-0007 gates every persistent change behind its own approval. ADR-0018 then made the
mutation reversible but deliberately left the gate alone: "This ADR adds recovery and relaxes
no gate. Reducing friction is a separate decision that this journal makes defensible rather
than speculative."

The cost of that gate scales with the length of a task. An agentic task that writes forty files
asks forty times, and each prompt blocks the conversation until the human answers. ADR-0018
named the risk that comes with removing the prompts — an approval that is not looked at is not
an approval — and named the precondition for reducing them: the journal's coverage defines
exactly which operations could be relaxed, and which must keep prompting.

There is a second, sharper question inside the first. "Write inside the mindspace" is not a
sufficient rule: the mindspace contains `.humbug/`, which the journal never covers, and an
editor tab may hold a path outside the mindspace entirely. A rule stated in terms of tool names
would be wrong for `editor.save`, right for `filesystem.write_file`, and silent about the next
write operation someone adds.

## Decision

The user may grant a standing approval for the rest of the current turn, and the approval
resolves exactly those requests whose whole effect is a change the file change journal records.

**The classification is a property of the change, not of the tool.** A tool states what it is
about to do — a change inside the mindspace that the journal will record, or anything else —
and the approval policy decides whether that class of change is currently approved. This
extends ADR-0007's dividing line rather than replacing it: the axis was persistent versus
ephemeral, and is now also journalled versus not, which is the axis ADR-0018 created.

**The tool states it; the policy decides it.** A tool cannot grant itself an approval. It
classifies a request at the point where it has resolved the paths it is about to mutate, using
the journal's own scope test rather than its own opinion of its operations, and the policy in
the conversation is what honours or ignores that classification. A tool that over-reaches can
only cause a request the human must still answer; it cannot cause a change no one sees.

**Scope follows the journal, not a list.** A change is covered when `Mindspace.journalled_path()`
reports it: inside the mindspace and outside every `.humbug/` directory. A future write
operation joins the feature by being journalled, not by being added to a table, so the table
cannot drift out of step with the store. `.git` is inside the mindspace and is journalled even
when a mindspace's scan filters hide it, so a change there is covered — and reversing a file is
still not reversing an effect, which the documentation says rather than implies.

**Operations the journal will not record are not covered**, even though they are writes and even
though they are inside the mindspace. One remains: moving a directory that holds files, whose
contents are not captured and so cannot be reversed. It asks, and its prompt says the contents
will be lost. Everything else that writes inside the mindspace is covered, because it records
itself — a download, a git result written to a file, a created directory and the image folder a
document conversion writes beside its output all record before they mutate. See ADR-0023.

**The approval is turn-scoped and never remembered.** It lapses when the user sends a message,
when the turn completes, and when the turn is cancelled, and a conversation never starts with
one in force. It belongs to the conversation that granted it, so a delegated child asking on its
own behalf is not covered by its parent's approval.

**Granting it is one-way within the turn.** The switch lives on the approval card the user is
looking at, and it is shown only on requests it could actually resolve. Arming it approves later
requests and never the one on screen, so a mis-click cannot approve something unread; and the
only way to withdraw it is to send a message, which ends the turn the approval belongs to.
There is one deliberate consequence: for the rest of that turn, a covered change happens without
the user seeing it first. What replaces the prompt is the record — every write is logged as
before, the arming and its lapse are logged as human actions, and each request resolved without
asking is announced so the log says a standing approval did it.

**Transparency is not traded for convenience.** A standing approval is a decision the user made
once, deliberately, with the effect stated on the switch: file changes, from now until the turn
ends. That is the difference between this and automation that hides itself.

## Alternatives considered

- **Auto-approve writes that do not overwrite.** Rejected in ADR-0018 and still wrong: it
  addresses a minority of prompts while removing the gate on creation, which is the usual route
  by which hooks and workflow files are planted.

- **A per-mindspace setting, always on.** Would eliminate the prompts entirely and is what a
  confident user eventually wants, but it makes the state invisible: nothing on screen would
  distinguish a mindspace where the AI writes freely from one where it does not. A turn-scoped
  approval is recoverable by typing, which is why the withdrawal path can be omitted.

- **A list of auto-approvable tool operations.** Simpler to read, and reviewable in one place, but
  it is a second place where "a change the journal records" is encoded, and it would silently
  cover an operation added later or miss one whose paths were narrowed. `editor.save` alone
  disproves the per-operation framing: the same operation is covered or not depending on which
  file the tab holds.

- **Let the tool decide whether an approval applies.** Fewer moving parts, but then the policy
  lives in nine tools rather than one, and a defect in any of them grants an approval the user did
  not give. The dialogue the tool cannot reach — whether the human is in a hurry, and how long the
  approval lasts — is the policy's business.

- **Keep the switch visible for the rest of the turn so it can be withdrawn.** More symmetrical,
  and rejected only because the turn boundary already provides the withdrawal: the switch would
  sit in an approval card that is destroyed as soon as it is used, and a second control elsewhere
  would duplicate a decision that sending a message already makes.

- **Remember the last position of the switch.** Convenient, and rejected: a safety control that
  resumes armed is one the user has to remember to disarm, and "it was on last time" is how a
  standing approval becomes a standing assumption.

## Consequences

### Positive

- A long task that changes many files costs one decision instead of forty, and the user can still
  stop it by typing — the act they were going to perform anyway.
- What the approval covers is defined by the store that makes it safe, so the coverage cannot be
  wider than the reversibility it relies on.
- A new write operation is covered by being journalled, which is the same condition that makes its
  changes reversible, so the two cannot diverge.
- The prompt remains for everything the journal cannot reverse, and for anything that leaves the
  mindspace, so the approval never appears where it would be misleading.
- Every resolved request is logged as resolved by a standing approval, so an audit trail can
  distinguish a change the human saw from one they pre-authorised.

### Negative

- For the rest of the turn, covered changes happen unseen. The user trades attention for speed
  and the log, not the interface, is what records it.
- The switch has no persistent indicator, so the only sign that it is armed is the user's memory
  of having armed it and the absence of prompts. Sending a message is the only withdrawal.
- Operations the journal does not record — directory creation, directory moves, document
  conversion, git output written to a file — still prompt in the middle of an otherwise
  uninterrupted run, which is a visible inconsistency that the documentation has to explain rather
  than the interface resolve.
- The classification is made by each tool at its own call site. A tool that classifies too
  generously causes a change to happen without a prompt; the scope test is the safeguard, not the
  tool author's judgement, and a misclassification that widens scope is a defect that no test can
  detect from outside the tool that made it.
- A change the journal records can still exceed the capture threshold or already have been read,
  executed or committed. The approval rests on reversibility, which was never a guarantee.
- Two conversations in the same mindspace hold separate approvals, so a delegated child prompting
  while its parent is armed looks like an inconsistency to anyone who reads it as one mindspace
  acting.
