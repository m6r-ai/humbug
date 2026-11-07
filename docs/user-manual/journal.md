# Undoing Changes

Humbug keeps a record of every change made to the files in your mindspace, by you or by an AI. The
**Journal** panel shows that record, newest first, and lets you put a file back the way it was.

The record behind the panel is the **change journal**. It is the counterpart to the
[Mindspace Log](mindspace-log.md): the log is a witness to what happened, and the journal is what
makes some of it reversible.

---

## What is recorded

A change is recorded whenever a file in the mindspace is mutated through Humbug:

- Files an AI writes, edits, creates, renames, moves, or deletes
- Files you save, rename, move, or delete from the mindspace panel
- Folders either of you creates, moves or removes
- Documents converted from one format to another, together with the image folder a conversion to
  Markdown or HTML writes beside its output
- Git output an AI asks to be written to a file

Changes made **outside** Humbug are also picked up. Humbug compares the mindspace tree against a
checkpoint taken a few seconds earlier, so a file rewritten by a terminal command, an external
editor, or a build script is recorded too — as a change that is *detected* rather than attributed,
because Humbug cannot see who made it and will not guess.

Nothing outside the mindspace is watched, and `.humbug` itself never is: it holds the journal, so
watching it would record the act of watching.

Deleting a file or an empty folder is recorded here, by either actor: the content is kept so the
deletion can be reverted, and the file does not appear in the [Trash](trash.md).  The trash holds
conversations, whose transcripts live under `.humbug` and so fall outside the journal.

---

## Opening the Journal panel

Click the **Journal** icon in the left icon rail of the mindspace sidebar. The panel reloads each
time you switch to it, so the list always reflects the journal as it stands.

There is no menu item or keyboard shortcut for the panel: it reports on the mindspace you are
looking at, and refreshes when you turn to it.

---

## Reading the list

Each row is one recorded change and shows the **name of the file** it affected. Everything else
about the change is in the row's tooltip:

```text
src/parser.py
write · AI · 2026-11-05 12:41

conversation .humbug/conversations/2026-11-05-12-38-02-114.conv
```

That gives the file's path within the mindspace, the operation, who made the change — **You**,
**AI**, or **Detected** for a change made outside Humbug — and when. Where the change belongs to a
conversation, the conversation's transcript is named, so an AI's edits can be traced back to the
discussion that asked for them.

Times are shown in UTC throughout, as they were recorded.

A change whose content is no longer stored is marked **this change cannot be reverted** in its
tooltip. The rest of the list is still usable; only that row is affected.

### Ordering and how far back the list goes

Changes are grouped under **Today** and **Earlier**, newest first within each.

The panel lists the newest 500 changes. If the journal holds more than that, a final row says how
many older changes are not shown. Older changes are still recorded and still reversible — the
panel simply does not display them, so that a long-lived mindspace cannot make it expensive to
open.

### Narrowing the list to a time

To see what the mindspace looked like at an earlier moment, click **Show changes on or before…**
and pick a time. The list narrows to the changes recorded at or before that time, and the panel
reports the bound it is applying.

The picker applies as soon as you change it — there is no OK button — and the **×** beside it drops
the bound and lists everything again (**Show all changes**).

This is the fastest way to answer "what changed during that session?" without leaving the panel.

---

## Looking at a recorded version

A change is about a file's history, not about the file as it stands now, so clicking one shows the
version it produced rather than opening the file. The file may have been rewritten — or deleted —
since the change was recorded, and opening it would answer a different question from the one the
row asks.

- **Single click** a change to show the version it produced, in a tab that closes as soon as you
  open something else beside it
- **Double click** it to open the same tab and keep it
- **Drag** it onto a column, or onto the strip either side of the columns, to open it in that
  column and keep it

A change that produced no version of its own — a removal — opens nothing when clicked, and cannot
be dragged. The version a change produced is also not always still held (see below), and a change
whose version is gone likewise opens nothing.

Right-click a change to see the versions around it:

| Action | What it shows |
|---|---|
| **View the version this change produced** | The file as it stood immediately after the change |
| **View the version this change replaced** | The file as it stood immediately before it |
| **Compare the replaced version with this one** | A diff of that earlier version against the file as it is now |

The versions are shown in tabs marked **read-only**, with the tab title ending in
**(earlier version)** where the content comes from the journal rather than from disk. Nothing can
be edited or saved from them: the panel is a history, and an edit made from it would be an edit
dressed up as inspection.

Two of these actions are not always available, and the panel says so rather than showing something
misleading:

- A change that **removed** a file produced no version of its own, so only the version it replaced
  can be shown — and only while the journal still holds it
- Content that is **not text** can only be previewed from a file on disk, so it is shown only while
  the file still holds exactly that version

The journal holds the content a change *replaced*. A change's own version therefore becomes
available only once something has replaced it in turn, which for the newest change to a file it
usually has not. Clicking such a change does nothing rather than showing you the current file as if
it were that version, and the only way to see that state is to look at the file itself.

---

## Reverting a change

Right-click a change and choose **Revert this change**. Every file affected by that change returns
to the state it held before it, and a confirmation tells you how many changes will be reversed
before anything happens.

Some operations record more than one change — a move, for instance, is a removal and a creation —
and can only be reversed as a whole. Reverting either half reverts both.

When the work is done the panel reports the result:

```text
3 path(s) reverted, 1 could not be.
src/report.py — diverged: the file has changed since the change was recorded
```

A file that has changed since the journal recorded it is **skipped**, not overwritten. Divergence
is reported per file, so one file that moved on can never cause another file's work to be lost.

---

## Restoring the mindspace to a point

Right-click a change and choose **Restore the mindspace to this change**. Every change recorded
*after* that one is reversed, so the affected files end up as they stood at the change you picked.

The confirmation tells you how many changes will be undone. As with a single revert, the result is
reported per path, and anything that cannot be reversed is named along with the reason.

---

## What reversal cannot do

Reverting is more honest than most undo features, and it is worth being plain about its limits.
Humbug can only reverse what it recorded, and putting a file back is not the same as undoing an
effect.

- **Only inside the mindspace.** Files outside it are neither watched nor journalled.
- **Only what was recorded.** A file changed outside Humbug between two checkpoints is recorded as
  one detected change, so restoring to a point *near* it lands on the nearest recorded state, not
  the exact instant. The confirmation says as much.
- **Only while the content is held.** Journalled content is eventually forgotten, either by you or
  because it was too large to capture. Once it is gone, the change can no longer be reversed.
- **Nothing that has already happened elsewhere.** If a command deployed, sent, published,
  committed, or deleted something on a remote, reversing the file it rewrote does not undo any of
  that. Humbug reverses files, not effects.

For that reason the panel never claims a change is reversible: it reports what it actually did,
per file, and names what it refused.

---

## Forgetting history

The journal holds copies of the content it replaced, so it grows. It is bounded by you, not by a
schedule.

- Right-click a change and choose **Forget this change and everything before it** to discard that
  change and every earlier one
- Right-click empty space in the panel and choose **Forget all changes** to discard the whole
  journal

Both ask for confirmation, and both report what was reclaimed:

```text
Forgot 412 change(s) and reclaimed 86 MB.
```

Forgetting is permanent. Those changes can no longer be reverted, and any content held only by the
journal is deleted. Changes newer than the ones you forgot are untouched and still reversible.

When the journal grows beyond **512 MB**, the panel says so in a line above the list and points you
at this menu. That warning is the only thing in Humbug that will suggest pruning history; nothing
is ever discarded without you asking.

---

## Settings

Two aspects of the journal are configured per mindspace, in **Mindspace Settings → Journal**:

- **Checkpoint interval** — how often, in seconds, Humbug compares the tree against a checkpoint to
  detect changes made outside it. The default is 5 seconds; a larger value costs less work and
  reports external changes in coarser batches
- **Watched tree → ignored files, directories and paths** — what the comparison leaves out.
  Entries are glob patterns, matched in exactly the terms the
  [File Access](user-settings.md#file-access) allow and deny lists use, which means: a single
  name matches that whole name anywhere in the tree and never a longer one, so `venv` does not
  match `venvity`, and `*.swp` excludes an editor's swap files wherever they appear. A path
  matches when it ends with those components, so `src/generated` does not match
  `src/generatedly`; add `/**` to the end to include everything below it. `.git` and `.humbug`
  are always ignored

The defaults ignore caches, virtual environments, and generated output. A test run or a build
rewrites hundreds of files that no one would ever want to revert, and the source change that
produced them is already recorded against the file it was made in. You can prune more than the
defaults, or less.

---

## Where the record lives

The journal is stored in `.humbug/journal` within the mindspace. Like the rest of `.humbug` it is
invisible in the Files view, cannot be read or modified by the AI, and never appears in a git
repository.

It is a different store from the [Mindspace Log](mindspace-log.md), and deliberately so. The log is
never truncated — it is the record of what happened. The journal is reclaimed on request, because
holding old file content forever is not something a project folder should do quietly.

---

*[Index](index.md) · Previous: [The Trash](trash.md) · Next: [Editing Files](editing-files.md)*
