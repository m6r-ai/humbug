# The Trash

Deleting a conversation in Humbug does not destroy it. Every conversation or conversation folder you
delete from the **Conversations** view is moved into the mindspace's **trash**, where it stays until
you either restore it or delete it permanently.

This chapter explains what goes into the trash, how to get things back out of it, and what emptying
it means for the rest of Humbug's history.

---

## What goes into the trash

The trash collects conversations deleted from the **Conversations** view. Conversations can be
deleted by folder or individually, and a deleted conversation takes its delegates and forks with it.

Deleting a conversation asks you to confirm first, and tells you that it is moving to the trash
rather than being removed.

**Not everything you delete lands here.** Files and folders you delete from the **Files**,
**Preview**, or **Version Control** view are recorded in the change journal instead, and come back
by reverting the deletion there. The reason is that a conversation's transcript lives inside the
mindspace's `.humbug` directory, which the journal deliberately does not cover, so conversations
need a bin of their own — while your own files do not. See [Undoing Changes](journal.md) for the
other half of the picture.

---

## Opening the trash

The trash is a panel in the mindspace sidebar. It sits at the **bottom of the left icon rail**, so
it is available from whichever view you are working in. Click its icon to switch to it.

Open the trash each time you want to look at it — the panel refreshes when it is activated, so what
you see is always what is actually in the trash.

If no mindspace is open, the panel says so rather than showing an empty list.

---

## Reading the listing

Each row describes one deleted conversation:

```text
notes review.conv  —  projects/alpha  ·  2h ago
```

The row shows the item's **name**, the **folder it was deleted from**, and **how long ago** it was
deleted — *Just now*, then minutes, hours, and days. Hovering over a row shows the opening of the
first prompt you sent in that conversation, above the item's original path within the mindspace —
the prompt trimmed to a few lines, so a name you never changed is still recognisable.

Items are listed newest first.

An item with no recorded original location — for example one deleted by a version of Humbug from
before the trash existed — is shown with **Unknown location**. It can still be restored, but
Humbug cannot tell where it came from, so it is put back into the mindspace's conversations folder.

Deleted conversations can be read without restoring them: single-clicking one opens it in an
ephemeral conversation tab, and double-clicking opens it persistently. This lets you check that a
conversation is the one you want before putting it back.

---

## Restoring an item

Right-click an item and choose **Restore**. It returns to the folder it was deleted from, with the
name it had.

Humbug will not overwrite anything. If something now occupies that path — you deleted a
conversation and have since created another with the same name — the restore is refused and you are
told which path is in the way. Move or rename the item that is there, then restore again.

---

## Deleting permanently

Right-click an item and choose **Delete Forever** to destroy it. You are asked to confirm, because
this cannot be undone.

Right-click **empty space** in the panel (or the region below the last item) and choose **Empty
Trash** to destroy everything in the trash at once. This also cannot be undone.

Until you do one of these, deleted conversations take up space on disk. There is no automatic
expiry: the trash holds what you put in it until you say otherwise.

Deleting a conversation for good does **not** destroy anything else. It has no bearing on the
journal, and the journal has no bearing on it.

---

## The trash and the change journal

The trash and the [Journal panel](journal.md) are separate stores, and each is the only home for
what it holds.

The trash holds conversations, because their transcripts are Humbug's own state rather than your
files, and the journal never covers them.

The journal holds everything else, including every file and folder you delete from the Files,
Preview, or Version Control view. A deletion there is a recorded change like any other, so it is
reverted from the Changes panel rather than restored from the trash. Emptying the trash therefore
has no effect on a deleted file: what destroys that file's content is forgetting the journalled
history that holds it, from the Changes panel.

Both limits are deliberate. A history feature that quietly restores something *different* from what
you deleted would be worse than one that says plainly what it can no longer do.

---

## Where the trash lives

The trash is per mindspace, and lives inside the mindspace's `.humbug` directory, alongside its
settings, conversations, change journal, and log. That means:

- It travels with the mindspace, and is not shared between projects
- It is never visible in the Files view and cannot be read or modified by the AI
- It never appears in a git repository, because `.humbug` ignores itself

Deleting things outside a mindspace — from your operating system's file manager, for example — is
not something Humbug can intercept, and does not go through this trash.

---

*[Index](index.md) · Previous: [Managing Files & Folders](managing-files.md) · Next: [Undoing Changes](journal.md)*
