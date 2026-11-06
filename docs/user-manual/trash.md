# The Trash

Deleting something in Humbug does not destroy it. Every file or folder you delete from the
mindspace panel, and every conversation you delete from the Conversations view, is moved into the
mindspace's **trash**, where it stays until you either restore it or delete it permanently.

This chapter explains what goes into the trash, how to get things back out of it, and what
emptying it means for the rest of Humbug's history.

---

## What goes into the trash

The trash collects deletions made from the mindspace panel:

- Files and folders you delete from the **Files**, **Preview**, or **Version Control** view
- Conversations you delete from the **Conversations** view

Deleting from any of these views asks you to confirm first, and tells you that the item is moving
to the trash rather than being removed. Any tab open on that file is closed without saving, since
the file it was showing is no longer in your mindspace. If you have unsaved changes in that tab,
close or save it before deleting.

Folders can only be deleted when they are empty, so the trash never holds a folder with files
still inside it.

---

## Opening the trash

The trash is a panel in the mindspace sidebar. It sits at the **bottom of the left icon rail**, so
it is available from whichever view you are working in. Click its icon to switch to it.

Open the trash each time you want to look at it — the panel refreshes when it is activated, so what
you see is always what is actually in the trash.

If no mindspace is open, the panel says so rather than showing an empty list.

---

## Reading the listing

Each row describes one deleted item:

```text
notes.md  —  projects/alpha  ·  2h ago
```

The row shows the item's **name**, the **folder it was deleted from**, and **how long ago** it was
deleted — *Just now*, then minutes, hours, and days. Hovering over a row shows the item's full
original path.

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

Humbug will not overwrite anything. If something now occupies that path — you deleted `notes.md`
and have since created another one — the restore is refused and you are told which path is in the
way. Move or rename the item that is there, then restore again.

Restoring does not remove the record of the deletion from the change journal. A restored file is
simply back where it was; the history of what happened to it remains intact.

---

## Deleting permanently

Right-click an item and choose **Delete Forever** to destroy it. You are asked to confirm, because
this cannot be undone.

Right-click **empty space** in the panel (or the region below the last item) and choose **Empty
Trash** to destroy everything in the trash at once. This also cannot be undone.

Until you do one of these, deleted items take up space on disk. There is no automatic expiry: the
trash holds what you put in it until you say otherwise.

---

## The trash and the change journal

The trash and the [Journal panel](journal.md) work together, and it is worth knowing how.

A deletion is a recorded change like any other. Because the deleted content is still available in
the trash, a deletion can be reverted from the Changes panel — the file is put back from the trash,
not recreated from a snapshot.

Deleting forever, or emptying the trash, removes that possibility. The change journal will then
report the deletion as a change it cannot reverse, rather than pretending the content is still
there. This is deliberate: a history feature that quietly restores something *different* from what
you deleted would be worse than one that says it can no longer help.

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
