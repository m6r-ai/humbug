# Managing Files & Folders

The **Files** view in the mindspace panel is your primary tool for organising files and folders
within a mindspace. This chapter covers everything you can do from it.

---

## Opening files

- **Single-click** a file to open it in an ephemeral editor tab
- **Double-click** a file to open it in a persistent editor tab

Clicking a folder expands or collapses it in the tree. Folders cannot be opened as tabs directly,
but you can open a read-only preview of a folder's contents via the right-click context menu.

See [Tabs & Columns](tabs-and-columns.md) for an explanation of ephemeral vs persistent tabs.

---

## The right-click context menu

Right-clicking any item in the Files view opens a context menu. The options depend on whether
you clicked a file, a folder, or empty space.

### On a file

| Action | What it does |
|---|---|
| **Edit** | Opens the file in a persistent editor tab |
| **Preview** | Opens the file in a read-only preview tab |
| **Diff** | Opens a side-by-side diff against the last git commit (only shown if the file has uncommitted changes) |
| **Duplicate** | Creates a copy of the file with a "— copy" suffix and starts inline renaming |
| **Rename** | Starts inline renaming of the file |
| **Delete** | Deletes the file after confirmation |

### On a folder

| Action | What it does |
|---|---|
| **Preview** | Opens a read-only preview of the folder contents |
| **New Folder** | Creates a new sub-folder inside this folder |
| **New File** | Creates a new file inside this folder |
| **Rename** | Starts inline renaming of the folder |
| **Delete** | Deletes the folder (only allowed if it is empty) |

### On empty space or the root item

Right-clicking empty space or the `.` item at the root shows options to create a new file or
folder at the mindspace root level.

---

## Creating files and folders

To create a new file or folder, right-click the location where you want it and choose
**New File** or **New Folder**.

Humbug creates the item immediately with a default name ("New File.txt" or "New Folder") and
starts inline editing so you can type the name you want. Press **Enter** to confirm, or **Esc**
to cancel — cancelling removes the item that was just created.

When a new file is confirmed, it opens automatically in an editor tab.

---

## Renaming

To rename a file or folder, right-click it and choose **Rename**. The name becomes editable
in place. Press **Enter** to confirm or **Esc** to cancel.

When renaming a file, if the new name has a different file extension from the original, Humbug
will ask you to confirm the change — this prevents accidentally breaking a file's type by
mistyping the extension.

When renaming, the file extension is not selected by default so you can type a new base name
without needing to retype the extension.

---

## Duplicating files

Right-click a file and choose **Duplicate**. Humbug copies the file, gives it a name with
"— copy" appended, and immediately starts inline renaming so you can give it a proper name.
If a file with the copy name already exists, a number is added (e.g. "— copy (2)").

---

## Moving files and folders

You can move files and folders by **dragging and dropping** them within the Files view. Drop
a file onto a folder to move it inside that folder.

Humbug shows a confirmation dialog before completing the move, showing the source and
destination paths. If an item with the same name already exists at the destination, the move
is blocked and you are told why.

---

## Deleting

To delete a file, right-click it and choose **Delete**. You will be asked to confirm.

Folders can only be deleted if they are **empty**. If you try to delete a non-empty folder,
Humbug will tell you it cannot proceed. Remove or move the contents first.

You can also press the **Delete key** while a file or folder is selected in the tree to trigger
deletion.

---

## External changes

Humbug watches your mindspace files for changes made by other applications. If a file that is
open in an editor tab is modified externally, and you have not made any unsaved changes to it
in Humbug, the editor will automatically reload the file.

If a file is **deleted** externally while open in a tab, the tab label gains a strikethrough
to indicate the file no longer exists on disk. The tab remains open so you can recover the
content if needed.

---

*[Index](index.md) · Previous: [AI Models & Backends](ai-models-and-backends.md) · Next: [Editing Files](editing-files.md)*
