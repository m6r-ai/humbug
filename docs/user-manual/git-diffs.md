# Viewing Git Diffs

The diff tab shows a side-by-side comparison of a file's current state against its last
committed version in git. It gives you a clear view of exactly what has changed before you
commit — or before you hand work off to the AI to continue.

The diff tab is only available when your mindspace is inside a git repository.

---

## Opening a diff

There are several ways to open a diff for a file:

- **Right-click** a file in the Files view and choose **Diff** — this option only appears for
  files that have uncommitted changes
- Click any file in the **Version Control** view in the mindspace panel
- Use **Ctrl+Shift+D** to open a diff for the current file

---

## Reading the diff

The diff tab shows the two versions of the file side by side:

- The **left pane** shows the last committed version (HEAD)
- The **right pane** shows your current working copy

Changed lines are highlighted. Added lines appear in green, removed lines in red, and
unchanged lines are shown for context.

The status bar shows the filename and the total number of changed rows in the diff.

---

## Navigating between hunks

A hunk is a contiguous block of changed lines. You can jump between hunks using:

- **Alt+Down** — move to the next hunk
- **Alt+Up** — move to the previous hunk

---

## Finding text

Press **Cmd+F** / **Ctrl+F** to open the find bar and search across both panes of the diff.
The find bar supports case-sensitive search, regular expressions, and whole-word matching.

---

## Opening the file for editing

A button in the diff tab lets you open the file directly in an editor tab, scrolled to the
same position you are viewing in the diff. This makes it easy to jump straight to a changed
section and edit it.

---

## Automatic refresh

The diff tab watches the file for changes. If you save an edit, the diff refreshes
automatically to reflect the new state. It also refreshes when git's state changes — for
example, after you make a commit — so the diff always reflects the current difference between
your working copy and HEAD.

---

*[Index](index.md) · Previous: [Previewing Content](preview.md) · Next: [Searching](searching.md)*
