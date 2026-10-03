# Quick Switcher

The Quick Switcher is a fast way to jump straight to a file, conversation, or already-open tab
by typing a few characters of its name — without leaving the keyboard or hunting through the
sidebar.

---

## Opening the Quick Switcher

Press **Cmd+P** / **Ctrl+P**, or open **Mindspace → Quick Switcher**.

A filter box appears over the current mindspace. Start typing, and the list below updates
live to show the best matches across three sources:

- **Open tabs** — every tab currently open in any column
- **Conversations** — every conversation stored in the mindspace
- **Files** — every other file in the mindspace

---

## Filtering

Matching is fuzzy: the characters you type just need to appear, in order, somewhere in a
candidate's name. For example, typing `qsw` matches `quick_switcher.py`.

Matches are ranked so that:

- A match against the item's **name** ranks above a match found only in its path
- Characters matched **consecutively**, or right after a separator like `/`, `_`, or `-`,
  rank higher than characters scattered deep inside a word

Clear the filter box to see the full, unranked list again.

---

## Choosing a result

| Action | Shortcut |
|---|---|
| Move selection down | **Down** |
| Move selection up | **Up** |
| Open the selected result | **Enter**, or click it |
| Close the Quick Switcher without opening anything | **Esc**, or click outside the panel |

Opening a result behaves the same as opening it from the sidebar: if it's already open in a
tab, that tab is focused; otherwise a new tab is created.

---

*[Index](index.md) · Previous: [Searching](searching.md) · Next: [Attaching Files to Conversations](attachments.md)*
