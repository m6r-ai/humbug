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

Anything already open appears once, as its tab, so choosing it takes you to the tab you
already have rather than opening a second copy.

To keep the list fast to build on large mindspaces, the Quick Switcher gathers up to 2000
files. If it reaches that limit it says so at the bottom of the panel — open tabs and
conversations are always listed in full.

---

## Filtering

Matching is fuzzy: the characters you type just need to appear, in order, somewhere in a
candidate's name. For example, typing `qsw` matches `quick_switcher.py`.

Matches are ranked so that:

- A match against the item's **name** ranks above a match found only in its path
- Characters matched **consecutively**, or right after a separator like `/`, `_`, or `-`,
  rank higher than characters scattered deep inside a word

Clear the filter box to see the full, unranked list again.

### Go to Anything prefixes

The same input can also narrow the task before you type a search term:

| Input | Result |
|---|---|
| `@plan` | Search conversations only, open or not |
| `>new conversation` | Search and run application commands |
| `:120` | Jump to line 120 in the current editor |
| `main.py:120` | Go to line 120 of the matching file |

Commands are the same actions as Humbug's menus. Only commands that are valid right now are
listed, and each one shows its keyboard shortcut so you can learn the shortcut for anything you
reach for often.

`:120` is offered only when the current tab can go to a line. `main.py:120` matches files and
open file tabs — not conversations or terminals, which have no lines to jump to. The text before
the colon is matched against the whole path, so `src:120` finds everything under `src/`. The file
opens in an editor at that line, whether or not it was already open — the one exception being an
image, which opens in a preview where there is no line to go to.

A line past the end of a file takes you to its last line rather than failing.

Because `>`, `@`, and `:` start a prefix, a file whose name begins with one of those characters
cannot be reached by typing its name directly. Search for a neighbouring part of its path instead.

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
