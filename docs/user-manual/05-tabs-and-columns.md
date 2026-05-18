# Tabs & Columns

Humbug's interface is built around tabs arranged into columns. You can have multiple columns open
side by side, each holding any number of tabs. This lets you keep a conversation, a file you are
editing, and a terminal all visible at the same time — and lets the AI open tabs alongside yours
without disrupting what you are looking at.

---

## Tab types

Humbug has seven types of tab, each serving a distinct purpose:

| Type | What it contains |
|---|---|
| **Conversation** | An AI conversation — messages, responses, and tool activity |
| **Editor** | A file open for editing, with syntax highlighting |
| **Preview** | A read-only rendered view of a file (Markdown is rendered; source files show with highlighting) |
| **Terminal** | A fully interactive shell session |
| **Diff** | A side-by-side comparison of a file against its last git commit |
| **Log** | The Humbug system log, showing internal events and activity |
| **Shell** | The Humbug shell — a command-line interface for Humbug-specific operations |

Each tab type is identified by a small icon in its label.

---

## Columns

Tabs are grouped into **columns** — vertical stacks of tabs that sit side by side in the main
area of the window. You can have up to **six columns** open at once.

Only one column is active at a time. The active column is visually distinct from the others, and
keyboard shortcuts act on tabs within it.

### Creating columns

New columns are created automatically when you drag a tab to the left or right edge of the main
area (past the outermost existing column). Drop zones appear at the edges to indicate where the
new column will be created.

You can also open a file or conversation into a new column by dragging it from the mindspace panel
to the left or right edge.

### Closing columns

A column closes automatically when its last tab is closed or moved out. You do not need to close
columns manually.

### Resizing columns

You can resize columns by dragging the splitter handles between them.

---

## Working with tabs

### Opening tabs

Tabs are opened by:

- Clicking or double-clicking items in the mindspace panel
- Dragging files or conversations from the mindspace panel into the tab area
- The AI opening a tab as part of its work (it will open a new column automatically if the tab
  would otherwise obscure your current view)

### Closing tabs

Close a tab using the **×** button that appears in the tab label when you hover over it.

If the tab contains unsaved changes, you will be asked to confirm. If the tab is a conversation
that is currently streaming a response, you will be asked to confirm cancellation.

When a tab closes, the tab that was most recently active in that column becomes the new active tab.

### Moving tabs

You can **drag a tab label** to move it. Drop it:

- **Onto another column's tab bar** to move it into that column
- **Onto the left or right edge spacer** to move it into a new column at that edge
- **Onto another tab** within the same column to reorder

Moving a tab is a deliberate action, so an ephemeral tab becomes persistent when moved.

### Scrolling the tab bar

If a column has more tabs than fit in the tab bar, you can scroll the tab bar using the **mouse
wheel**.

---

## Ephemeral and persistent tabs

Tabs opened by a single-click from the mindspace panel are **ephemeral**. Ephemeral tabs are
temporary placeholders:

- Their label is shown in **italic** text with a distinct colour to distinguish them from
  persistent tabs
- When the next new tab opens in the same column, the ephemeral tab is automatically replaced
- Only one ephemeral tab exists per column at a time

A tab becomes **persistent** — and stays open — when you:

- **Double-click** its label in the tab bar
- **Interact** with its content (for example, typing in a conversation or editing a file)
- **Move** the tab to a different column

If you want to browse through several files quickly, ephemeral tabs let you do so without
accumulating a stack of open tabs you then have to close.

---

## Tab label colours

The colour and style of a tab label carries meaning at a glance:

| Appearance | Meaning |
|---|---|
| Normal text | Persistent tab, up to date |
| **Italic** text | Ephemeral tab |
| **Highlighted background** | Content has been updated while the tab was not active (e.g. an AI finished a response, or a file was changed externally) |
| **Strikethrough** text | The file the tab is based on no longer exists on disk |

---

## The active column

Clicking anywhere inside a column makes it the **active column**. The active column's tab bar is
visually distinct from inactive columns.

The AI always opens new tabs in a column other than the one currently containing the conversation,
so you can see them side by side without the conversation being hidden.

---

*Previous: [Mindspaces](04-mindspaces.md)*
*Next: [Conversations](06-conversations.md)*
