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

Humbug distinguishes between two kinds of tabs: **ephemeral** and **persistent**. Understanding
the difference will help you work more efficiently.

### What are ephemeral tabs?

Tabs opened by a **single-click** from the mindspace panel are **ephemeral**. They are temporary
placeholders designed for quick browsing:

- Their label is shown in **italic** text with a distinct colour to distinguish them from
  persistent tabs
- When the next new tab opens in the same column, the ephemeral tab is automatically replaced
- Only one ephemeral tab exists per column at a time

### What are persistent tabs?

A tab becomes **persistent** — and stays open indefinitely — when you:

- **Double-click** its label in the tab bar
- **Interact** with its content (for example, typing in a conversation or editing a file)
- **Move** the tab to a different column

Once a tab is persistent, it will not be replaced when you open another tab. It will remain
visible until you explicitly close it.

### When to use ephemeral tabs

Ephemeral tabs are ideal for:

- **Quick browsing** — Looking at several files to understand the structure of a project
- **Checking references** — Opening a file to see how something is used, then moving on
- **Exploring with the AI** — When the AI opens a file to show you something, you can quickly
  look at it and then open the next one without accumulating tabs
- **One-off previews** — Glancing at a README or configuration file

**Example:** You ask an AI to review your code. It opens five different files in sequence to show
you issues. With ephemeral tabs, each new file replaces the previous one, so you stay focused on
the current file the AI is discussing. When you are done, there are no extra tabs left open.

### When to use persistent tabs

Persistent tabs are ideal for:

- **Active work** — A file you are editing or a conversation you are actively engaged with
- **Reference material** — A file you need to keep visible while working on something else
- **Side-by-side comparison** — Keeping two related files open at the same time
- **Long-running tasks** — A terminal session or conversation you will return to repeatedly

**Example:** You are writing code and need to refer to a data schema while you work. Open the
schema file in one column (making it persistent by double-clicking or by starting to edit it),
and keep your code file in another column. Both stay visible until you close them.

### Converting between ephemeral and persistent

- **Ephemeral → Persistent:** Double-click the tab label, or start editing/interacting with it
- **Persistent → Ephemeral:** There is no way to convert back. Instead, close the tab and
  single-click the file again to open it as ephemeral

### Visual distinction

Ephemeral tabs are always shown in **italic** text with a distinct colour. This makes it
immediately obvious which tabs are temporary. Persistent tabs use normal (non-italic) text.

---

## Tab overviews

When you have many tabs open across multiple columns it can be hard to find what you are looking
for at a glance. Humbug provides two full-screen overlay views — the **Tab Overview** and the
**Tab Carousel** — that let you see and navigate all open tabs quickly.

Both overlays show a live thumbnail of each tab's content, its icon, and its title. Pressing
**Esc** or clicking outside any card dismisses the overlay and returns focus to wherever you were.

Both overlays can also be triggered from the two buttons at the bottom of the left sidebar rail.

---

### Tab Overview

**Shortcut:** `Cmd+Shift+E` (macOS) / `Ctrl+Shift+E` (Windows/Linux)  
**Menu:** View → Show Open Tabs

The Tab Overview presents all open tabs as a **scrollable grid of cards**. Every tab across every
column is shown, making it easy to survey your entire workspace at once.

**Navigating the overview:**

- **Click** a card to switch to that tab
- **Arrow keys** or **Tab / Shift+Tab** move the keyboard selection between cards
- **Enter** activates the selected card
- **Scroll** vertically if there are more cards than fit on screen
- **Swipe a card upward** (or drag it upward with the mouse) to close that tab
- **Esc** or click the background to dismiss without switching

Invoking the shortcut again while the overview is open moves the keyboard selection to the next
card.

---

### Tab Carousel

**Shortcut:** `Cmd+Shift+T` (macOS) / `Ctrl+Shift+T` (Windows/Linux)  
**Menu:** View → Show Tab Carousel

The Tab Carousel presents open tabs as a **horizontally scrollable strip of cards** centred on
the current tab. The focused card is shown at full size in the centre; cards to either side are
shown progressively smaller, giving a clear sense of depth. Up to five cards are visible at once.

**Navigating the carousel:**

- **Click** the focused (centre) card to switch to that tab
- **Click** a non-focused card to bring it to the centre
- **Left / Right arrow keys** move the selection one card at a time
- **Mouse wheel** scrolls through the strip
- **Click and drag** horizontally to scrub through the strip; releasing snaps to the nearest card
- **Enter** activates the currently focused card
- **Swipe a card upward** (or drag it upward with the mouse) to close that tab
- **Esc** or click the background to dismiss without switching
- The **×** button appears on the focused card's header on mouse-over and closes that tab

Invoking the shortcut again while the carousel is already open advances it to the next card,
making it easy to cycle through tabs with repeated keypresses.

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

*[Index](index.md) · Previous: [Mindspaces](04-mindspaces.md) · Next: [Conversations](06-conversations.md)*
