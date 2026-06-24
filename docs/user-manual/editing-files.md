# Editing Files

The editor tab provides a full text editor with syntax highlighting for a wide range of
programming and markup languages. This chapter covers the editor's features.

---

## Opening files for editing

- **Double-click** a file in the Files view to open it in a persistent editor tab
- **Single-click** a file to open it in an ephemeral editor tab
- Right-click a file and choose **Edit** to open it persistently
- The AI can open files in editor tabs as part of its work

See [Tabs & Columns](tabs-and-columns.md) for an explanation of ephemeral vs persistent tabs.

---

## Syntax highlighting

The editor automatically applies syntax highlighting based on the file extension. Supported
languages include:

C, C++, C#, CSS, Diff, Go, HTML, Java, JavaScript, JSON, Kotlin, Lua, Markdown, Menai,
Move (Sui), Python, Rust, Scheme, Solidity, Swift, TypeScript, XML, and plain text.

---

## Editing

The editor supports standard text editing with undo and redo. The tab label gains a modification
indicator when you have unsaved changes.

**Tabs and indentation** behaviour is controlled by the mindspace settings:
- **Soft tabs** — pressing Tab inserts spaces rather than a tab character (on by default)
- **Tab size** — how many spaces a tab represents (default 4)

Block indentation and de-indentation work on selected text using **Tab** and **Shift+Tab**.

---

## Saving

| Action | Shortcut |
|---|---|
| Save | **Cmd+S** (macOS) / **Ctrl+S** |
| Save As | **Cmd+Shift+S** (macOS) / **Ctrl+Shift+S** |

If you try to close a tab with unsaved changes, Humbug will ask you to confirm.

---

## Finding and replacing

| Action | Shortcut |
|---|---|
| Find | **Cmd+F** (macOS) / **Ctrl+F** |
| Find and Replace | **Ctrl+Alt+F** (macOS) / **Ctrl+H** |

The find bar appears at the top of the editor. It supports:

- **Case-sensitive search** — toggle with the case button in the find bar
- **Regular expression search** — toggle with the regex button
- **Whole word matching** — toggle with the word button
- **Replace current match** — replaces the currently highlighted match
- **Replace all** — replaces all matches in the file at once

The find bar shows a match count and lets you step through matches with the previous/next
buttons. It has a history of recent searches accessible with the up and down arrow keys.

---

## Go to line

Press **Ctrl+G** to open the Go to Line dialog. Type a line number and press
**Enter** to jump directly to that line.

---

## Status bar

While an editor tab is active, the status bar at the bottom of the window shows the current
cursor position (line and column number) and the file path.

---

## External changes

If a file is modified by another application while open in Humbug:

- If you have **no unsaved changes**, the editor automatically reloads the file and the tab
  label shows a notification indicator
- If you **do have unsaved changes**, the file is not reloaded — your work is preserved and
  you can decide what to do

If the file is **deleted** externally, the tab label gains a strikethrough. Your content
remains in the editor until you close the tab.

---

## The AI and the editor

The AI can interact with editor tabs directly as part of its work. It can:

- Read file contents and navigate to specific lines
- Apply changes using unified diffs (shown in the tool approval widget before being applied)
- Save files (requires your approval)

When the AI applies a diff to an editor tab, the cursor moves to the last edited line so you
can see what changed. If the tab being edited is not currently visible, its label turns a
distinct colour to let you know something has happened.

---

*[Index](index.md) · Previous: [Managing Files & Folders](managing-files.md) · Next: [Using the Terminal](terminal.md)*
