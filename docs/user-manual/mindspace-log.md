# Mindspace Log

The mindspace log is a persistent, read-only record of activity within a mindspace. It captures
both human and AI actions — every file read or written, every tool call made, every approval
or denial, every settings change, and every file operation — providing a complete audit trail
of what happened and when.

The log is written by Humbug itself and cannot be modified by the AI, making it an independent
witness to activity within the mindspace.

---

## Opening the mindspace log

Open the log from **Mindspace → Mindspace Log**, or with **Ctrl+Shift+L**
(macOS: **Cmd+Shift+L**).

---

## What the log records

Each entry in the log has a severity level — **Trace**, **Info**, **Warning**, or **Error** —
shown in a colour-coded label alongside a timestamp. The types of activity recorded include:

- **AI tool calls** — every tool the AI invokes (filesystem reads and writes, terminal commands,
  editor operations, delegation, etc.)
- **Human actions** — files you save, rename, move, or delete; settings you change
- **Approvals and denials** — when you approve or reject an AI action that requires authorisation
- **Conversation events** — messages submitted, responses received, conversations created or
  deleted
- **System events** — errors, warnings, and internal status changes

---

## Reading the log

Log entries are displayed in chronological order, oldest first. The log auto-scrolls to the
latest entry as new events arrive. If you scroll up to review earlier entries, the tab label
shows a notification indicator when new entries arrive below your current position.

Each entry's header shows its severity level and a timestamp precise to the millisecond.

---

## Finding and searching

Press **Cmd+F** / **Ctrl+F** to open the find bar and search within the log. The find bar
supports case-sensitive search, regular expressions, and whole-word matching, and shows a
match count with next/previous navigation.

---

## Navigating entries

Use **Alt+Down** and **Alt+Up** to jump to the next or previous log entry, just as in
conversation tabs.

---

## Persistence

The log is stored in the `.humbug` directory within the mindspace and persists across sessions.
It is mindspace-scoped: each mindspace has its own independent log.

---

*[Index](index.md) · Previous: [The Humbug Shell](humbug-shell.md)*
