# Mindspaces

A mindspace is a self-contained workspace for a project. Everything belonging to that project —
its files, its conversations with AI, its settings, its history — lives in one place. When you
switch projects, you switch mindspace.

This chapter explains what a mindspace is, what is inside one, how to create and open them, and
how to use the mindspace panel to navigate your work.

---

## What a mindspace is

A mindspace is simply a folder on your computer, recognised by Humbug as a project home. Humbug
marks a folder as a mindspace by creating a hidden `.humbug` subdirectory inside it. This
directory holds your settings, your session state (which tabs were open and where), the system
log, and token usage data. Everything else in the folder is yours to organise however you like.

You can have as many mindspaces as you like — one per project, or one per area of work. Humbug
remembers the last mindspace you had open and restores it automatically on the next launch.

---

## Creating a mindspace

To create a new mindspace:

1. Click the **mindspace name** shown at the top of the left panel. If no mindspace is open, this
   area will prompt you to open one.
2. Choose **New Mindspace** and select a folder. This can be a new empty folder or an existing
   project folder.
3. Humbug will ask which sub-folders to create. The suggested options are **conversations**,
   **src**, and others depending on your project type. You can choose any combination or none —
   these are just conveniences to help you get organised from the start.

Humbug will initialise the mindspace and open it immediately.

---

## Opening an existing mindspace

To open a mindspace you have used before, or to open a folder that already contains a Humbug
mindspace:

1. Click the **mindspace name** at the top of the left panel.
2. Choose **Open Mindspace** and navigate to the folder.

Humbug will restore the session — the same tabs that were open when you last closed that mindspace
will reappear in the same columns and positions.

---

## The mindspace panel

The left side of the Humbug window is the **mindspace panel**. It shows the contents and state of
your current mindspace, and is where you navigate, open, and manage files and conversations.

The panel shows one view at a time. You switch between views using the small icon buttons on the
left rail of the panel:

### Conversations view

The **Conversations** view lists all your AI conversations. Conversations are grouped and can be
nested to reflect parent–child relationships (for example, when an AI delegates a task to another
AI, the child conversation appears beneath the parent).

From this view you can:

- **Single-click** a conversation to open it in an ephemeral tab — a temporary tab that will be
  replaced when the next tab is opened, unless you interact with it first (for example, by sending
  a message)
- **Double-click** a conversation to open it in a persistent tab
- **Right-click** to access a context menu with options to create a new conversation, rename,
  duplicate, delete, or open conversations in a preview view

### Files view

The **Files** view shows the files and folders in your mindspace. It works like a standard file
browser.

From this view you can:

- **Single-click** a file to open it in an ephemeral editor tab
- **Double-click** a file to open it in a persistent editor tab
- **Right-click** to access a context menu, including a **Preview** option to open the file in
  a read-only preview tab instead
- **Right-click** to access a context menu with options to create new files or folders, rename,
  duplicate, move, or delete items
- **Drag and drop** files between folders, or drag them into the tab area to open them

All file types that Humbug supports for syntax highlighting can be opened in the editor. PDF and
DOCX files can be opened for reading.

### Version control view

If your mindspace folder is inside a **git repository**, a version control view becomes available.
This shows which files have been modified compared to the last git commit, giving you a quick
overview of what has changed. From here you can open a side-by-side diff of any modified file.

This view only appears when Humbug detects a git repository at or above the mindspace root.

### Search

A global **search** lets you find text across all files in your mindspace at once. Results are
grouped by file and link directly to the matching location.

---

## The breadcrumb bar

Each view in the mindspace panel has a **breadcrumb bar** that shows which directories are
currently expanded. This makes it easy to see where you are when working inside a deep folder
structure, and lets you jump back to a parent directory quickly.

---

## Mindspace settings

Each mindspace has its own settings, separate from your user-level settings. Mindspace settings
cover things that are project-specific:

- **Default AI model and temperature** for new conversations in this mindspace
- **Editor behaviour** — whether to use soft tabs, and what tab size to use
- **Terminal behaviour** — fixed width, scrollback limit, and whether terminals close automatically
  when the shell exits
- **AI tools** — which tools are enabled for AI conversations in this mindspace

To open the mindspace settings, use **Mindspace → Mindspace Settings**, or click the **settings
icon** at the bottom of the left rail.

---

## Special files: AGENTS.md and blueprint.md

Humbug recognises two special Markdown files that you can create anywhere in your mindspace to
provide context to AI:

- **`blueprint.md`** — describes the purpose and intent of the mindspace or a subdirectory.
  When an AI reads this, it understands what the project is trying to achieve.
- **`AGENTS.md`** — provides guidance to AI agents about how to navigate and work within the
  mindspace or a subdirectory: where things are, what conventions to follow, what to watch out for.

These files are entirely optional, but they can significantly improve the quality of AI assistance
on larger or more complex projects. You write them yourself, in plain Markdown, and update them
as the project evolves.

---

## Mindspace isolation

Each mindspace is isolated from others. Conversations, settings, and session state do not cross
between mindspaces. By default, the AI's filesystem access is also limited to the current
mindspace, so it cannot read or modify files outside it unless you explicitly allow that in the
user settings.

---

*[Index](index.md) · Previous: [Your First Conversation with an AI](first-conversation.md) · Next: [Tabs & Columns](tabs-and-columns.md)*
