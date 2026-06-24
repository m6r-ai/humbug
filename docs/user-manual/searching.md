# Searching

Humbug has two levels of search: **within-tab find**, available in every tab type, and
**global search**, which searches across all files in your mindspace at once.

---

## Within-tab find

Every tab type — conversations, editor, preview, diff, and terminal — has a find bar.

| Shortcut | Action |
|---|---|
| **Cmd+F** / **Ctrl+F** | Open find bar |
| **Enter** / **Shift+Enter** | Next / previous match |
| **Esc** | Close find bar |

The find bar supports:

- **Case-sensitive matching** — toggle with the case button
- **Whole-word matching** — toggle with the word button
- **Regular expression matching** — toggle with the regex button

It shows a match count and updates live as you type. The find bar has a history of recent
searches accessible with the up and down arrow keys.

The editor tab also has **find and replace** (**Ctrl+Alt+F** on macOS, **Ctrl+H** elsewhere),
which adds Replace Current and Replace All actions.

---

## Global search

The global search searches file names, file contents, and conversation message contents
across your entire mindspace simultaneously.

### Opening global search

- Press **Ctrl+Shift+F**, or
- Open **Mindspace → Mindspace Search**, or
- Click the **Search** icon in the mindspace panel's icon rail

### Search options

| Option | What it does |
|---|---|
| **Match case** | Only return results that match the exact capitalisation of your query |
| **Whole word** | Only match the query when it appears as a complete word |
| **Regular expression** | Treat the query as a regular expression |

### How results are shown

Results are grouped by file. Each file entry shows the relative path within the mindspace,
and below it any matching lines or message snippets with context around the match.

A status label at the top of the results shows how many matches were found. Search is capped
at **500 total results** across the whole mindspace to keep results manageable.

### Opening a result

- **Single-click** a result to open it in an ephemeral tab, scrolled to the matching line or
  message
- **Double-click** a result to open it persistently

Results in source files navigate to the matching line in an editor tab. Results in
conversation files navigate to the matching message within the conversation.

### What is searched

Global search covers:

- The relative file path itself (so you can search for a filename)
- The text content of all non-binary files
- The message content of all conversation files

The following directories are excluded from search automatically:

`.git`, `.humbug`, `.venv`, `venv`, `__pycache__`, `node_modules`, `dist`, `build`

Binary files are also skipped automatically.

---

*[Index](index.md) · Previous: [Viewing Git Diffs](git-diffs.md) · Next: [Attaching Files to Conversations](attachments.md)*
