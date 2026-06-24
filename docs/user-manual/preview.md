# Previewing Content

A preview tab gives you a fast, read-only view of a file. Unlike the editor, preview tabs
render content in a polished form — Markdown becomes formatted prose, source code is shown
with syntax highlighting, and folders display a structured summary of their contents.

---

## Opening a preview

- **Single-click** a file in the Files or Conversations view to open it in an ephemeral
  preview tab
- **Right-click** a file and choose **Preview** to open a persistent preview tab
- Use **Ctrl+Shift+W** to open a preview of the current file

Preview tabs are read-only. To edit a file, use the **Edit** button that appears in the
preview, or open the file in an editor tab.

---

## What the preview shows

### Markdown files

Markdown files are rendered as formatted content — headings, paragraphs, lists, tables,
code blocks, blockquotes, and horizontal rules all display as they would in a document viewer.
Code blocks are syntax-highlighted based on their language annotation.

Links within the document work: local links to other files in your mindspace open a new
preview tab for that file, and external URLs open in your system's default browser.

### Source code files

Source code files are shown with full syntax highlighting and a language label at the top.
An **Edit** button opens the file in an editor tab.

### PDF and DOCX files

The text content of PDF and DOCX files is extracted and displayed. Formatting and images
are not shown, but the text is readable and searchable.

### Folders

Previewing a folder shows a summary of its contents, including file sizes, modification
timestamps, and permissions. If the folder contains a `README.md` file, it is rendered
as the main content of the preview, with the directory listing below it.

### Conversations

Previewing a conversation file shows the conversation's message history in a readable form.

---

## Finding text

Press **Cmd+F** / **Ctrl+F** to open the find bar. It supports case-sensitive search, regular
expressions, and whole-word matching. The find bar shows a match count and lets you step
through results.

---

## Automatic refresh

Preview tabs watch their source file for changes. If the file is modified externally or saved
from the editor, the preview refreshes automatically. The tab label shows a notification
indicator when content has been updated while the tab was not active.

---

*[Index](index.md) · Previous: [Using the Terminal](terminal.md) · Next: [Viewing Git Diffs](git-diffs.md)*
