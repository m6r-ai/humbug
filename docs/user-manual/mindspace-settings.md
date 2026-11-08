# Mindspace Settings

Mindspace settings apply only to the current mindspace. Open them from **Mindspace → Mindspace
Settings**, the **settings icon** at the bottom of the left rail, or with **Ctrl+Alt+,**.

They appear in the **This Mindspace** group of the settings dialog, below the user-wide
**All Mindspaces** pages described in [User Settings](user-settings.md). If no mindspace is open,
these pages are unavailable.

---

## AI Model

These settings are used as the starting values for every new conversation opened in this
mindspace. You can override them per-conversation at any time.

- **Provider** — filters the model list to a specific provider, or leave it as **All Providers**
  to see all available models grouped by provider
- **Model** — the default AI model for new conversations. The list is searchable — type to filter by name
- **Reasoning** — whether reasoning is enabled by default and, if so, whether it is visible
  or hidden
- **Reasoning effort** — the default effort level for models that support variable reasoning
  depth
- **Temperature** — the default temperature setting (where supported by the model)

---

## AI Tools

Enables or disables individual AI tools for this mindspace. Tools that are disabled will not
be offered to the AI, even if they are globally available.

| Tool | What it does |
|---|---|
| **Filesystem** | Read, write, search, and manage files in the mindspace |
| **Terminal** | Send keystrokes to and read output from terminal tabs |
| **Editor** | Read, search, and change files in editor tabs, writing each change to the file |
| **Preview** | Search and navigate the content of preview tabs |
| **Conversation** | Read, search, and navigate conversation history |
| **Git** | Read-only repository queries: status, diff, log, branches, and file contents at a ref |
| **DocumentConverter** | Convert documents between Markdown, DOCX, and HTML |
| **HTTP** | Fetch URLs, make API requests, and download files |
| **Menai** | Execute Menai programs for safe algorithmic processing |
| **Clock** | Read the current date and time, wait, and set alarms |
| **Help** | Retrieve detailed documentation about available tools |
| **Delegate** | Spawn a child AI conversation to handle a sub-task |
| **System** | Open, close, and organise tabs (conversations, editors, previews, terminals) |

Disabling tools you do not need can simplify the AI's context and reduce the chance of
unwanted side effects. Disabling **HTTP** is the way to stop the AI reaching the network
altogether, since the [Network access](user-settings.md#network-access) allowlist controls only
which requests skip approval.

---

## Editor

- **Soft tabs** — when enabled, pressing Tab inserts spaces rather than a tab character.
  On by default
- **Tab size** — the number of spaces a tab represents. Default is 4
- **Auto backup** — when enabled, Humbug saves a backup copy of open editor files at regular
  intervals. The copies are working state rather than content: they are kept in the mindspace's own
  storage under `.humbug/backups`, never beside your files, and are removed when the file is saved,
  when the tab closes, or when you turn the setting off. Off by default
- **Auto backup interval** — how often backups are saved, in seconds. Default is 300 (5 minutes)

---

## Diff

- **Diff View** — the default layout for new diff tabs. **Inline** shows a single-column unified
  diff; **Side by Side** shows the old and new versions in separate panes. Default is Inline.
  You can switch between layouts at any time from the diff tab's context menu.

---

## Terminal

- **Fixed width** — when enabled, the terminal has a minimum column width. If the tab is
  resized to be narrower than this, the terminal gains a horizontal scrollbar rather than
  reflowing. Default is 80 columns
- **Scrollback limit** — caps the number of lines kept in the terminal's scrollback buffer.
  Limiting this reduces memory use for long-running sessions. Default is 10,000 lines
- **Close on exit** — when enabled, a terminal tab closes automatically when its shell process
  exits. When disabled, the terminal stays open so you can review the final output.
  On by default

---

## Conversation

- **Prompt markers** — show a narrow marker beside each of your messages in a conversation, so you
  can see and jump to your prompts while scrolling through a long session. Off by default

The same switch is available as **Prompt Markers** in the **View** menu, for switching it without
opening settings.

---

## Journal

Two settings control the [change journal](journal.md).

- **Checkpoint interval** — how often, in seconds, Humbug compares the mindspace tree against a
  checkpoint so that changes made outside Humbug are recorded. Default is 5 seconds. A larger
  value does less work and reports external changes in coarser batches
- **Watched tree → ignored files, directories and paths** — what that comparison leaves out. An
  entry is a glob pattern, matched in the same terms as the
  [File Access](user-settings.md#file-access) allow and deny lists: a single name matches that
  whole name anywhere in the tree and never a longer one, so `venv` does not match `venvity` and
  `*.swp` excludes an editor's swap files wherever they appear; a path matches when it ends with
  those components; and `/**` at the end of an entry includes everything below that path. `.git`
  and `.humbug` are always ignored

The defaults ignore caches, virtual environments, and generated output — a build or a test run
rewrites hundreds of files that no one would ever want to revert. The list you save replaces the
defaults rather than adding to them, so a mindspace can prune less as well as more.

---

*[Index](index.md) · Previous: [User Settings](user-settings.md) · Next: [Conversation Settings](conversation-settings.md)*
