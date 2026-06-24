# Mindspace Settings

Mindspace settings apply only to the current mindspace. Open them from **Mindspace → Mindspace
Settings**, the **settings icon** at the bottom of the left rail, or with **Ctrl+Alt+,**.

---

## Conversation defaults

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

## Editor

- **Soft tabs** — when enabled, pressing Tab inserts spaces rather than a tab character.
  On by default
- **Tab size** — the number of spaces a tab represents. Default is 4
- **Auto backup** — when enabled, Humbug saves a backup copy of open editor files at regular
  intervals. Off by default
- **Auto backup interval** — how often backups are saved, in seconds. Default is 300 (5 minutes)

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

## AI Tools

Enables or disables individual AI tools for this mindspace. Tools that are disabled will not
be offered to the AI, even if they are globally available.

The available tools are:

| Tool | What it does |
|---|---|
| **Filesystem** | Read, write, and search files in the mindspace |
| **Terminal** | Send keystrokes to and read output from terminal tabs |
| **Editor** | Read, modify, and save files in editor tabs |
| **Clock** | Read the current date and time |
| **Menai** | Execute Menai programs for safe algorithmic processing |
| **Help** | Retrieve detailed documentation about available tools |
| **Delegate** | Spawn a child AI conversation to handle a sub-task |
| **System** | Open and interact with tabs (conversations, editors, previews, terminals) |
| **Conversation** | Read and navigate the current conversation |

Disabling tools you do not need can simplify the AI's context and reduce the chance of
unwanted side effects.

---

*[Index](index.md) · Previous: [User Settings](user-settings.md) · Next: [Conversation Settings](conversation-settings.md)*
