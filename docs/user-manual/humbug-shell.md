# The Humbug Shell

The Humbug Shell is a built-in command-line interface for Humbug-specific operations. It is
distinct from the terminal tab: the terminal gives you a full operating-system shell (bash,
zsh, PowerShell, etc.), while the Humbug Shell understands a small set of commands for
navigating and opening things within Humbug itself.

---

## Opening the Humbug Shell

Open the Humbug Shell from **Mindspace → Humbug Shell**, or with **Ctrl+Shift+Y**
(macOS: **Cmd+Shift+Y**).

---

## Shell vs. terminal

| | Terminal | Humbug Shell |
|---|---|---|
| **What it is** | A full OS shell (bash, zsh, PowerShell) | A Humbug-specific command interface |
| **What it runs** | Any command your OS supports | A fixed set of Humbug commands |
| **Approval needed** | Yes — every keystroke from the AI requires approval | No — commands are safe Humbug operations |
| **Use case** | Running builds, tests, git, scripts | Quickly opening files, conversations, diffs, and more |

---

## Commands

The Humbug Shell supports the following commands. Each command accepts arguments and options
familiar from standard command-line syntax.

| Command | Aliases | What it does |
|---|---|---|
| **`cat <file>`** | | Display the text content of a file (PDF and DOCX supported) |
| **`clear`** | | Clear the shell history |
| **`conversation`** | `conv`, `chat` | Start a new AI conversation |
| **`diff <file>`** | | Open a side-by-side git diff tab for a file |
| **`edit <file>`** | `open` | Open or create a file in an editor tab |
| **`help [command]`** | `?` | Show help for available commands, or detailed help for a specific command |
| **`log`** | | Open the mindspace log |
| **`preview <file>`** | | Open a file in a preview tab |
| **`terminal`** | `term` | Start a new terminal tab |

### Command options

Some commands accept options. For example, the `conversation` command lets you specify the
model, temperature, and reasoning effort:

```text
conversation --model claude-sonnet-4-20250514 --temperature 0.7
conversation -m gpt-4o -r high
```

Use `help <command>` or `<command> --help` to see the options available for any command.

---

## Tab completion

Press **Tab** while typing a command name or file path to auto-complete it. If multiple
completions are possible, pressing **Tab** again cycles through them.

---

## Command history

Use the **Up** and **Down** arrow keys to cycle through previously entered commands, just like
a standard shell.

---

## Finding text

Press **Cmd+F** / **Ctrl+F** to open the find bar and search through the shell output history.
The find bar supports case-sensitive search, regular expressions, and whole-word matching.

---

*[Index](index.md) · Previous: [Keyboard Shortcuts](keyboard-shortcuts.md) · Next: [Mindspace Log](mindspace-log.md)*
