# Using the Terminal

The terminal tab gives you a fully interactive shell session inside Humbug. You can run any
command you would normally run in your system terminal, and the AI can interact with it too.

---

## Opening a terminal

Open a new terminal from the **File** menu or with **Ctrl+Alt+T**.
The terminal opens in your mindspace's root directory.

You can have multiple terminal tabs open at the same time, each running an independent shell
session.

---

## Using the terminal

The terminal emulates a standard terminal. You can:

- Run commands, scripts, and programs
- Use all standard shell features (pipes, redirects, environment variables, etc.)
- Navigate with the keyboard as you would in any terminal
- Scroll through output with the mouse wheel

On macOS and Linux, the terminal uses your default shell (typically zsh or bash). On Windows,
it uses the command prompt or PowerShell.

---

## Finding text in terminal output

Press **Cmd+F** (macOS) / **Ctrl+F** to open the find bar and search through the terminal's
scrollback buffer. The find bar supports case-sensitive search, regular expressions, and whole
word matching.

---

## Copying and selecting

Click and drag to select text in the terminal. To copy selected text, either use the standard
copy shortcut (**Cmd+C** / **Ctrl+C**) or **right-click** and choose **Copy** from the context
menu. The right-click menu also offers **Paste**.

---

## Status bar

While a terminal tab is active, the status bar shows the name of the running process and the
current terminal dimensions (columns × rows).

---

## Terminal settings

Terminal behaviour can be configured per-mindspace in the **mindspace settings**:

- **Fixed width** — sets a minimum column width for the terminal grid. If enabled, the terminal
  will not become narrower than this width when the tab is resized; it will gain a horizontal
  scrollbar instead. The default is 80 columns
- **Scrollback limit** — caps the number of lines retained in the scrollback buffer. Limiting
  this improves performance and reduces memory use for sessions that produce a lot of output.
  The default is 10,000 lines
- **Close on exit** — when enabled, the terminal tab closes automatically when the shell process
  exits. When disabled, the terminal stays open so you can review the output

---

## The AI and the terminal

The AI can send keystrokes to a terminal tab and read its output as part of its work. Every
keystroke the AI sends requires your **explicit approval** before it is delivered to the shell
— you can review exactly what will be typed before allowing it.

This means the AI can run build commands, test suites, or any other shell operation, but you
always remain in control of what actually executes.

---

*[Index](index.md) · Previous: [Editing Files](editing-files.md) · Next: [Previewing Content](preview.md)*
