# AI Tools

When you ask the AI to do something — read a file, search the codebase, run a command, check git
status — it uses **tools**. Tools are the AI's hands and eyes: they let it interact with your
mindspace, inspect your files, and manipulate the Humbug interface on your behalf.

You do not need to invoke tools yourself or even think about them most of the time. Just ask the
AI what you want in plain language and it will use the appropriate tool automatically. But knowing
what tools exist helps you understand what the AI can and cannot do, and what it will ask your
permission for.

---

## Read-only vs. write operations

Tools fall into two categories:

- **Read-only tools** run automatically without asking. These include reading files, searching,
  checking git status, and viewing diffs. The AI can use these freely.
- **Write tools** require your approval before each action. These include writing files, running
  terminal commands, applying diffs, and converting documents. When the AI wants to use one, you
  will see an approval prompt showing exactly what it intends to do.

---

## Available tools

The following tools are available to the AI in every conversation:

| Tool | What it does | Approval needed? |
|---|---|---|
| **Clock** | Get the current time, sleep, and set alarms | No |
| **Conversation** | Browse, search, and navigate conversation history | No |
| **Delegation** | Delegate tasks to specialised child AI instances for parallel or focused work | No |
| **Document Converter** | Convert documents between Markdown, DOCX, and HTML | Yes |
| **Editor** | Read, search, navigate, and modify text in open editor tabs | Modifications only |
| **Filesystem** | Read, write, search, and manage files and directories in your mindspace | Writes only |
| **Git** | Read-only git operations: status, diff, log, branches, file contents at a ref, and files changed in a commit | No |
| **Help** | List available tools and get detailed documentation on any tool | No |
| **HTTP** | Fetch URLs, make API requests (GET, HEAD, POST, PUT, PATCH, DELETE), and download files. Supports Basic Auth, cookies, multipart uploads, timeouts, and HTTP/SOCKS5 proxies | Yes |
| **Menai** | Evaluate expressions in a pure functional programming language designed for AI use | No |
| **Preview** | Search and navigate file/directory preview content | No |
| **System** | Create, open, close, and organise UI tabs (editor, terminal, conversation, etc.) | No |
| **Terminal** | Send commands and read output from terminal tabs | Yes, each command |

---

## Tool output and truncation

Some tools can produce very large outputs — a file with thousands of lines, a git log with
hundreds of commits, or a search that matches across the entire codebase. To prevent the
conversation from being flooded with content, tools that can produce large outputs apply a
64KB limit. If the output exceeds this limit, it is truncated and a notice is appended indicating
how much was omitted.

This is not a restriction on what you can ask for — it is a safeguard. If the AI needs more
detail, it can make a more targeted request (for example, searching for a specific file rather
than diffing the entire working tree, or looking at a single commit rather than the full log).

---

*[Index](index.md) · Previous: [Viewing Git Diffs](git-diffs.md) · Next: [Searching](searching.md)*