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
- **Write tools** require your approval before each action. These include writing or deleting
  files, running terminal commands, fetching a URL, and converting documents. When the AI wants to
  use one, you will see an approval prompt showing exactly what it intends to do.

A change that only touches an open editor buffer is not yet a change to your files, so it is not
approved separately: the buffer is changed freely and you are asked when it is saved.

---

## Available tools

The following tools are available to the AI in every conversation:

| Tool | What it does | Approval needed? |
|---|---|---|
| **Clock** | Get the current time, sleep, and set alarms | No |
| **Conversation** | Browse, search, and navigate conversation history | No |
| **Delegation** | Delegate tasks to specialised child AI instances for parallel or focused work | No |
| **Document Converter** | Convert documents between Markdown, DOCX, and HTML | Yes |
| **Editor** | Read, search, navigate, and modify text in open editor tabs | Saving only |
| **Filesystem** | Read, write, search, and manage files and directories in your mindspace | Writes only |
| **Git** | Read-only git operations: status, diff, log, branches, file contents at a ref, and files changed in a commit | No |
| **Help** | List available tools and get detailed documentation on any tool | No |
| **HTTP** | Fetch URLs, make API requests (GET, HEAD, POST, PUT, PATCH, DELETE), and download files. Supports Basic Auth, cookies, multipart uploads, timeouts, and HTTP/SOCKS5 proxies | Yes |
| **Menai** | Evaluate expressions in a pure functional programming language designed for AI use | No |
| **Preview** | Search and navigate file/directory preview content | No |
| **System** | Create, open, close, and organise UI tabs (editor, terminal, conversation, etc.) | No |
| **Terminal** | Send commands and read output from terminal tabs | Yes, each command |

---

## Cancelling a wait

The clock tool can wait — for a few seconds while something settles, or until a specific time. A
wait longer than five seconds shows a **Finish Waiting Now** control inside the tool-call message
in the conversation.

Using it ends the wait immediately. The AI is told that you interrupted it, so it knows the time it
was waiting for has not passed and can decide what to do instead, rather than carrying on as though
the delay had happened.

---

## Tool output limits

Some tools can produce very large outputs — a file with thousands of lines, a git log with hundreds
of commits, or a search that matches across the entire codebase. To prevent a single call from
flooding the conversation, tools that can produce large outputs enforce a limit: 64KB of returned
content, and a maximum number of matches or files for the operations that enumerate.

Exceeding a limit is reported as an **error, not a truncated answer**. The AI is told which limit
it hit and how to narrow the request, and the call returns nothing rather than a partial result it
might mistake for the whole. For a large diff or file-at-ref, the git tool can instead write the
full output to a file in the mindspace and return a short summary; for an HTTP response, the
download operation writes straight to a file with no inline limit.

This is not a restriction on what you can ask for — it is a safeguard. If the AI needs more detail,
it can make a more targeted request (for example, searching for a specific file rather than diffing
the entire working tree, or looking at a single commit rather than the full log).

---

*[Index](index.md) · Previous: [Viewing Git Diffs](git-diffs.md) · Next: [Searching](searching.md)*
