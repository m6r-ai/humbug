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

## Approving file changes for the rest of a turn

When you are asked to approve a change to a file in your mindspace, the prompt also offers
**Auto-approve file changes until your next message**. Turning it on approves this kind of change
for the rest of the current turn, so the AI can carry on working through a long task — editing a
dozen files in a refactor, say — without stopping for you after each one.

What it covers is deliberately narrow:

- It covers only changes inside the mindspace that the **journal** records, which means changes it
  can reverse. That is where the switch appears, and why it does not appear on every prompt.
- It covers writing, appending to, overwriting, moving and copying files, applying diffs, saving
  from an editor, and downloading a file into the mindspace — including deleting files and empty
  folders, which the journal records along with the content they held, and creating folders, which
  it records so they can be removed again.
- It never covers terminal commands, HTTP requests other than a download, delegating to a child AI,
  or anything that would reach outside the mindspace or into `.humbug/`.
- Converting a document is covered too, including the image folder a conversion to Markdown or HTML
  writes beside its output. The one write that still asks is moving a folder that contains files,
  whose contents the journal does not capture, and that prompt says the contents will be lost.

Turning the switch on approves later requests, never the one you are looking at — that one still
needs its own click — and it is never remembered. It lapses when the turn finishes, and it lapses
the moment you send your next message, so sending a message is how you take it back.

While it is on, the changes happen without a prompt, so the record is what stands in their place:
every change is listed in the [Journal](journal.md) and [Mindspace Log](mindspace-log.md) as
usual, and the log notes that you enabled the approval and that it, rather than a prompt you read,
is what approved each call.

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
