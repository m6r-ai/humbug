# Blueprint: Humbug

## What is Humbug?

Humbug is an **operating system for human-AI collaboration** — a modular, extensible platform
that lets humans and AIs work together on ideas, projects, and software.

It is not a chatbot wrapper, a coding assistant plugin, or a thin shell around an AI API.
It is a full collaboration environment in which both humans and AIs are first-class actors,
with equal access to the same tools and UI capabilities.

## Who is it for?

Humbug is designed for anyone who needs to work with AIs on complex, multi-step tasks:

- **Developers** building and maintaining software
- **Researchers** exploring ideas and analysing information
- **General users** who want to collaborate with AIs on any kind of project

The mindspace concept means every project — whether software, writing, research, or
something else entirely — gets its own persistent, isolated environment.

## Core principles

### Humans and AIs are equal actors
All tools, including the GUI, are designed to be usable by both humans and AIs.
There is no separate "AI mode" — the same environment serves both.

### Conversations drive agentic workflows
The conversation is the primary unit of collaboration.
AIs discover what they need, edit content, run tools, and orchestrate the UI — all from
within a conversation, without requiring a separate orchestration layer.

### Transparency over automation
Humbug does not hide what is happening.
Every tool call, approval request, and AI action is visible to the user.
The human-in-the-loop is a design feature, not an afterthought.
This gives users the ability to catch problems early and course-correct before they
become expensive mistakes.

### Auditability
Humbug maintains a persistent interaction log in every mindspace that records both AI
and human actions: every file the AI reads or writes, every tool call made, every human
approval or denial, every prompt submitted, every file saved, renamed, moved, or deleted
by the human, and every settings change.

This log is inaccessible to the AI itself, making it an independent witness to what
occurred. It is the foundation for a tamper-evident audit trail — including future
Merkle-tree-based cryptographic verification of the log's integrity — that allows
organisations in regulated environments to demonstrate that AI actions were sanctioned,
traceable, and consistent with stated intent.

### Minimal dependencies, maximum understandability
Humbug follows the pattern of an OS kernel: simple, largely self-contained, and
comprehensible from a single repository.
Only 3 third party runtime dependencies exist beyond the Python standard library and
menai (a sibling project of Humbug, installed from [PyPI](https://pypi.org/project/menai/)).
Both humans and AIs should be able to understand almost every part of the system.

### Vendor independence
Humbug supports many AI backends (Anthropic, DeepSeek, Google, Mistral, Ollama, OpenAI,
vLLM, xAI, Z.ai) and is designed to make it easy to add more.
Users are never locked in to a single provider.

### Bootstrapped development
Each version of Humbug has been built using the previous version.
This means the software is both a product and a proof-of-concept: it demonstrates what
human-AI collaboration can achieve at scale.
Over 90% of the code has been written by LLMs working within Humbug itself.

### Clean, modular architecture
The codebase is held to strict standards: mypy type checking, pylint linting, and a
dependency rules system that prevents unwanted coupling between modules.
This discipline is what makes it possible for AIs to reliably modify and extend the code.

The dependency rules enforce a strict separation between top-level modules.
The dependency graph must be acyclic — no module may depend on another that depends (directly
or transitively) on it.
This ensures each module can be reasoned about, tested, and evolved independently.
It also prevents the kind of gradual coupling that is especially risky when LLMs are
contributing code, since an AI might otherwise introduce a convenience import that silently
creates a circular dependency between modules.

### Frontend-agnostic workspace state
All workspace layout state — which tabs are open, which column each tab is in,
which tab is focused, and whether a tab is ephemeral — lives in the
frontend-agnostic `ContextRegistry` within the `Mindspace` model.  The desktop
frontend (`TabManager`) is a projection: it subscribes to registry events
(OPENED, CLOSED, UPDATED, FOCUSED, MOVED) and renders the corresponding Qt
widgets.  User actions (clicking a tab, dragging between columns) call back
into the registry, which emits events that the frontend reacts to.  This
separation is the foundation for a future remote frontend that visualises
activities on a headless backend.

### YAGNI — no speculative code
Humbug follows the YAGNI (You Aren't Gonna Need It) principle. Every method,
function, class, and module must have a concrete reason to exist: it must be used
somewhere in Humbug or its supporting tools. Code that cannot be reached at runtime
should not exist and must be removed. This applies to both human and AI contributors.

"It might be useful someday" is never a justification for adding code. Speculative
abstraction layers, unused helper functions, and unreachable methods add maintenance
burden and cognitive load, and make the codebase harder for both humans and AIs to
reason about.

With this said, where multiple uses become aparent we do want to refactor to avoid
needless duplications.  Clean architecture is important.  When restructuring code,
anything that is no longer called should be removed rather than left in place.

### AI tool safety
AI tools are the interface between the AI and the user's environment. They are governed by
three safety principles.

**Mindspace-scoped access.** Tools operate within the mindspace boundary. The `.humbug/`
directory — which contains conversations, settings, the audit log, and other internal state —
is excluded from all tool operations. This prevents the AI from accessing or modifying its own
audit trail, and keeps internal infrastructure invisible to both the user and the AI.

**Human-in-the-loop for side effects.** Tools that only read state — listing files, viewing
diffs, checking git status — run automatically without interruption. Tools that change state —
writing files, running terminal commands, applying diffs — require explicit user approval
before each action. The user sees exactly what the AI intends to do and can approve, reject,
or ask for clarification. This is not friction; it is the user's ability to course-correct
before a mistake becomes expensive.

**Bounded outputs.** Tools that can produce large outputs — file contents, search results,
git logs, diffs — must enforce a size limit (currently 64KB). If output exceeds the limit, it
is truncated with a notice indicating how much was omitted. This prevents a single tool call
from flooding the AI's context window with content it cannot use, which would waste tokens
and crowd out relevant information. The AI can always make a more targeted follow-up request.

### Two levels of version control
Humbug recognises two fundamentally different kinds of content within a mindspace, each
with its own versioning needs and ownership model.

**Humbug-internal versioning** concerns the `.humbug/` directory — conversations, settings,
the audit log, usage data, and shell history. This is Humbug's content, not the user's.
The user should not have to think about versioning it, just as they do not have to think
about how conversations are stored on disk. Humbug manages this transparently and
automatically, providing protection against accidental deletion, the ability to recover past
state, and a foundation for future features such as cross-device sync and cryptographic
verification of the audit log. This level is infrastructure: invisible to the user and
inaccessible to the AI.

**User repository versioning** concerns everything else in the mindspace. The user's
content follows the user's rules — they choose their repositories, branching strategies,
remotes, and commit conventions. Humbug's role is to be a capable participant in the user's
existing version control workflow, not to replace or override it. A mindspace is not a
repository: it may contain multiple git repositories, partial repositories, or no version
control at all. Version control tools must therefore be path-aware, discovering which
repository (if any) contains a given file or directory, rather than assuming a single
repo root.

These two levels are kept strictly separate. Conflating them would create two problems:
an AI able to rewrite its own audit trail (a security risk) and internal state management
requiring user intervention (a usability failure). When a mindspace root is itself a git
repository and `.humbug/` lives physically inside it, the `.humbug/` directory should be
excluded from the user's repository so the two versioning concerns never interfere.

## What Humbug is NOT

- **Not a replacement for the OS.** Humbug runs on top of Windows, macOS, and Linux
  and provides OS-like abstractions without replacing the underlying system.
- **Not just a developer tool.** While it has strong support for software development,
  it is designed for any activity where humans and AIs need to work together.
- **Not a walled garden.** It is open-source and designed to be extended.
- **Not a single-AI system.** Multiple AI conversations can run simultaneously, with
  support for task delegation between AI instances.

## Menai: a language designed for AIs

Previously, programming languages were designed for human developers.
While AIs are proficient with most of these languages, they can be unsafe because they
support potentially dangerous I/O operations.

Menai is a pure, functional, Lisp-inspired language designed specifically for AI use.
Being side-effect free, it requires no sandboxing and no user approval to execute.
This lets AIs build and run complex algorithmic tools freely and safely, without
interrupting the human collaborator.

Menai supports higher-order functions, tail-call optimisation, strict numeric typing,
and a rich standard library. It is compiled to bytecode and executed by a C VM for
performance.
