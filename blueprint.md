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
Only 3 external runtime dependencies exist beyond the Python standard library.
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
performance, with a Python VM fallback.

