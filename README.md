# Humbug: building an operating system for human-AI collaboration

Humbug is a modular, extensible platform that aims to let you and your AIs work on ideas together.
Think of it as an operating system for human-AI collaboration.

![Humbug in action](docs/v0.22-demo.gif)

## What are the core ideas?

- **Human–AI collaboration at the heart of the design**  

  When you're using AI, you're no longer working alone.
  Humbug treats both humans and AIs as first-class actors.
  All tools, including the GUI, are designed to be available for both to use, so it's faster and easier to get things done (there's still work to do on this though).

- **Bootstrapped: built using itself**  

  Each version of Humbug has been built using the previous version.
  This has meant over 80% of the code has been built by LLMs.
  This isn't just "vibe-coded", however!
  Using each ever more capable version of Humbug to help design and build its successor has freed up huge amounts of human time to think about bigger design problems.
  The tireless ability of LLMs to morph software into new forms also means the software has a clean, highly modular architecture.

- **LLMs, lots of LLMs**  

  Humbug lets you work with multiple large language models (LLMs) simultaneously, supporting local, cloud-based, or hybrids of both.
  Works with LLMs from Anthropic, DeepSeek, Google, Mistral, Ollama, OpenAI, vLLM, xAI, and Z.ai, so you're not tied to any one provider.
  You can optimize for cost, and are future-proofed when you want to use something new.
  You can seamlessly switch between models, even switching mid-conversation.

- **Structured context engineering with Metaphor**  

  By going beyond ad-hoc prompts and making your intentions clear, you can get dramatically better results and lower operating costs.
  Metaphor, Humbug’s open context and prompting language, turns intent into repeatable, composable, and auditable workflows.
  It's a language for AI orchestration.

- **Powerful, pluggable tools**  

  We designed computers to give humans better, faster, and more reliable tools.
  Humbug's tool framework sets out to do the same for AIs.
  Extends your LLMs with task delegation, dynamic filesystem operations, a clock, a custom pure functional programming language (AIFPL), and UI orchestration.
  Humbug’s tool system is flexible, secure, and designed to make it easy to add new capabilities.
  Task delegation allows one LLM to make use of one or more other LLMs.
  AIFPL (AI Functional Programming Language) is a pure functional language designed for AIs to use for calculations, string processing, or other algorithmic tasks.
  The UI supports simultaneous conversations, file editing with syntax highlights, markdown preview pages, terminal emulators, a system shell, and a system log.
  UI orchestration means your AI can help you work and visualize things using any of these tools too.
  LLMs can check the status of terminal tabs and issue commands to them (subject to user approval).
  LLMs can also control and edit files using editor tabs.
  LLMs can search and query the contents of conversation tabs.

- **AIFPL: a programming language designed for AIs**  

  Previously, programming languages were designed to meet the needs of human developers.
  While AIs are proficient with most of these languages, they risk being unsafe because they support potentially dangerous I/O operations.
  AIFPL is an experimental, Lisp-inspired language designed to suit the needs of AIs for algorithmic processing operations.
  It supports higher-order functions and tail-call optimizations.
  It supports simple calculations and string processing, all the way through to highly complex algorithmic tasks.
  Being side-effect free, AIFPL is intrinsically safe for AIs to use and does not require any user approvals.
  The AIFPL implementation offers detailed exception handling to allow AIs to debug any issues in their own code.

- **Open and extensible**  

  Add new AI backends, tools, or integrations with minimal friction.
  Humbug is open-source and highly modular.
  You don't need to worry about being locked into a vendor tool and can add new features if you want them.  

- **Mindspaces: project-centric workspaces**  

  Every project lives in its own mindspace: a persistent, context-rich environment with isolated files, settings, and conversations.

- **OS-agnostic**  

  Humbug provides OS-like concepts but doesn't try to replace your computer's operating system.
  It runs on top of Windows, macOS, or Linux, and provides a unified experience on all of them.

- **Minimal dependencies**  

  Humbug follows the pattern of most operating system kernels.
  It aims to be simple and largely self-contained.
  The code has only 4 external package dependencies other than the standard Python library, so both you and your LLMs can understand almost every part from the one git repo.

- **Not just a platform for developers**  

  It's designed to help with any activities where you and your AIs need to work together on a problem.
  While it has a lot of tools for software developers, it has been designed to support a much wider set of needs.
  With its extensibility it's also easy to think about adding new tools for AIs, humans, or both.

## Learn more

- **What's new:** [Latest updates](./CHANGELOG.md)
- **Dive deeper:** [Getting started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor)
- **Download:** [Download Humbug](https://github.com/m6r-ai/humbug/releases)
- **Blog posts:** [Dave's blog posts about Humbug, Metaphor, and AIFPL](https://davehudson.io/blog)
- **Developer notes:** [Dave's project notes](https://davehudson.io/notes)
- **AIFPL summary:** [AIFPL project page](https://davehudson.io/projects/aifpl)
- **Discord:** [Discord](https://discord.gg/GZhJ7ZtgwN)
- **YouTube:** [@m6rai on YouTube](https://youtube.com/@m6rai)

## Get involved

Humbug is open source and the project welcomes contributions. If you're interested in helping, join the Discord server.

## Requirements

- Python 3.10 or higher
- You will need API keys for most cloud-based LLMs, but some are available for free, and Ollama will run locally without API keys.
- PySide6 (the GUI framework)
- qasync (allows the GUI framework to work nicely with async Python code)
- aiohttp (async HTTP client)
- certifi (SSL/TLS root certificates to allow TLS network connections without any other system changes)

## Developer installation

1. Create and activate a virtual environment:

   Linux and macOS:

   ```bash
   python -m venv venv
   source venv/bin/activate
   ```

   Windows:

   ```bash
   python -m venv venv
   venv\Scripts\activate
   ```

2. Install build tools:

   ```bash
   pip install build
   ```

3. Install in development mode:

   ```bash
   pip install -e .
   ```

4. Launch the application:

   ```bash
   python -m humbug
   ```

5. Initial configuration:

   If you load Humbug and don't have any AIs already configured, it will guide you how to use any AI API keys you have.

   See [Getting Started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor) for a step-by-step guide to getting Humbug up and running.
