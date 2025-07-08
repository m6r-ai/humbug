# Humbug: an AI-native collaborative workspace

Build, reason, and create, side by side with AI.

## What is Humbug?

Humbug is a modular, extensible platform that unifies your technical workflows with the power of AI.
Think of it as an operating system for human-AI collaboration.
It's a workspace where you and your AI colleagues manage code, documents, and ideas together.

![Humbug in action](docs/v0.18-demo.gif)

## Why Humbug?

- **AI as a first-class citizen**  
  Humbug lets you work with multiple large language models (LLMs) simultaneously, supporting local, cloud-based, or hybrids of both.
  Compare outputs, orchestrate tools, and explore new solutions with AIs as flexible, autonomous teammates.

- **Mindspaces: project-centric workspaces**  
  Every project lives in its own mindspace: a persistent, context-rich environment with isolated files, settings, and conversations.
  Switch between projects as easily as you switch between desktops.

- **Structured context engineering with Metaphor**  
  Go beyond ad-hoc prompts and make your intentions clear to get dramatically better results and lower operating costs.
  Metaphor, Humbug’s open context and prompting language, turns intent into repeatable, composable, and auditable workflows.
  Think of it like a shell language for AI orchestration.

- **Unified interface for building, **  
  Interact through a modern GUI with multi-pane editing, real-time AI conversations, and rich documentation.
  Or drop into a programmable terminal for full control.

- **Powerful, pluggable tools**  
  Extend your LLMs with dynamic file operations, a scientific calculator, system clock access, UI orchestration, Markdown parsing, syntax highlighting, and more.
  Humbug’s tool system makes it easy to add new capabilities for both human and AI use.

- **Designed for extensibility**  
  Humbug is open-source and modular by design.
  Add new AI backends, tools, or integrations with minimal friction.

![Humbug in action](docs/v0.18-explore.gif)

## What sets Humbug apart?

- **OS-like modularity without the baggage**  
  Humbug brings isolation, concurrent processes, and the extensibility of an OS, without trying to replace your real operating system.
  It sits on top of Windows, macOS, or Linux, giving a unified experience across all of them.

- **Human–AI collaboration, engineered**  
  When you're using AI, you're no longer working alone.
  Both humans and AIs are first-class actors in Humbug.
  Share a mindspace, exchange ideas, and use the same tools to get work done.

- **Adaptable to many problems.**  
  Whether you’re scoping, prototyping, refactoring, extending, maintaining, documenting, or experimenting, Humbug gives you a new way to work.

- **AI agnostic.**  
  Works with LLMs from Anthropic, DeepSeek, Google, Mistral, Ollama, OpenAI, and xAI.
  Seamlessly switch between models.
  You can even switch model mid-conversation.

- **Built by bootstrapping.**
  Each version of Humbug has been built using the previous version.
  Over 80% of the code was implemented by LLMs, allowing the designers to focus on functionality and maintaining a clean architecture.

- **What you see is what you get.**
  Humbug has only 4 package dependencies other than the standard Python library.
  You can understand almost every part from the one git repo.

## Learn more

- **What's new:** [Latest updates](./CHANGELOG.md)
- **Dive deeper:** [Getting started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor)
- **Download:** [Download Humbug](https://github.com/m6r-ai/humbug/releases)
- **Discord:** [Discord](https://discord.gg/GZhJ7ZtgwN)
- **YouTube:** [@m6rai on YouTube](https://youtube.com/@m6rai)

## Requirements

- Python 3.10 or higher
- You will need API keys for any AI models other than Ollama
- PySide6 (the GUI framework)
- qasync (allows the GUI framework to work nicely with async Python code)
- aiohttp (async HTTP client)
- certifi (SSL/TLS root certificates to allow TLS network connections without any other system changes)

## Developer installation

1. Create and activate a virtual environment:

   Linux and MacOS:

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

   See [Getting Started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor) for a step-by-step guide to getting Humbug up and running.
