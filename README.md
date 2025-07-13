# Humbug: an AI-native collaborative workspace

Build, reason, and create, side by side with AI.

## What is Humbug?

Humbug is a modular, extensible platform that augments your workflows with AI.
Think of it as being like an operating system for human-AI collaboration.
It's a workspace where you and your AI colleagues work on ideas together.

If you know exactly what you want it has tools to let you do those things, but if you're not sure then you can let your AI
take control and guide you through what you want to do.

Humbug lets you work on lots of different types of problems, but it shines at supporting you build software.

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

- **Unified interface for building**  
  Interact through a GUI with multi-pane editing, real-time AI conversations, and rich documentation.
  Aternatively, drop into a programmable terminal for full control.

- **Powerful, pluggable tools**  
  Extend your LLMs with dynamic file operations, a scientific calculator, system clock access, UI orchestration, Markdown parsing, syntax highlighting, and more.
  Humbug’s tool system makes it easy to add new capabilities for both human and AI use.

- **Designed for extensibility**  
  Humbug is open-source and modular by design.
  Add new AI backends, tools, or integrations with minimal friction.

![Humbug in action](docs/v0.18-explore.gif)

## What sets Humbug apart?

- **OS-like abstraction and modularity without the baggage**  
  Humbug provides OS-like concepts without trying to replace your real operating system.
  It unifies AI and UI interactions so it's easy to add tools that can be used by any AI, any user, or both.
  It sits on top of Windows, macOS, or Linux, and provides a unified experience on all of them.

- **Human–AI collaboration at the core**  
  When you're using AI, you're no longer working alone.
  Both humans and AIs are first-class actors in Humbug.
  Share a mindspace, exchange ideas, and use the same tools to get work done.

- **AI agnostic.**  
  Works with LLMs from Anthropic, DeepSeek, Google, Mistral, Ollama, OpenAI, and xAI.
  Seamlessly switch between models.
  You can even switch model mid-conversation.
  You're not tied to any one provider, can optimize for cost, and are future-proofed when you want to use something new.

- **Built by bootstrapping.**
  Each version of Humbug has been built using the previous version.
  Over 80% of the code was implemented by LLMs, allowing the designers to focus on functionality and maintaining a clean architecture.

- **What you see is what you get.**
  Humbug has only 4 package dependencies other than the standard Python library.
  You can understand almost every part from the one git repo.

- **Not just a platform for developers.**
  It's designed to help with any activities where you and your AIs need to work together on a problem.
  While it has a lot of tools for software developers, it has been designed to support a much wider set of needs.
  With its extensibility it's also easy to think about adding new tools for AIs, humans, or both.

## Learn more

- **What's new:** [Latest updates](./CHANGELOG.md)
- **Dive deeper:** [Getting started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor)
- **Download:** [Download Humbug](https://github.com/m6r-ai/humbug/releases)
- **Discord:** [Discord](https://discord.gg/GZhJ7ZtgwN)
- **YouTube:** [@m6rai on YouTube](https://youtube.com/@m6rai)

## Get involved!

Humbug is open source and the project welcomes contributions.  If you're interested in how then join the Discover server.

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

## Navigating the code

The code is designed to be modular to make it easy to understand.
Humbug itself includes a wiki-like viewer so if you clone this git repo you can simply create a mindspace with your humbug directory as the root of the mindspace.
Open it up and click the top-level `humbug` folder and you'll see this `README.md` file.
Any folders with their own `README.md` will give you pointers about what's in that folder.
