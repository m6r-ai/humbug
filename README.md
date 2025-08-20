# Humbug: building an operating system for human-AI collaboration

Humbug is a modular, extensible platform that aims to let you and your AIs work on ideas together.
Think of it as an operating system for human-AI collaboration.

It's designed to let you work on lots of different types of problems, but it currently shines at supporting you build software.
A demonstration is that each version has been built using earlier versions of itself!

![Humbug in action](docs/v0.18-demo.gif)

## What are the core ideas?

- **Human–AI collaboration at the core**  

  When you're using AI, you're no longer working alone.
  Humbug treats both humans and AIs as first-class actors.
  All tools, including the GUI, are designed to be available for both to use, so it's faster and easier to get things done.

- **LLMs, lots of LLMs**  

  Humbug lets you work with multiple large language models (LLMs) simultaneously, supporting local, cloud-based, or hybrids of both.
  Works with LLMs from Anthropic, DeepSeek, Google, Mistral, Ollama, OpenAI, xAI, and Z.ai so you're not tied to any one provider.
  You can optimize for cost, and are future-proofed when you want to use something new.
  You can seamlessly switch between models, even switching mid-conversation.

- **Mindspaces: project-centric workspaces**  

  Every project lives in its own mindspace: a persistent, context-rich environment with isolated files, settings, and conversations.

- **Structured context engineering with Metaphor**  

  By going beyond ad-hoc prompts and making your intentions clear, you can get dramatically better results and lower operating costs.
  Metaphor, Humbug’s open context and prompting language, turns intent into repeatable, composable, and auditable workflows.
  It's a language for AI orchestration.

- **Powerful, pluggable tools**  

  Extends your LLMs with task delegation, dynamic filesystem operations, a clock, a scientific calculator, and UI orchestration.
  Humbug’s tool system is flexible, secure, and designed to make it easy to add new capabilities.
  Task delegation allows one LLM to make use of one or more other LLS.
  The UI supports simultaneous conversations, file editing with syntax highlights, dynamic wiki pages, terminal emulators, a system shell, and a system log.
  UI orchestration means your AI can help you work and visualise things using any of these tools too.

- **Open and extensible**  

  Add new AI backends, tools, or integrations with minimal friction.
  Humbug is open-source and modular by design.
  You don't need to worry about being locked into a vendor tool and can add new features if you want them.  

- **Bootstrapped with LLMs**  

  Each version of Humbug has been built using the previous version.
  This allowed over 80% of the code to be implemented by LLMs.

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

![Humbug in action](docs/v0.18-explore.gif)

## Learn more

- **What's new:** [Latest updates](./CHANGELOG.md)
- **Dive deeper:** [Getting started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor)
- **Download:** [Download Humbug](https://github.com/m6r-ai/humbug/releases)
- **Blog posts:** [Dave's blog posts about Humbug and Metaphor](https://davehudson.io/blog)
- **Developer notes:** [Dave's project notes](https://davehudson.io/notes)
- **Discord:** [Discord](https://discord.gg/GZhJ7ZtgwN)
- **YouTube:** [@m6rai on YouTube](https://youtube.com/@m6rai)

## Get involved!

Humbug is open source and the project welcomes contributions.  If you're interested in helping, then join the Discord server.

## Requirements

- Python 3.10 or higher
- You will need API keys for most cloud-based LLMs, but some are available for free, and Ollama will run locally without API keys.
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

Humbug itself includes a wiki-like viewer so if you clone this git repo you can simply create a mindspace with your humbug directory as the root of the mindspace.
Open it up and click the top-level `humbug` folder and you'll see this `README.md` file.
Any folders with their own `README.md` will give you pointers about what's in that folder.
