# Humbug v0.15

Humbug is an extensible open-source tool designed to help you design, build and maintain software using AI.

Whether you're a human or an AI, the most important things in software development are understanding the
problem you're trying to solve and the software you've building with to solve it.  Humbug is designed to
help you with both.

For human developers, Humbug gives you a dynamic wiki-like view into your software and lets you to
discuss any aspect of it with an AI.  You can also preview files, edit files, and run commands in the
terminal.

For AIs, Humbug lets you create complex prompts that capture all the context required to do complex design and
implementation work.

Humbug supports large language models (LLMs) from Anthropic, DeepSeek, Google, Mistral, Ollama, OpenAI, and xAI,
and you have total flexibility over which one you use at any time.  You can use 2 or more at
the same time, so if you want multiple opinions then it's easy to get them.

While Humbug lets you use cloud-based tools, it runs locally on your computer.  Everything it does is captured
in local files so you always retain complete control over your projects.  It's up to you how you structure them,
how you handle version management, and where you run your AI tools.

## Metaphor - solving the AI context problem

The key to getting great results from AIs is to ensure they have the right context to work with.

To do this, Humbug uses [Metaphor](https://m6r.ai/metaphor), a simple, open source, natural-language-based prompt creation
language.  Metaphor is very quick to learn (just 5 keywords) and is structured to let you be clear about what you want
your AI to do for you, so it's not left guessing or hallucinating.

By making prompt creation a repeatable engineering process it's easy to try new ideas, and iterate towards what you
want.  If you don't like something, you can make adjustments to your Metaphor scripts and try again.  If you want to
see if a different AI models will give you better results you can do that too!

Read [Getting started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor) to find out more.

While Metaphor is designed to help start your conversations with AI, and Humbug is the tool designed to get the most out of
those conversations.  It lets you control and capture any aspect of those conversations and incorporate the results into
your software and documentation.  The built-in editor and terminal also lets you update and test anything you get back from
the AI, and provides you access to any command line tools you might want to use.

![Humbug in action](./docs/humbug-v0.12-1.webp)

![Humbug in action](./docs/humbug-v0.12-2.webp)

![Humbug in action](./docs/humbug-v0.12-3.webp)

![Humbug in action](./docs/humbug-v0.12-4.webp)

![Humbug in action](./docs/humbug-v0.12-5.webp)

## What does that mean in practice?

Providing an AI everything it needs lets Metaphor stretch an AI model way beyond code completions.  This approach
has been used throughout Humbug's development.

In most versions there have been many 100+ line changes to the code across multiple files that were all generated from
single Metaphor prompts.  The biggest single AI-coded change was in v0.13, where 1600 lines of code were generated
by Claude Sonnet 3.7.  That change had one bug that took 5 minutes to resolve!

Well over 80% of Humbug has been built and maintained by AI using Metaphor.

The ability to use AI for large refactoring and design tasks also means Humbug is a codebase that has been able to
evolve very fast but with very little technical debt.

One fun aspect of using Metaphor is you can use Humbug to become an expert on its own design.  If you use the
Metaphor script `humbug-expert.m6r` and run this with a model that has a very large context window (e.g. Gemini)
then you can ask the AI about the design or implementation of the software!

## Engineering over vibes!

Naive vibe coding can be a great way to experiment and try out ideas but can be incredibly frustrating for experienced
engineers.  It can be an incredible way to support prototyping and brainstorming, but high quality code requires
careful planning, review, and understanding.

By offering a solid grounding context, Metaphor and Humbug aim to provide a way to capture the learnings from
vibe coding into something that can be used in a more predictable way.  You can start from our reference context,
explore interactively, update the context and iterate.  You can go further than normal vibe coding because you can also
have conversations with the AI about what it doesn't understand, or what else it might need.  All that feedback
can be used to refine the Metaphor context.

## What's new in v0.16

TBD

## Features

### Cross-platform support

Compatible with MacOS X, Linux, and Windows 10/11.

### AI interaction

- Real-time streaming of AI responses.
- Configurable AI model settings per conversation.
- Supports Anthropic, DeepSeek, Google, Mistral, Ollama, OpenAI, and xAI models.
- Configurable temperature settings for supported models.
- Handles reasoning outputs for models that support them.
- Error handling and retry mechanisms for API requests.

### Conversation features

- Markdown-style code formatting in input and history.
- Message history with distinct cards for user, AI reasoning, AI response, and system messages.
- Code blocks are broken into sections and it's easy to copy a whole section or save it as a file.
- Copy or save whole messages as Markdown.
- Full text search across all parts of a conversation.
- Bookmarks messages for rapid navigation.
- Syntax highlighting for various languages and file formats, including nested systax highlighting where code for
  one language appears inside a different one.  Currently supported: C, C++, C#, CSS, Go, HTML, Java,
  JavaScript, JSON, Kotlin, Markdown, Metaphor, Move, Python, Rust, Scheme, Solidity, Swift, and TypeScript.

### System shell

- Command line interface, giving access to a variety of features within Humbug.
- Using the `m6rc` command withinn the shell will automatically start a conversation if the Metaphor prompt compiles
  successfully.
- Shell commands support context-aware command line completions using the tab key.
- For more information, open select "Show System Shell" from the View menu and enter the command `help`.

### File editing

- Full file editing with syntax highlighting.
- Auto-backup functionality for unsaved changes.

### Terminal emulator

- Support for accessing the local shells/command prompts (Unix shells, or Windows command prompt).
- Full text search across the terminal history.

### Multi-tab interface

- Tabs can be arranged into multiple columns, with columns being able to split, merge and swap.  It's a natural way to
  discuss what you want with an AI and also look at the code you're working with.
- Tabs can be easily rearranged and closed.
- Drag and drop between columns.

### Mindspace management

- Project-specific environments with their own settings and state.
- Mindspace settings include language, soft tabs, tab size, font size, and auto-backup options.
- Mindspace state persistence for restoring open tabs and cursor positions.
- Home directory tracking of last opened mindspace.

### Multi-language support

- Humbug can be configured to use different human languages.  Currently supported English, French,
  and Arabic.

## Getting started

Humbug can use an Ollama model running locally on your system but the best results currently come from one of
the cloud-based AIs.  To use them you'll need to get an API key (they're available from the various AI
provider websites).  Most of them require you to pay for access, but Google and Mistral both currently offer
free API keys for low volume testing, so you can get started with either, or both, of them.

## Requirements

- Python 3.10 or higher
- You will need API keys for any AI models other than Ollama
- PySide6 (the GUI framework)
- qasync (allows the GUI framework to work nicely with async Python code)
- aiohttp (async HTTP client)
- certifi (SSL/TLS root certificates to allow TLS network connections without any other system changes)

## Installation

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

## Initial configuration

The main configuration you require is to set up your API keys for whichever AI services you wish to use.
As of v0.8 you can do this by opening the "User Settings" dialog on the Humbug menu.

Ollama does not need API keys as Humbug assumes it's running locally on your computer.

## Usage

Launch the application:

```bash
python -m humbug
```

## Development

Project structure:

```text
src/humbug/
╷
├── ai/            # AI backend and conversation implementations
├── ast/           # Abstract syntax tree implementation
├── gui/           # GUI components
├── language/      # I18n management and strings
├── markdown/      # Markdown parser
├── metaphor/      # Metaphor parser and compiler
├── mindspace/     # Mindspace management
├── syntax/        # Syntax highlighting and parsing
├── terminal/      # Terminal emulation
├── user/          # User settings management
└── __main__.py    # Main entry point
```

## Logging

Debug logs are written to `~/.humbug/logs/` with timestamped filenames. The application maintains the last 50 log files, rotating them when they exceed 1MB.
Please note this is in your home directory, not the `.humbug` directory that you get in a mindspace.

## Licensing

The software is released under an Apache 2.0 open source license.

## Find out more on YouTube

Find out more about Humbug and Metaphor on YouTube: [@m6rai on YouTube](https://youtube.com/@m6rai).

## Join us on Discord

Join us on [Discord](https://discord.gg/GZhJ7ZtgwN) to get help or share your ideas.

## More information

You can find out more about Metaphor and some of the things that have been done with it here:

- [Getting started with Metaphor](https://github.com:/m6r-ai/getting-started-with-metaphor)
- [m6rclib (Metaphor compiler library)](https://github.com:/m6r-ai/m6rclib)
- [m6rc (Stand-alone Metaphor compiler)](https://github.com:/m6r-ai/m6rc)
- [commit-critic (Code review tool)](https://github.com:/m6r-ai/commit-critic)
- [demo-blog-editor (How we code up blog posts)](https://github.com:/m6r-ai/demo-blog-editor)

## Commercial support

Humbug and Metaphor are supported by M6R Ltd.  To find out more please head over to [m6r.ai](https://m6r.ai)

## Contributing

Contributions are welcome! Please submit a pull request with your proposed changes.
