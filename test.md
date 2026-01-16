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

   ```python
   for i in range(10):
       print("hello")
   ```

5. Initial configuration:

   See [Getting Started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor) for a step-by-step guide to getting Humbug up and running.
