# Installation & First Launch

Humbug runs on Windows, macOS, and Linux. There are two ways to install it:

- **Download a release** — the simplest option for most users
- **Install from source** — if you want to run the very latest code or contribute to the project

---

## Option 1: Download a release

Pre-built releases are available on the
[Humbug releases page](https://github.com/m6r-ai/humbug/releases).

Download the appropriate package for your platform and follow the installation instructions
included with the release.

---

## Option 2: Install from source

Before installing from source, make sure you have **Python 3.10 or higher** installed on your system. You can
check by running:

```text
python --version
```

or, on some systems:

```text
python3 --version
```

**Windows only:** You will need
[Microsoft C++ Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools/) (Visual
C++ 14.0 or later) to compile the Menai language engine. During installation, select the
**"Desktop development with C++"** workload. This is a one-time setup.

**Linux only:** You will need Python development headers and a C compiler. On Debian and Ubuntu:

```text
sudo apt install gcc python3-dev
```

On other distributions, install the equivalent packages for your package manager.

**macOS:** No additional requirements beyond Python itself.

### 1. Get the source code

Clone the repository from GitHub:

```text
git clone https://github.com/m6r-ai/humbug.git
cd humbug
```

Or download and unzip a source archive from the
[releases page](https://github.com/m6r-ai/humbug/releases).

### 2. Create a virtual environment

It is strongly recommended to use a Python virtual environment to keep Humbug's dependencies
separate from the rest of your system.

**macOS and Linux:**

```text
python3 -m venv venv
source venv/bin/activate
```

**Windows:**

```text
python -m venv venv
venv\Scripts\activate
```

### 3. Install dependencies

```text
pip install -e ".[dev]"
```

This installs Humbug along with all its dependencies. Humbug has very few external dependencies
by design: the GUI framework (PySide6), async support (qasync), and SSL certificates (certifi).

### 4. Build the Menai engine (optional but recommended)

Humbug includes a fast C implementation of the Menai language engine. Building it is optional —
Humbug will fall back to a slower Python implementation if it is not present — but it is
recommended for the best experience.

```text
python setup.py build_ext --inplace
```

### 5. Launch Humbug

```text
python -m desktop
```

---

## First launch

The first time you launch Humbug, you will see the welcome screen. If you have not yet configured
any AI providers, Humbug will let you know and guide you to the settings where you can add your
API keys.

### Setting up AI providers

To have conversations with AI, you need credentials for at least one AI provider. Humbug supports
providers including Anthropic, DeepSeek, Google, Mistral, OpenAI, and xAI, as well as local
models running through Ollama (which requires no API key).

To configure a provider:

1. Open **Settings** using the settings icon in the sidebar, or from the application menu
2. Go to the **AI Backends** section
3. Enable the providers you want to use and enter your API keys

You only need to do this once. Your settings are saved and will be available every time you launch
Humbug.

### Opening or creating a mindspace

When Humbug first opens, it will either restore your most recent mindspace or prompt you to open
one. A mindspace is simply a folder on your computer that Humbug will use to store your
conversations and project files.

To open an existing folder as a mindspace, or to create a new one, click the mindspace name shown
at the top of the left panel.

---

*[Index](index.md) · Previous: [What is Humbug?](what-is-humbug.md) · Next: [Your First Conversation with an AI](first-conversation.md)*
