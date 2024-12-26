# Humbug

Humbug is a GUI-based application that enables interaction with AI backends through a tabbed chat interface. It supports multiple conversations, streaming responses, and configurable AI models.

## Features

- Multi-tab conversation interface
- Real-time streaming of AI responses
- Configurable AI model settings
- Command support
- Conversation transcript logging
- Cross-platform support (Windows, MacOS, Linux)

## Requirements

- Python 3.10 or higher
- OpenAI API key
- PySide6
- aiohttp

## Installation

1. Create and activate a virtual environment:

```bash
python -m venv venv
source venv/bin/activate  # Linux/MacOS
# or
venv\Scripts\activate     # Windows
```

2. Install build tools:

```bash
pip install build
```

3. Install in development mode:

```bash
pip install -e .
```

## Configuration

Set your OpenAI API key as an environment variable:

```bash
export OPENAI_API_KEY='your-api-key'  # Linux/MacOS
# or
set OPENAI_API_KEY=your-api-key       # Windows
```

## Usage

Launch the application:

```bash
python -m humbug
```

### Keyboard Shortcuts

- `Ctrl+J` - Submit message
- `Ctrl+,` - Open conversation settings
- `Esc` - Cancel current AI response
- Standard editing shortcuts (Cut, Copy, Paste, Undo, Redo)

## Development

Project structure:
```
src/humbug/
├── ai/            # AI backend implementations
├── commands/      # Command processing
├── conversation/  # Conversation management
├── gui/           # GUI components
├── transcript/    # Transcript handling
└── utils/         # Utility functions
```

## Logging

Debug logs are written to `logs/humbug_debug.log`
