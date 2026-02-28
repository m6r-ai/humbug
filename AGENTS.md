# AGENTS.md - Humbug Project Directory Structure

## Overview

Humbug is a platform for human-AI collaboration, written in Python. This document describes the directory structure to help AI agents navigate the codebase.

## Tool use

- If you want to use the terminal you will require user authorization every time you send keystrokes.  If you load files into
  an editor tab, however, you don't, so if you just want to do a simple search of a file then consider using the editor
  tabs.  They can get pretty cluttered though so if you don't need the tab again then close it.
- If you open a terminal it will automatically be in the root of the mindspace directory.  Don't change directory unless
  you want to be somewhere else.
- Terminals will not open with a python virtual environment by default.  If you want a venv then you have to do that yourself.


## Top-Level Structure

```
humbug/
├── src/                    # Main source code
├── tests/                  # Test suite
├── tools/                  # Development and analysis tools
├── docs/                   # Documentation
├── menai_modules/          # Menai standard library modules
├── conversations/          # Example AI conversations
├── icons/                  # Application icons
├── metaphor/               # Metaphor prompt examples
└── pyproject.toml          # Python project configuration
```

## `src/` Directory

### `src/humbug/`
Main application and GUI components.

**Subdirectories:**
- `tabs/` - Tab management (conversations, editors, terminals, preview, log)
- `mindspace/` - Project workspace management
- `settings/` - Application and user settings
- `user/` - User management
- `language/` - Localization support

### `src/ai/`
Multi-backend AI conversation system.

**Backend subdirectories:**
- `anthropic/` - Claude models
- `deepseek/` - DeepSeek models
- `google/` - Gemini models
- `mistral/` - Mistral models
- `ollama/` - Local Ollama models
- `openai/` - GPT models
- `vllm/` - vLLM server integration
- `xai/` - xAI (Grok) models
- `zai/` - Z.ai models

### `src/ai_tool/`
Framework for AI tools and capabilities.

**Tool subdirectories:**
- `menai/` - Menai language execution
- `clock/` - Date/time operations
- `filesystem/` - File operations
- `help/` - Tool documentation

### `src/menai/`
Pure functional programming language designed for AI use. Includes lexer, parser, compiler, and virtual machine.

### `src/ai_conversation_transcript/`
Handles saving and loading AI conversation history.

### `src/diff/`
Unified diff parsing and application with fuzzy matching.

### `src/dmarkdown/`
Advanced markdown parsing to AST.

### `src/metaphor/`
Context and prompting language for AI orchestration.

### `src/dast/`
Dependency AST for analyzing module dependencies.

### `src/syntax/`
Language-specific syntax highlighting system.

**Language subdirectories:**
- `menai/`, `c/`, `cpp/`, `csharp/`, `css/`, `diff/`, `go/`, `html/`, `java/`
- `javascript/`, `json/`, `kotlin/`, `lua/`, `markdown/`, `metaphor/`, `move/`
- `python/`, `rust/`, `scheme/`, `solidity/`, `swift/`, `text/`, `typescript/`, `xml/`

### `src/terminal/`
Cross-platform terminal emulator with Unix and Windows implementations.

## `tests/` Directory

Test structure mirrors `src/` organization:
- `ai_tool/` - AI tool tests
- `menai/` - Menai language tests
- `diff/` - Diff system tests
- `dmarkdown/` - Markdown parser tests
- `metaphor/` - Metaphor language tests
- `syntax/` - Syntax highlighting tests

## `tools/` Directory

Development and debugging utilities:
- `menai_benchmark/` - Menai performance benchmarking
- `menai_bytecode_analyzer/` - Bytecode inspection
- `menai_checker/` - Static analysis
- `menai_disassembler/` - Bytecode disassembly
- `menai_pretty_print/` - Code formatting
- `dependency_checker/` - Module dependency validation
- `planner/` - Project planning

## Key Documentation Files

- `README.md` - Project overview
- `CHANGELOG.md` - Version history
- `src/menai/README.md` - Menai implementation overview and Python API
- `src/menai/AGENTS.md` - Guide for AIs working on the Menai implementation
- Component-specific README.md files in subdirectories
