# AGENTS.md - Humbug Project Directory Structure

## Overview

Humbug is a platform for human-AI collaboration, written in Python. This document describes the directory structure to help AI agents navigate the codebase.

## Tool use

- If you want to use the terminal you will require user authorization every time you send keystrokes.  If you load files into
  an editor tab, however, you don't, so if you just want to do a simple search of a file then consider using the editor
  tabs.  They can get pretty cluttered though so if you don't need the tab again then close it.
- If you open a terminal it will automatically be in the root of the mindspace directory.  Don't change directory unless
  you want to be somewhere else.
- Terminals will not open with a python virtual environment by default.  If you want a venv then you must do that yourself.


## Code generation

- When you help write code, do not write lengthy file-level docstrings.  These go stale very fast as the code evolves.
- Do not add comments marking blocks of functionality within files.  Functions, classes, etc., have docstrings so we have
  everything we need anyway and these sorts of delimeter comments simply add clutter to the code.
- If you are writing tests, the tests must reflect the correct and desired behaviour.  NEVER write or patch a test to
  mask broken implementation logic.  If the logic is wrong then a test must fail.


## Top-Level Structure

```text
humbug/
├── src/                    # Main source code
├── tests/                  # Test suite
├── tools/                  # Development and analysis tools
├── docs/                   # Documentation
├── menai_modules/          # Menai standard library modules
├── conversations/          # Example AI conversations
├── icons/                  # Application icons
└── pyproject.toml          # Python project configuration
```

## `src/` Directory

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
Core framework for AI tools and capabilities (base classes, manager, definitions, exceptions).

### `src/ai_transcript_conversation/`
AI transcript and conversation support.

### `src/clock_ai_tool/`
AI tool implementation for date/time operations.

### `src/context/`
Registry infrastructure for tracking open contexts within a mindspace.

### `src/conversation_ai_tool/`
AI tool implementation for conversation tab operations (read, search, navigate messages).

### `src/conversation_context/`
Context model for open conversation tabs (`ConversationContext`).

### `src/delegate_ai_tool/`
AI tool implementation for task delegation to child AI instances. Contains the backend
logic (conversation creation, session management, persistence, completion handling) and
the `DelegateAIListener` protocol for frontend integration. GUI wiring lives in `src/humbug/`.

### `src/diff/`
Unified diff parsing and application with fuzzy matching.

### `src/dmarkdown/`
Advanced markdown parsing to AST.

### `src/docx/`
DOCX file handling support.

### `src/editor_ai_tool/`
AI tool implementation for editor tab operations (read, search, diff, apply diffs, save).

### `src/editor_context/`
Context model for open editor tabs (`EditorContext`, `EditorDiffApplier`).

### `src/filesystem_ai_tool/`
AI tool implementation for file operations.

### `src/git/`
Lightweight, GUI-free tools for git operations.

### `src/help_ai_tool/`
AI tool implementation for tool documentation.

### `src/humbug/`
Main application and GUI components.

**Subdirectories:**
- `column/` - Column manager, tab bar, and chrome widgets
- `conversation_sidebar/` - Conversations panel implementation
- `conversation_tab/` - Conversation tab implementation
- `diff_tab/` - Diff tab implementation
- `editor_tab/` - Editor tab implementation
- `file_sidebar/` - Files panel implementation
- `file_watcher/` - Singleton file/directory polling service
- `icons/` - Application icon pack
- `language/` - Localization support
- `log_tab/` - Log tab implementation
- `markdown/` - Markdown and code rendering stack
- `preview_sidebar/` - Preview panel implementation
- `preview_tab/` - Preview tab implementation
- `search_sidebar/` - Search panel implementation
- `settings/` - Application and user settings UI widgets
- `shell_tab/` - Shell tab implementation
- `sidebar/` - Sidebar container, rail, and shared sidebar infrastructure
- `tab/` - Abstract tab contract
- `terminal_tab/` - Terminal tab implementation
- `user/` - User management
- `vcs_sidebar/` - VCS/diff panel implementation
- `widgets/` - Reusable Qt widgets

### `src/mindspace/`
Front-end-agnostic mindspace model. Manages the mindspace path, settings, search,
interactions log, and the context registry. No Qt or GUI dependencies.

### `src/menai/`
Pure functional programming language designed for AI use. Includes lexer, parser, compiler, and virtual machine.

### `src/menai_ai_tool/`
AI tool implementation for Menai language execution.

### `src/pdf/`
Pure-Python PDF text extraction (stdlib only). Parses PDF structure, decodes streams
(FlateDecode, ASCII85Decode, ASCIIHexDecode), and extracts text from content streams.
Public API: `parse(data: bytes) -> PDFDocument` and `extract_text(doc: PDFDocument) -> str`.

### `src/preview_ai_tool/`
AI tool implementation for preview tab operations (search, scroll).

### `src/preview_context/`
Context model for open preview tabs (`PreviewContext`).

### `src/syntax/`
Language-specific syntax highlighting system.

**Language subdirectories:**
- `menai/`, `c/`, `cpp/`, `csharp/`, `css/`, `diff/`, `go/`, `html/`, `java/`
- `javascript/`, `json/`, `kotlin/`, `lua/`, `markdown/`, `metaphor/`, `move/`
- `python/`, `rust/`, `scheme/`, `solidity/`, `swift/`, `text/`, `typescript/`, `xml/`

### `src/terminal/`
Cross-platform terminal emulator with Unix and Windows implementations.

### `src/terminal_ai_tool/`
AI tool implementation for terminal tab operations (read, write, status).

### `src/terminal_context/`
Context model for open terminal tabs (`TerminalContext`).

## `tests/` Directory

Test structure mirrors `src/` organization:
- `ai_transcript_conversation/` - AI transcript/conversation tests
- `clock_ai_tool/` - Clock AI tool tests
- `diff/` - Diff system tests
- `docx/` - DOCX handling tests
- `dmarkdown/` - Markdown parser tests
- `filesystem_ai_tool/` - Filesystem AI tool tests
- `menai/` - Menai language tests
- `menai_ai_tool/` - Menai AI tool tests
- `pdf/` - PDF extraction tests
- `syntax/` - Syntax highlighting tests
- `terminal/` - Terminal emulator tests

## `tools/` Directory

Development and debugging utilities:
- `menai/benchmark/` - Menai performance benchmarking
- `menai/checker/` - Static analysis
- `menai/disassembler/` - Bytecode disassembly
- `menai/pretty-print/` - Code formatting
- `menai/profiler/` - Menai profiling
- `menai/test-runner/` - Menai test runner
- `dependency_checker/` - Module dependency validation
- `docx/` - DOCX-related tooling
- `md_to_docx/` - Markdown to DOCX conversion
- `pdf/` - PDF-related tooling
- `pipeline-runner/` - Pipeline execution
- `planner/` - Project planning
- `reformat_c/` - C code reformatting

## Key Documentation Files

- `README.md` - Project overview
- `CHANGELOG.md` - Version history
- `src/menai/README.md` - Menai implementation overview and Python API
- `src/menai/AGENTS.md` - Guide for AIs working on the Menai implementation
- Component-specific README.md files in subdirectories
