# AGENTS.md - Humbug Project Structure

## Overview

Humbug is a platform for human-AI collaboration, written in Python. This document describes how AI agents should navigate the codebase.

## Tool use

- If you want to use the terminal you will require user authorization every time you send keystrokes.  If you load files into
  an editor tab, however, you don't.  If you just want to do a simple search of a file then consider using the editor
  tabs.  They can get pretty cluttered though so if you don't need the tab again then close it.
- If you open a terminal it will automatically be in the root of the mindspace directory.  Don't change directory unless
  you want to be somewhere else.
- Terminals will not open with a python virtual environment by default.  The venv is at `venv/` in the mindspace root.

## Code quality

- Before considering any code change complete, run the full suite of static analysis tools:
  ```bash
  source venv/bin/activate && python -m tools.code_checker
  ```
  All checks must pass cleanly before the work is done.

## Code generation

- When you help write code, do not write lengthy file-level docstrings.  These go stale very fast as the code evolves.
- Do not add comments marking blocks of functionality within files.  Functions, classes, etc., have docstrings so we have
  everything we need anyway and these sorts of delimeter comments simply add clutter to the code.
- If you are writing tests, the tests must reflect the correct and desired behaviour.  NEVER write or patch a test to
  mask broken implementation logic.  If the logic is wrong then a test must fail.
- Test docstrings must describe the expected correct behaviour only.  They must not reference previously broken
  behaviour, historical bugs, or implementation details of past fixes.  A test is a specification, not a changelog.
- Never write block comments using lines of dashes.  E.g never do this:
  ```python
  # -----------------------------------------------------------
  # This is a block level comment because I like wasting tokens
  # -----------------------------------------------------------
  ```
  Functions/methods have doc strings and we don't need comments about grouping of things because they go stale.
- We use modern Python, so never use `Optional`, always use `type | None`.
- Do not use `@property`.  Simple getter methods (e.g. `def foo(self) -> T:`) are used instead.

## Code restructuring

- When restructuring or rewriting code you MUST NOT remove comments.  If a comment is likely or definitely wrong then draw the
  user's attention to it and seek advice.

## Top-Level structure

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
the `DelegateAIListener` protocol for frontend integration. GUI wiring lives in `src/desktop/`.

### `src/desktop/`
Qt desktop front-end for Humbug. This is the graphical desktop application built on top of
the front-end-agnostic modules (`mindspace`, the `*_context` modules, `ai`, the `*_ai_tool`
modules, etc.). It is one of potentially several front-ends (e.g. a future CLI).

**Subdirectories:**
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
- `mindspace/` - Qt-facing mindspace singleton (`MindspaceManager`), directory tracking, VCS polling, and mindspace folder management
- `preview_sidebar/` - Preview panel implementation
- `preview_tab/` - Preview tab implementation
- `search_sidebar/` - Search panel implementation
- `settings/` - Application and user settings UI widgets
- `shell_tab/` - Shell tab implementation
- `sidebar/` - Shared sidebar component infrastructure (breadcrumbs, tree views, delegates, pane styles) used by individual `*_sidebar/` panels
- `sidebar_manager/` - Sidebar manager: generic sidebar container with panel registration
- `tab/` - Abstract tab contract
- `tab_manager/` - Tab manager: generic tab container with tab factory registration
- `terminal_tab/` - Terminal tab implementation
- `user/` - User management
- `vcs_sidebar/` - VCS/diff panel implementation
- `widgets/` - Reusable Qt widgets

### `src/dhtml/`
HTML document processing. Includes a self-contained lexer and DOM-building parser
(no external dependencies), a plain-text extractor, and converters to/from the
`document_ir` intermediate representation. Supports HTML5 implied-close rules and
void elements. Used by the filesystem tool (transparent text extraction on read)
and the document converter tool (HTML ↔ other formats).

### `src/diff/`
Unified diff parsing and application with fuzzy matching.

### `src/dmarkdown/`
Advanced markdown support, including converting to/from a markdown AST.
Includes `doc_ir_to_markdown.py` for serialising a doc_ir tree to Markdown text.

### `src/document_converter_ai_tool/`
AI tool implementation for converting documents between supported formats (e.g. Markdown ↔ DOCX).
Both input and output paths must be inside the mindspace. Write operations require user authorization
and are audit-logged. Conversion pipelines are built on `src/dmarkdown/` and `src/docx/`.

### `src/document_ir`
Abstract document IR.

### `src/docx/`
DOCX file handling support, including converting to/from a DOCX AST.

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
- `ai/` - AI backend tests
- `ai_transcript_conversation/` - AI transcript/conversation tests
- `clock_ai_tool/` - Clock AI tool tests
- `diff/` - Diff system tests
- `dhtml/` - HTML processing tests
- `docx/` - DOCX handling tests
- `document_converter_ai_tool/` - Document converter AI tool tests
- `document_ir/` - Document IR tests
- `dmarkdown/` - Markdown parser tests
- `editor_ai_tool/` - Editor AI tool tests
- `filesystem_ai_tool/` - Filesystem AI tool tests
- `menai/` - Menai language tests
- `menai_ai_tool/` - Menai AI tool tests
- `pdf/` - PDF extraction tests
- `syntax/` - Syntax highlighting tests
- `terminal/` - Terminal emulator tests

## `tools/` Directory

Development and debugging utilities:
- `__init__.py` — makes `tools/` a package so mypy resolves all tool subpackages as `tools.*`
- `menai/benchmark/` - Menai performance benchmarking
- `menai/checker/` - Static analysis
- `menai/disassembler/` - Bytecode disassembly
- `menai/pretty-print/` - Code formatting
- `menai/profiler/` - Menai profiling
- `menai/test-runner/` - Menai test runner
- `convert_document/` - Document conversion between docx, html, and md formats
- `code_checker/` - Runs all static analysis tools (dependency checker, mypy, pylint) in sequence
- `dependency_checker/` - Module dependency validation
- `pdf/` - PDF-related tooling
- `pipeline-runner/` - Pipeline execution

## Key Documentation Files

- `README.md` - Project overview
- `CHANGELOG.md` - Version history
- `src/menai/README.md` - Menai implementation overview and Python API
- `src/menai/AGENTS.md` - Guide for AIs working on the Menai implementation
- Component-specific README.md files in subdirectories
