# Markdown Module

This module provides a comprehensive markdown parser that builds an Abstract Syntax Tree (AST) from markdown text. It supports incremental parsing, full CommonMark compatibility, and various markdown extensions.

## Files

### `markdown_ast_builder.py`
The core parser that constructs an AST from markdown text:

- **`MarkdownASTBuilder`** - Main builder class for parsing markdown into AST
  - Handles incremental parsing with line-by-line processing
  - Supports all major markdown elements (headings, lists, tables, code blocks, etc.)
  - Manages complex state tracking for nested structures
  - Provides line number mapping for editor integration
  - Handles table parsing with proper alignment detection
  - Supports code block syntax highlighting through language detection

- **`TableBufferState`** - Manages table parsing state and buffering
- **`ListState`** - Tracks nested list structures during parsing

### `markdown_ast_node.py`
Defines all AST node types for markdown elements:

- **`MarkdownASTNode`** - Base class for all markdown AST nodes
- **`MarkdownDocumentNode`** - Root document node
- **`MarkdownTextNode`** - Plain text content
- **`MarkdownParagraphNode`** - Paragraph blocks
- **`MarkdownHeadingNode`** - Headings (H1-H6) with anchor ID generation
- **`MarkdownEmphasisNode`** - Italic text (*text*)
- **`MarkdownBoldNode`** - Bold text (**text**)
- **`MarkdownInlineCodeNode`** - Inline code (`code`)
- **`MarkdownCodeBlockNode`** - Code blocks with language support
- **`MarkdownListNode`** - Base for list types
- **`MarkdownOrderedListNode`** - Numbered lists
- **`MarkdownUnorderedListNode`** - Bullet lists
- **`MarkdownListItemNode`** - Individual list items
- **`MarkdownTableNode`** - Table structures
- **`MarkdownTableHeaderNode`** - Table headers
- **`MarkdownTableBodyNode`** - Table body
- **`MarkdownTableRowNode`** - Table rows
- **`MarkdownTableCellNode`** - Table cells with alignment
- **`MarkdownLinkNode`** - Links with URL and title
- **`MarkdownImageNode`** - Images with alt text
- **`MarkdownHorizontalRuleNode`** - Horizontal rules
- **`MarkdownLineBreakNode`** - Line breaks

### `markdown_ast_printer.py`
Converts AST back to markdown text:

- **`MarkdownASTPrinter`** - Visitor that renders AST nodes back to markdown
  - Maintains proper markdown formatting
  - Handles nested structures correctly
  - Preserves original formatting where possible
  - Supports custom formatting options

### `markdown_converter.py`
High-level interface for markdown processing:

- **`MarkdownConverter`** - Main interface for markdown conversion
  - Provides simple API for parsing and rendering
  - Handles configuration options
  - Manages parser state and settings

## Key Features

### Comprehensive Markdown Support
- **Headings**: All levels (H1-H6) with automatic anchor ID generation
- **Text Formatting**: Bold, italic, inline code with proper nesting
- **Lists**: Ordered and unordered lists with proper nesting and tight/loose detection
- **Code Blocks**: Fenced code blocks with language detection and syntax highlighting integration
- **Tables**: Full table support with alignment detection and proper parsing
- **Links and Images**: Full support for links and images with titles
- **Horizontal Rules**: Standard horizontal rule support
- **Line Breaks**: Proper handling of line breaks and paragraphs

### Advanced Parsing Features
- **Incremental Updates**: Efficient incremental parsing for editor integration
- **Line Mapping**: Maintains mapping between source lines and AST nodes
- **State Management**: Complex state tracking for nested structures
- **Error Recovery**: Robust error handling and recovery
- **Table Buffering**: Sophisticated table parsing with proper validation

### Integration Features
- **Syntax Highlighting**: Integration with syntax highlighting for code blocks
- **Parser Registry**: Integration with language-specific parsers
- **Programming Language Detection**: Automatic detection of programming languages in code blocks

## Usage

The markdown module can be used to:
1. Parse markdown text into a structured AST
2. Modify the AST programmatically
3. Render the AST back to markdown
4. Extract specific elements from markdown documents
5. Provide real-time parsing for markdown editors
