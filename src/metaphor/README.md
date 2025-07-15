# Metaphor Module

This module provides an embedded compiler for the Metaphor language, a specialized domain-specific language for AI interactions. It includes lexical analysis, parsing, AST construction, and formatting capabilities.

## Files

### `__init__.py`
Package initialization and exports:
- Exports main classes for easy importing
- Provides clean API surface for the Metaphor language compiler
- Includes AST nodes, builder, visitor, and formatting utilities

### `metaphor_ast_node.py`
Defines AST node types for the Metaphor language:

- **`MetaphorASTNode`** - Base class for all Metaphor AST nodes
- **`MetaphorRootNode`** - Root node for Metaphor documents
- **`MetaphorTextNode`** - Plain text content
- **`MetaphorCodeNode`** - Code blocks with language specification
- **`MetaphorRoleNode`** - Role definitions for AI interactions
- **`MetaphorContextNode`** - Context blocks for AI prompts
- **`MetaphorActionNode`** - Action blocks for AI instructions
- **`MetaphorASTVisitor`** - Base visitor for traversing Metaphor ASTs

### `metaphor_ast_builder.py`
Core parser for building Metaphor ASTs:

- **`MetaphorASTBuilder`** - Main builder class for parsing Metaphor language
  - Handles lexical analysis and token processing
  - Constructs AST from token stream
  - Manages parsing state and error recovery
  - Supports incremental parsing for editor integration

- **`MetaphorASTBuilderError`** - Base exception for parsing errors
- **`MetaphorASTBuilderSyntaxError`** - Specific syntax error exception

### `metaphor_ast_printer.py`
Converts Metaphor AST back to source code:

- **`MetaphorASTPrinter`** - Visitor that renders AST nodes back to Metaphor syntax
  - Maintains proper formatting and indentation
  - Handles nested structures correctly
  - Preserves semantic meaning while potentially reformatting

### `metaphor_lexer.py`
Lexical analyzer for the Metaphor language:

- **`MetaphorLexer`** - Tokenizes Metaphor source code
  - Recognizes Metaphor-specific syntax elements
  - Handles string literals, identifiers, operators
  - Provides token stream for parser consumption
  - Supports error recovery and reporting

### `metaphor_embed_lexer.py`
Specialized lexer for embedded content:

- **`MetaphorEmbedLexer`** - Handles embedded code blocks within Metaphor
  - Manages transitions between Metaphor and embedded languages
  - Preserves embedded content for external processing
  - Handles nested embedding scenarios

### `metaphor_token.py`
Token definitions for the Metaphor language:

- **`MetaphorToken`** - Represents tokens in Metaphor source code
  - Defines token types specific to Metaphor syntax
  - Includes position information for error reporting
  - Supports token classification and categorization

### `metaphor_format_visitor.py`
Formatting visitor for code beautification:

- **`MetaphorFormatVisitor`** - Specialized visitor for code formatting
  - Applies consistent formatting rules
  - Handles indentation and spacing
  - Preserves semantic structure while improving readability

### `metaphor_formatters.py`
Utility functions for formatting Metaphor code:

- **`format_errors`** - Formats error messages for display
- **`format_preamble`** - Formats document preambles
- Additional formatting utilities for various Metaphor constructs

## Key Features

### Language Support
- **Domain-Specific**: Designed specifically for AI interaction patterns
- **Role-Based**: Support for role definitions and context management
- **Action-Oriented**: Built-in support for action blocks and instructions
- **Code Integration**: Seamless integration of code blocks within AI prompts

### Parsing Infrastructure
- **Lexical Analysis**: Complete tokenization of Metaphor syntax
- **AST Construction**: Full Abstract Syntax Tree representation
- **Error Handling**: Comprehensive error reporting and recovery
- **Incremental Parsing**: Support for real-time editing scenarios

### Formatting and Output
- **Code Formatting**: Automatic code beautification and standardization
- **Error Formatting**: User-friendly error message presentation
- **AST Printing**: Conversion from AST back to source code
- **Embedded Content**: Proper handling of embedded languages

### Integration Features
- **Visitor Pattern**: Extensible visitor pattern for AST traversal
- **Modular Design**: Clean separation of concerns across components
- **Error Recovery**: Robust error handling for partial parsing
- **Editor Support**: Features designed for IDE and editor integration

## Usage

The Metaphor module provides a complete compiler pipeline for the Metaphor language:

1. **Lexical Analysis**: Convert source text into tokens
2. **Parsing**: Build AST from token stream
3. **Transformation**: Modify AST using visitors
4. **Formatting**: Apply consistent formatting rules
5. **Output**: Generate formatted source code

The module is designed to support both batch processing and interactive editing scenarios, making it suitable for integration into development tools and AI interaction systems.