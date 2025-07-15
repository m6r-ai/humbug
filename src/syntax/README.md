# Syntax Module

This module provides a comprehensive syntax highlighting and parsing framework that supports multiple programming languages. It includes lexical analysis, parsing infrastructure, and language-specific implementations.

## Core Files

### `lexer.py`
Base lexer infrastructure with optimized implementation:

- **`Lexer`** - Abstract base class for all language lexers
  - Optimized character classification using pre-computed lookup tables
  - Generic token handling and operator parsing
  - Shared character classification methods (letters, digits, whitespace, etc.)
  - Static lookup tables for performance optimization

- **`Token`** - Represents tokens in source code with type, value, and position
- **`TokenType`** - Comprehensive enumeration of token types across all supported languages
- **`LexerState`** - State information for lexer instances

### `parser.py`
Base parser infrastructure:

- **`Parser`** - Abstract base class for language parsers
  - Provides common parsing functionality
  - Handles token stream processing
  - Supports error recovery and reporting

### `parser_registry.py`
Central registry for language parsers:

- **`ParserRegistry`** - Manages registration and creation of language-specific parsers
  - Maps programming languages to their respective parsers
  - Provides factory methods for parser creation
  - Supports dynamic parser registration

### `parser_imports.py`
Handles dynamic importing of language-specific parsers:

- Manages lazy loading of parser implementations
- Handles import errors gracefully
- Provides fallback mechanisms for unsupported languages

### `programming_language.py`
Language enumeration and definitions:

- **`ProgrammingLanguage`** - Enumeration of all supported programming languages
  - Comprehensive list of programming languages
  - Used for language identification and parser selection

### `programming_language_utils.py`
Utility functions for programming language operations:

- **`ProgrammingLanguageUtils`** - Utility class for language-related operations
  - Language detection from file extensions
  - Language name normalization and conversion
  - File extension to language mapping
  - Language capability detection

## Language Support

The module includes dedicated subdirectories for each supported programming language:

### Web Technologies
- **`html/`** - HTML markup language support
- **`css/`** - CSS stylesheet language support
- **`javascript/`** - JavaScript language support
- **`typescript/`** - TypeScript language support
- **`json/`** - JSON data format support

### Systems Programming
- **`c/`** - C programming language support
- **`cpp/`** - C++ programming language support
- **`rust/`** - Rust programming language support
- **`go/`** - Go programming language support

### Object-Oriented Languages
- **`java/`** - Java programming language support
- **`csharp/`** - C# programming language support
- **`kotlin/`** - Kotlin programming language support
- **`swift/`** - Swift programming language support

### Dynamic Languages
- **`python/`** - Python programming language support
- **`scheme/`** - Scheme/Lisp language support

### Specialized Languages
- **`solidity/`** - Solidity smart contract language support
- **`move/`** - Move programming language support

### Documentation and Text
- **`markdown/`** - Markdown markup language support
- **`metaphor/`** - Metaphor domain-specific language support
- **`text/`** - Plain text support

## Key Features

### Performance Optimizations
- **Character Lookup Tables**: Pre-computed sets for fast character classification
- **Shared Resources**: Static lookup tables shared across all lexer instances
- **Optimized Token Handling**: Efficient token creation and management
- **Greedy Operator Matching**: Longest-match operator parsing

### Language Detection
- **File Extension Mapping**: Automatic language detection from file extensions
- **Content-Based Detection**: Fallback detection based on file content
- **Language Normalization**: Consistent language name handling

### Extensibility
- **Plugin Architecture**: Easy addition of new language support
- **Registry Pattern**: Centralized management of language parsers
- **Abstract Base Classes**: Consistent interface across all language implementations

### Error Handling
- **Graceful Degradation**: Fallback mechanisms for unsupported languages
- **Error Recovery**: Robust error handling in lexing and parsing
- **Diagnostic Information**: Detailed error reporting with position information

## Usage

The syntax module provides a unified interface for syntax highlighting and parsing across multiple programming languages:

1. **Language Detection**: Automatically detect programming language from file extension or content
2. **Lexical Analysis**: Tokenize source code using language-specific lexers
3. **Parsing**: Parse token streams into structured representations
4. **Syntax Highlighting**: Generate syntax highlighting information
5. **Error Reporting**: Provide detailed error information for invalid syntax
