# DAST (Distributed Abstract Syntax Tree) Module

This module provides base classes and infrastructure for Abstract Syntax Trees (ASTs) that can be shared across different language implementations. It serves as a foundation for building language-specific AST implementations while providing common tree operations.

## Files

### `ast.py`
Contains the core base classes for AST infrastructure:

- **`ASTNode`** - Base class for all AST nodes in any language
  - Provides common tree operations like adding/removing children
  - Supports navigation between siblings (previous/next)
  - Maintains parent-child relationships
  - Designed to be language-agnostic

- **`ASTVisitor`** - Base visitor class for AST traversal
  - Implements the visitor pattern for tree traversal
  - Allows specialized processing of different node types
  - Provides generic visit method for nodes without specific handlers
  - Supports method dispatch based on node class names

## Key Features

- **Language Agnostic**: Provides shared infrastructure that can be used by different language implementations
- **Tree Operations**: Common operations like adding/removing children, sibling navigation
- **Visitor Pattern**: Built-in support for the visitor pattern for tree traversal and processing
- **Parent-Child Relationships**: Automatic maintenance of parent-child relationships in the tree
- **Method Dispatch**: Automatic method dispatch in visitors based on node types

## Usage

This module is designed to be extended by language-specific AST implementations. For example, the `markdown` and `metaphor` modules build upon these base classes to create their own specialized AST node types while inheriting the common tree operations and visitor functionality.

The separation of concerns allows for:
- Code reuse across different language implementations
- Consistent tree operations regardless of language
- Standardized visitor pattern implementation
- Easy extension for new language support
