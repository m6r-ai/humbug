#!/usr/bin/env python3
"""Debug single blockquote test."""

import sys
sys.path.insert(0, 'src')

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from dmarkdown import MarkdownASTBuilder


def print_tree(node, indent=0):
    """Print AST tree structure."""
    node_type = node.__class__.__name__.replace("MarkdownAST", "").replace("Node", "")
    
    # Get content preview for text/code nodes
    content_preview = ""
    if hasattr(node, 'content'):
        preview = node.content[:40].replace('\n', '\\n')
        content_preview = f' "{preview}"'
    
    print("  " * indent + f"- {node_type}{content_preview}")
    
    if hasattr(node, 'children'):
        for i, child in enumerate(node.children):
            print("  " * indent + f"  [{i}]:")
            print_tree(child, indent + 1)


markdown = """- First item

  > Blockquote in list
  > Second line
  
  Back to list item

- Second item"""

print("Input markdown:")
print(repr(markdown))
print("\nParsing...")

builder = MarkdownASTBuilder(no_underscores=False)
doc = builder.build_ast(markdown)

print("\nAST Structure:")
print_tree(doc)

print("\nExpected structure:")
print("- Document")
print("  - UnorderedList")
print("    - ListItem")
print("      [0]: Paragraph 'First item'")
print("      [1]: Blockquote")
print("        - Paragraph 'Blockquote in list Second line' (both lines together)")
print("      [2]: Paragraph 'Back to list item'")
print("    - ListItem")
print("      [0]: Paragraph 'Second item'")
