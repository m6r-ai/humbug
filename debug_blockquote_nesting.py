#!/usr/bin/env python3
"""Debug blockquote nesting issues."""

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
        content_preview = f' "{preview}..."' if len(node.content) > 40 else f' "{preview}"'
    
    print("  " * indent + f"- {node_type}{content_preview}")
    
    if hasattr(node, 'children'):
        for child in node.children:
            print_tree(child, indent + 1)


print("=" * 80)
print("TEST 1: Blockquote with nested list and code")
print("=" * 80)
markdown1 = """> This is a quote
> 
> - List item in quote
>   
>   ```python
>   code_in_list_in_quote()
>   ```
> 
> - Second list item
>
> Back to quote text"""

builder1 = MarkdownASTBuilder(no_underscores=False)
doc1 = builder1.build_ast(markdown1)
print_tree(doc1)

print("\n" + "=" * 80)
print("TEST 2: List with blockquote")
print("=" * 80)
markdown2 = """- First item

  > Blockquote in list
  > Second line
  
  Back to list item

- Second item"""

builder2 = MarkdownASTBuilder(no_underscores=False)
doc2 = builder2.build_ast(markdown2)
print_tree(doc2)

print("\n" + "=" * 80)
print("TEST 3: Deeply nested mixed blocks")
print("=" * 80)
markdown3 = """- Level 1 list
  
  > Level 1 blockquote
  > 
  > - Level 2 list in quote
  >   
  >   ```python
  >   level_2_code()
  >   ```
  >   
  >   > Level 2 nested quote
  >   > in the list
  > 
  > Back to level 1 quote

- Level 1 list item 2"""

builder3 = MarkdownASTBuilder(no_underscores=False)
doc3 = builder3.build_ast(markdown3)
print_tree(doc3)
