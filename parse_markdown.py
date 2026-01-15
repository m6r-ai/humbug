#!/usr/bin/env python3
"""Parse and print markdown using dmarkdown."""

import sys
sys.path.insert(0, 'src')

# Import parser_imports to register all parsers
import syntax.parser_imports  # noqa: F401

from dmarkdown.markdown_ast_builder import MarkdownASTBuilder
from dmarkdown.markdown_ast_printer import MarkdownASTPrinter

# The markdown content to parse
markdown_content = r"""## Developer installation


1. Create and activate a virtual environment:

   Linux and macOS:

   ```bash
   python -m venv venv
   source venv/bin/activate
   ```

   Windows:

   ```bash
   python -m venv venv
   venv\Scripts\activate
   ```

2. Install build tools:

   ```bash
   pip install build
   ```

3. Install in development mode:

   ```bash
   pip install -e .
   ```

4. Launch the application:

   ```bash
   python -m humbug
   ```

5. Initial configuration:

   See [Getting Started with Metaphor](https://github.com/m6r-ai/getting-started-with-metaphor) for a step-by-step guide to getting Humbug up and running.

"""

# Parse the markdown (no_underscores=False to allow underscores in header IDs)
builder = MarkdownASTBuilder(no_underscores=False)
ast = builder.build_ast(markdown_content)

# Print the AST
printer = MarkdownASTPrinter()
printer.visit(ast)
