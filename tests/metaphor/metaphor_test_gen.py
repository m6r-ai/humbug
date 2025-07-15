#!/usr/bin/env python
"""
Utility script to generate test fixtures from Metaphor files.
"""

import argparse
import sys

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from metaphor.metaphor_ast_builder import MetaphorASTBuilder
from metaphor.metaphor_ast_node import MetaphorASTRootNode

from tests.metaphor.metaphor_ast_serializer import save_ast_to_json


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(description='Generate test fixtures from Metaphor files.')
    parser.add_argument('metaphor_file', help='Path to the Metaphor file')
    parser.add_argument('--output', '-o', help='Output JSON file (defaults to <metaphor_file>.json)')
    parser.add_argument('--source-info', action='store_true', help='Include source information in output')
    parser.add_argument('--search-paths', nargs='*', default=['.'],
                       help='Search paths for included files (defaults to current directory)')
    parser.add_argument('--embed-path', default='',
                       help='Path used to search for embedded files (defaults to current working directory)')
    parser.add_argument('--arguments', nargs='*', default=[],
                       help='Command line arguments for the Metaphor parser')

    args = parser.parse_args()

    # Determine output file
    output_file = args.output
    if not output_file:
        output_file = args.metaphor_file + '.json'

    # Parse the Metaphor file
    try:
        with open(args.metaphor_file, 'r', encoding='utf-8') as f:
            metaphor_text = f.read()

        ast_builder = MetaphorASTBuilder()
        root_node = MetaphorASTRootNode()

        ast_builder.build_ast(
            root_node,
            metaphor_text,
            args.metaphor_file,
            args.search_paths,
            args.embed_path,
            args.arguments
        )

        # Save the AST
        save_ast_to_json(root_node, output_file, include_source_info=args.source_info)
        print(f"Saved AST to {output_file}")

    except FileNotFoundError:
        print(f"Error: File '{args.metaphor_file}' not found.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error parsing Metaphor file: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
