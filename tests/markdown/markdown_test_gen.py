#!/usr/bin/env python
"""
Utility script to generate test fixtures from markdown files.
"""
import argparse

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from markdown.markdown_ast_builder import MarkdownASTBuilder

from tests.markdown.markdown_ast_serializer import save_ast_to_json


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(description='Generate test fixtures from markdown files.')
    parser.add_argument('markdown_file', help='Path to the markdown file')
    parser.add_argument('--no-underscores', action='store_true', help='Disable underscore formatting')
    parser.add_argument('--output', '-o', help='Output JSON file (defaults to <markdown_file>.json)')
    parser.add_argument('--line-numbers', action='store_true', help='Include line numbers in output')

    args = parser.parse_args()

    # Determine output file
    output_file = args.output
    if not output_file:
        output_file = args.markdown_file + '.json'

    # Parse the markdown
    with open(args.markdown_file, 'r', encoding='utf-8') as f:
        markdown_text = f.read()

    md_ast_builder = MarkdownASTBuilder(no_underscores=args.no_underscores)
    ast = md_ast_builder.build_ast(markdown_text)

    # Save the AST
    save_ast_to_json(ast, output_file, include_line_numbers=args.line_numbers)
    print(f"Saved AST to {output_file}")


if __name__ == "__main__":
    main()
