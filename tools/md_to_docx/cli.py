"""
Command-line interface for the Markdown to DOCX converter.
"""

import argparse
import os
import sys

from dmarkdown.markdown_ast_builder import MarkdownASTBuilder

import syntax.parser_imports  # noqa: F401  registers all syntax parsers in ParserRegistry
from .docx_builder import build_docx
from .xml_writer import DocxXmlVisitor


def main() -> int:
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Convert a Markdown file to a DOCX file using only the Python standard library.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s README.md                     # Writes README.docx
  %(prog)s README.md -o output.docx      # Writes output.docx
        """
    )
    parser.add_argument("input", help="Path to the input Markdown file")
    parser.add_argument(
        "-o", "--output",
        help="Path to the output DOCX file (default: input filename with .docx extension)"
    )

    args = parser.parse_args()

    input_path: str = args.input
    if not os.path.isfile(input_path):
        print(f"Error: input file not found: {input_path}", file=sys.stderr)
        return 1

    if args.output:
        output_path = args.output
    else:
        base, _ = os.path.splitext(input_path)
        output_path = base + ".docx"

    try:
        with open(input_path, "r", encoding="utf-8") as f:
            md_text = f.read()
    except OSError as e:
        print(f"Error reading input file: {e}", file=sys.stderr)
        return 1

    try:
        builder = MarkdownASTBuilder(True)
        builder.update_ast(md_text, "", input_path)
        document = builder.document()

        visitor = DocxXmlVisitor()
        visitor.visit(document)

        build_docx(visitor.body_parts, output_path)
    except Exception as e:  # pylint: disable=broad-exception-caught
        print(f"Error during conversion: {e}", file=sys.stderr)
        return 1

    print(f"Written: {output_path}")
    return 0
