"""Command-line interface for the DOCX to Markdown converter."""

import argparse
import os
import sys

from docx import DocxError, DocxUnsupportedError, parse_docx, docx_ast_to_doc_ir
from dmarkdown.doc_ir_to_markdown import doc_ir_to_markdown


def main() -> int:
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Convert a DOCX file to Markdown using only the Python standard library.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s document.docx                  # Writes document.md
  %(prog)s document.docx -o output.md     # Writes output.md
        """
    )
    parser.add_argument("input", help="Path to the input DOCX file")
    parser.add_argument(
        "-o", "--output",
        help="Path to the output Markdown file (default: input filename with .md extension)"
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
        output_path = base + ".md"

    try:
        with open(input_path, "rb") as f:
            docx_bytes = f.read()
    except OSError as e:
        print(f"Error reading input file: {e}", file=sys.stderr)
        return 1

    try:
        docx_ast = parse_docx(docx_bytes)
        doc_ir = docx_ast_to_doc_ir(docx_ast)
        md_text = doc_ir_to_markdown(doc_ir)
    except DocxUnsupportedError as e:
        print(f"Unsupported DOCX: {e}", file=sys.stderr)
        return 2
    except DocxError as e:
        print(f"Error during conversion: {e}", file=sys.stderr)
        return 1
    except Exception as e:  # pylint: disable=broad-exception-caught
        print(f"Error during conversion: {e}", file=sys.stderr)
        return 1

    try:
        with open(output_path, "w", encoding="utf-8") as f:
            f.write(md_text)
    except OSError as e:
        print(f"Error writing output file: {e}", file=sys.stderr)
        return 1

    print(f"Written: {output_path}")
    return 0
