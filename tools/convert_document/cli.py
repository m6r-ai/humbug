import argparse
import sys
from pathlib import Path

from dhtml import HtmlError, HtmlParseError, document_ir_to_html, html_ast_to_document_ir, parse_html
from dmarkdown.document_ir_to_markdown import document_ir_to_markdown
from dmarkdown.markdown_ast_builder import MarkdownASTBuilder
from dmarkdown.markdown_to_document_ir import markdown_ast_to_document_ir
from document_ir.image_sidecar import extract_images_to_sidecar
from document_ir.document_ir_node import DocumentIRDocumentNode
from docx import (
    DocxError, DocxUnsupportedError,
    docx_ast_to_document_ir, document_ir_to_docx_ast, parse_docx, serialise_docx
)

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

_FORMAT_EXTENSIONS: dict[str, str] = {
    "docx": ".docx",
    "html": ".html",
    "md": ".md",
}

_EXTENSION_FORMATS: dict[str, str] = {v: k for k, v in _FORMAT_EXTENSIONS.items()}
_EXTENSION_FORMATS[".htm"] = "html"

_SUPPORTED_FORMATS = sorted(_FORMAT_EXTENSIONS.keys())

# Formats that use text files and need embedded images extracted to a sidecar directory.
_SIDECAR_FORMATS = frozenset({"html", "md"})


def _infer_format(path: Path, label: str) -> str:
    fmt = _EXTENSION_FORMATS.get(path.suffix.lower())
    if fmt is None:
        print(
            f"Error: cannot infer {label} format from extension '{path.suffix}'. "
            f"Use --from/--to to specify. Supported formats: {', '.join(_SUPPORTED_FORMATS)}.",
            file=sys.stderr,
        )
        sys.exit(1)

    return fmt


def _import_md(path: Path) -> DocumentIRDocumentNode:
    try:
        md_text = path.read_text(encoding="utf-8")

    except OSError as e:
        print(f"Error reading '{path}': {e}", file=sys.stderr)
        sys.exit(1)

    builder = MarkdownASTBuilder(True)
    builder.update_ast(md_text, "", str(path))
    return markdown_ast_to_document_ir(builder.document())


def _import_html(path: Path) -> DocumentIRDocumentNode:
    try:
        source = path.read_text(encoding="utf-8")

    except OSError as e:
        print(f"Error reading '{path}': {e}", file=sys.stderr)
        sys.exit(1)

    try:
        return html_ast_to_document_ir(parse_html(source, source_path=str(path)))

    except (HtmlParseError, HtmlError) as e:
        print(f"Error parsing HTML '{path}': {e}", file=sys.stderr)
        sys.exit(1)


def _import_docx(path: Path) -> DocumentIRDocumentNode:
    try:
        data = path.read_bytes()

    except OSError as e:
        print(f"Error reading '{path}': {e}", file=sys.stderr)
        sys.exit(1)

    try:
        return docx_ast_to_document_ir(parse_docx(data))

    except DocxUnsupportedError as e:
        print(f"Unsupported DOCX '{path}': {e}", file=sys.stderr)
        sys.exit(2)

    except DocxError as e:
        print(f"Error parsing DOCX '{path}': {e}", file=sys.stderr)
        sys.exit(1)


_IMPORTERS = {
    "docx": _import_docx,
    "html": _import_html,
    "md": _import_md,
}

_EXPORTERS: dict[str, object] = {
    "docx": lambda doc: serialise_docx(document_ir_to_docx_ast(doc)),
    "html": document_ir_to_html,
    "md": document_ir_to_markdown,
}


def main() -> int:
    """Entry point for the document conversion CLI tool."""
    parser = argparse.ArgumentParser(
        description="Convert documents between docx, html, and md formats.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s report.docx                   # Writes report.md (inferred)
  %(prog)s report.docx -o report.html    # Explicit output path
  %(prog)s README.md --to docx           # Writes README.docx
  %(prog)s page.htm --from html --to md  # Explicit formats
        """
    )
    parser.add_argument("input", help="Path to the input file")
    parser.add_argument("-o", "--output", help="Path to the output file")
    parser.add_argument(
        "--from", dest="from_format", choices=_SUPPORTED_FORMATS,
        metavar="FORMAT", help=f"Input format ({', '.join(_SUPPORTED_FORMATS)})"
    )
    parser.add_argument(
        "--to", dest="to_format", choices=_SUPPORTED_FORMATS,
        metavar="FORMAT", help=f"Output format ({', '.join(_SUPPORTED_FORMATS)})"
    )

    args = parser.parse_args()

    input_path = Path(args.input)
    if not input_path.is_file():
        print(f"Error: input file not found: '{input_path}'", file=sys.stderr)
        return 1

    from_format: str = args.from_format or _infer_format(input_path, "input")

    if args.output:
        output_path = Path(args.output)
        to_format: str = args.to_format or _infer_format(output_path, "output")

    elif args.to_format:
        to_format = args.to_format
        output_path = input_path.with_suffix(_FORMAT_EXTENSIONS[to_format])

    else:
        print(
            "Error: cannot determine output format. "
            "Provide -o/--output or --to.",
            file=sys.stderr,
        )
        return 1

    if from_format == to_format:
        print(
            f"Error: input and output formats are both '{from_format}'; no conversion needed.",
            file=sys.stderr,
        )
        return 1

    try:
        doc_ir = _IMPORTERS[from_format](input_path)

        if to_format in _SIDECAR_FORMATS:
            sidecar_dir = output_path.parent / f"{output_path.stem}_files"
            extract_images_to_sidecar(
                doc_ir,
                sidecar_dir,
            )

        content = _EXPORTERS[to_format](doc_ir)  # type: ignore[operator]

    except SystemExit:
        raise

    except Exception as e:  # pylint: disable=broad-exception-caught
        print(f"Error during conversion: {e}", file=sys.stderr)
        return 1

    try:
        if isinstance(content, bytes):
            output_path.write_bytes(content)

        else:
            output_path.write_text(content, encoding="utf-8")

    except OSError as e:
        print(f"Error writing '{output_path}': {e}", file=sys.stderr)
        return 1

    print(f"Written: {output_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
