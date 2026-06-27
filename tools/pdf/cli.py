import argparse
import sys

from pdf import PDFUnsupportedError, PDFError, extract_text, parse


def main() -> int:
    """Entry point for the PDF text extraction CLI tool."""
    parser = argparse.ArgumentParser(
        description="Extract text from a PDF file",
        epilog="Example: python -m tools.pdf document.pdf"
    )
    parser.add_argument("file", help="Path to the PDF file")
    parser.add_argument(
        "--pages",
        action="store_true",
        help="Show page separators in output"
    )
    args = parser.parse_args()

    try:
        with open(args.file, "rb") as f:
            data = f.read()

    except OSError as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        return 1

    try:
        doc = parse(data)

    except PDFUnsupportedError as e:
        print(f"Unsupported PDF: {e}", file=sys.stderr)
        return 2

    except PDFError as e:
        print(f"Failed to parse PDF: {e}", file=sys.stderr)
        return 1

    try:
        text = extract_text(doc)

    except PDFError as e:
        print(f"Failed to extract text: {e}", file=sys.stderr)
        return 1

    if args.pages:
        pages = text.split("\f")
        for i, page in enumerate(pages, 1):
            print(f"--- Page {i} ---")
            print(page)
            print()

    else:
        print(text.replace("\f", "\n\n"))

    return 0
