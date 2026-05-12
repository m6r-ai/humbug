import argparse
import sys

from docx import DocxError, DocxUnsupportedError, extract_text


def main() -> int:
    """Entry point for the DOCX text extraction CLI tool."""
    parser = argparse.ArgumentParser(
        description="Extract text from a DOCX file",
        epilog="Example: python -m tools.docx document.docx"
    )
    parser.add_argument("file", help="Path to the DOCX file")
    args = parser.parse_args()

    try:
        with open(args.file, "rb") as f:
            data = f.read()

    except OSError as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        return 1

    try:
        text = extract_text(data)

    except DocxUnsupportedError as e:
        print(f"Unsupported DOCX: {e}", file=sys.stderr)
        return 2

    except DocxError as e:
        print(f"Failed to extract text: {e}", file=sys.stderr)
        return 1

    print(text)
    return 0
