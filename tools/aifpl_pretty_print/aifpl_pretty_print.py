#!/usr/bin/env python3
"""Command-line tool for AIFPL pretty-printer."""

import sys
import argparse
from pathlib import Path
import traceback

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from aifpl.aifpl_pretty_printer import AIFPLPrettyPrinter, FormatOptions


def main():
    """Main entry point for the AIFPL pretty-printer CLI."""
    parser = argparse.ArgumentParser(
        description='Pretty-print AIFPL source code',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Format a file and print to stdout
  aifpl_pretty_print myfile.aifpl
  
  # Format a file and save to output file
  aifpl_pretty_print myfile.aifpl -o formatted.aifpl
  
  # Format from stdin
  echo "(let ((x 5)(y 10)) (+ x y))" | aifpl_pretty_print -
  
  # Format in-place (overwrites original file)
  aifpl_pretty_print myfile.aifpl --in-place
  
  # Format with custom options
  aifpl_pretty_print myfile.aifpl --indent 4 --width 100
  
  # Check if file is already formatted
  aifpl_pretty_print myfile.aifpl --check
"""
    )
    parser.add_argument(
        'input',
        help='Input file (use "-" for stdin)'
    )
    parser.add_argument(
        '-o', '--output',
        help='Output file (default: stdout)'
    )
    parser.add_argument(
        '-i', '--in-place',
        action='store_true',
        help='Format file in-place (overwrites input file)'
    )
    parser.add_argument(
        '--indent',
        type=int,
        default=2,
        help='Number of spaces per indentation level (default: 2)'
    )
    parser.add_argument(
        '--width',
        type=int,
        default=80,
        help='Maximum line width hint (default: 80)'
    )
    parser.add_argument(
        '--compact-threshold',
        type=int,
        default=60,
        help='Maximum length for compact formatting (default: 60)'
    )
    parser.add_argument(
        '--comment-spacing',
        type=int,
        default=2,
        help='Spaces before end-of-line comments (default: 2)'
    )
    parser.add_argument(
        '--check',
        action='store_true',
        help='Check if file is already formatted (exit code 0 if formatted, 1 if not)'
    )
    args = parser.parse_args()

    # Validate arguments
    if args.in_place and args.input == '-':
        print("Error: Cannot use --in-place with stdin", file=sys.stderr)
        sys.exit(1)

    if args.in_place and args.output:
        print("Error: Cannot use both --in-place and --output", file=sys.stderr)
        sys.exit(1)

    # Read input
    if args.input == '-':
        source_code = sys.stdin.read()
        input_path = None

    else:
        input_path = Path(args.input)
        if not input_path.exists():
            print(f"Error: File not found: {args.input}", file=sys.stderr)
            sys.exit(1)

        source_code = input_path.read_text(encoding='utf-8')

    # Create format options
    options = FormatOptions(
        max_line_width=args.width,
        indent_size=args.indent,
        compact_threshold=args.compact_threshold,
        comment_spacing=args.comment_spacing
    )

    # Format the code
    try:
        printer = AIFPLPrettyPrinter(options)
        formatted_code = printer.format(source_code)

    except Exception as e:
        print(f"Error formatting code: {e}", file=sys.stderr)
        traceback.print_exc()
        sys.exit(1)

    # Check mode
    if args.check:
        if formatted_code == source_code:
            print(f"✓ {args.input} is already formatted")
            sys.exit(0)

        else:
            print(f"✗ {args.input} needs formatting")
            sys.exit(1)

    # Write output
    if args.in_place and input_path:
        input_path.write_text(formatted_code, encoding='utf-8')
        print(f"Formatted {args.input}")

    elif args.output:
        output_path = Path(args.output)
        output_path.write_text(formatted_code, encoding='utf-8')
        print(f"Formatted code written to {args.output}")

    else:
        # Print to stdout
        print(formatted_code, end='')


if __name__ == '__main__':
    main()
