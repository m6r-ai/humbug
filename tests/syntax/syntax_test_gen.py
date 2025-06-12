#!/usr/bin/env python
"""
Utility script to generate test fixtures from source code files using the parser registry.
"""
import argparse
import os
from pathlib import Path

from humbug.syntax.parser_registry import ParserRegistry
from humbug.syntax.programming_language_utils import ProgrammingLanguageUtils

# pylint: disable=unused-import
import humbug.syntax.parser_imports
# pylint: enable=unused-import

from tests.syntax.syntax_test_serializer import save_tokens_to_json


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(description='Generate test fixtures from source code files.')
    parser.add_argument('source_file', help='Path to the source code file')
    parser.add_argument('--output', '-o', help='Output JSON file (defaults to <source_file>.json)')
    parser.add_argument('--language', '-l', help='Override language detection (use language name or extension)')

    args = parser.parse_args()

    # Determine output file
    output_file = args.output
    if not output_file:
        output_file = args.source_file + '.json'

    # Determine programming language
    if args.language:
        language = ProgrammingLanguageUtils.from_name(args.language)
    else:
        language = ProgrammingLanguageUtils.from_file_extension(args.source_file)

    if language.name == 'UNKNOWN':
        print(f"Error: Could not determine programming language for {args.source_file}")
        print("Use --language to specify the language explicitly")
        return 1

    # Read the source file
    try:
        with open(args.source_file, 'r', encoding='utf-8') as f:
            source_text = f.read()

    except FileNotFoundError:
        print(f"Error: File not found: {args.source_file}")
        return 1

    except UnicodeDecodeError:
        print(f"Error: Could not decode file as UTF-8: {args.source_file}")
        return 1

    # Create parser and parse the source
    parser_instance = ParserRegistry.create_parser(language)
    if parser_instance is None:
        print(f"Error: No parser available for language: {language.name}")
        return 1

    try:
        parser_state = parser_instance.parse(None, source_text)

        # Extract tokens from parser
        tokens = []
        while True:
            token = parser_instance.get_next_token()
            if token is None:
                break

            tokens.append(token)

        # Save the tokens
        save_tokens_to_json(tokens, parser_state, output_file, language, args.source_file)
        print(f"Saved {len(tokens)} tokens to {output_file}")
        return 0

    except Exception as e:
        print(f"Error parsing {args.source_file}: {e}")
        return 1


if __name__ == "__main__":
    exit(main())
