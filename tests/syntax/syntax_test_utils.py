"""
Utility functions for syntax parser tests.
"""
import os
import json
import difflib
from pathlib import Path
from typing import Dict, Any, List, Tuple, Optional

from syntax.parser_registry import ParserRegistry
from syntax.programming_language_utils import ProgrammingLanguageUtils

from syntax_test_serializer import serialize_tokens_and_state, load_tokens_from_json


def get_test_fixtures_dir() -> Path:
    """
    Get the directory containing test fixtures.

    Returns:
        Path to the test fixtures directory
    """
    current_dir = Path(__file__).parent
    return current_dir / "fixtures"


def load_source_file(file_path: str) -> str:
    """
    Load a source code file.

    Args:
        file_path: Path to the source file

    Returns:
        The contents of the source file

    Raises:
        FileNotFoundError: If the source file cannot be found
        UnicodeDecodeError: If the file cannot be decoded as UTF-8
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return f.read()


def find_test_files() -> List[Tuple[str, str]]:
    """
    Find all test files (source files with corresponding JSON files).

    Returns:
        List of tuples containing (source_path, json_path)
    """
    fixtures_dir = get_test_fixtures_dir()
    test_files = []

    # Get all supported extensions
    supported_extensions = ProgrammingLanguageUtils.get_supported_file_extensions()

    for root, _, files in os.walk(fixtures_dir):
        for file in files:
            file_path = os.path.join(root, file)

            # Check if this file has a supported extension
            _, ext = os.path.splitext(file)
            if ext.lower() in supported_extensions:
                json_path = file_path + '.json'
                if os.path.exists(json_path):
                    test_files.append((file_path, json_path))

    return test_files


def compare_token_data(
    actual_data: Dict[str, Any],
    expected_data: Dict[str, Any]
) -> Tuple[bool, Optional[str]]:
    """
    Compare actual token data with expected token data.

    Args:
        actual_data: The actual token data dictionary
        expected_data: The expected token data dictionary

    Returns:
        Tuple of (is_match, diff_message)
    """
    # Convert to JSON strings for comparison
    actual_json = json.dumps(actual_data, indent=2, sort_keys=True)
    expected_json = json.dumps(expected_data, indent=2, sort_keys=True)

    if actual_json == expected_json:
        return True, None

    # Generate a diff if they don't match
    diff = difflib.unified_diff(
        expected_json.splitlines(keepends=True),
        actual_json.splitlines(keepends=True),
        fromfile='expected',
        tofile='actual'
    )

    diff_message = ''.join(diff)
    return False, diff_message


def parse_and_compare(
    source_path: str,
    expected_json_path: str
) -> Tuple[bool, Optional[str]]:
    """
    Parse a source file and compare the result with an expected JSON file.

    Args:
        source_path: Path to the source file
        expected_json_path: Path to the expected JSON file

    Returns:
        Tuple of (is_match, diff_message)
    """
    # Determine language from file extension
    language = ProgrammingLanguageUtils.from_file_extension(source_path)
    if language.name == 'UNKNOWN':
        return False, f"Unknown language for file: {source_path}"

    # Create parser
    parser = ParserRegistry.create_parser(language)
    if parser is None:
        return False, f"No parser available for language: {language.name}"

    # Load and parse the source incrementally
    try:
        source_text = load_source_file(source_path)
        lines = source_text.splitlines()

        # Parse incrementally, line by line, maintaining state between calls
        parser_state = None
        for line_number, line in enumerate(lines, start=1):
            try:
                parser_state = parser.parse(parser_state, line)

            except Exception as e:
                return False, f"Error parsing line {line_number} in {source_path}: {str(e)}"

        # Extract tokens
        tokens = []
        while True:
            token = parser.get_next_token()
            if token is None:
                break

            tokens.append(token)

        # Serialize the results
        actual_data = serialize_tokens_and_state(tokens, parser_state, language, source_path)

        # Load expected data
        expected_data = load_tokens_from_json(expected_json_path)

        # Compare (excluding source_file path which may differ)
        actual_comparison = {k: v for k, v in actual_data.items() if k != 'source_file'}
        expected_comparison = {k: v for k, v in expected_data.items() if k != 'source_file'}

        return compare_token_data(actual_comparison, expected_comparison)

    except FileNotFoundError as e:
        return False, f"File not found: {source_path}"

    except UnicodeDecodeError as e:
        return False, f"Could not decode file as UTF-8: {source_path}"

    except Exception as e:
        return False, f"Error parsing {source_path}: {str(e)}"

