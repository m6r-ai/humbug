"""
Utility functions for markdown AST builder tests
"""
import os
import json
from pathlib import Path
import difflib
from typing import Dict, Any, List, Tuple, Optional

from humbug.markdown.markdown_ast_builder import MarkdownASTBuilder
from markdown_ast_serializer import serialize_ast, load_ast_from_json


def get_test_fixtures_dir() -> Path:
    """
    Get the directory containing test fixtures.

    Returns:
        Path to the test fixtures directory
    """
    current_dir = Path(__file__).parent
    return current_dir / "fixtures"


def load_markdown_file(file_path: str) -> str:
    """
    Load a markdown file.

    Args:
        file_path: Path to the markdown file

    Returns:
        The contents of the markdown file
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return f.read()


def find_test_files() -> List[Tuple[str, str]]:
    """
    Find all test files (markdown files with corresponding JSON files).

    Returns:
        List of tuples containing (markdown_path, json_path)
    """
    fixtures_dir = get_test_fixtures_dir()
    test_files = []

    for root, _, files in os.walk(fixtures_dir):
        for file in files:
            if file.endswith('.md'):
                md_path = os.path.join(root, file)
                json_path = md_path.replace('.md', '.json')

                if os.path.exists(json_path):
                    test_files.append((md_path, json_path))

    return test_files


def compare_ast_with_expected(
    actual_ast: Dict[str, Any],
    expected_ast: Dict[str, Any]
) -> Tuple[bool, Optional[str]]:
    """
    Compare an actual AST with an expected AST.

    Args:
        actual_ast: The actual AST dictionary
        expected_ast: The expected AST dictionary

    Returns:
        Tuple of (is_match, diff_message)
    """
    # Convert to JSON strings for comparison
    actual_json = json.dumps(actual_ast, indent=2, sort_keys=True)
    expected_json = json.dumps(expected_ast, indent=2, sort_keys=True)

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
    markdown_path: str,
    expected_json_path: str,
    no_underscores: bool = False
) -> Tuple[bool, Optional[str]]:
    """
    Parse a markdown file and compare the result with an expected JSON file.

    Args:
        markdown_path: Path to the markdown file
        expected_json_path: Path to the expected JSON file
        no_underscores: Whether to disable underscore formatting

    Returns:
        Tuple of (is_match, diff_message)
    """
    # Load the markdown and parse it
    markdown_text = load_markdown_file(markdown_path)
    ast_builder = MarkdownASTBuilder(no_underscores=no_underscores)
    ast = ast_builder.build_ast(markdown_text)

    # Serialize the AST
    actual_ast = serialize_ast(ast)

    # Load the expected AST
    expected_ast = load_ast_from_json(expected_json_path)

    # Compare
    return compare_ast_with_expected(actual_ast, expected_ast)
