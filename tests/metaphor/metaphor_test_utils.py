"""
Utility functions for Metaphor AST builder tests.
"""

import os
import json
import difflib
from pathlib import Path
from typing import Dict, Any, List, Tuple, Optional

from metaphor.metaphor_ast_builder import MetaphorASTBuilder
from metaphor.metaphor_ast_node import MetaphorASTRootNode

from metaphor_ast_serializer import serialize_ast, load_ast_from_json


def get_test_fixtures_dir() -> Path:
    """
    Get the directory containing test fixtures.

    Returns:
        Path to the test fixtures directory
    """
    current_dir = Path(__file__).parent
    return current_dir / "fixtures"


def load_metaphor_file(file_path: str) -> str:
    """
    Load a Metaphor file.

    Args:
        file_path: Path to the Metaphor file

    Returns:
        The contents of the Metaphor file
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return f.read()


def find_test_files() -> List[Tuple[str, str]]:
    """
    Find all test files (Metaphor files with corresponding JSON files).

    Returns:
        List of tuples containing (metaphor_path, json_path)
    """
    fixtures_dir = get_test_fixtures_dir()
    test_files = []

    for root, _, files in os.walk(fixtures_dir):
        for file in files:
            if file.endswith('.m6r'):
                metaphor_path = os.path.join(root, file)
                json_path = metaphor_path + '.json'
                test_files.append((metaphor_path, json_path))

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
    metaphor_path: str,
    expected_json_path: str,
    search_paths: Optional[List[str]] = None,
    embed_path: str = "",
    arguments: Optional[List[str]] = None
) -> Tuple[bool, Optional[str]]:
    """
    Parse a Metaphor file and compare the result with an expected JSON file.

    Args:
        metaphor_path: Path to the Metaphor file
        expected_json_path: Path to the expected JSON file
        search_paths: Search paths for included files
        embed_path: Path used to search for embedded files
        arguments: Command line arguments for the parser

    Returns:
        Tuple of (is_match, diff_message)
    """
    # Set defaults
    if search_paths is None:
        search_paths = [os.path.dirname(metaphor_path)]
    if arguments is None:
        arguments = []

    # Load the Metaphor file and parse it
    metaphor_text = load_metaphor_file(metaphor_path)
    ast_builder = MetaphorASTBuilder()
    root_node = MetaphorASTRootNode()

    ast_builder.build_ast(
        root_node,
        metaphor_text,
        metaphor_path,
        search_paths,
        embed_path,
        arguments
    )

    # Serialize the AST
    actual_ast = serialize_ast(root_node)

    # Load the expected AST
    expected_ast = load_ast_from_json(expected_json_path)

    # Compare
    return compare_ast_with_expected(actual_ast, expected_ast)


def parse_metaphor_text(
    text: str,
    filename: str = "test.m6r",
    search_paths: Optional[List[str]] = None,
    embed_path: str = "",
    arguments: Optional[List[str]] = None
) -> MetaphorASTRootNode:
    """
    Parse Metaphor text and return the root node.

    Args:
        text: The Metaphor text to parse
        filename: Filename for error reporting
        search_paths: Search paths for included files
        embed_path: Path used to search for embedded files
        arguments: Command line arguments for the parser

    Returns:
        The parsed root node
    """
    # Set defaults
    if search_paths is None:
        search_paths = ["."]
    if arguments is None:
        arguments = []

    ast_builder = MetaphorASTBuilder()
    root_node = MetaphorASTRootNode()

    ast_builder.build_ast(
        root_node,
        text,
        filename,
        search_paths,
        embed_path,
        arguments
    )

    return root_node
