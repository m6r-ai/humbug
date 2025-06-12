"""
Serializer for syntax parser tokens and parser state for test comparison.
"""
import json
from typing import List, Dict, Any, Optional

from humbug.syntax.lexer import Token
from humbug.syntax.parser import ParserState
from humbug.syntax.programming_language import ProgrammingLanguage


def serialize_token(token: Token) -> Dict[str, Any]:
    """
    Serialize a token to a dictionary.

    Args:
        token: The token to serialize

    Returns:
        Dictionary representation of the token
    """
    return {
        "type": token.type.name,
        "value": token.value,
        "start": token.start
    }


def serialize_parser_state(parser_state: ParserState | None) -> Dict[str, Any] | None:
    """
    Serialize parser state to a dictionary.

    Args:
        parser_state: The parser state to serialize

    Returns:
        Dictionary representation of the parser state, or None if state is None
    """
    if parser_state is None:
        return None

    result = {
        "parsing_continuation": parser_state.parsing_continuation,
        "continuation_state": parser_state.continuation_state
    }

    # Add lexer state information if available
    if hasattr(parser_state, 'lexer_state') and parser_state.lexer_state is not None:
        lexer_state = parser_state.lexer_state
        lexer_state_dict = {}

        # Serialize common lexer state attributes
        if hasattr(lexer_state, 'in_block_comment'):
            lexer_state_dict['in_block_comment'] = lexer_state.in_block_comment

        if lexer_state_dict:
            result['lexer_state'] = lexer_state_dict

    # Add parser-specific state if available
    if hasattr(parser_state, 'in_element'):
        result['in_element'] = parser_state.in_element

    return result


def serialize_tokens_and_state(
    tokens: List[Token],
    parser_state: ParserState | None,
    language: ProgrammingLanguage,
    source_file: str
) -> Dict[str, Any]:
    """
    Serialize tokens and parser state to a dictionary.

    Args:
        tokens: List of tokens to serialize
        parser_state: Parser state to serialize
        language: Programming language used
        source_file: Source file path

    Returns:
        Dictionary containing serialized data
    """
    return {
        "source_file": source_file,
        "language": language.name,
        "tokens": [serialize_token(token) for token in tokens],
        "parser_state": serialize_parser_state(parser_state)
    }


def save_tokens_to_json(
    tokens: List[Token],
    parser_state: ParserState | None,
    file_path: str,
    language: ProgrammingLanguage,
    source_file: str
) -> None:
    """
    Save tokens and parser state to a JSON file.

    Args:
        tokens: List of tokens to save
        parser_state: Parser state to save
        file_path: Path to save the JSON file
        language: Programming language used
        source_file: Source file path
    """
    serialized = serialize_tokens_and_state(tokens, parser_state, language, source_file)
    with open(file_path, 'w', encoding='utf-8') as f:
        json.dump(serialized, f, indent=2)


def load_tokens_from_json(file_path: str) -> Dict[str, Any]:
    """
    Load serialized tokens and state from a JSON file.

    Args:
        file_path: Path to the JSON file

    Returns:
        Dictionary containing the serialized data
    """
    with open(file_path, 'r', encoding='utf-8') as f:
        return json.load(f)
