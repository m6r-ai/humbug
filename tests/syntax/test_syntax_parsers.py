"""
Tests for syntax parsers using the parser registry.
"""
import os
import pytest
from pathlib import Path

# Import all parsers to ensure they are registered
import syntax.parser_imports

from syntax import ParserRegistry, ProgrammingLanguage, ProgrammingLanguageUtils

from syntax_test_utils import (
    find_test_files,
    parse_and_compare
)


@pytest.mark.parametrize("source_path,expected_json_path", find_test_files())
def test_parse_fixture_files(source_path, expected_json_path):
    """Test parsing source files against expected JSON token outputs."""
    is_match, diff = parse_and_compare(source_path, expected_json_path)
    assert is_match, f"Token stream mismatch for {os.path.basename(source_path)}:\n{diff}"


def test_parser_registry_has_parsers():
    """Test that the parser registry has parsers for supported languages."""
    supported_languages = ProgrammingLanguageUtils.get_all_programming_languages()

    # Verify we have at least some parsers registered
    registered_count = 0
    for language in supported_languages:
        parser = ParserRegistry.create_parser(language)
        if parser is not None:
            registered_count += 1

    assert registered_count > 0, "No parsers are registered in the parser registry"


def test_parser_registry_c_parser():
    """Test that C parser is available and functional."""
    parser = ParserRegistry.create_parser(ProgrammingLanguage.C)
    assert parser is not None, "C parser should be available"

    # Test basic parsing
    source = 'int main() { return 0; }'
    parser_state = parser.parse(None, source)

    # Should have some tokens
    token_count = 0
    while True:
        token = parser.get_next_token()
        if token is None:
            break
        token_count += 1

    assert token_count > 0, "C parser should produce tokens"
    assert parser_state is not None, "C parser should return parser state"


def test_language_detection_from_extensions():
    """Test that language detection works for supported file extensions."""
    test_cases = [
        ('.c', ProgrammingLanguage.C),
        ('.cpp', ProgrammingLanguage.CPP),
        ('.py', ProgrammingLanguage.PYTHON),
        ('.js', ProgrammingLanguage.JAVASCRIPT),
        ('.java', ProgrammingLanguage.JAVA),
        ('.rs', ProgrammingLanguage.RUST),
        ('.go', ProgrammingLanguage.GO),
    ]

    for extension, expected_language in test_cases:
        detected = ProgrammingLanguageUtils.from_file_extension(f"test{extension}")
        assert detected == expected_language, f"Expected {expected_language.name} for {extension}"


def test_parser_creation_for_detected_languages():
    """Test that parsers can be created for languages detected from file extensions."""
    extensions_to_test = ['.c', '.cpp', '.py', '.js']  # Test a subset

    for extension in extensions_to_test:
        language = ProgrammingLanguageUtils.from_file_extension(f"test{extension}")
        parser = ParserRegistry.create_parser(language)

        # Not all languages may have parsers, but the registry should handle gracefully
        if parser is not None:
            # If parser exists, it should be functional
            assert hasattr(parser, 'parse'), f"Parser for {language.name} should have parse method"
            assert hasattr(parser, 'get_next_token'), f"Parser for {language.name} should have get_next_token method"


def test_unknown_language_handling():
    """Test handling of unknown languages."""
    # Unknown language should return None parser
    parser = ParserRegistry.create_parser(ProgrammingLanguage.UNKNOWN)
    assert parser is None, "Unknown language should return None parser"

    # Unknown file extension should return TEXT language
    language = ProgrammingLanguageUtils.from_file_extension("test.unknown")
    assert language == ProgrammingLanguage.TEXT, "Unknown extension should return TEXT language"
