"""
Tests for TOML table headers, key classification, and real-world scenarios.
"""
from syntax.toml.toml_parser import TOMLParser
from syntax.lexer import TokenType


class TestTOMLTableHeaders:
    """Test TOML table header tokenization."""

    def test_simple_table(self):
        """Test a simple table header."""
        lexer_parser = TOMLParser()
        lexer_parser.parse(None, '[section]')

        tokens = list(lexer_parser._tokens)
        element_tokens = [t for t in tokens if t.type == TokenType.ELEMENT]
        assert len(element_tokens) == 1
        assert element_tokens[0].value == '[section]'

    def test_dotted_table(self):
        """Test a dotted table header."""
        parser = TOMLParser()
        parser.parse(None, '[tool.coverage]')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type == TokenType.ELEMENT]
        assert len(element_tokens) == 1
        assert element_tokens[0].value == '[tool.coverage]'

    def test_array_of_tables(self):
        """Test an array of tables header ([[name]])."""
        parser = TOMLParser()
        parser.parse(None, '[[products]]')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type == TokenType.ELEMENT]
        assert len(element_tokens) == 1
        assert element_tokens[0].value == '[[products]]'

    def test_dotted_array_of_tables(self):
        """Test a dotted array of tables header."""
        parser = TOMLParser()
        parser.parse(None, '[[servers.demo]]')

        tokens = list(parser._tokens)
        element_tokens = [t for t in tokens if t.type == TokenType.ELEMENT]
        assert len(element_tokens) == 1
        assert element_tokens[0].value == '[[servers.demo]]'

    def test_table_header_not_key(self):
        """Test that table headers don't produce key tokens."""
        parser = TOMLParser()
        parser.parse(None, '[section]')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 0


class TestTOMLKeys:
    """Test TOML key classification by the parser."""

    def test_simple_key_value(self):
        """Test a simple key = value pair."""
        parser = TOMLParser()
        parser.parse(None, 'name = "Alice"')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'name'

    def test_quoted_key(self):
        """Test a quoted key."""
        parser = TOMLParser()
        parser.parse(None, '"quoted key" = "value"')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 1
        assert key_tokens[0].value == '"quoted key"'

    def test_dotted_key(self):
        """Test a dotted key (a.b.c = value)."""
        parser = TOMLParser()
        parser.parse(None, 'a.b.c = "value"')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'a' in key_values
        assert 'b' in key_values
        assert 'c' in key_values

    def test_key_with_number_value(self):
        """Test key with a number value."""
        parser = TOMLParser()
        parser.parse(None, 'port = 8080')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'port'

        number_tokens = [t for t in tokens if t.type == TokenType.NUMBER]
        assert len(number_tokens) == 1
        assert number_tokens[0].value == '8080'

    def test_key_with_boolean_value(self):
        """Test key with a boolean value."""
        parser = TOMLParser()
        parser.parse(None, 'debug = true')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 1

        boolean_tokens = [t for t in tokens if t.type == TokenType.BOOLEAN]
        assert len(boolean_tokens) == 1
        assert boolean_tokens[0].value == 'true'

    def test_key_with_dash(self):
        """Test key names containing dashes."""
        parser = TOMLParser()
        parser.parse(None, 'max-line-length = 132')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'max-line-length'

    def test_value_not_classified_as_key(self):
        """Test that bare text in value position is not a key."""
        parser = TOMLParser()
        parser.parse(None, 'key = bare_value')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'key'

        text_tokens = [t for t in tokens if t.type == TokenType.TEXT]
        assert len(text_tokens) >= 1
        assert text_tokens[-1].value == 'bare_value'


class TestTOMLRealWorld:
    """Test real-world TOML documents."""

    def test_pyproject_style(self):
        """Test a pyproject.toml-like document."""
        lines = [
            '[build-system]',
            'requires = ["hatchling"]',
            '',
            '[project]',
            'name = "humbug"',
            'version = "52"',
            'dependencies = [',
            '    "certifi",',
            '    "pyside6",',
            ']',
        ]
        parser = TOMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = TOMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        element_tokens = [t for t in all_tokens if t.type == TokenType.ELEMENT]
        element_values = [t.value for t in element_tokens]
        assert '[build-system]' in element_values
        assert '[project]' in element_values

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'name' in key_values
        assert 'version' in key_values
        assert 'dependencies' in key_values

    def test_tool_config_style(self):
        """Test a tool configuration section."""
        lines = [
            '[tool.mypy]',
            'disallow_untyped_defs = true',
            'check_untyped_defs = true',
            '',
            '[tool.pylint]',
            'max-line-length = 132',
            'max-args = 8',
        ]
        parser = TOMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = TOMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        element_tokens = [t for t in all_tokens if t.type == TokenType.ELEMENT]
        element_values = [t.value for t in element_tokens]
        assert '[tool.mypy]' in element_values
        assert '[tool.pylint]' in element_values

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'disallow_untyped_defs' in key_values
        assert 'max-line-length' in key_values

    def test_multiline_string_in_config(self):
        """Test a document with multi-line strings."""
        lines = [
            '[project]',
            'description = """',
            'This is a',
            'multi-line description.',
            '"""',
            'name = "test"',
        ]
        parser = TOMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = TOMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'description' in key_values
        assert 'name' in key_values, "Key after multi-line string should be classified normally"

    def test_arrays_spanning_lines(self):
        """Test arrays that span multiple lines."""
        lines = [
            'items = [',
            '    "first",',
            '    "second",',
            ']',
            'key = true',
        ]
        parser = TOMLParser()
        state = None

        for line in lines:
            parser = TOMLParser()
            state = parser.parse(state, line)

    def test_empty_lines(self):
        """Test that empty lines produce no errors."""
        lines = [
            'key = "value"',
            '',
            'other = 42',
        ]
        parser = TOMLParser()
        state = None

        for line in lines:
            parser = TOMLParser()
            state = parser.parse(state, line)

    def test_inline_table(self):
        """Test inline table syntax."""
        parser = TOMLParser()
        parser.parse(None, 'point = {x = 1, y = 2}')

        tokens = list(parser._tokens)
        operators = [t.value for t in tokens if t.type == TokenType.OPERATOR]
        assert '{' in operators
        assert '}' in operators

        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'point' in key_values
        assert 'x' in key_values
        assert 'y' in key_values
