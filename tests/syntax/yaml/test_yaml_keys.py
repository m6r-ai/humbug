"""
Tests for YAML key-value pair and list marker classification by the parser.
"""
from syntax.yaml.yaml_parser import YAMLParser


class TestYAMLKeys:
    """Test YAML key classification by the parser."""

    def test_simple_key_value(self):
        """Test a simple key: value pair."""
        parser = YAMLParser()
        parser.parse(None, 'name: Alice')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'name'

    def test_key_with_no_value(self):
        """Test a key with no value on the same line."""
        parser = YAMLParser()
        parser.parse(None, 'parent:')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'parent'

    def test_quoted_key(self):
        """Test a quoted string key."""
        parser = YAMLParser()
        parser.parse(None, '"quoted key": value')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1
        assert key_tokens[0].value == '"quoted key"'

    def test_indented_key(self):
        """Test an indented key inside a nested mapping."""
        parser = YAMLParser()
        parser.parse(None, '  child: value')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'child'

    def test_key_value_is_not_key(self):
        """Test that the value after a colon is not classified as a key."""
        parser = YAMLParser()
        parser.parse(None, 'key: value')

        tokens = list(parser._tokens)
        text_tokens = [t for t in tokens if t.type.name == 'TEXT']
        assert len(text_tokens) >= 1
        assert text_tokens[-1].value == 'value'

    def test_colon_inside_value_not_key(self):
        """Test that a colon inside a URL-like value doesn't create a key."""
        parser = YAMLParser()
        parser.parse(None, 'url: http://example.com')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'url'

    def test_key_with_number_value(self):
        """Test key with a number value."""
        parser = YAMLParser()
        parser.parse(None, 'port: 8080')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'port'

        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(number_tokens) == 1
        assert number_tokens[0].value == '8080'

    def test_key_with_boolean_value(self):
        """Test key with a boolean keyword value."""
        parser = YAMLParser()
        parser.parse(None, 'debug: true')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1

        keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
        assert len(keyword_tokens) == 1
        assert keyword_tokens[0].value == 'true'

    def test_flow_map_keys(self):
        """Test keys in flow-style mapping."""
        parser = YAMLParser()
        parser.parse(None, '{x: 1, y: 2}')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        key_values = [t.value for t in key_tokens]
        assert 'x' in key_values
        assert 'y' in key_values


class TestYAMLListMarkers:
    """Test YAML list marker classification."""

    def test_simple_list_item(self):
        """Test a simple list item."""
        parser = YAMLParser()
        parser.parse(None, '- item')

        tokens = list(parser._tokens)
        list_tokens = [t for t in tokens if t.type.name == 'LIST_MARKER']
        assert len(list_tokens) == 1
        assert list_tokens[0].value == '- '

    def test_list_item_alone(self):
        """Test a bare dash at end of line."""
        parser = YAMLParser()
        parser.parse(None, '-')

        tokens = list(parser._tokens)
        list_tokens = [t for t in tokens if t.type.name == 'LIST_MARKER']
        assert len(list_tokens) == 1
        assert list_tokens[0].value == '-'

    def test_indented_list_item(self):
        """Test an indented list item."""
        parser = YAMLParser()
        parser.parse(None, '  - nested')

        tokens = list(parser._tokens)
        list_tokens = [t for t in tokens if t.type.name == 'LIST_MARKER']
        assert len(list_tokens) == 1
        assert list_tokens[0].value == '- '

    def test_list_item_with_key_value(self):
        """Test a list item containing a key-value pair."""
        parser = YAMLParser()
        parser.parse(None, '- key: value')

        tokens = list(parser._tokens)
        list_tokens = [t for t in tokens if t.type.name == 'LIST_MARKER']
        assert len(list_tokens) == 1

        key_tokens = [t for t in tokens if t.type.name == 'JSON_KEY']
        assert len(key_tokens) == 1
        assert key_tokens[0].value == 'key'

    def test_multiple_list_items(self):
        """Test multiple list items across lines."""
        lines = ['items:', '  - first', '  - second', '  - third']
        parser = YAMLParser()
        state = None

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)

        tokens = list(parser._tokens)
        list_tokens = [t for t in tokens if t.type.name == 'LIST_MARKER']
        assert len(list_tokens) == 1

    def test_dash_in_value_not_list_marker(self):
        """Test that a dash in a value position is not a list marker."""
        parser = YAMLParser()
        parser.parse(None, 'key: -')

        tokens = list(parser._tokens)
        list_tokens = [t for t in tokens if t.type.name == 'LIST_MARKER']
        assert len(list_tokens) == 0

    def test_negative_number_not_list_marker(self):
        """Test that a negative number value is not a list marker."""
        parser = YAMLParser()
        parser.parse(None, 'value: -42')

        tokens = list(parser._tokens)
        list_tokens = [t for t in tokens if t.type.name == 'LIST_MARKER']
        assert len(list_tokens) == 0

        number_tokens = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(number_tokens) == 1
        assert number_tokens[0].value == '-42'
