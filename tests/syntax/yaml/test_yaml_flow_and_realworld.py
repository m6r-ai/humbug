"""
Tests for YAML flow-style constructs and real-world integration scenarios.
"""
from syntax.yaml.yaml_parser import YAMLParser
from syntax.lexer import TokenType


class TestYAMLFlowStyle:
    """Test YAML flow-style (inline) syntax."""

    def test_flow_sequence(self):
        """Test inline list [a, b, c]."""
        parser = YAMLParser()
        parser.parse(None, 'items: [a, b, c]')

        tokens = list(parser._tokens)
        operators = [t.value for t in tokens if t.type == TokenType.OPERATOR]
        assert '[' in operators
        assert ']' in operators

    def test_flow_mapping(self):
        """Test inline map {x: 1}."""
        parser = YAMLParser()
        parser.parse(None, 'config: {x: 1, y: 2}')

        tokens = list(parser._tokens)
        operators = [t.value for t in tokens if t.type == TokenType.OPERATOR]
        assert '{' in operators
        assert '}' in operators

    def test_flow_keys_are_classified(self):
        """Test that keys inside flow mappings are classified as JSON_KEY."""
        parser = YAMLParser()
        parser.parse(None, '{x: 1, y: 2}')

        tokens = list(parser._tokens)
        key_tokens = [t for t in tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'x' in key_values
        assert 'y' in key_values

    def test_nested_flow(self):
        """Test nested flow constructs."""
        parser = YAMLParser()
        parser.parse(None, 'data: {inner: [1, 2]}')

        tokens = list(parser._tokens)
        operators = [t.value for t in tokens if t.type == TokenType.OPERATOR]
        assert '{' in operators
        assert '[' in operators
        assert ']' in operators
        assert '}' in operators


class TestYAMLRealWorld:
    """Test real-world YAML documents."""

    def test_docker_compose_style(self):
        """Test a docker-compose-like YAML document."""
        lines = [
            'version: "3"',
            'services:',
            '  web:',
            '    image: nginx',
            '    ports:',
            '      - 80:80',
            '      - 443:443',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'version' in key_values
        assert 'services' in key_values
        assert 'web' in key_values
        assert 'image' in key_values
        assert 'ports' in key_values

    def test_github_actions_style(self):
        """Test a GitHub Actions-like YAML document."""
        lines = [
            'name: CI',
            'on:',
            '  push:',
            '    branches:',
            '      - main',
            'jobs:',
            '  build:',
            '    runs-on: ubuntu-latest',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'name' in key_values
        assert 'push' in key_values
        assert 'branches' in key_values
        assert 'jobs' in key_values
        assert 'build' in key_values

    def test_config_with_comments(self):
        """Test a config file with comments."""
        lines = [
            '# Database configuration',
            'database:',
            '  host: localhost',
            '  port: 5432  # default port',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'database' in key_values
        assert 'host' in key_values
        assert 'port' in key_values

        comment_tokens = [t for t in all_tokens if t.type == TokenType.COMMENT]
        comment_values = [t.value for t in comment_tokens]
        assert any('Database configuration' in c for c in comment_values)
        assert any('default port' in c for c in comment_values)

    def test_multi_document(self):
        """Test multi-document YAML with --- and ... markers."""
        lines = [
            '---',
            'name: first',
            '...',
            '---',
            'name: second',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        doc_tokens = [t for t in all_tokens if t.type == TokenType.DOC_COMMENT]
        assert len(doc_tokens) == 3, "Should have ---, ..., --- markers"

    def test_anchors_and_merge(self):
        """Test anchors and merge keys."""
        lines = [
            'defaults: &defs',
            '  timeout: 30',
            '  retries: 3',
            'production:',
            '  <<: *defs',
            '  timeout: 60',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        hash_tokens = [t for t in all_tokens if t.type == TokenType.HASH]
        hash_values = [t.value for t in hash_tokens]
        assert '&defs' in hash_values
        assert '*defs' in hash_values

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'defaults' in key_values
        assert 'production' in key_values
        assert 'timeout' in key_values
        assert 'retries' in key_values

    def test_block_scalar_in_real_document(self):
        """Test a realistic document with block scalars."""
        lines = [
            'deploy:',
            '  script: |',
            '    #!/bin/bash',
            '    echo "Deploying..."',
            '    npm run build',
            '  timeout: 300',
            '  enabled: true',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'deploy' in key_values
        assert 'script' in key_values
        assert 'timeout' in key_values
        assert 'enabled' in key_values, "Key after block scalar should be classified normally"

        string_content = [
            t for t in all_tokens
            if t.type == TokenType.STRING and 'Deploying' in t.value
        ]
        assert len(string_content) == 1

    def test_empty_lines(self):
        """Test that empty lines produce no errors."""
        lines = [
            'key: value',
            '',
            'other: data',
        ]
        parser = YAMLParser()
        state = None

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)

    def test_deeply_nested(self):
        """Test deeply nested structures."""
        lines = [
            'a:',
            '  b:',
            '    c:',
            '      d: deep_value',
        ]
        parser = YAMLParser()
        state = None
        all_tokens: list = []

        for line in lines:
            parser = YAMLParser()
            state = parser.parse(state, line)
            all_tokens.extend(parser._tokens)

        key_tokens = [t for t in all_tokens if t.type == TokenType.JSON_KEY]
        key_values = [t.value for t in key_tokens]
        assert 'a' in key_values
        assert 'b' in key_values
        assert 'c' in key_values
        assert 'd' in key_values
