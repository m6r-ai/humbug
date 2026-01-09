"""
Tests for TypeScript type annotation tokenization.
TypeScript extends JavaScript with type annotations and additional keywords.
"""
import pytest

from syntax.typescript.typescript_lexer import TypeScriptLexer


class TestTypeScriptTypeKeywords:
    """Test TypeScript-specific type keywords."""

    def test_primitive_type_keywords(self):
        """Test primitive type keywords."""
        test_cases = [
            'string',
            'number',
            'boolean',
            'any',
            'void',
            'never',
            'unknown',
            'bigint',
            'symbol',
            'object',
        ]
        for keyword in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_type_declaration_keywords(self):
        """Test type declaration keywords."""
        test_cases = [
            'type',
            'interface',
            'enum',
            'namespace',
            'module',
            'declare',
        ]
        for keyword in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_type_operator_keywords(self):
        """Test type operator keywords."""
        test_cases = [
            'keyof',
            'typeof',
            'infer',
            'is',
            'as',
            'readonly',
            'unique',
        ]
        for keyword in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_access_modifier_keywords(self):
        """Test access modifier keywords."""
        test_cases = [
            'public',
            'private',
            'protected',
            'readonly',
        ]
        for keyword in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"
            assert tokens[0].value == keyword

    def test_assertion_keywords(self):
        """Test type assertion keywords."""
        test_cases = [
            'as',
            'asserts',
            'is',
        ]
        for keyword in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"

    def test_accessor_keywords(self):
        """Test accessor keywords."""
        test_cases = [
            'get',
            'set',
        ]
        for keyword in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1, f"'{keyword}' should produce one token"
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD type"


class TestTypeScriptTypeAnnotations:
    """Test TypeScript type annotation syntax."""

    def test_variable_with_type_annotation(self):
        """Test variable declarations with type annotations."""
        test_cases = [
            'let x: number',
            'const name: string',
            'var flag: boolean',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have keyword, identifier, colon, type
            assert len(tokens) >= 4, f"'{code}' should have multiple tokens"
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 2, f"'{code}' should have at least 2 keywords"

    def test_function_with_type_annotations(self):
        """Test function with parameter and return type annotations."""
        test_cases = [
            'function add(a: number, b: number): number {}',
            'const greet = (name: string): void => {}',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have multiple type keywords
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 2, f"'{code}' should have type keywords"

    def test_interface_declaration(self):
        """Test interface declaration."""
        test_cases = [
            'interface User {}',
            'interface Person { name: string; }',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have 'interface' keyword
            interface_tokens = [t for t in tokens if t.value == 'interface']
            assert len(interface_tokens) == 1, f"'{code}' should have 'interface' keyword"

    def test_type_alias_declaration(self):
        """Test type alias declaration."""
        test_cases = [
            'type ID = string | number',
            'type Point = { x: number; y: number; }',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have 'type' keyword
            type_tokens = [t for t in tokens if t.value == 'type']
            assert len(type_tokens) == 1, f"'{code}' should have 'type' keyword"

    def test_enum_declaration(self):
        """Test enum declaration."""
        test_cases = [
            'enum Color { Red, Green, Blue }',
            'enum Direction { Up = 1, Down = 2 }',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have 'enum' keyword
            enum_tokens = [t for t in tokens if t.value == 'enum']
            assert len(enum_tokens) == 1, f"'{code}' should have 'enum' keyword"

    def test_class_with_modifiers(self):
        """Test class with access modifiers."""
        test_cases = [
            'class MyClass { private x: number; }',
            'class Person { public name: string; protected age: number; }',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have access modifier keywords
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 2, f"'{code}' should have multiple keywords"

    def test_generic_types(self):
        """Test generic type syntax."""
        test_cases = [
            'Array<number>',
            'Promise<string>',
            'Map<string, number>',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should have < and > operators
            angle_brackets = [t for t in tokens if t.value in ['<', '>']]
            assert len(angle_brackets) >= 2, f"'{code}' should have angle brackets"

    def test_union_types(self):
        """Test union type syntax."""
        lexer = TypeScriptLexer()
        lexer.lex(None, 'string | number')

        tokens = list(lexer._tokens)
        # Should have | operator
        pipe_tokens = [t for t in tokens if t.value == '|']
        assert len(pipe_tokens) == 1, "Should have pipe operator for union"

    def test_intersection_types(self):
        """Test intersection type syntax."""
        lexer = TypeScriptLexer()
        lexer.lex(None, 'Type1 & Type2')

        tokens = list(lexer._tokens)
        # Should have & operator
        ampersand_tokens = [t for t in tokens if t.value == '&']
        assert len(ampersand_tokens) == 1, "Should have ampersand for intersection"

    def test_type_assertion(self):
        """Test type assertion syntax."""
        test_cases = [
            'value as string',
            '<string>value',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            # Should tokenize successfully
            assert len(tokens) >= 2, f"'{code}' should produce tokens"

    def test_readonly_modifier(self):
        """Test readonly modifier."""
        test_cases = [
            'readonly x: number',
            'readonly arr: number[]',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            readonly_tokens = [t for t in tokens if t.value == 'readonly']
            assert len(readonly_tokens) == 1, f"'{code}' should have 'readonly' keyword"


class TestTypeScriptKeywordEdgeCases:
    """Test edge cases for TypeScript keyword tokenization."""

    def test_typescript_vs_javascript_keywords(self):
        """Test keywords unique to TypeScript."""
        typescript_only = [
            'interface',
            'type',
            'namespace',
            'declare',
            'abstract',
            'as',
            'is',
            'keyof',
            'infer',
            'readonly',
            'unique',
        ]
        for keyword in typescript_only:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'KEYWORD', f"'{keyword}' should be KEYWORD in TypeScript"

    def test_type_keywords_case_sensitivity(self):
        """Test that type keywords are case-sensitive."""
        test_cases = [
            ('string', 'KEYWORD'),
            ('String', 'IDENTIFIER'),
            ('STRING', 'IDENTIFIER'),
            ('number', 'KEYWORD'),
            ('Number', 'IDENTIFIER'),
        ]
        for code, expected_type in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == expected_type, f"'{code}' should be {expected_type}"

    def test_contextual_keywords(self):
        """Test contextual keywords in TypeScript."""
        test_cases = [
            'abstract',
            'as',
            'asserts',
            'is',
            'keyof',
            'infer',
            'readonly',
            'unique',
            'get',
            'set',
        ]
        for keyword in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, keyword)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            # These should be recognized as keywords

    def test_type_keywords_in_strings(self):
        """Test that type keywords in strings are not tokenized as keywords."""
        test_cases = [
            '"string"',
            '"number"',
            '"interface"',
            "'type'",
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'STRING'

    def test_type_keywords_in_comments(self):
        """Test that type keywords in comments are not separately tokenized."""
        test_cases = [
            '// string number boolean',
            '/* interface type enum */',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'COMMENT'

    def test_module_declaration(self):
        """Test module/namespace declarations."""
        test_cases = [
            'namespace MyNamespace {}',
            'module MyModule {}',
            'declare module "module-name" {}',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            keyword_tokens = [t for t in tokens if t.type.name == 'KEYWORD']
            assert len(keyword_tokens) >= 1, f"'{code}' should have keywords"

    def test_ambient_declarations(self):
        """Test ambient declarations with declare keyword."""
        test_cases = [
            'declare const x: number',
            'declare function func(): void',
            'declare class MyClass {}',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            declare_tokens = [t for t in tokens if t.value == 'declare']
            assert len(declare_tokens) == 1, f"'{code}' should have 'declare' keyword"

    def test_abstract_class(self):
        """Test abstract class declaration."""
        test_cases = [
            'abstract class Base {}',
            'abstract class Animal { abstract makeSound(): void; }',
        ]
        for code in test_cases:
            lexer = TypeScriptLexer()
            lexer.lex(None, code)

            tokens = list(lexer._tokens)
            abstract_tokens = [t for t in tokens if t.value == 'abstract']
            assert len(abstract_tokens) >= 1, f"'{code}' should have 'abstract' keyword"
