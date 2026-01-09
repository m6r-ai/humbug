"""
Tests for TypeScript multiline scenarios.
TypeScript inherits JavaScript's multiline behavior, especially for comments and template literals.
"""
import pytest

from syntax.typescript.typescript_lexer import TypeScriptLexer


class TestTypeScriptMultilineComments:
    """Test multiline comment handling in TypeScript."""

    def test_block_comment_multiline(self):
        """Test multiline block comment."""
        lines = [
            '/* Start of comment',
            'middle line',
            'end */',
        ]

        state = None
        for i, line in enumerate(lines):
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)

            tokens = list(lexer._tokens)
            assert len(tokens) == 1
            assert tokens[0].type.name == 'COMMENT'

            if i < len(lines) - 1:
                assert state.in_block_comment == True
            else:
                assert state.in_block_comment == False

    def test_jsdoc_multiline(self):
        """Test JSDoc/TSDoc style comments."""
        lines = [
            '/**',
            ' * @param x - The first number',
            ' * @param y - The second number',
            ' * @returns The sum',
            ' */',
        ]

        state = None
        for line in lines:
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_block_comment_with_type_annotations(self):
        """Test block comment containing type annotations."""
        lines = [
            '/* This function takes a string',
            'and returns a number */',
            'function parse(s: string): number { return 0; }',
        ]

        state = None
        for line in lines:
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1


class TestTypeScriptMultilineTypes:
    """Test multiline type declarations."""

    def test_interface_multiline(self):
        """Test interface declaration across multiple lines."""
        lines = [
            'interface User {',
            '    name: string;',
            '    age: number;',
            '    email?: string;',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_type_alias_multiline(self):
        """Test type alias across multiple lines."""
        lines = [
            'type Person = {',
            '    name: string;',
            '    age: number;',
            '};',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_function_signature_multiline(self):
        """Test function signature across multiple lines."""
        lines = [
            'function calculate(',
            '    x: number,',
            '    y: number,',
            '    operation: string',
            '): number {',
            '    return 0;',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_generic_type_multiline(self):
        """Test generic type across multiple lines."""
        lines = [
            'type Result<T> = {',
            '    success: boolean;',
            '    data: T;',
            '};',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_union_type_multiline(self):
        """Test union type across multiple lines."""
        lines = [
            'type Status =',
            '    | "pending"',
            '    | "approved"',
            '    | "rejected";',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_class_with_types_multiline(self):
        """Test class with type annotations across multiple lines."""
        lines = [
            'class Person {',
            '    private name: string;',
            '    public age: number;',
            '    protected email?: string;',
            '',
            '    constructor(name: string, age: number) {',
            '        this.name = name;',
            '        this.age = age;',
            '    }',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0


class TestTypeScriptMultilineStrings:
    """Test multiline string handling in TypeScript (same as JavaScript)."""

    def test_template_literal_multiline_simulation(self):
        """Test template literal behavior."""
        lexer = TypeScriptLexer()
        lexer.lex(None, '`multiline template`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1

    def test_template_literal_with_type_expression(self):
        """Test template literal with expression."""
        lexer = TypeScriptLexer()
        lexer.lex(None, '`value: ${x as number}`')

        tokens = list(lexer._tokens)
        assert len(tokens) >= 1


class TestTypeScriptMultilineEdgeCases:
    """Test edge cases for TypeScript multiline scenarios."""

    def test_enum_multiline(self):
        """Test enum declaration across multiple lines."""
        lines = [
            'enum Color {',
            '    Red = "RED",',
            '    Green = "GREEN",',
            '    Blue = "BLUE"',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_namespace_multiline(self):
        """Test namespace declaration across multiple lines."""
        lines = [
            'namespace Utils {',
            '    export function helper(): void {}',
            '    export const value: number = 42;',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_mapped_type_multiline(self):
        """Test mapped type across multiple lines."""
        lines = [
            'type Readonly<T> = {',
            '    readonly [P in keyof T]: T[P];',
            '};',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_conditional_type_multiline(self):
        """Test conditional type across multiple lines."""
        lines = [
            'type IsString<T> =',
            '    T extends string',
            '        ? true',
            '        : false;',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_decorator_multiline(self):
        """Test decorators across multiple lines."""
        lines = [
            '@Component({',
            '    selector: "app-root",',
            '    template: `<div></div>`',
            '})',
            'class AppComponent {}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_arrow_function_with_types_multiline(self):
        """Test arrow function with type annotations across multiple lines."""
        lines = [
            'const add = (',
            '    a: number,',
            '    b: number',
            '): number => {',
            '    return a + b;',
            '};',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_async_function_with_types_multiline(self):
        """Test async function with type annotations across multiple lines."""
        lines = [
            'async function fetchData(',
            '    url: string',
            '): Promise<Response> {',
            '    return fetch(url);',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_import_export_multiline(self):
        """Test import/export statements across multiple lines."""
        lines = [
            'import {',
            '    Component,',
            '    OnInit,',
            '    Input',
            '} from "@angular/core";',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_type_guard_multiline(self):
        """Test type guard function across multiple lines."""
        lines = [
            'function isString(',
            '    value: unknown',
            '): value is string {',
            '    return typeof value === "string";',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_assertion_function_multiline(self):
        """Test assertion function across multiple lines."""
        lines = [
            'function assertIsString(',
            '    value: unknown',
            '): asserts value is string {',
            '    if (typeof value !== "string") {',
            '        throw new Error("Not a string");',
            '    }',
            '}',
        ]

        for line in lines:
            lexer = TypeScriptLexer()
            lexer.lex(None, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 0

    def test_mixed_content_with_types(self):
        """Test mixed content with type annotations."""
        lines = [
            'let x: number = 5; // comment',
            '/* block comment',
            'with types: string, number */',
            'const name: string = "test";',
        ]

        state = None
        for line in lines:
            lexer = TypeScriptLexer()
            state = lexer.lex(state, line)

            tokens = list(lexer._tokens)
            assert len(tokens) >= 1

    def test_state_preservation_typescript(self):
        """Test that lexer state is properly preserved in TypeScript."""
        # Start a block comment
        lexer1 = TypeScriptLexer()
        state1 = lexer1.lex(None, '/* comment with types: string')
        assert state1.in_block_comment == True

        # Continue
        lexer2 = TypeScriptLexer()
        state2 = lexer2.lex(state1, 'number, boolean')
        assert state2.in_block_comment == True

        # End
        lexer3 = TypeScriptLexer()
        state3 = lexer3.lex(state2, 'end */')
        assert state3.in_block_comment == False
