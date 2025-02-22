from typing import Set

from humbug.syntax.javascript.javascript_lexer import JavaScriptLexer


class TypeScriptLexer(JavaScriptLexer):
    """
    Lexer for TypeScript code.

    This lexer extends the JavaScript lexer to handle TypeScript-specific syntax,
    particularly its additional keywords.
    """

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a TypeScript keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a TypeScript keyword, False otherwise
        """
        keywords: Set[str] = {
            'abstract', 'any', 'as', 'asserts', 'async', 'await',
            'bigint', 'boolean', 'break', 'case', 'catch', 'class',
            'const', 'continue', 'debugger', 'declare', 'default',
            'delete', 'do', 'else', 'enum', 'export', 'extends',
            'false', 'finally', 'for', 'from', 'function', 'get',
            'if', 'implements', 'import', 'in', 'infer', 'instanceof',
            'interface', 'is', 'keyof', 'let', 'module', 'namespace',
            'new', 'null', 'number', 'object', 'of', 'private',
            'protected', 'public', 'readonly', 'return', 'require',
            'set', 'static', 'string', 'super', 'switch', 'symbol',
            'this', 'throw', 'true', 'try', 'type', 'typeof',
            'unique', 'unknown', 'var', 'void', 'while', 'with',
            'yield'
        }
        return value in keywords
