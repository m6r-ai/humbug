"""
C++ Lexer

This module implements a lexer for C++ code, extending the functionality of the C lexer.
"""
from syntax.c.c_lexer import CLexer
from syntax.lexer import Lexer


class CppLexer(CLexer):
    """
    Lexer for C++ code.

    This lexer extends the C lexer to handle C++-specific syntax including
    additional keywords and operators.
    """

    # Operators list
    _OPERATORS = [
        '>>=', '<<=', '&&=', '||=', '!=', '==', '+=', '-=', '*=',
        '/=', '%=', '&=', '|=', '^=', '<=', '>=', '&&', '||', '<<',
        '>>', '++', '--', '->', '::', '+', '-', '*', '/', '%', '&',
        '~', '!', '|', '^', '=', '<', '>', '(', ')', '{', '}', '[',
        ']', ';', ':', '?', '.', ','
    ]

    # Build the operator map
    _OPERATORS_MAP = Lexer.build_operator_map(_OPERATORS)

    def _is_keyword(self, value: str) -> bool:
        """
        Check if a given value is a C++ keyword.

        Args:
            value: The string to check

        Returns:
            True if the value is a C++ keyword, False otherwise
        """
        keywords = {
            'alignas', 'alignof', 'and', 'and_eq', 'asm',
            'atomic_cancel', 'atomic_commit', 'atomic_noexcept',
            'auto', 'bitand', 'bitor', 'bool', 'break', 'case',
            'catch', 'char', 'char16_t', 'char32_t', 'class',
            'compl', 'concept', 'const', 'const_cast', 'consteval',
            'constexpr', 'constinit', 'continue', 'co_await',
            'co_return', 'co_yield', 'decltype', 'default',
            'delete', 'do', 'double', 'dynamic_cast', 'else',
            'enum', 'explicit', 'export', 'extern', 'false',
            'float', 'for', 'friend', 'goto', 'if', 'inline',
            'int', 'long', 'mutable', 'namespace', 'new',
            'noexcept', 'not', 'not_eq', 'nullptr', 'operator',
            'or', 'or_eq', 'private', 'protected', 'public',
            'register', 'reinterpret_cast', 'requires', 'return',
            'short', 'signed', 'sizeof', 'static', 'static_assert',
            'static_cast', 'struct', 'switch', 'template', 'this',
            'thread_local', 'throw', 'true', 'try', 'typedef',
            'typeid', 'typename', 'union', 'unsigned', 'using',
            'virtual', 'void', 'volatile', 'wchar_t', 'while',
            'xor', 'xor_eq'
        }
        return value in keywords
