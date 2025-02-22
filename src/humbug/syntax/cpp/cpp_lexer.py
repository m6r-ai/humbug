from humbug.syntax.c.c_lexer import CLexer
from humbug.syntax.lexer import Token


class CppLexer(CLexer):
    """
    Lexer for C++ code.

    This lexer extends the C lexer to handle C++-specific syntax including
    additional keywords and operators.
    """

    def _read_operator(self) -> None:
        """
        Read an operator or punctuation token.

        Extends the C lexer's operator handling to include C++-specific operators.
        """
        operators = [
            '>>=', '<<=', '&&=', '||=', '!=', '==', '+=', '-=', '*=',
            '/=', '%=', '&=', '|=', '^=', '<=', '>=', '&&', '||', '<<',
            '>>', '++', '--', '->', '::', '+', '-', '*', '/', '%', '&',
            '~', '!', '|', '^', '=', '<', '>', '(', ')', '{', '}', '[',
            ']', ';', ':', '?', '.', ','
        ]

        for operator in operators:
            if self._input[self._position:].startswith(operator):
                start = self._position
                self._position += len(operator)
                self._tokens.append(Token(
                    type='OPERATOR',
                    value=operator,
                    start=start
                ))
                return

        start = self._position
        ch = self._input[self._position]
        self._position += 1
        self._tokens.append(Token(type='ERROR', value=ch, start=start))

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
