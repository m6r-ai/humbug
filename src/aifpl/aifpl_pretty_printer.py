"""Pretty-printer for AIFPL code with comment preservation."""

from typing import List, Optional
from dataclasses import dataclass

from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


@dataclass
class FormatOptions:
    """Options for controlling pretty-printer behavior."""
    max_line_width: int = 80
    indent_size: int = 2
    compact_threshold: int = 60  # Keep on one line if total length < this
    comment_spacing: int = 2  # Spaces before end-of-line comments


class PrettyPrinter:
    """
    Pretty-prints AIFPL code while preserving comments.

    Formatting rules:
    - 2-space indentation by default
    - Compact format for short expressions
    - Multi-line format for complex expressions
    - Special handling for let/letrec/lambda/if/match
    - Comments preserved with smart spacing
    - Blank line before comment blocks (unless first in scope)
    - End-of-line comments stay inline with 2-space padding
    """

    def __init__(self, options: Optional[FormatOptions] = None):
        """Initialize pretty-printer with options."""
        self.options = options or FormatOptions()
        self.lexer = AIFPLLexer()
        self.tokens: List[AIFPLToken] = []
        self.pos = 0
        self.current_token: Optional[AIFPLToken] = None
        self.last_token_line: int = 0  # Track line of last non-comment token

    def format(self, source_code: str) -> str:
        """
        Format AIFPL source code.

        Args:
            source_code: The AIFPL source code to format

        Returns:
            Formatted source code with preserved comments
        """
        # Lex with comments preserved
        self.tokens = self.lexer.lex(source_code, preserve_comments=True)
        self.pos = 0
        self.current_token = self.tokens[self.pos] if self.tokens else None
        self.last_token_line = 0

        output_parts = []
        prev_was_comment = False

        # Format all top-level expressions
        while self.current_token is not None:
            if self.current_token.type == AIFPLTokenType.COMMENT:
                # Check if this is an end-of-line comment
                is_eol = self._is_end_of_line_comment()

                if is_eol:
                    # End-of-line comment - add with spacing
                    output_parts.append(' ' * self.options.comment_spacing)
                    output_parts.append(self.current_token.value)
                    output_parts.append('\n')

                else:
                    # Standalone comment
                    if output_parts and not prev_was_comment:
                        output_parts.append('\n')
                    output_parts.append(self.current_token.value)
                    output_parts.append('\n')

                prev_was_comment = True
                self._advance()

            else:
                result = self._format_expression(0)
                output_parts.append(result)
                output_parts.append('\n')
                prev_was_comment = False

        # Join and clean up
        result = ''.join(output_parts)

        # Clean up: remove trailing spaces on each line
        lines = result.split('\n')
        lines = [line.rstrip() for line in lines]

        # Remove excessive blank lines (max 2 consecutive)
        cleaned_lines = []
        blank_count = 0
        for line in lines:
            if line == '':
                blank_count += 1
                if blank_count <= 2:
                    cleaned_lines.append(line)

            else:
                blank_count = 0
                cleaned_lines.append(line)

        # Ensure single trailing newline
        result = '\n'.join(cleaned_lines)
        while result.endswith('\n\n\n'):
            result = result[:-1]

        if result and not result.endswith('\n'):
            result += '\n'

        return result

    def _is_end_of_line_comment(self) -> bool:
        """Check if current comment token is an end-of-line comment."""
        if self.current_token is None or self.current_token.type != AIFPLTokenType.COMMENT:
            return False

        # Check if comment is on the same line as the last non-comment token
        return self.current_token.line == self.last_token_line

    def _format_expression(self, indent: int) -> str:
        """Format a single expression at the given indentation level."""
        if self.current_token is None:
            return ''

        token = self.current_token

        if token.type == AIFPLTokenType.LPAREN:
            return self._format_list(indent)

        if token.type == AIFPLTokenType.QUOTE:
            return self._format_quote(indent)

        if token.type == AIFPLTokenType.COMMENT:
            # Standalone comment
            comment = token.value
            self._advance()
            return ' ' * indent + comment

        # Atom (number, string, boolean, symbol)
        return self._format_atom()

    def _peek_ahead_for_comments(self, start_pos: int, end_pos: int) -> bool:
        """Check if there are any comments in the token range."""
        for i in range(start_pos, min(end_pos, len(self.tokens))):
            if self.tokens[i].type == AIFPLTokenType.COMMENT:
                return True

        return False

    def _find_matching_rparen(self, lparen_pos: int) -> int:
        """Find the position of the matching right paren."""
        depth = 0
        for i in range(lparen_pos, len(self.tokens)):
            if self.tokens[i].type == AIFPLTokenType.LPAREN:
                depth += 1

            elif self.tokens[i].type == AIFPLTokenType.RPAREN:
                depth -= 1
                if depth == 0:
                    return i

        return len(self.tokens)

    def _format_list(self, indent: int) -> str:
        """Format a list expression."""
        lparen_pos = self.pos
        rparen_pos = self._find_matching_rparen(lparen_pos)

        self._advance()  # consume '('

        if self.current_token is None or self.current_token.type == AIFPLTokenType.RPAREN:
            # Empty list
            self._advance()  # consume ')'
            return '()'

        # Check for special forms
        if self.current_token.type == AIFPLTokenType.SYMBOL:
            symbol = self.current_token.value

            if symbol in ('let', 'let*', 'letrec'):
                return self._format_let_form(symbol, indent)

            if symbol == 'lambda':
                return self._format_lambda(indent)

            if symbol == 'if':
                return self._format_if(indent)

            if symbol == 'match':
                return self._format_match(indent)

        # Check if we should use compact format
        has_comments = self._peek_ahead_for_comments(lparen_pos, rparen_pos)

        if not has_comments:
            # Try compact format
            compact = self._try_compact_list(indent)
            if compact and len(compact) <= self.options.compact_threshold:
                return compact

        # Use multi-line format
        return self._format_multiline_list(indent)

    def _try_compact_list(self, indent: int) -> Optional[str]:
        """Try to format the list compactly. Returns None if not possible."""
        saved_pos = self.pos
        saved_token = self.current_token

        parts = ['(']
        first = True

        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            if self.current_token.type == AIFPLTokenType.COMMENT:
                # Can't do compact with comments
                self.pos = saved_pos
                self.current_token = saved_token
                return None

            if self.current_token.type == AIFPLTokenType.LPAREN:
                # Try to format nested list compactly
                if not first:
                    parts.append(' ')

                self._advance()  # consume '(' for nested list
                nested = self._try_compact_list(indent)
                if nested is None:
                    # Nested list can't be compact, so parent can't either
                    self.pos = saved_pos
                    self.current_token = saved_token
                    return None

                parts.append(nested)
                first = False
                continue

            if not first:
                parts.append(' ')

            parts.append(self._format_atom())
            first = False

        parts.append(')')

        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

        return ''.join(parts)

    def _format_multiline_list(self, indent: int) -> str:
        """Format a list across multiple lines."""
        parts = ['(']
        first = True
        elem_indent = indent + self.options.indent_size
        prev_was_comment = False

        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            if self.current_token.type == AIFPLTokenType.COMMENT:
                is_eol = self._is_end_of_line_comment()

                if is_eol:
                    # End-of-line comment
                    parts.append(' ' * self.options.comment_spacing)
                    parts.append(self.current_token.value)
                    self._advance()
                    continue

                # Standalone comment
                if not first and not prev_was_comment:
                    parts.append('\n')

                parts.append('\n')
                parts.append(' ' * elem_indent)
                parts.append(self.current_token.value)
                self._advance()
                prev_was_comment = True
                continue

            if not first:
                parts.append('\n')
                parts.append(' ' * elem_indent)

            parts.append(self._format_expression(elem_indent))
            first = False
            prev_was_comment = False

        parts.append(')')

        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

        return ''.join(parts)

    def _format_let_form(self, form_type: str, indent: int) -> str:
        """Format let/let*/letrec expressions."""
        parts = [f'({form_type} (']
        self._advance()  # consume let/let*/letrec symbol

        # Expect bindings list
        if self.current_token is None or self.current_token.type != AIFPLTokenType.LPAREN:
            # Malformed
            if self.current_token is not None:
                parts.append(self._format_expression(indent))

            parts.append(')')
            if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
                self._advance()

            return ''.join(parts)

        self._advance()  # consume '(' for bindings

        # Format bindings
        binding_indent = indent + len(form_type) + 3  # Align with first binding
        first_binding = True
        prev_was_comment = False
        prev_binding_was_complex = False

        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            # Handle comments
            if self.current_token.type == AIFPLTokenType.COMMENT:
                is_eol = self._is_end_of_line_comment()

                if is_eol:
                    # End-of-line comment
                    parts.append(' ' * self.options.comment_spacing)
                    parts.append(self.current_token.value)
                    self._advance()
                    continue

                # Standalone comment before binding
                if not first_binding and not prev_was_comment:
                    parts.append('\n')

                parts.append('\n')
                parts.append(' ' * binding_indent)
                parts.append(self.current_token.value)
                self._advance()
                prev_was_comment = True
                first_binding = False  # Treat comment as taking up a "slot"
                continue

            # Add spacing before binding
            if not first_binding:
                # For letrec, add extra blank line between complex bindings
                if form_type == 'letrec' and prev_binding_was_complex:
                    parts.append('\n')

                parts.append('\n')
                parts.append(' ' * binding_indent)

            # Format binding
            binding_str = self._format_binding(binding_indent)
            parts.append(binding_str)

            prev_binding_was_complex = 'lambda' in binding_str or '\n' in binding_str
            first_binding = False
            prev_was_comment = False

        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()  # consume ')' for bindings

        parts.append(')')

        # Format body
        if self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            # Check for comment before body
            if self.current_token.type == AIFPLTokenType.COMMENT:
                is_eol = self._is_end_of_line_comment()
                if is_eol:
                    parts.append(' ' * self.options.comment_spacing)
                    parts.append(self.current_token.value)
                    self._advance()

                else:
                    parts.append('\n')
                    parts.append(' ' * (indent + self.options.indent_size))
                    parts.append(self.current_token.value)
                    self._advance()

            # Format body expression
            if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
                parts.append('\n')
                parts.append(' ' * (indent + self.options.indent_size))
                parts.append(self._format_expression(indent + self.options.indent_size))

        # Closing paren
        parts.append(')')
        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

        return ''.join(parts)

    def _format_binding(self, indent: int) -> str:
        """Format a single binding (name value)."""
        if self.current_token is None or self.current_token.type != AIFPLTokenType.LPAREN:
            return ''

        self._advance()  # consume '('

        parts = ['(']

        # Binding name
        if self.current_token:
            name = self._format_atom()
            parts.append(name)
            parts.append(' ')

            # Calculate indent for the value (after "(name ")
            # indent is where the binding starts, +1 for '(', +len(name), +1 for ' '
            value_indent = indent + 1 + len(name) + 1

            # Binding value
            if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
                parts.append(self._format_expression(value_indent))

        parts.append(')')
        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

        return ''.join(parts)

    def _format_lambda(self, indent: int) -> str:
        """Format lambda expressions."""
        parts = ['(lambda (']
        self._advance()  # consume 'lambda' symbol

        # Expect parameter list
        if self.current_token is None or self.current_token.type != AIFPLTokenType.LPAREN:
            if self.current_token is not None:
                parts.append(self._format_expression(indent))

            parts.append(')')
            if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
                self._advance()

            return ''.join(parts)

        self._advance()  # consume '(' for parameters

        # Format parameters
        first_param = True
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            if not first_param:
                parts.append(' ')

            parts.append(self._format_atom())
            first_param = False

        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()  # consume ')' for parameters

        parts.append(')')

        # Format body
        if self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append('\n')
            parts.append(' ' * (indent + self.options.indent_size))
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Closing paren
        parts.append(')')
        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

        return ''.join(parts)

    def _format_if(self, indent: int) -> str:
        """Format if expressions."""
        parts = ['(if ']
        self._advance()  # consume 'if' symbol

        # Condition
        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Then branch
        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append('\n')
            parts.append(' ' * (indent + self.options.indent_size))
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Else branch
        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append('\n')
            parts.append(' ' * (indent + self.options.indent_size))
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Closing paren
        parts.append(')')
        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

        return ''.join(parts)

    def _format_match(self, indent: int) -> str:
        """Format match expressions."""
        parts = ['(match ']
        self._advance()  # consume 'match' symbol

        # Value to match
        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Match clauses
        clause_indent = indent + self.options.indent_size
        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append('\n')
            parts.append(' ' * clause_indent)
            parts.append(self._format_expression(clause_indent))

        # Closing paren
        parts.append(')')
        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

        return ''.join(parts)

    def _format_quote(self, indent: int) -> str:
        """Format quoted expressions."""
        parts = ["'"]
        self._advance()  # consume quote

        if self.current_token:
            parts.append(self._format_expression(indent))

        return ''.join(parts)

    def _format_atom(self) -> str:
        """Format an atomic value (number, string, boolean, symbol)."""
        if self.current_token is None:
            return ''

        token = self.current_token
        result = ''

        if token.type == AIFPLTokenType.STRING:
            # Escape and quote the string
            result = f'"{self._escape_string(token.value)}"'

        elif token.type == AIFPLTokenType.BOOLEAN:
            result = '#t' if token.value else '#f'

        elif token.type == AIFPLTokenType.SYMBOL:
            result = str(token.value)

        elif token.type in (AIFPLTokenType.INTEGER, AIFPLTokenType.FLOAT, AIFPLTokenType.COMPLEX):
            result = str(token.value)

        # Track the line of this token before advancing
        self.last_token_line = token.line

        self._advance()
        return result

    def _escape_string(self, s: str) -> str:
        """Escape a string for output."""
        result = []
        for char in s:
            if char == '"':
                result.append('\\"')

            elif char == '\\':
                result.append('\\\\')

            elif char == '\n':
                result.append('\\n')

            elif char == '\t':
                result.append('\\t')

            elif char == '\r':
                result.append('\\r')

            elif ord(char) < 32:  # Other control characters
                result.append(f'\\u{ord(char):04x}')

            else:
                result.append(char)

        return ''.join(result)

    def _advance(self) -> None:
        """Move to the next token."""
        # Track line number before advancing (for non-comment tokens)
        if self.current_token and self.current_token.type != AIFPLTokenType.COMMENT:
            self.last_token_line = self.current_token.line

        self.pos += 1
        if self.pos < len(self.tokens):
            self.current_token = self.tokens[self.pos]

        else:
            self.current_token = None


def pretty_print(source_code: str, options: Optional[FormatOptions] = None) -> str:
    """
    Pretty-print AIFPL source code.

    Args:
        source_code: The AIFPL source code to format
        options: Optional formatting options

    Returns:
        Formatted source code with preserved comments

    Example:
        >>> code = "(let ((x 5)(y 10)) (+ x y))"
        >>> print(pretty_print(code))
        (let ((x 5)
              (y 10))
          (+ x y))
    """
    printer = PrettyPrinter(options)
    return printer.format(source_code)
