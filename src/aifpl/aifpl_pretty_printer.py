"""Pretty-printer for AIFPL code with comment preservation."""

from typing import List, Optional, cast
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


class AIFPLPrettyPrinter:
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
        prev_token_line = 0  # Track line number of previous token

        # Format all top-level expressions
        while self.current_token is not None:
            current_line = self.current_token.line

            # Check if there's a blank line in the original source
            # (more than 1 line gap between tokens)
            has_blank_line_before = (current_line - prev_token_line) > 1

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
                    # Add blank line before comment if:
                    # 1. There was a blank line in the original source, OR
                    # 2. Previous was not a comment (separating code from comments)
                    if output_parts and (has_blank_line_before or not prev_was_comment):
                        output_parts.append('\n')
                    output_parts.append(self.current_token.value)
                    output_parts.append('\n')

                prev_was_comment = True
                prev_token_line = current_line
                self._advance()

            else:
                # Code expression
                # Add blank line before code if there was one in the original
                if output_parts and has_blank_line_before:
                    output_parts.append('\n')

                result = self._format_expression(0)
                output_parts.append(result)

                had_eol_comment = False
                # Check for end-of-line comment after the expression
                if self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
                    is_eol = self._is_end_of_line_comment()
                    if is_eol:
                        # Add EOL comment on same line
                        output_parts.append(' ' * self.options.comment_spacing)
                        output_parts.append(self.current_token.value)
                        self._advance()
                        had_eol_comment = True

                    # If not EOL, it will be handled in next iteration

                output_parts.append('\n')

                # prev_was_comment is True only if we just processed an EOL comment
                prev_was_comment = had_eol_comment
                prev_token_line = current_line

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
            saved_pos = self.pos
            saved_token = self.current_token
            compact = self._try_compact_list(indent)
            if compact and len(compact) <= self.options.compact_threshold:
                return compact

            # Compact format was too long or failed, restore position
            self.pos = saved_pos
            self.current_token = saved_token

        # Use multi-line format
        return self._format_multiline_list(indent)

    def _try_compact_list(self, indent: int) -> Optional[str]:
        """Try to format the list compactly. Returns None if not possible."""
        saved_pos = self.pos
        saved_token = self.current_token

        parts = ['(']
        first = True

        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            # Early bailout: if we've already exceeded threshold, stop trying
            current_length = len(''.join(parts))
            if current_length > self.options.compact_threshold:
                self.pos = saved_pos
                self.current_token = saved_token
                return None

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
        element_count = 0
        # Start with default indent
        elem_indent = indent + 1  # After opening paren
        prev_was_comment = False
        first_elem_str: str | None = None

        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            if self.current_token.type == AIFPLTokenType.COMMENT:
                if first:
                    parts.append(' ')

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

            # Add newline before element if it's not first or second
            if element_count >= 2:
                parts.append('\n')
                parts.append(' ' * elem_indent)

            elem_str = self._format_expression(elem_indent)

            if element_count == 0:
                # First element (function name) - no space before it
                parts.append(elem_str)
                first_elem_str = elem_str

            elif element_count == 1:
                # Second element (first argument) - space before it, on same line
                parts.append(' ')
                parts.append(elem_str)
                # Calculate indent for remaining elements: align under second element
                # Position is: indent + 1 (for '(') + len(first_elem) + 1 (space)
                elem_indent = indent + 1 + len(cast(str, first_elem_str)) + 1

            else:
                # Third+ elements - already have newline and indent from above
                parts.append(elem_str)

            element_count += 1
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
            if not binding_str:
                # _format_binding returned empty (token wasn't an LPAREN)
                # This means we've reached the end of bindings
                break

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
            while self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
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

        # Handle comments after body but before closing paren
        while self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
            is_eol = self._is_end_of_line_comment()
            if not is_eol:
                # Standalone comment - shouldn't happen here normally
                break

            parts.append(' ' * self.options.comment_spacing)
            parts.append(self.current_token.value)
            self._advance()

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

            # Binding value
            if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
                parts.append(' ')

                # Calculate indent for the value (after "(name ")
                # indent is where the binding starts, +1 for '(', +len(name), +1 for ' '
                value_indent = indent + 1 + len(name) + 1
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
        # Handle comments before the body expression
        while self.current_token is not None and self.current_token.type == AIFPLTokenType.COMMENT:
            is_eol = self._is_end_of_line_comment()
            if is_eol:
                parts.append(' ' * self.options.comment_spacing)
                parts.append(self.current_token.value)
                self._advance()

            else:
                # Standalone comment in lambda body
                parts.append('\n')
                parts.append(' ' * (indent + self.options.indent_size))
                parts.append(self.current_token.value)
                self._advance()

        # Format the actual body expression
        if self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append('\n')
            parts.append(' ' * (indent + self.options.indent_size))
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Handle comments after body but before closing paren
        while self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
            is_eol = self._is_end_of_line_comment()
            if not is_eol:
                # Standalone comment - shouldn't happen here normally
                break

            parts.append(' ' * self.options.comment_spacing)
            parts.append(self.current_token.value)
            self._advance()

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
        # Handle comments before then branch
        while self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
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

        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append('\n')
            parts.append(' ' * (indent + self.options.indent_size))
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Else branch
        # Handle comments before else branch
        while self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
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

        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            parts.append('\n')
            parts.append(' ' * (indent + self.options.indent_size))
            parts.append(self._format_expression(indent + self.options.indent_size))

        # Handle comments after else branch but before closing paren
        while self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
            is_eol = self._is_end_of_line_comment()
            if not is_eol:
                # Standalone comment - shouldn't happen here normally
                break

            parts.append(' ' * self.options.comment_spacing)
            parts.append(self.current_token.value)
            self._advance()

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
            # Handle comments between clauses
            if self.current_token.type == AIFPLTokenType.COMMENT:
                is_eol = self._is_end_of_line_comment()
                if is_eol:
                    parts.append(' ' * self.options.comment_spacing)
                    parts.append(self.current_token.value)
                    self._advance()
                else:
                    parts.append('\n')
                    parts.append(' ' * clause_indent)
                    parts.append(self.current_token.value)
                    self._advance()
            else:
                # Regular clause
                parts.append('\n')
                parts.append(' ' * clause_indent)
                parts.append(self._format_expression(clause_indent))

        # Handle comments after last clause but before closing paren
        while self.current_token and self.current_token.type == AIFPLTokenType.COMMENT:
            is_eol = self._is_end_of_line_comment()
            if not is_eol:
                # Standalone comment - shouldn't happen here normally
                break

            parts.append(' ' * self.options.comment_spacing)
            parts.append(self.current_token.value)
            self._advance()

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
