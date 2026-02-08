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


class OutputBuilder:
    """
    Helper class for building formatted output with automatic spacing/indentation.
    Tracks state to avoid duplicate newlines and handle indentation consistently.
    """

    def __init__(self, options: FormatOptions):
        """Initialize the output builder."""
        self.options = options
        self.parts: List[str] = []

    def add(self, text: str) -> None:
        """Add text to output without any automatic spacing."""
        self.parts.append(text)

    def add_space(self) -> None:
        """Add a single space."""
        self.parts.append(' ')

    def add_newline(self) -> None:
        """Add a newline."""
        self.parts.append('\n')

    def ensure_newline(self) -> None:
        """Add a newline only if the last character isn't already a newline."""
        if not (self.parts and self.parts[-1].endswith('\n')):
            self.parts.append('\n')

    def add_indent(self, indent: int) -> None:
        """Add indentation (spaces)."""
        self.parts.append(' ' * indent)

    def add_line(self, text: str, indent: int) -> None:
        """Add text on a new line with indentation."""
        self.ensure_newline()
        self.add_indent(indent)
        self.add(text)

    def add_inline(self, text: str) -> None:
        """Add text inline (same line, no spacing)."""
        self.add(text)

    def add_eol_comment(self, comment_text: str, prefix: str = '') -> None:
        """Add an end-of-line comment with proper spacing and newline."""
        if prefix:
            self.add(prefix)
        self.add(' ' * self.options.comment_spacing)
        self.add(comment_text)
        self.add_newline()

    def add_standalone_comment(self, comment_text: str, indent: int) -> None:
        """Add a standalone comment on its own line with indentation."""
        self.add_indent(indent)
        self.add(comment_text)
        self.add_newline()

    def ends_with_newline(self) -> bool:
        """Check if output currently ends with a newline."""
        return bool(self.parts and self.parts[-1].endswith('\n'))

    def add_closing_paren_with_indent(self, indent: int) -> None:
        """Add closing paren, with indent if last char is newline."""
        if self.ends_with_newline():
            self.add_indent(indent)
        self.add(')')

    def get_output(self) -> str:
        """Get the final output string."""
        return ''.join(self.parts)

    def get_parts(self) -> List[str]:
        """Get the parts list (for compatibility with existing code)."""
        return self.parts


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

        out = OutputBuilder(self.options)
        prev_was_comment = False
        prev_token_line = 0  # Track line number of previous token

        # Format all top-level expressions
        while self.current_token is not None:
            current_line = self.current_token.line

            # Check if there's a blank line in the original source
            # (more than 1 line gap between tokens)
            has_blank_line_before = (current_line - prev_token_line) > 1

            if self.current_token.type == AIFPLTokenType.COMMENT:
                # Standalone comment.  Add blank line before comment if:
                # 1. There was a blank line in the original source, OR
                # 2. Previous was not a comment (separating code from comments)
                if out.get_parts() and (has_blank_line_before or not prev_was_comment):
                    out.add_newline()

                out.add(self.current_token.value)
                out.add_newline()

                prev_was_comment = True
                prev_token_line = current_line
                self._advance()

            else:
                # Code expression
                # Add blank line before code if there was one in the original
                if out.get_parts() and has_blank_line_before:
                    out.add_newline()

                result = self._format_expression(0)
                out.add(result)

                # Check for end-of-line comment after the expression
                had_eol_comment = self._handle_comments(out, 0, handle_standalone=False)

                out.add_newline()

                # prev_was_comment is True only if we just processed an EOL comment
                prev_was_comment = had_eol_comment
                prev_token_line = current_line

        # Join and clean up
        result = out.get_output()

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
        return '\n'.join(cleaned_lines)

    def _handle_comments(self, out: OutputBuilder, indent: int,
                        handle_eol: bool = True,
                        handle_standalone: bool = True,
                        add_leading_newline: bool = True,
                        eol_prefix: str = '') -> bool:
        """
        Handle both EOL and standalone comments at current position.

        Args:
            out: OutputBuilder to append formatted comments to
            indent: Indentation level for standalone comments
            handle_eol: If True, check for and format EOL comments
            handle_standalone: If True, check for and format standalone comments
            add_leading_newline: If True, adds newline before standalone comments
            eol_prefix: Optional prefix before EOL comment spacing

        Returns:
            True if any comments were processed, False otherwise
        """
        if self.current_token is None or self.current_token.type != AIFPLTokenType.COMMENT:
            return False

        found_any = False

        # Handle EOL comment first (same line as last token)
        if handle_eol and self.current_token.line == self.last_token_line:
            out.add_eol_comment(self.current_token.value, eol_prefix)
            self._advance()
            found_any = True

        # Handle standalone comments (different line from last token)
        if handle_standalone:
            # Add leading newline if requested
            if add_leading_newline and not out.ends_with_newline():
                out.add_newline()

            while (self.current_token and
                   self.current_token.type == AIFPLTokenType.COMMENT and
                   self.current_token.line != self.last_token_line):
                out.add_standalone_comment(self.current_token.value, indent)
                self._advance()
                found_any = True

        return found_any

    def _consume_rparen(self) -> None:
        """Consume a right paren token if present."""
        if self.current_token and self.current_token.type == AIFPLTokenType.RPAREN:
            self._advance()

    def _format_branch_with_comments(self, indent: int, out: OutputBuilder) -> None:
        """Format a branch with comments, then the expression."""
        self._handle_comments(out, indent)
        # Format the branch expression
        if self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            out.add_line(self._format_expression(indent), indent)

    def _finish_form(self, indent: int, out: OutputBuilder) -> str:
        """Finish a special form: trailing comment, closing paren, consume RPAREN."""
        if self._handle_comments(out, indent, handle_standalone=False):
            out.add_indent(indent)
        out.add_closing_paren_with_indent(indent)

        # Consume the RPAREN token
        self._consume_rparen()

        return out.get_output()

    def _format_expression(self, indent: int) -> str:
        """Format a single expression at the given indentation level."""
        if self.current_token is None:
            return ''

        token = self.current_token

        if token.type == AIFPLTokenType.LPAREN:
            return self._format_list(indent)

        if token.type == AIFPLTokenType.QUOTE:
            return self._format_quote(indent)

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

        out = OutputBuilder(self.options)
        out.add('(')
        first = True

        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            # Early bailout: if we've already exceeded threshold, stop trying
            current_length = len(out.get_output())
            if current_length > self.options.compact_threshold:
                self.pos = saved_pos
                self.current_token = saved_token
                return None

            if self.current_token.type == AIFPLTokenType.LPAREN:
                # Try to format nested list compactly
                if not first:
                    out.add_space()

                self._advance()  # consume '(' for nested list
                nested = self._try_compact_list(indent)
                if nested is None:
                    # Nested list can't be compact, so parent can't either
                    self.pos = saved_pos
                    self.current_token = saved_token
                    return None

                out.add(nested)
                first = False
                continue

            if not first:
                out.add_space()

            out.add(self._format_expression(indent))
            first = False

        out.add(')')

        self._consume_rparen()
        return out.get_output()

    def _format_multiline_list(self, indent: int) -> str:
        """Format a list across multiple lines."""
        out = OutputBuilder(self.options)
        out.add('(')
        first = True
        element_count = 0
        # Start with default indent
        elem_indent = indent + 1  # After opening paren
        prev_was_comment = False
        first_elem_str: str | None = None

        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            # Handle EOL comments
            if self._handle_comments(out, elem_indent, handle_standalone=False, eol_prefix=' ' if first else ''):
                continue

            # Handle standalone comments
            add_newline = not first and not prev_was_comment
            if self._handle_comments(out, elem_indent, handle_eol=False, add_leading_newline=add_newline):
                prev_was_comment = True
                continue

            # Add newline before element if it's not first or second
            if element_count >= 2:
                out.add_newline()
                out.add_indent(elem_indent)

            elem_str = self._format_expression(elem_indent)

            if element_count == 0:
                # First element (function name) - no space before it
                out.add(elem_str)
                first_elem_str = elem_str

            elif element_count == 1:
                # Second element (first argument) - space before it, on same line
                out.add_space()
                out.add(elem_str)
                # Calculate indent for remaining elements: align under second element
                # Position is: indent + 1 (for '(') + len(first_elem) + 1 (space)
                elem_indent = indent + 1 + len(cast(str, first_elem_str)) + 1

            else:
                # Third+ elements - already have newline and indent from above
                out.add(elem_str)

            element_count += 1
            first = False
            prev_was_comment = False

        out.add(')')

        self._consume_rparen()
        return out.get_output()

    def _format_let_form(self, form_type: str, indent: int) -> str:
        """Format let/let*/letrec expressions."""
        out = OutputBuilder(self.options)
        out.add(f'({form_type} (')
        self._advance()  # consume let/let*/letrec symbol

        # Expect bindings list
        if self.current_token is None or self.current_token.type != AIFPLTokenType.LPAREN:
            # Malformed
            if self.current_token is not None:
                out.add(self._format_expression(indent))

            out.add(')')
            self._consume_rparen()

            return out.get_output()

        self._advance()  # consume '(' for bindings

        # Format bindings
        binding_indent = indent + len(form_type) + 3  # Align with first binding
        first_binding = True
        prev_was_comment = False
        prev_binding_was_complex = False

        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            # Handle comments
            if self._handle_comments(out, binding_indent, handle_standalone=False):
                continue

            # Handle standalone comments
            add_newline = not first_binding and not prev_was_comment
            if self._handle_comments(out, binding_indent, handle_eol=False, add_leading_newline=add_newline):
                prev_was_comment = True
                first_binding = False
                continue

            # Add spacing before binding
            if not first_binding:
                # For letrec, add extra blank line between complex bindings
                if form_type == 'letrec' and prev_binding_was_complex:
                    out.add_newline()

                out.add_newline()
                out.add_indent(binding_indent)

            # Format binding
            binding_str = self._format_binding(binding_indent)
            if not binding_str:
                # _format_binding returned empty (token wasn't an LPAREN)
                # This means we've reached the end of bindings
                break

            out.add(binding_str)

            prev_binding_was_complex = 'lambda' in binding_str or '\n' in binding_str
            first_binding = False
            prev_was_comment = False

        self._consume_rparen()  # consume ')' for bindings
        out.add(')')

        # Format body
        if self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            body_indent = indent + self.options.indent_size
            self._format_branch_with_comments(body_indent, out)

        # Handle comments after body but before closing paren
        self._handle_comments(out, indent, handle_standalone=False)

        # Closing paren
        out.add(')')
        self._consume_rparen()

        return out.get_output()

    def _format_binding(self, indent: int) -> str:
        """Format a single binding (name value)."""
        if self.current_token is None or self.current_token.type != AIFPLTokenType.LPAREN:
            return ''

        self._advance()  # consume '('

        out = OutputBuilder(self.options)
        out.add('(')

        # Binding name
        if self.current_token:
            name = self._format_atom()
            out.add(name)

            # Binding value
            if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
                out.add_space()

                # Calculate indent for the value (after "(name ")
                # indent is where the binding starts, +1 for '(', +len(name), +1 for ' '
                value_indent = indent + 1 + len(name) + 1
                out.add(self._format_expression(value_indent))

        out.add(')')
        self._consume_rparen()

        return out.get_output()

    def _format_lambda(self, indent: int) -> str:
        """Format lambda expressions."""
        out = OutputBuilder(self.options)
        out.add('(lambda (')
        self._advance()  # consume 'lambda' symbol

        # Expect parameter list
        if self.current_token is None or self.current_token.type != AIFPLTokenType.LPAREN:
            if self.current_token is not None:
                out.add(self._format_expression(indent))

            out.add(')')
            self._consume_rparen()
            return out.get_output()

        self._advance()  # consume '(' for parameters

        # Format parameters
        first_param = True
        while self.current_token is not None and self.current_token.type != AIFPLTokenType.RPAREN:
            if not first_param:
                out.add_space()

            out.add(self._format_atom())
            first_param = False

        self._consume_rparen()  # consume ')' for parameters
        out.add(')')

        # Format body
        body_indent = indent + self.options.indent_size
        self._format_branch_with_comments(body_indent, out)

        return self._finish_form(indent, out)

    def _format_if(self, indent: int) -> str:
        """Format if expressions."""
        out = OutputBuilder(self.options)
        out.add('(if ')
        self._advance()  # consume 'if' symbol

        branch_indent = indent + self.options.indent_size

        # Condition
        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            out.add(self._format_expression(branch_indent))

        # Then branch
        self._format_branch_with_comments(branch_indent, out)

        # Else branch
        self._format_branch_with_comments(branch_indent, out)

        return self._finish_form(indent, out)

    def _format_match(self, indent: int) -> str:
        """Format match expressions."""
        out = OutputBuilder(self.options)
        out.add('(match ')
        self._advance()  # consume 'match' symbol

        # Value to match
        if self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            out.add(self._format_expression(indent + self.options.indent_size))

        # Match clauses
        clause_indent = indent + self.options.indent_size
        while self.current_token and self.current_token.type != AIFPLTokenType.RPAREN:
            # Format each clause with comment handling
            self._format_branch_with_comments(clause_indent, out)

        return self._finish_form(indent, out)

    def _format_quote(self, indent: int) -> str:
        """Format quoted expressions."""
        out = OutputBuilder(self.options)
        out.add("'")
        self._advance()  # consume quote

        if self.current_token:
            out.add(self._format_expression(indent))

        return out.get_output()

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
