"""Multi-pass AIFPL pretty printer with clean separation of concerns."""

from typing import List, Optional, Union
from dataclasses import dataclass
from enum import Enum

from aifpl.aifpl_lexer import AIFPLLexer
from aifpl.aifpl_token import AIFPLToken, AIFPLTokenType


@dataclass
class FormatOptions:
    """Options for controlling pretty-printer behavior."""
    indent_size: int = 2
    compact_threshold: int = 60
    comment_spacing: int = 2


class ASTNode:
    """Base class for AST nodes."""
    source_line: int


@dataclass
class ASTAtom(ASTNode):
    """An atomic value (symbol, number, string, boolean)."""
    value: str

    def __init__(self, source_value: str, source_line: int):
        self.value = source_value
        self.source_line = source_line


@dataclass
class ASTQuote(ASTNode):
    """A quoted expression."""
    expr: ASTNode

    def __init__(self, expr: ASTNode, source_line: int):
        self.expr = expr
        self.source_line = source_line


@dataclass
class ASTComment(ASTNode):
    """A comment."""
    text: str
    is_eol: bool  # True if end-of-line comment, False if standalone

    def __init__(self, text: str, source_line: int, is_eol: bool):
        self.text = text
        self.source_line = source_line
        self.is_eol = is_eol


@dataclass
class ASTList(ASTNode):
    """A list with elements and associated comments."""
    elements: List[Union[ASTNode, 'ASTComment']]  # Mix of nodes and comments

    def __init__(self, elements: List[Union[ASTNode, 'ASTComment']], source_line: int):
        self.elements = elements
        self.source_line = source_line

# === PASS 2: Formatting Decisions ===

class FormatStyle(Enum):
    """How a list should be formatted."""
    COMPACT = 1   # All on one line
    MULTILINE = 2  # Each element on its own line


@dataclass
class FormatDecision:
    """Formatting decision for a list."""
    style: FormatStyle
    column: int  # Column where '(' appears


class TreeBuilder:
    """Build tree from tokens (Pass 1)."""

    def __init__(self, tokens: List[AIFPLToken]):
        self.tokens = tokens
        self.pos = 0

    def build(self) -> List[ASTNode]:
        """Build list of top-level expressions."""
        result = []
        while self.pos < len(self.tokens):
            node = self._parse_expr()
            if node:
                result.append(node)

        return result

    def _parse_expr(self) -> Optional[ASTNode]:
        """Parse a single expression."""
        if self.pos >= len(self.tokens):
            return None

        token = self.tokens[self.pos]

        if token.type == AIFPLTokenType.COMMENT:
            # Comments at top level
            comment = ASTComment(token.value, token.line, False)
            self.pos += 1
            return comment

        if token.type == AIFPLTokenType.LPAREN:
            return self._parse_list()

        if token.type == AIFPLTokenType.QUOTE:
            source_line = token.line
            self.pos += 1
            expr = self._parse_expr()
            return ASTQuote(expr, source_line) if expr else None

        # Atom
        atom = ASTAtom(self._format_atom_value(token), token.line)
        self.pos += 1
        return atom

    def _format_atom_value(self, token: AIFPLToken) -> str:
        """Format an atom's value as a string."""
        if token.type == AIFPLTokenType.STRING:
            return f'"{self._escape_string(token.value)}"'

        if token.type == AIFPLTokenType.BOOLEAN:
            return '#t' if token.value else '#f'

        return str(token.value)

    def _escape_string(self, s: str) -> str:
        """Escape a string."""
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

            elif ord(char) < 32:
                result.append(f'\\u{ord(char):04x}')

            else:
                result.append(char)

        return ''.join(result)

    def _parse_list(self) -> ASTList:
        """Parse a list."""
        source_line = self.tokens[self.pos].line
        self.pos += 1  # consume '('

        elements: List[Union[ASTNode, ASTComment]] = []
        last_code_line = source_line

        while self.pos < len(self.tokens) and self.tokens[self.pos].type != AIFPLTokenType.RPAREN:
            token = self.tokens[self.pos]

            if token.type == AIFPLTokenType.COMMENT:
                # Determine if EOL or standalone
                is_eol = bool(token.line == last_code_line)
                comment = ASTComment(token.value, token.line, is_eol)
                elements.append(comment)
                self.pos += 1

            else:
                expr = self._parse_expr()
                if expr:
                    elements.append(expr)
                    if isinstance(expr, (ASTAtom, ASTList, ASTQuote)):
                        last_code_line = expr.source_line

        if self.pos < len(self.tokens):
            self.pos += 1  # consume ')'

        return ASTList(elements, source_line)


class FormatPlanner:
    """Decide formatting for each node (Pass 2)."""

    def __init__(self, options: FormatOptions):
        self.options = options
        self.decisions: dict[int, FormatDecision] = {}  # Map from ASTList node id to FormatDecision

    def plan(self, nodes: List[ASTNode], start_column: int = 0) -> None:
        """Plan formatting for all nodes."""
        for node in nodes:
            self._plan_node(node, start_column)

    def _plan_node(self, node: ASTNode, column: int) -> None:
        """Plan formatting for a single node."""
        if isinstance(node, ASTList):
            self._plan_list(node, column)

        elif isinstance(node, ASTQuote):
            if node.expr:
                self._plan_node(node.expr, column + 1)  # +1 for the '

    def _plan_list(self, lst: ASTList, column: int) -> None:
        """Plan formatting for a list."""
        # Try compact first
        compact_str = self._try_compact(lst)
        if compact_str and len(compact_str) <= self.options.compact_threshold:
            # Use compact
            self.decisions[id(lst)] = FormatDecision(FormatStyle.COMPACT, column)
            # Still need to plan nested lists in case they appear in compact mode
            for elem in lst.elements:
                if isinstance(elem, ASTList):
                    self._plan_node(elem, 0)  # Column doesn't matter for compact

        else:
            # Use multiline
            self.decisions[id(lst)] = FormatDecision(FormatStyle.MULTILINE, column)

            # Plan children - they'll be indented
            child_atom_col = column + 1  # After the '('
            subsequent_col = column + self.options.indent_size  # Subsequent elements get +2 indent
            first = True

            for elem in lst.elements:
                if isinstance(elem, ASTComment):
                    continue  # Comments handled during render

                if first:
                    # First element right after '('
                    self._plan_node(elem, child_atom_col)
                    first = False
                else:
                    # All subsequent elements get +indent_size
                    self._plan_node(elem, subsequent_col)

    def _try_compact(self, lst: ASTList) -> Optional[str]:
        """Try to render list compactly, return None if not possible."""
        # Can't be compact if it has comments
        if any(isinstance(elem, ASTComment) for elem in lst.elements):
            return None

        parts = ['(']
        for i, elem in enumerate(lst.elements):
            if i > 0:
                parts.append(' ')

            if isinstance(elem, ASTAtom):
                parts.append(elem.value)

            elif isinstance(elem, ASTQuote):
                parts.append("'")
                if elem.expr:
                    compact_expr = self._try_compact_expr(elem.expr)
                    if not compact_expr:
                        return None
                    parts.append(compact_expr)

            elif isinstance(elem, ASTList):
                compact_list = self._try_compact(elem)
                if not compact_list:
                    return None

                parts.append(compact_list)

        parts.append(')')
        return ''.join(parts)

    def _try_compact_expr(self, node: ASTNode) -> Optional[str]:
        """Try to render any expression compactly."""
        if isinstance(node, ASTAtom):
            return node.value

        if isinstance(node, ASTList):
            return self._try_compact(node)

        if isinstance(node, ASTQuote):
            if node.expr:
                compact = self._try_compact_expr(node.expr)
                return f"'{compact}" if compact else None

            return "'"

        return None


class Renderer:
    """Render tree to string (Pass 3)."""

    def __init__(self, options: FormatOptions, decisions: dict):
        self.options = options
        self.decisions: dict[int, FormatDecision] = decisions

    def render(self, nodes: List[ASTNode]) -> str:
        """Render all top-level nodes."""
        parts: list[str] = []
        prev_was_comment = False
        prev_line = 0

        for node in nodes:
            if isinstance(node, ASTComment):
                # Top-level comment
                current_line = node.source_line
                if parts and (current_line - prev_line > 1 or not prev_was_comment):
                    parts.append('\n')
                parts.append(node.text)
                parts.append('\n')
                prev_was_comment = True
                prev_line = current_line

            else:
                # Code
                if parts and isinstance(nodes[nodes.index(node) - 1] if nodes.index(node) > 0 else None, ASTComment):
                    # Previous was comment, check for blank line in source
                    pass  # Already handled above

                rendered = self._render_node(node, 0)
                parts.append(rendered)
                parts.append('\n')
                prev_was_comment = False
                prev_line = node.source_line

        result = ''.join(parts)

        # Clean up trailing spaces
        lines = result.split('\n')
        lines = [line.rstrip() for line in lines]

        # Remove excessive blank lines
        cleaned = []
        blank_count = 0
        for line in lines:
            if line == '':
                blank_count += 1
                if blank_count <= 2:
                    cleaned.append(line)

            else:
                blank_count = 0
                cleaned.append(line)

        result = '\n'.join(cleaned)
        if result and not result.endswith('\n'):
            result += '\n'

        return result

    def _render_node(self, node: ASTNode, column: int) -> str:
        """Render a single node."""
        if isinstance(node, ASTAtom):
            return node.value

        if isinstance(node, ASTQuote):
            expr_str = self._render_node(node.expr, column + 1) if node.expr else ""
            return f"'{expr_str}"

        if isinstance(node, ASTList):
            return self._render_list(node, column)

        return ""

    def _render_list(self, lst: ASTList, column: int) -> str:
        """Render a list."""
        decision = self.decisions.get(id(lst))
        if not decision:
            # Fallback to multiline
            decision = FormatDecision(FormatStyle.MULTILINE, column)

        if decision.style == FormatStyle.COMPACT:
            return self._render_compact(lst)

        return self._render_multiline(lst, decision.column)

    def _render_compact(self, lst: ASTList) -> str:
        """Render list compactly."""
        parts = ['(']
        for i, elem in enumerate(lst.elements):
            if isinstance(elem, ASTComment):
                continue  # Skip comments in compact mode

            if i > 0:
                parts.append(' ')

            parts.append(self._render_node(elem, 0))
        parts.append(')')
        return ''.join(parts)

    def _render_multiline(self, lst: ASTList, lparen_col: int) -> str:
        """Render list in multiline format."""
        indent = lparen_col + 1
        subsequent_col = lparen_col + self.options.indent_size
        parts = ['(']
        first = True
        prev_comment_line = None
        prev_code_indent = None
        just_output_newline = False
        prev_was_standalone_comment = False

        for elem in lst.elements:
            if isinstance(elem, ASTComment):
                if elem.is_eol:
                    # End-of-line comment
                    parts.append(' ' * self.options.comment_spacing)
                    parts.append(elem.text)
                    just_output_newline = False

                else:
                    # Standalone comment
                    if not parts[-1].endswith('\n'):
                        parts.append('\n')

                    blank_line_added = False

                    # Check for blank line from source
                    if prev_comment_line and elem.source_line - prev_comment_line > 1:
                        parts.append('\n')
                        blank_line_added = True

                    # Comments align with subsequent elements
                    comment_indent = subsequent_col

                    # Add blank line if comment is at same or lower indent than previous code
                    # But not if previous element was also a standalone comment
                    if not blank_line_added and not prev_was_standalone_comment and \
                       prev_code_indent is not None and comment_indent <= prev_code_indent:
                        parts.append('\n')

                    parts.append(' ' * comment_indent)
                    parts.append(elem.text)
                    parts.append('\n')
                    prev_comment_line = elem.source_line
                    just_output_newline = True
                    prev_was_standalone_comment = True
                    first = False

                continue

            # Code element
            if not first:
                # Only add newline if we didn't just output one
                if not just_output_newline:
                    parts.append('\n')

                # All subsequent elements use the same indent
                indent = subsequent_col
                parts.append(' ' * indent)

            # Track the indent of this code element
            prev_code_indent = indent
            parts.append(self._render_node(elem, indent))
            first = False
            just_output_newline = False
            prev_was_standalone_comment = False

        parts.append(')')
        return ''.join(parts)

    def _find_next_code_element(self, elements: List, start_idx: int) -> ASTNode | None:
        """Find the next non-comment element."""
        for i in range(start_idx + 1, len(elements)):
            if not isinstance(elements[i], ASTComment):
                return elements[i]

        return None


class AIFPLPrettyPrinter:
    """Main pretty printer using multi-pass approach."""

    def __init__(self, options: Optional[FormatOptions] = None):
        self.options = options or FormatOptions()

    def format(self, source_code: str) -> str:
        """Format AIFPL source code."""
        # Pass 1: Lex and build tree
        lexer = AIFPLLexer()
        tokens = lexer.lex(source_code, preserve_comments=True)
        builder = TreeBuilder(tokens)
        tree = builder.build()

        # Pass 2: Plan formatting
        planner = FormatPlanner(self.options)
        planner.plan(tree, start_column=0)

        # Pass 3: Render
        renderer = Renderer(self.options, planner.decisions)
        return renderer.render(tree)
