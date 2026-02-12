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
    start_line: int   # Line where node starts
    end_line: int     # Line where node ends (for tracking EOL comments)


@dataclass
class ASTAtom(ASTNode):
    """An atomic value (symbol, number, string, boolean)."""
    value: str

    def __init__(self, source_value: str, start_line: int):
        self.value = source_value
        self.start_line = start_line
        self.end_line = start_line


@dataclass
class ASTQuote(ASTNode):
    """A quoted expression."""
    expr: ASTNode

    def __init__(self, expr: ASTNode, start_line: int):
        self.expr = expr
        self.start_line = start_line
        self.end_line = expr.end_line if expr else start_line


@dataclass
class ASTComment(ASTNode):
    """A comment."""
    text: str
    is_eol: bool  # True if end-of-line comment, False if standalone

    def __init__(self, text: str, start_line: int, is_eol: bool):
        self.text = text
        self.start_line = start_line
        self.end_line = start_line
        self.is_eol = is_eol


@dataclass
class ASTList(ASTNode):
    """A list with elements and associated comments."""
    elements: List[Union[ASTNode, 'ASTComment']]  # Mix of nodes and comments

    def __init__(self, elements: List[Union[ASTNode, 'ASTComment']], start_line: int):
        self.elements = elements
        self.start_line = start_line
        self.end_line = start_line  # Will be updated when we know the closing paren line


# === Special Form Rules ===

# Maps special form names to number of elements that should stay on first line
SPECIAL_FORM_RULES = {
    'lambda': 2,
    'let': 1,
    'let*': 1,
    'letrec': 1,
    'if': 2,
    'match': 2,
}

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
    elements_on_first_line: int = 1  # How many elements stay on first line


class TreeBuilder:
    """Build tree from tokens (Pass 1)."""

    def __init__(self, tokens: List[AIFPLToken]):
        self.tokens = tokens
        self.pos = 0

    def build(self) -> List[ASTNode]:
        """Build list of top-level expressions."""
        result = []
        last_code_line = -1  # Track the line of the last code element

        while self.pos < len(self.tokens):
            token = self.tokens[self.pos]

            if token.type == AIFPLTokenType.COMMENT:
                # Check if this is an EOL comment at top level
                is_eol = bool(token.line == last_code_line)
                node = ASTComment(token.value, token.line, is_eol)
                self.pos += 1
                result.append(node)

            else:
                node = self._parse_expr()
                if node:
                    result.append(node)
                    last_code_line = node.end_line

        return result

    def _parse_expr(self) -> Optional[ASTNode]:
        """Parse a single expression."""
        if self.pos >= len(self.tokens):
            return None

        token = self.tokens[self.pos]

        if token.type == AIFPLTokenType.LPAREN:
            return self._parse_list()

        if token.type == AIFPLTokenType.QUOTE:
            start_line = token.line
            self.pos += 1
            expr = self._parse_expr()
            return ASTQuote(expr, start_line) if expr else None

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
        start_line = self.tokens[self.pos].line
        self.pos += 1  # consume '('
        end_line = start_line  # Track where the list ends

        elements: List[Union[ASTNode, ASTComment]] = []
        last_code_line = start_line

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
                        last_code_line = expr.end_line

        if self.pos < len(self.tokens):
            # Track the line where the closing ')' appears
            end_line = self.tokens[self.pos].line
            self.pos += 1  # consume ')'

        result = ASTList(elements, start_line)
        result.end_line = end_line
        return result


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
                # Plan nested lists and quotes (which may contain lists)
                if isinstance(elem, (ASTList, ASTQuote)):
                    self._plan_node(elem, 0)  # Column doesn't matter for compact

        else:
            # Use multiline

            # Check if this is a special form
            elements_on_first_line = 1  # Default
            is_special_form = False

            if lst.elements:
                first_elem = lst.elements[0]
                if isinstance(first_elem, ASTAtom):
                    form_name = first_elem.value
                    is_special_form = form_name in SPECIAL_FORM_RULES
                    if is_special_form:
                        elements_on_first_line = SPECIAL_FORM_RULES[form_name]

                    else:
                        # Regular function calls: use traditional Lisp alignment
                        # (function name + first argument on same line)
                        elements_on_first_line = 2

            self.decisions[id(lst)] = FormatDecision(FormatStyle.MULTILINE, column, elements_on_first_line)

            # Plan children - they'll be indented
            child_atom_col = column + 1  # After the '('

            # For special forms, indent subsequent elements by indent_size
            # For regular lists, align subsequent elements with first argument position
            if is_special_form:
                subsequent_col = column + self.options.indent_size
            else:
                # For traditional Lisp alignment, subsequent arguments align with first argument
                # First argument position = column + '(' + function_name + ' '
                # We need to calculate function name length
                if lst.elements and isinstance(lst.elements[0], ASTAtom):
                    func_name_len = len(lst.elements[0].value)
                    subsequent_col = column + 1 + func_name_len + 1

                else:
                    subsequent_col = column + 1

            elements_on_current_line = 0

            for elem in lst.elements:
                if isinstance(elem, ASTComment):
                    continue  # Comments handled during render

                if elements_on_current_line < elements_on_first_line:
                    # This element stays on the first line
                    # For the first element, use child_atom_col
                    # For subsequent elements on the first line, use subsequent_col
                    # This better approximates where they'll actually be rendered
                    if elements_on_current_line == 0:
                        self._plan_node(elem, child_atom_col)

                    else:
                        self._plan_node(elem, subsequent_col)

                    elements_on_current_line += 1

                else:
                    # Subsequent elements get indented
                    self._plan_node(elem, subsequent_col)

            # Note: We don't track exact column positions for elements on first line,
            # just plan them at child_atom_col. Renderer will space them properly.

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

        i = 0
        while i < len(nodes):
            node = nodes[i]

            if isinstance(node, ASTComment):
                # Top-level comment
                current_line = node.start_line
                if parts and (current_line - prev_line > 1 or not prev_was_comment):
                    parts.append('\n')

                parts.append(node.text)
                parts.append('\n')
                prev_was_comment = True
                prev_line = current_line

            else:
                # Code
                if parts and i > 0 and isinstance(nodes[i - 1], ASTComment):
                    # Previous was comment, check for blank line in source
                    pass  # Already handled above

                rendered = self._render_node(node, 0)
                parts.append(rendered)

                # Check if next node is an EOL comment
                if i + 1 < len(nodes):
                    comment = nodes[i + 1]
                    if isinstance(comment, ASTComment) and comment.is_eol:
                        # Next node is EOL comment, append it on same line
                        parts.append(' ' * self.options.comment_spacing)
                        parts.append(comment.text)
                        i += 1  # Skip the comment node since we just rendered it

                parts.append('\n')

                prev_was_comment = False
                prev_line = node.end_line

            i += 1

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

        return self._render_multiline(lst, decision)

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

    def _render_multiline(self, lst: ASTList, decision: FormatDecision) -> str:
        """Render list in multiline format."""
        lparen_col = decision.column
        elements_on_first_line = decision.elements_on_first_line
        indent = lparen_col + 1

        # Check if this is a special form
        is_special_form = False
        if lst.elements:
            first_elem = lst.elements[0]
            if isinstance(first_elem, ASTAtom):
                is_special_form = first_elem.value in SPECIAL_FORM_RULES

        # For special forms, indent subsequent elements by indent_size
        # For regular lists, align subsequent elements with first argument position
        if is_special_form:
            subsequent_col = lparen_col + self.options.indent_size
        else:
            # For traditional Lisp alignment, subsequent arguments align with first argument
            # First argument position = column + '(' + function_name + ' '
            # We need to calculate function name length
            if lst.elements and isinstance(lst.elements[0], ASTAtom):
                func_name_len = len(lst.elements[0].value)
                subsequent_col = lparen_col + 1 + func_name_len + 1
            else:
                subsequent_col = lparen_col + 1


        parts = ['(']
        elements_on_current_line = 0
        prev_comment_line = None
        prev_code_indent = None
        just_output_newline = False
        prev_was_standalone_comment = False

        for elem in lst.elements:
            if isinstance(elem, ASTComment):
                if elem.is_eol:
                    # End-of-line comment
                    parts.append(' ' * self.options.comment_spacing)

                else:
                    # Standalone comment
                    if not parts[-1].endswith('\n'):
                        parts.append('\n')

                    blank_line_added = False

                    # Check for blank line from source
                    if prev_comment_line and elem.start_line - prev_comment_line > 1:
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
                    prev_comment_line = elem.start_line
                    prev_was_standalone_comment = True

                parts.append(elem.text)
                parts.append('\n')
                just_output_newline = True

                # After a comment, we're past the "first line"
                elements_on_current_line = elements_on_first_line

                continue

            # Code element
            if elements_on_current_line < elements_on_first_line:
                # This element stays on the first line
                if elements_on_current_line > 0:
                    # Add space before this element (not before the very first)
                    parts.append(' ')

                prev_code_indent = indent
                parts.append(self._render_node(elem, indent))
                elements_on_current_line += 1

            else:
                # This element goes on a new line with indent
                if not just_output_newline:
                    parts.append('\n')

                indent = subsequent_col
                parts.append(' ' * indent)

                prev_code_indent = indent
                parts.append(self._render_node(elem, indent))

            just_output_newline = False
            prev_was_standalone_comment = False

        # Add closing paren with proper indentation
        if just_output_newline:
            # Last element ended with newline, indent closing paren to match opening
            parts.append(' ' * lparen_col)

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
