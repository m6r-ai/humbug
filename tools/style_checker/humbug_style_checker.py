"""
Pylint plugin implementing Humbug-specific style checks.

The plugin is loaded automatically by pylint via ``load-plugins`` in
``pyproject.toml``.  Each checker class registers one or more message IDs.

Checks implemented:

* ``humbug-no-property``         -- ``@property`` is banned; use a getter method.
* ``humbug-no-optional``         -- ``Optional[X]`` is banned; use ``X | None``.
* ``humbug-no-aligned-assigns``  -- consecutive assignments must not have their
                                    ``=`` signs padded into alignment.
* ``humbug-blank-before-else``   -- a blank line is required before block-level
                                    ``elif`` / ``else``.
* ``humbug-multiline-docstring`` -- multi-line docstring ``\"\"\"`` delimiters must
                                    sit on their own lines.
"""

from __future__ import annotations

from astroid import nodes
from pylint.checkers import BaseChecker, BaseRawFileChecker
from pylint.typing import MessageDefinitionTuple

# astroid.nodes is used at runtime (not just for type hints) by the
# aligned-assignment checker, so it is imported unconditionally above.


# ---------------------------------------------------------------------------
# Public message-ID constants (shared with tests)
# ---------------------------------------------------------------------------

MSG_NO_PROPERTY = "humbug-no-property"
MSG_NO_OPTIONAL = "humbug-no-optional"
MSG_NO_ALIGNED_ASSIGNS = "humbug-no-aligned-assigns"
MSG_BLANK_BEFORE_ELSE = "humbug-blank-before-else"
MSG_MULTILINE_DOCSTRING = "humbug-multiline-docstring"


# ===========================================================================
# Checker 1: AST-based name conventions (@property and Optional)
# ===========================================================================

class HumbugNameChecker(BaseChecker):
    """Flag ``@property`` decorators and ``Optional[...]`` subscripts."""

    name = "humbug-names"

    msgs: dict[str, MessageDefinitionTuple] = {
        "C6401": (
            "@property is banned; use a plain getter method (e.g. def foo(self) -> T:)",
            MSG_NO_PROPERTY,
            "Used when @property is used.  Humbug uses simple getter methods instead.",
        ),
        "C6402": (
            "Optional[X] is banned; use X | None",
            MSG_NO_OPTIONAL,
            "Used when Optional[...] appears.  Humbug uses the modern X | None syntax.",
        ),
    }

    def visit_decorators(self, node: nodes.Decorators) -> None:
        """Flag any decorator whose name resolves to ``property``."""
        for decorator in node.nodes:
            expr = decorator.func if hasattr(decorator, "func") else decorator
            if _dotted_name(expr) == "property":
                self.add_message(MSG_NO_PROPERTY, node=decorator)

    def visit_subscript(self, node: nodes.Subscript) -> None:
        """Flag ``Optional[...]`` subscript expressions."""
        if _dotted_name(node.value) == "Optional":
            self.add_message(MSG_NO_OPTIONAL, node=node)


# ===========================================================================
# Checker 2: Aligned consecutive assignments
# ===========================================================================

class HumbugAlignedAssignmentChecker(BaseRawFileChecker):
    """
    Detect consecutive assignment statements whose ``=`` signs have been padded into alignment.

    Only actual assignment statements (``Assign`` and ``AnnAssign`` nodes) are
    considered; keyword arguments in function calls are not affected.  When two
    assignments have the same target length the ``=`` naturally lines up without
    padding -- that is fine and not flagged.
    """

    name = "humbug-aligned-assigns"

    msgs: dict[str, MessageDefinitionTuple] = {
        "C6411": (
            "Consecutive assignments must not have their = signs aligned",
            MSG_NO_ALIGNED_ASSIGNS,
            "Used when two or more consecutive assignment lines share an = column "
            "through extra padding.  Aligned assignments are a maintenance burden.",
        ),
    }

    def process_module(self, node: nodes.Module) -> None:
        """Find padded-alignment ``=`` runs among assignment statements."""
        source = _module_source(node)
        lines = source.decode("utf-8").splitlines()

        # For each assignment line, record (column of ``=``, length of the
        # target expression).  Two lines are "aligned" only when their ``=``
        # columns match but their target lengths differ -- that is the case
        # where extra padding was inserted to force alignment.
        assign_info: dict[int, tuple[int, int]] = {}

        for assign_node in node.nodes_of_class((nodes.Assign, nodes.AnnAssign)):
            lineno = assign_node.lineno
            col = _find_assign_eq_column(lines, lineno)
            if col is None:
                continue

            target_len = _target_length(lines[lineno - 1], col)
            assign_info[lineno] = (col, target_len)

        if not assign_info:
            return

        for group in _consecutive_groups(assign_info):
            col_entries: dict[int, list[tuple[int, int]]] = {}

            for lineno in group:
                col_val, target_len = assign_info[lineno]
                col_entries.setdefault(col_val, []).append((lineno, target_len))

            for _, entries in col_entries.items():
                # Flag only when at least two different target lengths share the
                # same = column, which means padding was used.
                target_lengths = {tlen for _, tlen in entries}
                if len(entries) >= 2 and len(target_lengths) >= 2:
                    for lineno, _ in entries:
                        self.add_message(MSG_NO_ALIGNED_ASSIGNS, line=lineno)


# ===========================================================================
# Checker 3: Raw-source format checks (blank before else, docstring layout)
# ===========================================================================

class HumbugFormatChecker(BaseRawFileChecker):
    """Flag missing blank lines before ``elif``/``else`` and bad docstring layout."""

    name = "humbug-format"

    msgs: dict[str, MessageDefinitionTuple] = {
        "C6421": (
            "Missing blank line before block-level '%s'",
            MSG_BLANK_BEFORE_ELSE,
            "Used when an ``elif`` or ``else`` clause at the start of a logical "
            "block is not preceded by a blank line.",
        ),
        "C6422": (
            "Multi-line docstring delimiter must be on its own line",
            MSG_MULTILINE_DOCSTRING,
            "Used when a multi-line docstring has text on the same line as the "
            "opening or closing \"\"\".",
        ),
    }

    def process_module(self, node: nodes.Module) -> None:
        """Run both format checks over the raw source."""
        source = _module_source(node)
        lines = source.decode("utf-8").splitlines()

        self._check_blank_before_else(lines)
        self._check_docstrings(node, lines)

    def _check_blank_before_else(self, lines: list[str]) -> None:
        """Flag ``elif`` / ``else`` keywords not preceded by a blank line."""
        for lineno, text in enumerate(lines, start=1):
            stripped = text.lstrip()
            if not _is_block_else_or_elif(stripped):
                continue

            if lineno == 1:
                continue

            prev_line = lines[lineno - 2].strip()
            if prev_line != "":
                keyword = "elif" if stripped.startswith("elif") else "else"
                self.add_message(MSG_BLANK_BEFORE_ELSE, line=lineno, args=(keyword,))

    def _check_docstrings(self, node: nodes.Module, lines: list[str]) -> None:
        """Validate docstring layout for the module and all its nested definitions."""
        for doc_node in _all_docstrings(node):
            self._check_single_docstring(doc_node, lines)

    def _check_single_docstring(self, doc_node: nodes.Const, lines: list[str]) -> None:
        """Validate that a multi-line docstring has its delimiters on own lines."""
        start_line = doc_node.lineno
        end_line = doc_node.end_lineno

        if start_line is None or end_line is None:
            return

        if start_line == end_line:
            return  # single-line docstring -- always fine

        opening = lines[start_line - 1].strip()
        closing = lines[end_line - 1].strip()

        if opening != '"""':
            self.add_message(MSG_MULTILINE_DOCSTRING, line=start_line)

        if closing != '"""':
            self.add_message(MSG_MULTILINE_DOCSTRING, line=end_line)


# ===========================================================================
# Helper functions
# ===========================================================================

def _dotted_name(node: nodes.NodeNG) -> str | None:
    """Return the dotted name of a Name/Attribute node, or None."""
    try:
        return node.as_string()
    except Exception:  # pylint: disable=broad-exception-caught
        return None


def _module_source(node: nodes.Module) -> bytes:
    """Return the raw source bytes for a module node."""
    stream = node.stream()
    if isinstance(stream, bytes):
        return stream

    return stream.read()


def _target_length(line: str, eq_col: int) -> int:
    """
    Return the length of the assignment target on *line*.

    *eq_col* is the 0-based column of the ``=`` sign.  The target length is
    the number of characters from the end of indentation to the start of the
    whitespace preceding ``=``, i.e. the length of the target expression
    without any padding spaces.
    """
    prefix = line[:eq_col].rstrip()
    return len(prefix.lstrip())


def _is_block_else_or_elif(stripped: str) -> bool:
    """Return True if *stripped* (an already-left-stripped line) starts an elif/else block."""
    if stripped.startswith("elif") and (len(stripped) == 4 or stripped[4] in " \t:"):
        return True

    return stripped.startswith("else") and (len(stripped) == 4 or stripped[4] in " \t:")


def _find_assign_eq_column(lines: list[str], lineno: int) -> int | None:
    """
    Return the 0-based column of the assignment ``=`` on line *lineno*.

    *lineno* is 1-based.  The function scans the source line character by
    character, skipping strings and comments, and finds the first ``=`` that is
    not part of a comparison operator (``==``, ``!=``, ``<=``, ``>=``) or
    walrus operator (``:=``).  Returns None if no assignment ``=`` is found.
    """
    if lineno < 1 or lineno > len(lines):
        return None

    text = lines[lineno - 1]
    in_string = False
    string_char = ""
    i = 0

    while i < len(text):
        ch = text[i]

        if in_string:
            if ch == "\\":
                i += 2  # skip escaped char

                continue

            if ch == string_char:
                in_string = False

            i += 1
            continue

        if ch in ("'", '"'):
            in_string = True
            string_char = ch
            i += 1
            continue

        if ch == "#":
            break  # rest of line is a comment

        if ch == "=":
            prev = text[i - 1] if i > 0 else ""
            nxt = text[i + 1] if i + 1 < len(text) else ""

            if nxt != "=" and prev not in ("=", "!", "<", ">", ":"):
                return i  # bare assignment =

            i += 1
            continue

        i += 1

    return None


def _consecutive_groups(line_map: dict[int, object]) -> list[list[int]]:
    """
    Split the keys of *line_map* into maximal runs of consecutive integers.

    Each returned list contains line numbers that immediately follow one
    another (e.g. [3, 4, 5] is one group; [3, 5] is two single-element groups).
    """
    sorted_lines = sorted(line_map)
    if not sorted_lines:
        return []

    groups: list[list[int]] = [[sorted_lines[0]]]

    for lineno in sorted_lines[1:]:
        if lineno == groups[-1][-1] + 1:
            groups[-1].append(lineno)

        else:
            groups.append([lineno])

    return groups


def _all_docstrings(node: nodes.NodeNG) -> list[nodes.Const]:
    """Collect docstring nodes from *node* and all nested classes/functions."""
    results: list[nodes.Const] = []

    for child in node.nodes_of_class(
        (nodes.Module, nodes.ClassDef, nodes.FunctionDef, nodes.AsyncFunctionDef)
    ):
        doc_node = getattr(child, "doc_node", None)
        if doc_node is not None and isinstance(doc_node, nodes.Const):
            results.append(doc_node)

    return results


# ===========================================================================
# Plugin registration
# ===========================================================================

def register(linter: object) -> None:
    """Register the plugin's checkers with pylint."""
    linter.register_checker(HumbugNameChecker(linter))
    linter.register_checker(HumbugAlignedAssignmentChecker(linter))
    linter.register_checker(HumbugFormatChecker(linter))
