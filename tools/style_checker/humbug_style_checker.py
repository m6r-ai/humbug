"""
Pylint plugin implementing Humbug-specific style checks.

The plugin is loaded automatically by pylint via ``load-plugins`` in
``pyproject.toml``.  A single checker class registers all message IDs.
Messages are emitted in line-number order within each file.

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
from pylint.checkers import BaseRawFileChecker
from pylint.typing import MessageDefinitionTuple


# ---------------------------------------------------------------------------
# Public message-ID constants (shared with tests)
# ---------------------------------------------------------------------------

MSG_NO_PROPERTY = "humbug-no-property"
MSG_NO_OPTIONAL = "humbug-no-optional"
MSG_NO_ALIGNED_ASSIGNS = "humbug-no-aligned-assigns"
MSG_BLANK_BEFORE_ELSE = "humbug-blank-before-else"
MSG_MULTILINE_DOCSTRING = "humbug-multiline-docstring"

_SCOPE_NODE_TYPES = (nodes.Module, nodes.ClassDef, nodes.FunctionDef, nodes.AsyncFunctionDef)


# ===========================================================================
# Single checker: all Humbug style checks
#
# All checks run from one BaseRawFileChecker so the file is read and decoded
# once and the AST is walked once per check.  Messages are buffered and emitted
# in line-number order so that violations appear top-to-bottom regardless of
# which check detects them first.
# ===========================================================================

class HumbugStyleChecker(BaseRawFileChecker):
    """Enforce Humbug-specific style conventions across all checks."""

    name = "humbug-style"

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
        "C6411": (
            "Consecutive assignments must not have their = signs aligned",
            MSG_NO_ALIGNED_ASSIGNS,
            "Used when two or more consecutive assignment lines share an = column "
            "through extra padding.  Aligned assignments are a maintenance burden.",
        ),
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
        """Run all checks over a single decode and AST walk of the file."""
        lines = node.stream().read().decode("utf-8").splitlines()

        pending: list[tuple[int, str, tuple[str, ...]]] = []

        self._check_property(node, pending)
        self._check_optional(node, pending)
        self._check_aligned_assignments(node, lines, pending)
        self._check_blank_before_else(node, lines, pending)
        self._check_docstrings(node, lines, pending)

        for line, msg_id, args in sorted(pending, key=lambda m: m[0]):
            if args:
                self.add_message(msg_id, line=line, args=args)

            else:
                self.add_message(msg_id, line=line)

    def _check_property(
        self, node: nodes.Module, pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """Flag any decorator whose name resolves to ``property``."""
        for dec_node in node.nodes_of_class(nodes.Decorators):
            for decorator in dec_node.nodes:
                expr = decorator.func if hasattr(decorator, "func") else decorator
                if _dotted_name(expr) == "property":
                    pending.append((decorator.lineno, MSG_NO_PROPERTY, ()))

    def _check_optional(
        self, node: nodes.Module, pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """Flag ``Optional[...]`` subscript expressions."""
        for sub_node in node.nodes_of_class(nodes.Subscript):
            if _dotted_name(sub_node.value) == "Optional":
                pending.append((sub_node.lineno, MSG_NO_OPTIONAL, ()))

    def _check_aligned_assignments(
        self, node: nodes.Module, lines: list[str], pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """
        Find padded-alignment ``=`` runs among assignment statements.

        Only actual assignment statements (``Assign`` and ``AnnAssign`` nodes)
        are considered; keyword arguments in function calls are not affected.
        When two assignments have the same target length the ``=`` naturally
        lines up without padding -- that is fine and not flagged.
        """
        # Collect assignment lines in a single tree walk.  Record (column of
        # ``=``, length of the target expression) for each.  Two lines are
        # "aligned" only when their ``=`` columns match but their target lengths
        # differ -- that is the case where extra padding was inserted.
        assign_info: dict[int, tuple[int, int]] = {}

        for assign_node in node.nodes_of_class((nodes.Assign, nodes.AnnAssign)):
            lineno = assign_node.lineno
            col = _find_assign_eq_column(lines, lineno)
            if col is None:
                continue

            target_len = _target_length(lines[lineno - 1], col)
            assign_info[lineno] = (col, target_len)

        if len(assign_info) < 2:
            return

        for group in _consecutive_groups(assign_info):
            if len(group) < 2:
                continue

            col_entries: dict[int, list[int]] = {}

            for lineno in group:
                col_val, _ = assign_info[lineno]
                col_entries.setdefault(col_val, []).append(lineno)

            for col_val, linenos in col_entries.items():
                if len(linenos) < 2:
                    continue

                # Flag only when at least two different target lengths share the
                # same = column, which means padding was used.
                target_lengths = {assign_info[ln][1] for ln in linenos}
                if len(target_lengths) >= 2:
                    for lineno in linenos:
                        pending.append((lineno, MSG_NO_ALIGNED_ASSIGNS, ()))

    def _check_blank_before_else(
        self, node: nodes.Module, lines: list[str], pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """
        Flag block-level ``elif`` / ``else`` keywords not preceded by a blank line.

        Uses the AST to identify block-level ``else`` / ``elif`` keywords.  This
        avoids false positives on ternary conditional expressions (``x if cond
        else y``), which are ``IfExp`` nodes rather than ``If`` nodes, and
        correctly handles multi-line ``elif`` conditions where the colon is not
        on the keyword line.
        """
        for keyword_line, keyword in _block_else_elif_lines(node, lines):
            if keyword_line <= 1:
                continue

            # A blank line or a comment-only line before the keyword is fine.
            # We only flag when actual code runs directly into the else/elif.
            prev = lines[keyword_line - 2].strip()
            if prev != "" and not prev.startswith("#"):
                pending.append((keyword_line, MSG_BLANK_BEFORE_ELSE, (keyword,)))

    def _check_docstrings(
        self, node: nodes.Module, lines: list[str], pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """Validate docstring layout for the module and all its nested definitions."""
        for child in node.nodes_of_class(_SCOPE_NODE_TYPES):
            doc_node = getattr(child, "doc_node", None)
            if doc_node is None or not isinstance(doc_node, nodes.Const):
                continue

            self._check_single_docstring(doc_node, lines, pending)

    def _check_single_docstring(
        self, doc_node: nodes.Const, lines: list[str], pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """Validate that a multi-line docstring has its delimiters on own lines."""
        start_line = doc_node.lineno
        end_line = doc_node.end_lineno

        if start_line is None or end_line is None:
            return

        if start_line == end_line:
            return  # single-line docstring -- always fine

        if lines[start_line - 1].strip() != '"""':
            pending.append((start_line, MSG_MULTILINE_DOCSTRING, ()))

        if lines[end_line - 1].strip() != '"""':
            pending.append((end_line, MSG_MULTILINE_DOCSTRING, ()))


# ===========================================================================
# Helper functions
# ===========================================================================

def _dotted_name(node: nodes.NodeNG) -> str | None:
    """Return the dotted name of a Name/Attribute node, or None."""
    try:
        return node.as_string()
    except Exception:  # pylint: disable=broad-exception-caught
        return None


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


def _block_else_elif_lines(
    module: nodes.Module, lines: list[str]
) -> list[tuple[int, str]]:
    """
    Find all block-level ``else:`` and ``elif`` keyword lines in *module*.

    Returns a list of ``(lineno, keyword)`` pairs where *keyword* is either
    ``"elif"`` or ``"else"``.  Only block-level keywords are returned; ternary
    conditional expressions (``IfExp`` nodes) are never included because they
    are a different AST node type.
    """
    results: list[tuple[int, str]] = []

    for if_node in module.nodes_of_class(nodes.If):
        if not if_node.orelse:
            continue

        first = if_node.orelse[0]

        # elif: orelse is a single If at the same indent as the parent.
        if isinstance(first, nodes.If) and first.col_offset == if_node.col_offset:
            results.append((first.lineno, "elif"))
            continue

        # else: orelse is a block of statements.  The ``else:`` keyword line is
        # not stored in the AST, so we scan backwards from the first body
        # statement for a line ending in ``:`` at the if's indentation.
        else_line = _find_else_keyword_line(lines, first.lineno, if_node.col_offset)
        if else_line is not None:
            results.append((else_line, "else"))

    return results


def _find_else_keyword_line(
    lines: list[str], body_start: int, target_indent: int
) -> int | None:
    """Scan backwards from *body_start* for the ``else:`` keyword line."""
    for lineno in range(body_start - 1, 0, -1):
        raw = lines[lineno - 1]
        code = raw.split("#", 1)[0].rstrip()

        if code == "":
            continue

        indent = len(raw) - len(raw.lstrip())

        if indent == target_indent and code.endswith(":"):
            return lineno

        if indent <= target_indent:
            break  # we've gone past the else without finding it

    return None


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


# ===========================================================================
# Plugin registration
# ===========================================================================

def register(linter: object) -> None:
    """Register the plugin's checker with pylint."""
    linter.register_checker(HumbugStyleChecker(linter))
