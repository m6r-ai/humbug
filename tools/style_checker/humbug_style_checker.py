"""
Pylint plugin implementing Humbug-specific style checks.

The plugin is loaded automatically by pylint via ``load-plugins`` in
``pyproject.toml``.  A single checker class registers all message IDs.
Messages are emitted in line-number order within each file.

Checks implemented:

* ``humbug-no-property``         -- ``@property`` is banned; use a getter method.
* ``humbug-no-optional``         -- ``Optional[X]`` is banned; use ``X | None``.
* ``humbug-no-union``            -- ``Union[X, Y]`` is banned; use ``X | Y``.
* ``humbug-no-typing-aliases``   -- legacy ``typing.Dict``, ``typing.List``, etc. are
                                    banned; use builtins or ``collections.abc``.
* ``humbug-no-aligned-assigns``  -- consecutive assignments must not have their
                                    ``=`` signs padded into alignment.
* ``humbug-blank-before-dedent`` -- a blank line is required before any line
                                    that dedents from the previous statement.
* ``humbug-multiline-docstring`` -- multi-line docstring ``\"\"\"`` delimiters must
                                    sit on their own lines.
"""

from collections.abc import Mapping

from astroid import nodes  # type: ignore[import-untyped]
from pylint.checkers import BaseRawFileChecker
from pylint.lint import PyLinter
from pylint.typing import MessageDefinitionTuple


# Public message-ID constants (shared with tests)
MSG_NO_PROPERTY = "humbug-no-property"
MSG_NO_OPTIONAL = "humbug-no-optional"
MSG_NO_UNION = "humbug-no-union"
MSG_NO_TYPING_ALIASES = "humbug-no-typing-aliases"
MSG_NO_ALIGNED_ASSIGNS = "humbug-no-aligned-assigns"
MSG_BLANK_BEFORE_DEDENT = "humbug-blank-before-dedent"
MSG_MULTILINE_DOCSTRING = "humbug-multiline-docstring"

_SCOPE_NODE_TYPES = (nodes.Module, nodes.ClassDef, nodes.FunctionDef, nodes.AsyncFunctionDef)


# Single checker: all Humbug style checks
#
# All checks run from one BaseRawFileChecker so the file is read and decoded
# once and the AST is walked once per check.  Messages are buffered and emitted
# in line-number order so that violations appear top-to-bottom regardless of
# which check detects them first.

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
        "C6404": (
            "Union[X, Y] is banned; use X | Y",
            MSG_NO_UNION,
            "Used when Union[...] appears.  Humbug uses the modern X | Y syntax.",
        ),
        "C6403": (
            "Legacy typing alias %s is banned; use the builtin or collections.abc equivalent",
            MSG_NO_TYPING_ALIASES,
            "Used when Dict, List, Set, Tuple, Type, FrozenSet, Deque, Callable, "
            "Awaitable, AsyncGenerator, Generator, Iterator, Sequence, or Coroutine "
            "is imported from typing instead of using builtins or collections.abc.",
        ),
        "C6411": (
            "Consecutive assignments must not have their = signs aligned",
            MSG_NO_ALIGNED_ASSIGNS,
            "Used when two or more consecutive assignment lines share an = column "
            "through extra padding.  Aligned assignments are a maintenance burden.",
        ),
        "C6421": (
            "Missing blank line before dedent",
            MSG_BLANK_BEFORE_DEDENT,
            "Used when a line of code dedents (has less indentation than the "
            "previous statement) without a blank line separating it from the "
            "preceding block.",
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
        self._check_union(node, pending)
        self._check_typing_aliases(node, pending)
        self._check_aligned_assignments(node, lines, pending)
        self._check_blank_before_dedent(lines, pending)
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

    def _check_union(
        self, node: nodes.Module, pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """Flag ``Union[...]`` subscript expressions."""
        for sub_node in node.nodes_of_class(nodes.Subscript):
            if _dotted_name(sub_node.value) == "Union":
                pending.append((sub_node.lineno, MSG_NO_UNION, ()))

    def _check_typing_aliases(
        self, node: nodes.Module, pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """Flag imports of legacy typing aliases that have modern replacements."""
        # Names that should come from builtins (no import) or collections.abc
        legacy_names = {
            "Dict", "List", "Set", "Tuple", "Type", "FrozenSet", "Deque",
            "Callable", "Awaitable", "AsyncGenerator", "Generator",
            "Iterator", "Sequence", "Coroutine",
        }
        for imp_node in node.nodes_of_class(nodes.ImportFrom):
            if imp_node.modname != "typing":
                continue

            for alias_name, _asname in imp_node.names:
                if alias_name in legacy_names:
                    pending.append(
                        (imp_node.lineno, MSG_NO_TYPING_ALIASES, (alias_name,))
                    )

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

    def _check_blank_before_dedent(
        self, lines: list[str], pending: list[tuple[int, str, tuple[str, ...]]]
    ) -> None:
        """
        Flag any dedent line not preceded by a blank line.

        A dedent occurs when a line of code has strictly less indentation than
        the previous statement.  This generalises the old else/elif check:
        any block exit (``else``, ``elif``, ``return`` after an ``if`` body,
        the next function after a nested block, etc.) must be visually
        separated by a blank line.

        A comment-only line before the dedent is acceptable.
        """
        state = _DedentScanner(lines)
        for lineno in state.find_dedents_without_blank():
            pending.append((lineno, MSG_BLANK_BEFORE_DEDENT, ()))

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


class _DedentScanner:
    """
    Scan source lines and find dedents that lack a preceding blank line.

    The scanner walks lines sequentially, tracking:

    * **Bracket depth** — lines inside ``()``, ``[]``, ``{}`` are continuation
      lines, not statements.  Their indentation is irrelevant.
    * **String state** — lines inside a multi-line string are skipped entirely.
    * **Code indent** — the indentation of the last real statement line.

    A dedent is flagged when a real statement line has strictly less indentation
    than the previous real statement line and there is no blank line between
    them.  Comment-only lines between the two do not prevent the flag, but a
    blank line or the start of the file does.
    """

    def __init__(self, lines: list[str]) -> None:
        self._lines = lines

    def find_dedents_without_blank(self) -> list[int]:
        """Return line numbers (1-based) of dedents lacking a preceding blank line."""
        results: list[int] = []

        bracket_depth = 0
        in_triple: str | None = None
        prev_indent: int | None = None
        blank_since_last_code = True

        for idx, raw in enumerate(self._lines):
            lineno = idx + 1

            # If we are inside a multi-line string, consume the line and check
            # whether the string ends here.  Indentation of string-body lines
            # is irrelevant.
            if in_triple is not None:
                in_triple = _update_triple_state(raw, in_triple)
                continue

            stripped = raw.strip()

            # Blank lines reset the "no blank" condition.
            if stripped == "":
                blank_since_last_code = True
                continue

            # Comment-only lines are skipped.  They provide visual separation
            # equivalent to a blank line, so they satisfy the requirement.
            if stripped.startswith("#"):
                blank_since_last_code = True
                continue

            indent = len(raw) - len(raw.lstrip())

            # If we're inside brackets, this is a continuation line.  Track
            # bracket/string state but don't compare indentation.
            if bracket_depth > 0:
                bracket_depth, in_triple = _scan_line_brackets_and_strings(
                    raw, bracket_depth, in_triple
                )
                continue

            # This is a real statement line.  Check for dedent.
            if prev_indent is not None and indent < prev_indent:
                if not blank_since_last_code:
                    results.append(lineno)

            prev_indent = indent
            blank_since_last_code = False

            # Update bracket and string state for this line.
            bracket_depth, in_triple = _scan_line_brackets_and_strings(
                raw, bracket_depth, in_triple
            )

        return results


def _scan_line_brackets_and_strings(
    line: str, bracket_depth: int, in_triple: str | None
) -> tuple[int, str | None]:
    """
    Update bracket depth and triple-string state after scanning *line*.

    Handles all string types (single-quoted, double-quoted, triple-quoted)
    and bracket characters, correctly skipping characters inside strings.
    """
    i = 0

    while i < len(line):
        ch = line[i]

        if in_triple is not None:
            if _is_triple_at(line, i, in_triple):
                i += 3

                in_triple = None

                continue

            i += 1
            continue

        # Not inside any string.
        if ch in ("'", '"'):
            triple_ch = ch * 3

            if line[i:i + 3] == triple_ch:
                in_triple = ch
                i += 3

                continue

            # Single-line string — skip to the matching quote.
            close = line.find(ch, i + 1)

            if close == -1:
                # Unterminated string on this line (shouldn't happen in valid
                # Python, but be safe).
                return bracket_depth, in_triple

            i = close + 1
            continue

        if ch == "#":
            break  # rest of line is a comment

        if ch in "([{":
            bracket_depth += 1

        elif ch in ")]}":
            bracket_depth = max(0, bracket_depth - 1)

        i += 1

    return bracket_depth, in_triple


def _update_triple_state(line: str, in_triple: str) -> str | None:
    """
    Check whether *line* closes the current triple-quoted string.

    Returns None if the string is closed on this line, or *in_triple*
    unchanged if still open.
    """
    for i in range(len(line) - 2):
        if _is_triple_at(line, i, in_triple):
            # Check there isn't a second closing triple later (rare, but
            # e.g. """text""" all on one line).  For simplicity, return None
            # — the next call to _scan_line_brackets_and_strings from the
            # caller will handle any further triples on the same line.
            return None

    return in_triple


def _is_triple_at(line: str, i: int, quote_ch: str) -> bool:
    """Return True if *line* has three consecutive *quote_ch* characters at index *i*."""
    return line[i:i + 3] == quote_ch * 3


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


def _consecutive_groups(line_map: Mapping[int, object]) -> list[list[int]]:
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


def register(linter: PyLinter) -> None:
    """Register the plugin's checker with pylint."""
    linter.register_checker(HumbugStyleChecker(linter))
