"""
Tests for the Humbug style checker pylint plugin.

Each test runs the full plugin (all checkers) against a source snippet and
verifies the expected set of message IDs.
"""

import json
import os
import subprocess
import tempfile

from tools.style_checker.humbug_style_checker import (
    MSG_NO_PROPERTY,
    MSG_NO_OPTIONAL,
    MSG_NO_UNION,
    MSG_NO_TYPING_ALIASES,
    MSG_NO_ALIGNED_ASSIGNS,
    MSG_BLANK_BEFORE_DEDENT,
    MSG_MULTILINE_DOCSTRING,
)


def _run_pylint(source: str) -> list[tuple[str, int]]:
    """
    Run the Humbug style plugin on *source* and return (msg_id, line) pairs.

    The source is written to a temporary .py file so that both AST-based and
    raw-file checkers run correctly.
    """
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".py", delete=False, encoding="utf-8"
    ) as f:
        f.write(source)
        path = f.name

    try:
        env = os.environ.copy()
        env["PYTHONPATH"] = os.getcwd()

        result = subprocess.run(
            [
                "python", "-m", "pylint",
                "--load-plugins=tools.style_checker.humbug_style_checker",
                "--disable=all",
                f"--enable={MSG_NO_PROPERTY},{MSG_NO_OPTIONAL},{MSG_NO_UNION},{MSG_NO_TYPING_ALIASES},{MSG_NO_ALIGNED_ASSIGNS},{MSG_BLANK_BEFORE_DEDENT},{MSG_MULTILINE_DOCSTRING}",
                "--score=n",
                "--output-format=json",
                path,
            ],
            capture_output=True,
            text=True,
            env=env,
            check=False,
        )

        messages = json.loads(result.stdout) if result.stdout.strip() else []
        return [(msg["symbol"], msg["line"]) for msg in messages]
    finally:
        os.unlink(path)


def _msg_ids(results: list[tuple[str, int]]) -> set[str]:
    """Extract just the message IDs from a results list."""
    return {msg_id for msg_id, _ in results}


# ===========================================================================
# @property tests
# ===========================================================================

class TestNoProperty:
    """Tests for the humbug-no-property check."""

    def test_property_is_flagged(self):
        """@property decorator should be flagged."""
        source = '''\
class Foo:
    """Test class."""

    @property
    def bar(self) -> int:
        """Get bar."""
        return 1
'''
        results = _run_pylint(source)
        assert MSG_NO_PROPERTY in _msg_ids(results)

    def test_plain_getter_is_ok(self):
        """A plain getter method should not be flagged."""
        source = '''\
class Foo:
    """Test class."""

    def bar(self) -> int:
        """Get bar."""
        return 1
'''
        results = _run_pylint(source)
        assert MSG_NO_PROPERTY not in _msg_ids(results)


# ===========================================================================
# Optional tests
# ===========================================================================

class TestNoOptional:
    """Tests for the humbug-no-optional check."""

    def test_optional_is_flagged(self):
        """Optional[X] should be flagged."""
        source = '''\
"""Module."""
from typing import Optional


def foo(x: Optional[int] = None) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_OPTIONAL in _msg_ids(results)

    def test_pipe_none_is_ok(self):
        """X | None should not be flagged."""
        source = '''\
"""Module."""


def foo(x: int | None = None) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_OPTIONAL not in _msg_ids(results)

# ===========================================================================
# Union tests
# ===========================================================================

class TestNoUnion:
    """Tests for the humbug-no-union check."""

    def test_union_is_flagged(self):
        """Union[X, Y] should be flagged."""
        source = '''\
"""Module."""
from typing import Union


def foo(x: Union[int, str]) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_UNION in _msg_ids(results)

    def test_pipe_syntax_is_ok(self):
        """X | Y should not be flagged."""
        source = '''\
"""Module."""


def foo(x: int | str) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_UNION not in _msg_ids(results)

    def test_union_in_nested_subscript_is_flagged(self):
        """Union inside a generic should be flagged."""
        source = '''\
"""Module."""
from typing import Union


def foo(x: list[Union[int, str]]) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_UNION in _msg_ids(results)

# ===========================================================================
# Typing alias tests
# ===========================================================================

class TestNoTypingAliases:
    """Tests for the humbug-no-typing-aliases check."""

    def test_dict_import_is_flagged(self):
        """from typing import Dict should be flagged."""
        source = '''\
"""Module."""
from typing import Dict


def foo(x: Dict[str, int]) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_TYPING_ALIASES in _msg_ids(results)

    def test_list_import_is_flagged(self):
        """from typing import List should be flagged."""
        source = '''\
"""Module."""
from typing import List


def foo(x: List[int]) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_TYPING_ALIASES in _msg_ids(results)

    def test_callable_import_is_flagged(self):
        """from typing import Callable should be flagged."""
        source = '''\
"""Module."""
from typing import Callable


def foo(x: Callable[[], None]) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_TYPING_ALIASES in _msg_ids(results)

    def test_builtin_dict_is_ok(self):
        """Using builtin dict should not be flagged."""
        source = '''\
"""Module."""


def foo(x: dict[str, int]) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_TYPING_ALIASES not in _msg_ids(results)

    def test_collections_abc_callable_is_ok(self):
        """Using collections.abc.Callable should not be flagged."""
        source = '''\
"""Module."""
from collections.abc import Callable


def foo(x: Callable[[], None]) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_TYPING_ALIASES not in _msg_ids(results)

    def test_any_is_ok(self):
        """from typing import Any should not be flagged."""
        source = '''\
"""Module."""
from typing import Any


def foo(x: Any) -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_NO_TYPING_ALIASES not in _msg_ids(results)


# ===========================================================================
# Aligned assignment tests
# ===========================================================================

class TestNoAlignedAssigns:
    """Tests for the humbug-no-aligned-assigns check."""

    def test_aligned_assigns_are_flagged(self):
        """Consecutive assignments with aligned = should be flagged."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
    short_name      = 1
    long_name_here  = 2
'''
        results = _run_pylint(source)
        assert MSG_NO_ALIGNED_ASSIGNS in _msg_ids(results)

    def test_misaligned_assigns_are_ok(self):
        """Assignments with different = columns should not be flagged."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
    x = 1
    yyy = 2
'''
        results = _run_pylint(source)
        assert MSG_NO_ALIGNED_ASSIGNS not in _msg_ids(results)

    def test_single_assignment_is_ok(self):
        """A single assignment should never be flagged."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
    x = 1
'''
        results = _run_pylint(source)
        assert MSG_NO_ALIGNED_ASSIGNS not in _msg_ids(results)

    def test_blank_line_breaks_group(self):
        """A blank line between assignments breaks the alignment group."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
    short_name = 1

    long_name_here = 2
'''
        results = _run_pylint(source)
        assert MSG_NO_ALIGNED_ASSIGNS not in _msg_ids(results)


# ===========================================================================
# Blank line before dedent tests
# ===========================================================================

class TestBlankBeforeDedent:
    """Tests for the humbug-blank-before-dedent check."""

    def test_else_without_blank_is_flagged(self):
        """else: without a preceding blank line should be flagged."""
        source = '''\
"""Module."""


def foo(x: int) -> int:
    """Do something."""
    if x > 0:
        return x
    else:
        return -x
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT in _msg_ids(results)

    def test_else_with_blank_is_ok(self):
        """else: with a preceding blank line should not be flagged."""
        source = '''\
"""Module."""


def foo(x: int) -> int:
    """Do something."""
    if x > 0:
        return x

    else:
        return -x
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_elif_without_blank_is_flagged(self):
        """elif: without a preceding blank line should be flagged."""
        source = '''\
"""Module."""


def foo(x: int) -> str:
    """Do something."""
    if x > 0:
        return "pos"
    elif x < 0:
        return "neg"

    else:
        return "zero"
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT in _msg_ids(results)

    def test_elif_with_blank_is_ok(self):
        """elif: with a preceding blank line should not be flagged."""
        source = '''\
"""Module."""


def foo(x: int) -> str:
    """Do something."""
    if x > 0:
        return "pos"

    elif x < 0:
        return "neg"

    else:
        return "zero"
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_ternary_else_is_not_flagged(self):
        """A ternary ``else`` should never be flagged as a dedent."""
        source = '''\
"""Module."""


def foo(x: int) -> str:
    """Do something."""
    result = (
        "yes"
        if x > 0
        else "no"
    )

    return result
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_multiline_elif_condition_is_flagged(self):
        """An elif with a multi-line condition should still be detected."""
        source = '''\
"""Module."""


def foo(x: int) -> str:
    """Do something."""
    if x > 0:
        return "pos"
    elif (
        x < 0
        and x > -100
    ):
        return "neg"
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT in _msg_ids(results)

    def test_comment_before_dedent_is_ok(self):
        """A comment line before a dedent is acceptable (no blank line required)."""
        source = '''\
"""Module."""


def foo(x: int) -> str:
    """Do something."""
    if x > 0:
        return "pos"
    # Handle the zero case
    elif x == 0:
        return "zero"
    # Everything else
    else:
        return "neg"
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_return_after_if_without_blank_is_flagged(self):
        """A return at function level after an if body should be flagged."""
        source = '''\
"""Module."""


def foo(x: int) -> int:
    """Do something."""
    if x > 0:
        do_something()
    result = x + 1

    return result
'''
        results = _run_pylint(source)
        dedent_lines = [line for msg_id, line in results if msg_id == MSG_BLANK_BEFORE_DEDENT]
        # "result = x + 1" on line 8 dedents from the if body and lacks a blank line
        assert 8 in dedent_lines

    def test_statement_after_if_body_without_blank_is_flagged(self):
        """A statement dedenting from an if block body must be preceded by a blank."""
        source = '''\
"""Module."""


def foo(x: int) -> None:
    """Do something."""
    if x > 0:
        do_first_thing()

    do_second_thing()
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_statement_after_if_body_with_blank_is_ok(self):
        """A statement after an if body with a blank line is fine."""
        source = '''\
"""Module."""


def foo(x: int) -> None:
    """Do something."""
    if x > 0:
        do_first_thing()

    do_second_thing()
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_nested_function_without_blank_is_flagged(self):
        """A nested function dedenting from a block must have a blank line."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
    if True:
        x = 1

    def bar() -> None:
        """Inner function."""
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_multiline_string_contents_ignored(self):
        """Lines inside a multi-line string should not trigger dedent checks."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
    text = """
    deeply_indented_line
    another_line
    """
    pass
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)

    def test_bracket_continuation_not_flagged(self):
        """Continuation lines inside brackets should not trigger dedent checks."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
    result = some_function(
        arg_one,
        arg_two,
    )

    return result
'''
        results = _run_pylint(source)
        assert MSG_BLANK_BEFORE_DEDENT not in _msg_ids(results)


# ===========================================================================
# Multiline docstring tests
# ===========================================================================

class TestMultilineDocstring:
    """Tests for the humbug-multiline-docstring check."""

    def test_good_multiline_docstring_is_ok(self):
        """A correctly formatted multi-line docstring should not be flagged."""
        source = '''\
"""Module."""


def foo() -> None:
    """
    Do something.

    Details here.
    """
'''
        results = _run_pylint(source)
        assert MSG_MULTILINE_DOCSTRING not in _msg_ids(results)

    def test_single_line_docstring_is_ok(self):
        """A single-line docstring should never be flagged."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something."""
'''
        results = _run_pylint(source)
        assert MSG_MULTILINE_DOCSTRING not in _msg_ids(results)

    def test_text_on_opening_line_is_flagged(self):
        """Text after the opening triple-quote should be flagged."""
        source = '''\
"""Module."""


def foo() -> None:
    """Do something.
    More text.
    """
'''
        results = _run_pylint(source)
        assert MSG_MULTILINE_DOCSTRING in _msg_ids(results)

    def test_text_on_closing_line_is_flagged(self):
        """Text before the closing triple-quote should be flagged."""
        source = '''\
"""Module."""


def foo() -> None:
    """
    Do something.
    More text."""
'''
        results = _run_pylint(source)
        assert MSG_MULTILINE_DOCSTRING in _msg_ids(results)


# ===========================================================================
# Output ordering tests
# ===========================================================================

class TestOutputOrdering:
    """Tests that messages are emitted in line-number order."""

    def test_messages_are_in_line_order(self):
        """Messages from different checks should appear in line-number order."""
        source = '''\
"""Module docstring on one line."""


class Foo:
    """Class docstring."""

    @property
    def bar(self) -> int:
        """Bad docstring opening.
        More text.
        """
        return 1

    def baz(self) -> None:
        """Do something."""
        if True:
            pass

        else:
            pass
'''
        results = _run_pylint(source)
        lines = [line for _, line in results]
        assert lines == sorted(lines)
