"""
Tests for nested block structures in the markdown syntax highlighter.

Covers code fences inside list items, blockquotes inside lists, lists inside
blockquotes, multi-line blockquotes, and transitions between block types.
"""
import sys
sys.path.insert(0, 'src')

import syntax.parser_imports
from syntax.markdown.markdown_parser import MarkdownParser, MarkdownParserState
from syntax.programming_language import ProgrammingLanguage
from syntax.lexer import TokenType


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def parse_lines(lines):
    parser = MarkdownParser()
    state = None
    all_line_tokens = []
    for line in lines:
        state = parser.parse(state, line)
        tokens = []
        while True:
            t = parser.get_next_token()
            if t is None:
                break
            tokens.append(t)
        all_line_tokens.append(tokens)
    return all_line_tokens, state


def types(tokens):
    return [t.type for t in tokens]


def values(tokens):
    return [t.value for t in tokens]


# ---------------------------------------------------------------------------
# TestFenceInsideList — code fence blocks nested inside list items
# ---------------------------------------------------------------------------

class TestFenceInsideList:

    def test_fence_opens_inside_list_item(self):
        """An indented ``` inside a list item opens a fence block."""
        lines = ["* item", "  ```python"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.FENCE_START in types(all_tokens[1])
        assert TokenType.LANGUAGE in types(all_tokens[1])
        assert state.in_fence_block is True
        assert state.language == ProgrammingLanguage.PYTHON

    def test_fence_content_inside_list_is_highlighted(self):
        """Content lines inside a fence that is inside a list item are
        delegated to the embedded language parser."""
        lines = ["* item", "  ```python", "  x = 1", "  ```"]
        all_tokens, state = parse_lines(lines)
        content_types = types(all_tokens[2])
        assert TokenType.FENCE_START not in content_types
        assert TokenType.FENCE_END not in content_types
        assert TokenType.LIST_MARKER not in content_types

    def test_fence_closes_inside_list_item(self):
        """An indented closing ``` inside a list item closes the fence."""
        lines = ["* item", "  ```python", "  x = 1", "  ```"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.FENCE_END in types(all_tokens[3])
        assert state.in_fence_block is False
        assert state.language == ProgrammingLanguage.UNKNOWN

    def test_list_continues_after_fence_closes(self):
        """A new list item after a fence-inside-list is recognised correctly."""
        lines = ["* item", "  ```python", "  x = 1", "  ```", "* next item"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[4])
        assert state.in_list_item is True

    def test_fence_at_column_zero_after_list_resets_list_state(self):
        """A fence at column 0 after a list item is not a list continuation —
        it resets list state when it opens."""
        lines = ["* item", "```python", "x = 1", "```"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.FENCE_START in types(all_tokens[1])
        assert TokenType.FENCE_END in types(all_tokens[3])
        assert state.in_fence_block is False
        assert state.in_list_item is False

    def test_fence_no_language_inside_list(self):
        """A plain ``` inside a list item opens a TEXT fence."""
        lines = ["* item", "  ```", "  content", "  ```"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.FENCE_START in types(all_tokens[1])
        assert TokenType.LANGUAGE not in types(all_tokens[1])
        assert TokenType.FENCE_END in types(all_tokens[3])
        assert state.in_fence_block is False

    def test_multiple_fences_inside_list(self):
        """Two fence blocks inside a list both open and close correctly."""
        lines = [
            "* item",
            "  ```python",
            "  a = 1",
            "  ```",
            "  ```python",
            "  b = 2",
            "  ```",
        ]
        all_tokens, state = parse_lines(lines)
        fence_starts = sum(1 for tl in all_tokens for t in tl if t.type == TokenType.FENCE_START)
        fence_ends = sum(1 for tl in all_tokens for t in tl if t.type == TokenType.FENCE_END)
        assert fence_starts == 2
        assert fence_ends == 2
        assert state.in_fence_block is False


# ---------------------------------------------------------------------------
# TestFenceInsideBlockquote — code fence blocks inside blockquoted content
# ---------------------------------------------------------------------------

class TestFenceInsideBlockquote:

    def test_fence_opens_inside_blockquote(self):
        """A ``` line prefixed with '> ' opens a fence block."""
        lines = ["> ```python", "> x = 1", "> ```"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.FENCE_START in types(all_tokens[0])
        assert state.in_fence_block is False  # closed by end

    def test_fence_content_inside_blockquote_is_highlighted(self):
        """Content inside a blockquote fence is delegated to the embedded parser."""
        lines = ["> ```python", "> x = 1", "> ```"]
        all_tokens, _ = parse_lines(lines)
        content_types = types(all_tokens[1])
        assert TokenType.FENCE_START not in content_types
        assert TokenType.FENCE_END not in content_types

    def test_fence_closes_inside_blockquote(self):
        """The closing ``` inside a blockquote closes the fence."""
        lines = ["> ```python", "> x = 1", "> ```"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.FENCE_END in types(all_tokens[2])
        assert state.in_fence_block is False

    def test_blockquote_text_after_fence_close(self):
        """Text in a blockquote after a fence close is a BLOCKQUOTE token."""
        lines = ["> ```python", "> x = 1", "> ```", "> after"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.BLOCKQUOTE in types(all_tokens[3])
        assert state.in_fence_block is False

    def test_fence_content_blockquote_prefix_token_emitted(self):
        """A BLOCKQUOTE token is emitted for the '> ' prefix on each content line inside a blockquote fence."""
        lines = ["> ```python", "> x = 1", "> ```"]
        all_tokens, _ = parse_lines(lines)
        content_token_types = types(all_tokens[1])
        assert TokenType.BLOCKQUOTE in content_token_types
        blockquote_token = next(t for t in all_tokens[1] if t.type == TokenType.BLOCKQUOTE)
        assert blockquote_token.value == "> "
        assert blockquote_token.start == 0

    def test_fence_content_embedded_tokens_have_correct_offsets(self):
        """Embedded language tokens inside a blockquote fence have start offsets relative to the full line."""
        lines = ["> ```python", "> x = 1", "> ```"]
        all_tokens, _ = parse_lines(lines)
        # The content line is '> x = 1'. After the '> ' prefix (2 chars), the embedded
        # parser sees 'x = 1'. All embedded tokens must have start >= 2.
        embedded_tokens = [t for t in all_tokens[1] if t.type != TokenType.BLOCKQUOTE]
        assert len(embedded_tokens) > 0
        for token in embedded_tokens:
            assert token.start >= 2, (
                f"Token {token.type!r} value={token.value!r} has start={token.start}, expected >= 2"
            )


# ---------------------------------------------------------------------------
# TestListInsideBlockquote — list items nested inside blockquoted content
# ---------------------------------------------------------------------------

class TestListInsideBlockquote:

    def test_blockquote_list_item_produces_list_marker_token(self):
        """A list item inside a blockquote produces a LIST_MARKER token, not a plain BLOCKQUOTE token."""
        lines = ["> - item"]
        all_tokens, _ = parse_lines(lines)
        token_types = types(all_tokens[0])
        assert TokenType.LIST_MARKER in token_types

    def test_blockquote_list_item_prefix_is_blockquote_token(self):
        """The '> ' prefix of a blockquote list item is emitted as a BLOCKQUOTE token."""
        lines = ["> - item"]
        all_tokens, _ = parse_lines(lines)
        assert all_tokens[0][0].type == TokenType.BLOCKQUOTE
        assert all_tokens[0][0].value == "> "
        assert all_tokens[0][0].start == 0

    def test_blockquote_list_item_content_has_correct_offset(self):
        """The LIST_MARKER token for a blockquote list item has a start offset past the '> ' prefix."""
        lines = ["> - item"]
        all_tokens, _ = parse_lines(lines)
        list_token = next(t for t in all_tokens[0] if t.type == TokenType.LIST_MARKER)
        assert list_token.start >= 2

    def test_blockquote_plain_text_content_is_not_list_marker(self):
        """A plain text blockquote line does not produce a LIST_MARKER token."""
        lines = ["> some text"]
        all_tokens, _ = parse_lines(lines)
        assert TokenType.LIST_MARKER not in types(all_tokens[0])

    def test_blockquote_multiple_list_items(self):
        """Multiple consecutive blockquote list items each produce BLOCKQUOTE + LIST_MARKER tokens."""
        lines = ["> - first", "> - second"]
        all_tokens, _ = parse_lines(lines)
        for line_tokens in all_tokens:
            token_types = types(line_tokens)
            assert TokenType.BLOCKQUOTE in token_types
            assert TokenType.LIST_MARKER in token_types


# ---------------------------------------------------------------------------
# TestBlockquoteInsideList — blockquote as the content of a list item
# ---------------------------------------------------------------------------

class TestBlockquoteInsideList:

    def test_list_item_blockquote_produces_list_marker_token(self):
        """'- > quote' produces a LIST_MARKER token for the '- ' prefix."""
        lines = ["- > quote"]
        all_tokens, _ = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[0])

    def test_list_item_blockquote_produces_blockquote_token(self):
        """'- > quote' produces a BLOCKQUOTE token for the '> quote' part."""
        lines = ["- > quote"]
        all_tokens, _ = parse_lines(lines)
        assert TokenType.BLOCKQUOTE in types(all_tokens[0])

    def test_list_item_blockquote_prefix_value_and_offset(self):
        """The LIST_MARKER token covers only the '- ' prefix; the BLOCKQUOTE starts after it."""
        lines = ["- > quote"]
        all_tokens, _ = parse_lines(lines)
        list_token = next(t for t in all_tokens[0] if t.type == TokenType.LIST_MARKER)
        bq_token = next(t for t in all_tokens[0] if t.type == TokenType.BLOCKQUOTE)
        # LIST_MARKER covers '- ' (starts at 0)
        assert list_token.start == 0
        assert list_token.value == "- "
        # BLOCKQUOTE starts immediately after the list marker prefix
        assert bq_token.start == 2
        assert bq_token.value.startswith('>')

    def test_list_item_blockquote_asterisk_marker(self):
        """'* > quote' also produces LIST_MARKER + BLOCKQUOTE tokens."""
        lines = ["* > quote"]
        all_tokens, _ = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[0])
        assert TokenType.BLOCKQUOTE in types(all_tokens[0])

    def test_list_item_blockquote_plus_marker(self):
        """'+ > quote' also produces LIST_MARKER + BLOCKQUOTE tokens."""
        lines = ["+ > quote"]
        all_tokens, _ = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[0])
        assert TokenType.BLOCKQUOTE in types(all_tokens[0])

    def test_list_item_blockquote_inline_formatting_recognised(self):
        """Inline formatting inside the blockquote part of a list item is recognised."""
        lines = ["- > **bold** text"]
        all_tokens, _ = parse_lines(lines)
        assert TokenType.BOLD_START in types(all_tokens[0])
        assert TokenType.BOLD in types(all_tokens[0])
        assert TokenType.BOLD_END in types(all_tokens[0])

    def test_list_item_plain_content_not_split(self):
        """A list item whose content does not start with '>' is not split."""
        lines = ["- normal item"]
        all_tokens, _ = parse_lines(lines)
        assert TokenType.BLOCKQUOTE not in types(all_tokens[0])
        assert TokenType.LIST_MARKER in types(all_tokens[0])


# ---------------------------------------------------------------------------
# TestBlockquoteStructure — multi-line and transitional blockquote behaviour
# ---------------------------------------------------------------------------

class TestBlockquoteStructure:

    def test_multi_line_blockquote(self):
        """Consecutive blockquote lines each produce a BLOCKQUOTE token."""
        lines = ["> line one", "> line two", "> line three"]
        all_tokens, state = parse_lines(lines)
        for line_tokens in all_tokens:
            assert TokenType.BLOCKQUOTE in types(line_tokens)

    def test_blockquote_resets_list_state(self):
        """A top-level blockquote line resets in_list_item to False."""
        lines = ["* item", "> quote"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.BLOCKQUOTE in types(all_tokens[1])
        assert state.in_list_item is False
        assert state.list_indent_stack == []

    def test_blockquote_then_list(self):
        """A list item following a blockquote is recognised correctly."""
        lines = ["> quoted", "* list item"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.BLOCKQUOTE in types(all_tokens[0])
        assert TokenType.LIST_MARKER in types(all_tokens[1])
        assert state.in_list_item is True

    def test_blockquote_inline_bold(self):
        """Bold inside a blockquote line is recognised."""
        lines = ["> **bold text**"]
        all_tokens, _ = parse_lines(lines)
        t = types(all_tokens[0])
        assert TokenType.BLOCKQUOTE in t
        assert TokenType.BOLD_START in t
        assert TokenType.BOLD in t
        assert TokenType.BOLD_END in t

    def test_blockquote_inline_italic(self):
        """Italic inside a blockquote line is recognised."""
        lines = ["> *italic*"]
        all_tokens, _ = parse_lines(lines)
        t = types(all_tokens[0])
        assert TokenType.BLOCKQUOTE in t
        assert TokenType.ITALIC_START in t
        assert TokenType.ITALIC in t

    def test_blockquote_inline_code(self):
        """Inline code inside a blockquote line is recognised."""
        lines = ["> use `code` here"]
        all_tokens, _ = parse_lines(lines)
        t = types(all_tokens[0])
        assert TokenType.BLOCKQUOTE in t
        assert TokenType.INLINE_CODE_START in t
        assert TokenType.INLINE_CODE in t

    def test_blockquote_inline_link(self):
        """A link inside a blockquote line is recognised."""
        lines = ["> see [this](url)"]
        all_tokens, _ = parse_lines(lines)
        t = types(all_tokens[0])
        assert TokenType.BLOCKQUOTE in t
        assert TokenType.LINK_START in t
        assert TokenType.LINK_URL in t

    def test_blockquote_inline_strikethrough(self):
        """Strikethrough inside a blockquote line is recognised."""
        lines = ["> ~~struck~~"]
        all_tokens, _ = parse_lines(lines)
        t = types(all_tokens[0])
        assert TokenType.BLOCKQUOTE in t
        assert TokenType.STRIKETHROUGH_START in t
        assert TokenType.STRIKETHROUGH in t

    def test_blockquote_resets_nested_list_state(self):
        """A top-level blockquote after a nested list resets the full indent stack."""
        lines = ["* outer", "  * inner", "> quote"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.BLOCKQUOTE in types(all_tokens[2])
        assert state.in_list_item is False
        assert state.list_indent_stack == []

    def test_indented_blockquote_inside_list_preserves_list_state(self):
        """A blockquote continuation line indented inside a list item does not
        exit list context — the list is still active on the next item."""
        lines = ["* item", "  > quoted continuation", "* next item"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[2])
        assert state.in_list_item is True


# ---------------------------------------------------------------------------
# TestListBlockTransitions — transitions between lists and other block types
# ---------------------------------------------------------------------------

class TestListBlockTransitions:

    def test_heading_after_list_resets_and_starts_heading(self):
        """A heading after a list resets list state and produces a HEADING token."""
        lines = ["* item", "# Heading"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.HEADING in types(all_tokens[1])
        assert state.in_list_item is False

    def test_new_list_after_heading(self):
        """A list item after a heading starts a fresh list."""
        lines = ["# Heading", "* item"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[1])
        assert state.in_list_item is True

    def test_table_after_list_resets_list_state(self):
        """A table line after a list resets list state."""
        lines = ["* item", "| col1 | col2 |"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.TABLE in types(all_tokens[1])
        assert state.in_list_item is False

    def test_horizontal_rule_after_list(self):
        """A horizontal rule after a list is recognised correctly."""
        lines = ["* item", "---"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.HORIZONTAL_RULE in types(all_tokens[1])
        assert state.in_list_item is False

    def test_plain_text_after_list_resets_state(self):
        """A non-indented plain text line after a list resets list state."""
        lines = ["* item", "plain text"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.TEXT in types(all_tokens[1])
        assert state.in_list_item is False

    def test_list_after_fence(self):
        """A list item after a closed fence block is recognised correctly."""
        lines = ["```python", "x = 1", "```", "* list item"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[3])
        assert state.in_list_item is True
        assert state.in_fence_block is False

    def test_heading_after_fence(self):
        """A heading after a closed fence block is recognised correctly."""
        lines = ["```python", "x = 1", "```", "## Heading"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.HEADING in types(all_tokens[3])
        assert state.in_fence_block is False

    def test_blockquote_after_fence(self):
        """A blockquote after a closed fence block is recognised correctly."""
        lines = ["```python", "x = 1", "```", "> quoted"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.BLOCKQUOTE in types(all_tokens[3])
        assert state.in_fence_block is False


# ---------------------------------------------------------------------------
# TestListContinuation — indented continuation lines inside list items
# ---------------------------------------------------------------------------

class TestListContinuation:

    def test_indented_text_continuation_is_list_marker(self):
        """An indented text line inside a list is treated as list content."""
        lines = ["* item", "  continuation"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[1])
        assert state.in_list_item is True

    def test_indented_continuation_with_inline_code(self):
        """Inline code on an indented continuation line is recognised."""
        lines = ["* item", "  has `code` here"]
        all_tokens, _ = parse_lines(lines)
        t = types(all_tokens[1])
        assert TokenType.LIST_MARKER in t
        assert TokenType.INLINE_CODE_START in t
        assert TokenType.INLINE_CODE in t
        assert TokenType.INLINE_CODE_END in t

    def test_indented_continuation_with_bold(self):
        """Bold on an indented continuation line is recognised."""
        lines = ["* item", "  **bold** text"]
        all_tokens, _ = parse_lines(lines)
        t = types(all_tokens[1])
        assert TokenType.LIST_MARKER in t
        assert TokenType.BOLD_START in t
        assert TokenType.BOLD in t

    def test_deeply_indented_continuation_stays_in_list(self):
        """A deeply indented continuation line remains in list context."""
        lines = ["* outer", "  * inner", "    continued"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[2])
        assert state.in_list_item is True

    def test_de_indent_to_outer_list_level(self):
        """De-indenting from inner to outer list level stays in list."""
        lines = ["* outer", "  * inner", "* back to outer"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[2])
        assert state.in_list_item is True
        assert len(state.list_indent_stack) == 1

    def test_mixed_ordered_and_unordered_list(self):
        """Ordered and unordered items in sequence are both LIST_MARKER tokens."""
        lines = ["1. first", "2. second", "* bullet", "- dash"]
        all_tokens, state = parse_lines(lines)
        for line_tokens in all_tokens:
            assert TokenType.LIST_MARKER in types(line_tokens)
