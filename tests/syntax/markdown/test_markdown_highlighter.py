"""
Comprehensive tests for the markdown syntax highlighter (lexer + parser).

Covers block-level recognition, fence state machine, inline formatting (happy
path and edge cases), list parsing, nesting, token positions, and state
continuity across lines.
"""
import sys
sys.path.insert(0, 'src')

import syntax.parser_imports  # registers all parsers
from syntax.markdown.markdown_parser import MarkdownParser, MarkdownParserState, BlockContext
from syntax.programming_language import ProgrammingLanguage
from syntax.lexer import TokenType


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def parse_lines(lines):
    """Parse a list of lines incrementally.

    Returns (list_of_token_lists, final_state) where each element of
    list_of_token_lists is the list of tokens produced for that line.
    """
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


def parse_line(line, state=None):
    """Parse a single line and return (tokens, new_state)."""
    parser = MarkdownParser()
    new_state = parser.parse(state, line)
    tokens = []
    while True:
        t = parser.get_next_token()
        if t is None:
            break
        tokens.append(t)
    return tokens, new_state


def types(tokens):
    """Return list of token types."""
    return [t.type for t in tokens]


def values(tokens):
    """Return list of token values."""
    return [t.value for t in tokens]


def starts(tokens):
    """Return list of token start positions."""
    return [t.start for t in tokens]


def in_list(state):
    """Return True if the state has any LIST_MARKER entry in the block stack."""
    return any(ctx.type == TokenType.LIST_MARKER for ctx in state.block_stack)


def list_indent_stack(state):
    """Return the list of indent values from LIST_MARKER entries in block_stack."""
    return [ctx.indent for ctx in state.block_stack if ctx.type == TokenType.LIST_MARKER]


# ---------------------------------------------------------------------------
# TestLexerBlocks — block-level token recognition
# ---------------------------------------------------------------------------

class TestLexerBlocks:

    def test_plain_text(self):
        """A plain text line produces a single TEXT token."""
        tokens, _ = parse_line("hello world")
        assert types(tokens) == [TokenType.TEXT]
        assert values(tokens) == ["hello world"]

    def test_heading_h1(self):
        """# heading produces a HEADING token containing the whole line."""
        tokens, _ = parse_line("# Title")
        assert TokenType.HEADING in types(tokens)
        heading = tokens[types(tokens).index(TokenType.HEADING)]
        assert heading.value.startswith("# Title")

    def test_heading_h2(self):
        """## heading produces a HEADING token."""
        tokens, _ = parse_line("## Section")
        assert TokenType.HEADING in types(tokens)

    def test_heading_h6(self):
        """###### heading produces a HEADING token."""
        tokens, _ = parse_line("###### Deep")
        assert TokenType.HEADING in types(tokens)

    def test_blockquote(self):
        """> text produces a BLOCKQUOTE token."""
        tokens, _ = parse_line("> quoted text")
        assert TokenType.BLOCKQUOTE in types(tokens)

    def test_unordered_list_asterisk(self):
        """'* item' (asterisk + space) produces a LIST_MARKER token."""
        tokens, _ = parse_line("* item")
        assert TokenType.LIST_MARKER in types(tokens)

    def test_unordered_list_dash(self):
        """'- item' produces a LIST_MARKER token."""
        tokens, _ = parse_line("- item")
        assert TokenType.LIST_MARKER in types(tokens)

    def test_unordered_list_plus(self):
        """'+ item' produces a LIST_MARKER token."""
        tokens, _ = parse_line("+ item")
        assert TokenType.LIST_MARKER in types(tokens)

    def test_asterisk_no_space_is_text(self):
        """'*text' (no space after asterisk) is TEXT, not a list marker."""
        tokens, _ = parse_line("*text")
        # The lexer sees '*' not followed by whitespace and not 3+ same chars,
        # so it falls through to _read_text.
        assert TokenType.LIST_MARKER not in types(tokens)

    def test_horizontal_rule_dashes(self):
        """'---' produces a HORIZONTAL_RULE token."""
        tokens, _ = parse_line("---")
        assert TokenType.HORIZONTAL_RULE in types(tokens)

    def test_horizontal_rule_asterisks(self):
        """'***' produces a HORIZONTAL_RULE token."""
        tokens, _ = parse_line("***")
        assert TokenType.HORIZONTAL_RULE in types(tokens)

    def test_horizontal_rule_plus(self):
        """'+++' produces a HORIZONTAL_RULE token."""
        tokens, _ = parse_line("+++")
        assert TokenType.HORIZONTAL_RULE in types(tokens)

    def test_double_dash_is_text(self):
        """'--' (only 2 dashes) is TEXT, not a horizontal rule.
        The lexer checks pos+1 < len and input[pos]==marker and input[pos+1]==marker,
        which requires at least 3 chars total."""
        tokens, _ = parse_line("--")
        assert TokenType.HORIZONTAL_RULE not in types(tokens)
        assert TokenType.LIST_MARKER not in types(tokens)

    def test_ordered_list_dot(self):
        """'1. item' produces a LIST_MARKER token."""
        tokens, _ = parse_line("1. item")
        assert TokenType.LIST_MARKER in types(tokens)

    def test_ordered_list_paren(self):
        """'1) item' produces a LIST_MARKER token."""
        tokens, _ = parse_line("1) item")
        assert TokenType.LIST_MARKER in types(tokens)

    def test_ordered_list_multidigit(self):
        """'10. item' produces a LIST_MARKER token."""
        tokens, _ = parse_line("10. item")
        assert TokenType.LIST_MARKER in types(tokens)

    def test_ordered_list_no_space_is_text(self):
        """'1.text' (no space after dot) is TEXT, not a list marker."""
        tokens, _ = parse_line("1.text")
        assert TokenType.LIST_MARKER not in types(tokens)

    def test_ordered_list_paren_no_space_is_text(self):
        """'1)text' (no space after paren) is TEXT."""
        tokens, _ = parse_line("1)text")
        assert TokenType.LIST_MARKER not in types(tokens)

    def test_bare_number_is_text(self):
        """'123' with no dot or paren is TEXT."""
        tokens, _ = parse_line("123")
        assert TokenType.LIST_MARKER not in types(tokens)
        assert TokenType.TEXT in types(tokens)

    def test_table(self):
        """'| col1 | col2 |' produces a TABLE token."""
        tokens, _ = parse_line("| col1 | col2 |")
        assert TokenType.TABLE in types(tokens)

    def test_fence_no_language(self):
        """'```' opens a fence block with FENCE_START token and no LANGUAGE token."""
        tokens, state = parse_line("```")
        assert TokenType.FENCE_START in types(tokens)
        assert TokenType.LANGUAGE not in types(tokens)
        assert state.in_fence_block is True
        assert state.language == ProgrammingLanguage.TEXT

    def test_fence_with_language(self):
        """'```python' produces FENCE_START and LANGUAGE tokens."""
        tokens, state = parse_line("```python")
        assert TokenType.FENCE_START in types(tokens)
        assert TokenType.LANGUAGE in types(tokens)
        lang_token = tokens[types(tokens).index(TokenType.LANGUAGE)]
        assert lang_token.value == "python"
        assert state.language == ProgrammingLanguage.PYTHON

    def test_single_backtick_is_text(self):
        """A single backtick is not a fence — it's TEXT at the block level."""
        tokens, state = parse_line("`not a fence")
        assert TokenType.FENCE_START not in types(tokens)
        assert state.in_fence_block is False

    def test_double_backtick_is_text(self):
        """Two backticks are not a fence."""
        tokens, state = parse_line("``not a fence")
        assert TokenType.FENCE_START not in types(tokens)
        assert state.in_fence_block is False


# ---------------------------------------------------------------------------
# TestFenceBlocks — fence state machine across multiple lines
# ---------------------------------------------------------------------------

class TestFenceBlocks:

    def test_fence_open_close(self):
        """Opening and closing a plain fence block transitions state correctly."""
        lines = ["```", "content", "```"]
        all_tokens, state = parse_lines(lines)
        assert types(all_tokens[0]) == [TokenType.FENCE_START]
        assert types(all_tokens[2]) == [TokenType.FENCE_END]
        assert state.in_fence_block is False
        assert state.language == ProgrammingLanguage.UNKNOWN

    def test_fence_language_python(self):
        """Opening a python fence sets language to PYTHON."""
        _, state = parse_line("```python")
        assert state.language == ProgrammingLanguage.PYTHON
        assert state.in_fence_block is True

    def test_fence_no_language_sets_text(self):
        """Opening a fence with no language tag sets language to TEXT."""
        _, state = parse_line("```")
        assert state.language == ProgrammingLanguage.TEXT
        assert state.in_fence_block is True

    def test_fence_depth_tracked(self):
        """fence_depth records the indentation of the opening fence."""
        _, state = parse_line("    ```")
        assert state.in_fence_block is True
        assert state.fence_depth == 4

    def test_indented_fence_inside_closes_with_outer(self):
        """A ``` indented MORE than the opening fence is passed to the embedded
        parser as content, not treated as a closing fence."""
        lines = ["```", "    ```", "```"]
        all_tokens, state = parse_lines(lines)
        # Line 1: fence start
        assert TokenType.FENCE_START in types(all_tokens[0])
        # Line 2: indented ``` is content — fence stays open
        assert TokenType.FENCE_END not in types(all_tokens[1])
        # Line 3: the actual closing fence
        assert TokenType.FENCE_END in types(all_tokens[2])
        assert state.in_fence_block is False

    def test_fence_close_at_same_or_lower_indent(self):
        """A closing ``` at same indentation as opening closes the fence."""
        lines = ["    ```", "content", "    ```"]
        all_tokens, state = parse_lines(lines)
        assert state.in_fence_block is False

    def test_multiple_fence_blocks(self):
        """Two consecutive fence blocks both open and close correctly."""
        lines = ["```", "a", "```", "text", "```", "b", "```"]
        all_tokens, state = parse_lines(lines)
        fence_starts = sum(1 for tl in all_tokens for t in tl if t.type == TokenType.FENCE_START)
        fence_ends = sum(1 for tl in all_tokens for t in tl if t.type == TokenType.FENCE_END)
        assert fence_starts == 2
        assert fence_ends == 2
        assert state.in_fence_block is False

    def test_content_inside_fence_delegated(self):
        """Lines inside a fence block are delegated to the embedded parser,
        not processed by the markdown inline formatter."""
        lines = ["```python", "x = 1", "```"]
        all_tokens, _ = parse_lines(lines)
        # The content line should have been parsed by the Python parser,
        # not produce a plain TEXT token
        content_types = types(all_tokens[1])
        assert TokenType.FENCE_START not in content_types
        assert TokenType.FENCE_END not in content_types

    def test_state_after_fence_close_is_clean(self):
        """After a fence closes, language resets to UNKNOWN and in_fence_block is False."""
        lines = ["```python", "x = 1", "```"]
        _, state = parse_lines(lines)
        assert state.in_fence_block is False
        assert state.language == ProgrammingLanguage.UNKNOWN
        assert state.embedded_parser_state is None

    def test_indented_fence_inside_plain_fence_is_content(self):
        """A ``` indented more than the opening fence is content, not a closing fence."""
        lines = ["```", "    ```", "```"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.FENCE_START in types(all_tokens[0])
        assert TokenType.FENCE_END not in types(all_tokens[1])
        assert TokenType.FENCE_END in types(all_tokens[2])
        assert state.in_fence_block is False


# ---------------------------------------------------------------------------
# TestInlineFormatting — happy path
# ---------------------------------------------------------------------------

class TestInlineFormatting:

    def test_bold_double_asterisk(self):
        """**bold** produces BOLD_START, BOLD, BOLD_END."""
        tokens, _ = parse_line("**bold**")
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD in types(tokens)
        assert TokenType.BOLD_END in types(tokens)
        bold = tokens[types(tokens).index(TokenType.BOLD)]
        assert bold.value == "bold"

    def test_bold_double_underscore_at_start(self):
        """__bold__ at start of line produces bold tokens."""
        tokens, _ = parse_line("__bold__")
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD in types(tokens)
        assert TokenType.BOLD_END in types(tokens)

    def test_bold_double_underscore_after_space(self):
        """word __bold__ (after space) produces bold tokens."""
        tokens, _ = parse_line("word __bold__")
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD in types(tokens)

    def test_italic_asterisk(self):
        """*italic* produces ITALIC_START, ITALIC, ITALIC_END."""
        tokens, _ = parse_line("*italic*")
        assert TokenType.ITALIC_START in types(tokens)
        assert TokenType.ITALIC in types(tokens)
        assert TokenType.ITALIC_END in types(tokens)
        italic = tokens[types(tokens).index(TokenType.ITALIC)]
        assert italic.value == "italic"

    def test_italic_underscore_at_start(self):
        """_italic_ at start of line produces italic tokens."""
        tokens, _ = parse_line("_italic_")
        assert TokenType.ITALIC_START in types(tokens)
        assert TokenType.ITALIC in types(tokens)
        assert TokenType.ITALIC_END in types(tokens)

    def test_italic_underscore_after_space(self):
        """word _italic_ (after space) produces italic tokens."""
        tokens, _ = parse_line("word _italic_")
        assert TokenType.ITALIC_START in types(tokens)
        assert TokenType.ITALIC in types(tokens)

    def test_inline_code(self):
        """`code` produces INLINE_CODE_START, INLINE_CODE, INLINE_CODE_END."""
        tokens, _ = parse_line("`code`")
        assert TokenType.INLINE_CODE_START in types(tokens)
        assert TokenType.INLINE_CODE in types(tokens)
        assert TokenType.INLINE_CODE_END in types(tokens)
        code = tokens[types(tokens).index(TokenType.INLINE_CODE)]
        assert code.value == "code"

    def test_strikethrough(self):
        """~~strike~~ produces STRIKETHROUGH_START, STRIKETHROUGH, STRIKETHROUGH_END."""
        tokens, _ = parse_line("~~strike~~")
        assert TokenType.STRIKETHROUGH_START in types(tokens)
        assert TokenType.STRIKETHROUGH in types(tokens)
        assert TokenType.STRIKETHROUGH_END in types(tokens)
        strike = tokens[types(tokens).index(TokenType.STRIKETHROUGH)]
        assert strike.value == "strike"

    def test_link(self):
        """[text](url) produces LINK_START, LINK_TEXT, LINK_TEXT_END, LINK_URL, LINK_END."""
        tokens, _ = parse_line("[click here](https://example.com)")
        assert TokenType.LINK_START in types(tokens)
        assert TokenType.LINK_TEXT in types(tokens)
        assert TokenType.LINK_TEXT_END in types(tokens)
        assert TokenType.LINK_URL in types(tokens)
        assert TokenType.LINK_END in types(tokens)
        url = tokens[types(tokens).index(TokenType.LINK_URL)]
        assert url.value == "https://example.com"
        text = tokens[types(tokens).index(TokenType.LINK_TEXT)]
        assert text.value == "click here"

    def test_image(self):
        """![alt](url) produces IMAGE_START, IMAGE_ALT_TEXT, IMAGE_ALT_END, IMAGE_URL, IMAGE_END."""
        tokens, _ = parse_line("![logo](https://example.com/img.png)")
        assert TokenType.IMAGE_START in types(tokens)
        assert TokenType.IMAGE_ALT_TEXT in types(tokens)
        assert TokenType.IMAGE_ALT_END in types(tokens)
        assert TokenType.IMAGE_URL in types(tokens)
        assert TokenType.IMAGE_END in types(tokens)
        alt = tokens[types(tokens).index(TokenType.IMAGE_ALT_TEXT)]
        assert alt.value == "logo"

    def test_text_before_and_after_bold(self):
        """Text tokens appear correctly before and after a bold span."""
        tokens, _ = parse_line("before **bold** after")
        t = types(tokens)
        assert t[0] == TokenType.TEXT
        assert TokenType.BOLD_START in t
        assert t[-1] == TokenType.TEXT
        assert tokens[0].value == "before "
        assert tokens[-1].value == " after"

    def test_text_before_and_after_link(self):
        """Text tokens appear correctly before and after a link."""
        tokens, _ = parse_line("see [this](url) now")
        t = types(tokens)
        assert t[0] == TokenType.TEXT
        assert TokenType.LINK_START in t
        assert t[-1] == TokenType.TEXT

    def test_multiple_inline_spans(self):
        """Multiple inline spans on one line are all recognised."""
        tokens, _ = parse_line("**a** and *b* and `c`")
        t = types(tokens)
        assert TokenType.BOLD_START in t
        assert TokenType.ITALIC_START in t
        assert TokenType.INLINE_CODE_START in t

    def test_link_with_nested_parens_in_url(self):
        """Links whose URLs contain parentheses are handled by _find_closing_paren."""
        tokens, _ = parse_line("[text](https://en.wikipedia.org/wiki/Foo_(bar))")
        assert TokenType.LINK_START in types(tokens)
        url = tokens[types(tokens).index(TokenType.LINK_URL)]
        assert "Foo_(bar)" in url.value

    def test_empty_image_alt(self):
        """![](url) with empty alt text: no IMAGE_ALT_TEXT token, but rest present."""
        tokens, _ = parse_line("![](https://example.com/img.png)")
        assert TokenType.IMAGE_START in types(tokens)
        assert TokenType.IMAGE_ALT_TEXT not in types(tokens)
        assert TokenType.IMAGE_ALT_END in types(tokens)
        assert TokenType.IMAGE_URL in types(tokens)
        assert TokenType.IMAGE_END in types(tokens)

    def test_empty_link_text(self):
        """[](url) with empty link text: no LINK_TEXT token, but rest present."""
        tokens, _ = parse_line("[](https://example.com)")
        assert TokenType.LINK_START in types(tokens)
        assert TokenType.LINK_TEXT not in types(tokens)
        assert TokenType.LINK_TEXT_END in types(tokens)


# ---------------------------------------------------------------------------
# TestInlineEdgeCases — negative cases and boundary conditions
# ---------------------------------------------------------------------------

class TestInlineEdgeCases:

    def test_bold_at_end_of_string_not_triggered(self):
        """'**' at the very end (i+2 >= len) does not produce bold tokens."""
        tokens, _ = parse_line("text**")
        assert TokenType.BOLD_START not in types(tokens)

    def test_bold_followed_by_space_not_triggered(self):
        """'** text' (space after **) does not produce bold tokens."""
        tokens, _ = parse_line("** text")
        assert TokenType.BOLD_START not in types(tokens)

    def test_bold_underscore_mid_word_not_triggered(self):
        """'word__bold__' — __ not at start and not preceded by space → no bold."""
        tokens, _ = parse_line("word__bold__")
        assert TokenType.BOLD_START not in types(tokens), \
            "__bold__ mid-word should not be recognised as bold"

    def test_italic_at_end_of_string_not_triggered(self):
        """'*' at the very end (i+1 >= len) does not produce italic tokens."""
        tokens, _ = parse_line("text*")
        assert TokenType.ITALIC_START not in types(tokens)

    def test_italic_asterisk_followed_by_space_not_triggered(self):
        """'* ' (asterisk then space) does not produce italic.
        Note: at line start this becomes a LIST_MARKER via the lexer, so we
        test it mid-line where the inline formatter sees it."""
        tokens, _ = parse_line("foo * bar")
        assert TokenType.ITALIC_START not in types(tokens), \
            "* followed by space mid-line should not be italic"

    def test_italic_underscore_mid_word_not_triggered(self):
        """'word_italic_' — _ not at start and not preceded by space → no italic."""
        tokens, _ = parse_line("word_italic_")
        assert TokenType.ITALIC_START not in types(tokens), \
            "_italic_ mid-word should not be recognised as italic"

    def test_strikethrough_at_end_of_string_not_triggered(self):
        """'~~' at the very end (i+2 >= len) does not produce strikethrough."""
        tokens, _ = parse_line("text~~")
        assert TokenType.STRIKETHROUGH_START not in types(tokens)

    def test_strikethrough_followed_by_space_not_triggered(self):
        """'~~ text' (space after ~~) does not produce strikethrough."""
        tokens, _ = parse_line("~~ text~~")
        assert TokenType.STRIKETHROUGH_START not in types(tokens)

    def test_unclosed_bold_is_plain_text(self):
        """'**unclosed' with no closing ** is treated as plain text."""
        tokens, _ = parse_line("**unclosed")
        assert TokenType.BOLD_START not in types(tokens)
        assert TokenType.BOLD not in types(tokens)

    def test_unclosed_italic_is_plain_text(self):
        """'*unclosed' with no closing * is treated as plain text."""
        tokens, _ = parse_line("*unclosed")
        assert TokenType.ITALIC_START not in types(tokens)

    def test_unclosed_inline_code_is_plain_text(self):
        """'`unclosed' with no closing backtick is treated as plain text."""
        tokens, _ = parse_line("`unclosed")
        assert TokenType.INLINE_CODE_START not in types(tokens)

    def test_unclosed_strikethrough_is_plain_text(self):
        """'~~unclosed' with no closing ~~ is treated as plain text."""
        tokens, _ = parse_line("~~unclosed")
        assert TokenType.STRIKETHROUGH_START not in types(tokens)

    def test_link_no_closing_paren_is_plain_text(self):
        """[text](url-no-close produces no link tokens.

        Tracing the code: add_text_token(0, 0) emits nothing (i==0).
        text_end is found, but url_end=-1, so the inner block is skipped.
        'continue' is never reached, current_text_start stays 0.
        i increments to 1. Loop continues. At end, add_text_token(0, len)
        emits the whole string as one TEXT token.
        """
        tokens, _ = parse_line("[text](url-no-close")
        assert TokenType.LINK_START not in types(tokens)
        assert TokenType.LINK_URL not in types(tokens)
        # The whole line ends up as a single TEXT token
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.TEXT
        assert tokens[0].value == "[text](url-no-close"

    def test_link_no_bracket_pattern_is_plain_text(self):
        """[text] with no '](url)' part produces no link tokens.

        text.find('](', i+1) returns -1, so the link branch is not taken.
        add_text_token(0, i) emits nothing (i==0). Falls through to i += 1.
        The whole string is emitted as TEXT at the end.
        """
        tokens, _ = parse_line("[text]")
        assert TokenType.LINK_START not in types(tokens)
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.TEXT

    def test_image_no_bracket_pattern_is_plain_text(self):
        """'![alt text' with no '](' produces no image tokens — just one TEXT token."""
        tokens, _ = parse_line("![alt text")
        assert TokenType.IMAGE_START not in types(tokens)
        assert TokenType.LINK_START not in types(tokens)
        assert all(t.type == TokenType.TEXT for t in tokens), \
            "All tokens should be TEXT when image syntax is incomplete"
        assert len(tokens) == 1
        assert tokens[0].value == "![alt text"

    def test_four_asterisks_is_horizontal_rule(self):
        """'****' is classified as HORIZONTAL_RULE by the lexer (4 identical chars
        satisfies the 3+ same-char rule), not as bold. The inline formatter never
        sees it. This is consistent with standard Markdown behaviour."""
        tokens, _ = parse_line("****")
        assert TokenType.HORIZONTAL_RULE in types(tokens)
        assert TokenType.BOLD_START not in types(tokens)
        assert TokenType.BOLD_END not in types(tokens)

    def test_four_tildes_is_empty_strikethrough(self):
        """'~~~~' — _is_strikethrough_marker fires at i=0, strike_end=2.
        Content is empty, so STRIKETHROUGH_START + STRIKETHROUGH_END with no content."""
        tokens, _ = parse_line("~~~~")
        assert TokenType.STRIKETHROUGH_START in types(tokens)
        assert TokenType.STRIKETHROUGH_END in types(tokens)
        assert TokenType.STRIKETHROUGH not in types(tokens)

    def test_bold_does_not_produce_italic_tokens(self):
        """**bold** must not produce any ITALIC tokens.

        _is_bold_marker is checked before _is_italic_marker in the elif chain,
        so the bold branch fires first and the * characters are consumed.
        """
        tokens, _ = parse_line("**bold**")
        assert TokenType.ITALIC_START not in types(tokens)
        assert TokenType.ITALIC not in types(tokens)
        assert TokenType.ITALIC_END not in types(tokens)

    def test_italic_wrapping_bold(self):
        """'*hello **world** end*' — italic wrapping bold is parsed correctly.

        The closing-marker search skips over '*' candidates that are part of '**',
        so the outer '*' correctly finds its closing '*' at the end of the string.
        """
        tokens, _ = parse_line("*hello **world** end*")
        t = types(tokens)
        assert TokenType.ITALIC_START in t
        assert TokenType.ITALIC in t
        assert TokenType.BOLD_START in t
        assert TokenType.BOLD in t
        assert TokenType.ITALIC_END in t

    def test_horizontal_rule_not_list(self):
        """'---' is a HORIZONTAL_RULE, not a LIST_MARKER."""
        tokens, _ = parse_line("---")
        assert TokenType.HORIZONTAL_RULE in types(tokens)
        assert TokenType.LIST_MARKER not in types(tokens)

    def test_dash_space_is_list_not_rule(self):
        """'- item' is a LIST_MARKER, not a HORIZONTAL_RULE."""
        tokens, _ = parse_line("- item")
        assert TokenType.LIST_MARKER in types(tokens)
        assert TokenType.HORIZONTAL_RULE not in types(tokens)

    def test_asterisk_immediately_followed_by_asterisk_not_italic(self):
        """A '*' immediately followed by another '*' is never an italic opener.
        '*a**' contains no valid italic span."""
        tokens, _ = parse_line("*a**")
        assert TokenType.ITALIC_START not in types(tokens)
        assert TokenType.ITALIC not in types(tokens)

    def test_double_underscore_space_after_not_bold_or_italic(self):
        """'__ bold__' — the opening '__' is followed by a space so bold does not
        trigger, and the first '_' is followed by '_' so italic does not trigger
        either. The whole string is plain text."""
        tokens, _ = parse_line("__ bold__")
        assert TokenType.BOLD_START not in types(tokens)
        assert TokenType.ITALIC_START not in types(tokens)

    def test_italic_immediately_after_closing_bold(self):
        """'**bold***italic*' — a '*' that opens italic immediately after the
        closing '**' of a bold span is recognised correctly."""
        tokens, _ = parse_line("**bold***italic*")
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD in types(tokens)
        assert TokenType.BOLD_END in types(tokens)
        assert TokenType.ITALIC_START in types(tokens)
        assert TokenType.ITALIC in types(tokens)
        assert TokenType.ITALIC_END in types(tokens)
        italic = next(t for t in tokens if t.type == TokenType.ITALIC)
        assert italic.value == "italic"

    def test_unclosed_inline_code_each_character_emitted_once(self):
        """'`a`b`' — after the valid `a` span, the trailing lone '`' is plain text.
        The character 'b' must appear exactly once across all tokens."""
        tokens, _ = parse_line("`a`b`")
        assert TokenType.INLINE_CODE_START in types(tokens)
        assert TokenType.INLINE_CODE in types(tokens)
        all_text = "".join(t.value for t in tokens)
        assert all_text.count("b") == 1

    def test_unclosed_bold_each_character_emitted_once(self):
        """'**a** **unclosed' — text after the unclosed '**' appears exactly once."""
        tokens, _ = parse_line("**a** **unclosed")
        all_text = "".join(t.value for t in tokens)
        assert all_text.count("unclosed") == 1

    def test_unclosed_strikethrough_each_character_emitted_once(self):
        """'~~ok~~ ~~unclosed' — text after the unclosed '~~' appears exactly once."""
        tokens, _ = parse_line("~~ok~~ ~~unclosed")
        assert TokenType.STRIKETHROUGH_START in types(tokens)
        all_text = "".join(t.value for t in tokens)
        assert all_text.count("unclosed") == 1

    def test_incomplete_image_syntax_is_single_text_token(self):
        """'![alt text' with no '](' produces a single TEXT token for the whole string."""
        tokens, _ = parse_line("![alt text")
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.TEXT
        assert tokens[0].value == "![alt text"


# ---------------------------------------------------------------------------
# TestInlineFormattingInBlocks — inline formatting inside block elements
# ---------------------------------------------------------------------------

class TestInlineFormattingInBlocks:

    def test_bold_in_heading(self):
        """## **Title** — bold inside a heading is recognised."""
        tokens, _ = parse_line("## **Title**")
        assert TokenType.HEADING in types(tokens)
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD in types(tokens)
        assert TokenType.BOLD_END in types(tokens)

    def test_italic_in_blockquote(self):
        """> *quoted* — italic inside a blockquote is recognised."""
        tokens, _ = parse_line("> *quoted*")
        assert TokenType.BLOCKQUOTE in types(tokens)
        assert TokenType.ITALIC_START in types(tokens)
        assert TokenType.ITALIC in types(tokens)

    def test_inline_code_in_list_item(self):
        """'* item with `code`' — inline code inside a list item is recognised."""
        tokens, _ = parse_line("* item with `code`")
        assert TokenType.LIST_MARKER in types(tokens)
        assert TokenType.INLINE_CODE_START in types(tokens)
        assert TokenType.INLINE_CODE in types(tokens)

    def test_bold_in_blockquote(self):
        """> **bold** — bold inside a blockquote is recognised."""
        tokens, _ = parse_line("> **bold**")
        assert TokenType.BLOCKQUOTE in types(tokens)
        assert TokenType.BOLD_START in types(tokens)

    def test_link_in_heading(self):
        """## [link](url) — link inside a heading is recognised."""
        tokens, _ = parse_line("## [link](url)")
        assert TokenType.HEADING in types(tokens)
        assert TokenType.LINK_START in types(tokens)
        assert TokenType.LINK_URL in types(tokens)

    def test_strikethrough_in_list_item(self):
        """'- ~~struck~~' — strikethrough inside a list item is recognised."""
        tokens, _ = parse_line("- ~~struck~~")
        assert TokenType.LIST_MARKER in types(tokens)
        assert TokenType.STRIKETHROUGH_START in types(tokens)
        assert TokenType.STRIKETHROUGH in types(tokens)


# ---------------------------------------------------------------------------
# TestNesting — inline formatting nested inside other inline formatting
# ---------------------------------------------------------------------------

class TestNesting:

    def test_bold_containing_italic(self):
        """**hello *world* end** — italic nested inside bold."""
        tokens, _ = parse_line("**hello *world* end**")
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD_END in types(tokens)
        assert TokenType.ITALIC_START in types(tokens)
        assert TokenType.ITALIC in types(tokens)
        assert TokenType.ITALIC_END in types(tokens)

    def test_bold_containing_inline_code(self):
        """**hello `code` end** — inline code nested inside bold."""
        tokens, _ = parse_line("**hello `code` end**")
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.INLINE_CODE_START in types(tokens)
        assert TokenType.INLINE_CODE in types(tokens)

    def test_strikethrough_containing_bold(self):
        """~~**bold**~~ — bold nested inside strikethrough."""
        tokens, _ = parse_line("~~**bold**~~")
        assert TokenType.STRIKETHROUGH_START in types(tokens)
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD in types(tokens)
        assert TokenType.BOLD_END in types(tokens)
        assert TokenType.STRIKETHROUGH_END in types(tokens)

    def test_link_text_containing_bold(self):
        """[**bold**](url) — bold nested inside link text."""
        tokens, _ = parse_line("[**bold**](url)")
        assert TokenType.LINK_START in types(tokens)
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.BOLD in types(tokens)
        assert TokenType.LINK_URL in types(tokens)

    def test_image_alt_containing_italic(self):
        """![*alt*](url) — italic nested inside image alt text."""
        tokens, _ = parse_line("![*alt*](url)")
        assert TokenType.IMAGE_START in types(tokens)
        assert TokenType.ITALIC_START in types(tokens)
        assert TokenType.IMAGE_URL in types(tokens)

    def test_bold_containing_strikethrough(self):
        """**~~struck~~** — strikethrough nested inside bold."""
        tokens, _ = parse_line("**~~struck~~**")
        assert TokenType.BOLD_START in types(tokens)
        assert TokenType.STRIKETHROUGH_START in types(tokens)
        assert TokenType.STRIKETHROUGH in types(tokens)
        assert TokenType.BOLD_END in types(tokens)

    def test_italic_wrapping_bold(self):
        """'*hello **world** end*' — italic correctly wraps a bold span."""
        tokens, _ = parse_line("*hello **world** end*")
        t = types(tokens)
        assert TokenType.ITALIC_START in t
        assert TokenType.BOLD_START in t
        assert TokenType.BOLD in t
        assert TokenType.BOLD_END in t
        assert TokenType.ITALIC_END in t


# ---------------------------------------------------------------------------
# TestListParsing — multi-line list behaviour
# ---------------------------------------------------------------------------

class TestListParsing:

    def test_flat_list_multiple_items(self):
        """Multiple list items on consecutive lines all produce LIST_MARKER tokens."""
        lines = ["* one", "* two", "* three"]
        all_tokens, state = parse_lines(lines)
        for line_tokens in all_tokens:
            assert TokenType.LIST_MARKER in types(line_tokens)

    def test_ordered_list_multiple_items(self):
        """Multiple ordered list items produce LIST_MARKER tokens."""
        lines = ["1. first", "2. second", "3. third"]
        all_tokens, _ = parse_lines(lines)
        for line_tokens in all_tokens:
            assert TokenType.LIST_MARKER in types(line_tokens)

    def test_list_continuation_indented_text(self):
        """An indented continuation line inside a list is treated as LIST_MARKER."""
        lines = ["* item", "  continuation text"]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[1])

    def test_nested_list_pushes_indent_stack(self):
        """An indented sub-item pushes a new level onto the block stack."""
        lines = ["* outer", "  * inner"]
        all_tokens, state = parse_lines(lines)
        assert in_list(state) is True
        assert len(list_indent_stack(state)) == 2

    def test_de_indented_item_pops_stack(self):
        """Returning to outer indentation pops the inner level from the stack."""
        lines = ["* outer", "  * inner", "* outer again"]
        all_tokens, state = parse_lines(lines)
        assert in_list(state) is True
        assert len(list_indent_stack(state)) == 1

    def test_non_list_line_after_list_resets_state(self):
        """A non-indented, non-list line after a list resets list state."""
        lines = ["* item", "plain text"]
        all_tokens, state = parse_lines(lines)
        assert in_list(state) is False
        assert list_indent_stack(state) == []

    def test_heading_after_list_resets_state(self):
        """A heading line after a list resets list state."""
        lines = ["* item", "# Heading"]
        all_tokens, state = parse_lines(lines)
        assert in_list(state) is False

    def test_list_state_carries_across_lines(self):
        """List context remains active across consecutive list lines."""
        lines = ["* one", "* two"]
        all_tokens, state = parse_lines(lines)
        assert in_list(state) is True


# ---------------------------------------------------------------------------
# TestTokenPositions — .start field accuracy
# ---------------------------------------------------------------------------

class TestTokenPositions:

    def test_text_token_starts_at_zero(self):
        """A plain text line's TEXT token starts at position 0."""
        tokens, _ = parse_line("hello")
        assert tokens[0].start == 0

    def test_bold_start_position(self):
        """BOLD_START token has correct start position."""
        tokens, _ = parse_line("ab **bold**")
        bold_start = next(t for t in tokens if t.type == TokenType.BOLD_START)
        assert bold_start.start == 3

    def test_bold_content_position(self):
        """BOLD token (content) starts right after the opening **."""
        tokens, _ = parse_line("ab **bold**")
        bold = next(t for t in tokens if t.type == TokenType.BOLD)
        assert bold.start == 5

    def test_bold_end_position(self):
        """BOLD_END token starts at the closing **."""
        tokens, _ = parse_line("ab **bold**")
        bold_end = next(t for t in tokens if t.type == TokenType.BOLD_END)
        assert bold_end.start == 9

    def test_link_url_position(self):
        """LINK_URL token starts at the correct offset."""
        tokens, _ = parse_line("[x](url)")
        url = next(t for t in tokens if t.type == TokenType.LINK_URL)
        # "[x](" is 4 chars, so url starts at 4
        assert url.start == 4

    def test_strikethrough_positions(self):
        """STRIKETHROUGH_START, content, and END have correct positions."""
        tokens, _ = parse_line("aa ~~bb~~ cc")
        s_start = next(t for t in tokens if t.type == TokenType.STRIKETHROUGH_START)
        s_content = next(t for t in tokens if t.type == TokenType.STRIKETHROUGH)
        s_end = next(t for t in tokens if t.type == TokenType.STRIKETHROUGH_END)
        assert s_start.start == 3
        assert s_content.start == 5
        assert s_end.start == 7

    def test_heading_token_starts_at_hash(self):
        """HEADING token starts at position 0 (the # character)."""
        tokens, _ = parse_line("# Title")
        heading = next(t for t in tokens if t.type == TokenType.HEADING)
        assert heading.start == 0

    def test_fence_start_position_indented(self):
        """FENCE_START token start reflects indentation of the opening fence."""
        tokens, state = parse_line("    ```")
        fence = next(t for t in tokens if t.type == TokenType.FENCE_START)
        assert fence.start == 4
        assert state.fence_depth == 4

    def test_inline_code_positions(self):
        """INLINE_CODE_START, content, and END have correct positions."""
        tokens, _ = parse_line("x `code` y")
        cs = next(t for t in tokens if t.type == TokenType.INLINE_CODE_START)
        cc = next(t for t in tokens if t.type == TokenType.INLINE_CODE)
        ce = next(t for t in tokens if t.type == TokenType.INLINE_CODE_END)
        assert cs.start == 2
        assert cc.start == 3
        assert ce.start == 7


# ---------------------------------------------------------------------------
# TestMultiLine — state continuity across lines
# ---------------------------------------------------------------------------

class TestMultiLine:

    def test_plain_text_state_is_stateless(self):
        """Parsing plain text lines leaves no persistent block state."""
        lines = ["line one", "line two", "line three"]
        _, state = parse_lines(lines)
        assert state.in_fence_block is False
        assert in_list(state) is False

    def test_fence_state_persists_across_content_lines(self):
        """in_fence_block remains True for all lines inside a fence."""
        lines = ["```python", "line1", "line2"]
        all_tokens, state = parse_lines(lines)
        assert state.in_fence_block is True

    def test_inline_formatting_does_not_span_lines(self):
        """Inline formatting (bold, italic, etc.) does not carry state across lines.
        An unclosed ** on one line does not affect the next line."""
        lines = ["**unclosed", "normal line"]
        all_tokens, _ = parse_lines(lines)
        # Second line should be plain text
        assert types(all_tokens[1]) == [TokenType.TEXT]

    def test_empty_line_produces_no_tokens(self):
        """An empty string produces no tokens."""
        tokens, _ = parse_line("")
        assert tokens == []

    def test_whitespace_only_line_outside_fence(self):
        """A whitespace-only line outside a fence produces no meaningful tokens
        (whitespace is consumed by the lexer without emitting tokens)."""
        tokens, _ = parse_line("   ")
        assert TokenType.TEXT not in types(tokens)
        assert TokenType.LIST_MARKER not in types(tokens)

    def test_list_then_fence_then_text(self):
        """A list, followed by a fence block, followed by plain text all parse correctly."""
        lines = [
            "* list item",
            "```python",
            "x = 1",
            "```",
            "plain text",
        ]
        all_tokens, state = parse_lines(lines)
        assert TokenType.LIST_MARKER in types(all_tokens[0])
        assert TokenType.FENCE_START in types(all_tokens[1])
        assert TokenType.FENCE_END in types(all_tokens[3])
        assert TokenType.TEXT in types(all_tokens[4])
        assert state.in_fence_block is False
        assert in_list(state) is False

    def test_heading_resets_list_state(self):
        """A heading encountered while in a list resets list context."""
        lines = ["* item", "## Heading"]
        _, state = parse_lines(lines)
        assert in_list(state) is False
        assert list_indent_stack(state) == []
