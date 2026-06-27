from html import unescape as _html_unescape
import re
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Dict, List, Tuple

from dhtml.html_ast_node import (
    HtmlASTCommentNode,
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTTextNode,
)
from dhtml.html_errors import HtmlParseError


# Void elements — those that never have a closing tag.
_VOID_ELEMENTS = frozenset({
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "link", "meta", "param", "source", "track", "wbr",
})

# Implied-close rules.
#
# When an open tag is encountered, any of the tags listed as its "closers"
# that are currently open on the stack are implicitly closed first.
# This covers the most common HTML5 optional-close rules.
_IMPLIED_CLOSE: Dict[str, frozenset] = {
    "p":   frozenset({
        "p",
    }),
    "li":  frozenset({
        "li",
    }),
    "dt":  frozenset({
        "dt", "dd",
    }),
    "dd":  frozenset({
        "dt", "dd",
    }),
    "tr":  frozenset({
        "tr",
    }),
    "th":  frozenset({
        "th", "td",
    }),
    "td":  frozenset({
        "th", "td",
    }),
    "colgroup": frozenset({
        "colgroup",
    }),
    "option": frozenset({
        "option",
    }),
    "optgroup": frozenset({
        "option", "optgroup",
    }),
    "rt":  frozenset({
        "rt", "rp",
    }),
    "rp":  frozenset({
        "rt", "rp",
    }),
    "caption": frozenset({
        "caption",
    }),
    "thead": frozenset({
        "tbody", "tfoot",
    }),
    "tbody": frozenset({
        "tbody", "tfoot",
    }),
    "tfoot": frozenset({
        "tbody",
    }),
}

# Raw-text elements — content is treated as opaque text until the matching
# close tag, regardless of what appears inside.
_RAW_TEXT_ELEMENTS = frozenset({"script", "style"})


class _TokenType(Enum):
    DOCTYPE = auto()
    OPEN_TAG = auto()          # <tag attrs>
    CLOSE_TAG = auto()         # </tag>
    SELF_CLOSING_TAG = auto()  # <tag attrs />
    TEXT = auto()
    COMMENT = auto()
    RAW_TEXT = auto()          # content of script/style blocks


@dataclass
class _Token:
    type: _TokenType
    tag_name: str = ""
    attributes: Dict[str, str] = field(default_factory=dict)
    content: str = ""


# Pre-compiled patterns used by the lexer.
_RE_TAG_NAME = re.compile(r"[a-zA-Z][a-zA-Z0-9\-_:.]*")
_RE_ATTR_NAME = re.compile(r"[^\s\"'>/=]+")
_RE_WHITESPACE = re.compile(r"\s+")


class _Lexer:
    """Tokenises an HTML string into a flat sequence of _Token objects."""

    def __init__(self, source: str) -> None:
        """
        Initialise the lexer with the full HTML source.

        Args:
            source: The complete HTML document or fragment as a string.
        """
        self._src = source
        self._pos = 0
        self._len = len(source)

    def tokenise(self) -> List[_Token]:
        """Tokenise the entire source and return the token list."""
        tokens: List[_Token] = []
        while self._pos < self._len:
            if self._src[self._pos] != "<":
                tokens.append(self._read_text())
                continue

            # Peek ahead to determine what kind of tag this is.
            rest = self._src[self._pos:]

            if rest.startswith("<!--"):
                tokens.append(self._read_comment())
                continue

            if re.match(r"<![Dd][Oo][Cc][Tt][Yy][Pp][Ee]", rest):
                tokens.append(self._read_doctype())
                continue

            if rest.startswith("</"):
                tokens.append(self._read_close_tag())
                continue

            if rest.startswith("<") and self._pos + 1 < self._len and (
                self._src[self._pos + 1].isalpha() or self._src[self._pos + 1] == "_"
            ):
                tok = self._read_open_tag()
                tokens.append(tok)
                if tok.type == _TokenType.OPEN_TAG and tok.tag_name in _RAW_TEXT_ELEMENTS:
                    raw = self._read_raw_text(tok.tag_name)
                    if raw.content:
                        tokens.append(raw)

                continue

            # A bare '<' that isn't a tag — treat as text.
            tokens.append(_Token(type=_TokenType.TEXT, content="<"))
            self._pos += 1

        return tokens

    def _read_text(self) -> _Token:
        """Read a run of text up to the next '<'."""
        start = self._pos
        while self._pos < self._len and self._src[self._pos] != "<":
            self._pos += 1

        raw = self._src[start:self._pos]
        return _Token(type=_TokenType.TEXT, content=_decode_entities(raw))

    def _read_comment(self) -> _Token:
        """Read an HTML comment (<!-- ... -->)."""
        self._pos += 4  # skip <!--
        start = self._pos
        end = self._src.find("-->", self._pos)
        if end == -1:
            content = self._src[start:]
            self._pos = self._len

        else:
            content = self._src[start:end]
            self._pos = end + 3

        return _Token(type=_TokenType.COMMENT, content=content)

    def _read_doctype(self) -> _Token:
        """Read a DOCTYPE declaration."""
        end = self._src.find(">", self._pos)
        if end == -1:
            self._pos = self._len

        else:
            self._pos = end + 1

        return _Token(type=_TokenType.DOCTYPE)

    def _read_open_tag(self) -> _Token:
        """Read an open or self-closing tag including its attributes."""
        self._pos += 1  # skip '<'
        tag_name = self._read_tag_name()

        if not tag_name:
            # Not a valid tag name — treat the '<' as literal text and leave
            # the position just after it so the main loop re-reads the rest.
            end = self._src.find(">", self._pos)
            self._pos = end + 1 if end != -1 else self._len
            return _Token(type=_TokenType.TEXT, content="<")

        attrs = self._read_attributes()
        self._skip_whitespace()

        self_closing = False
        if self._pos < self._len and self._src[self._pos] == "/":
            self_closing = True
            self._pos += 1

        if self._pos < self._len and self._src[self._pos] == ">":
            self._pos += 1

        tag_name_lower = tag_name.lower()
        tok_type = (
            _TokenType.SELF_CLOSING_TAG
            if (self_closing or tag_name_lower in _VOID_ELEMENTS)
            else _TokenType.OPEN_TAG
        )
        return _Token(type=tok_type, tag_name=tag_name_lower, attributes=attrs)

    def _read_close_tag(self) -> _Token:
        """Read a closing tag (</tag>)."""
        self._pos += 2  # skip '</'
        tag_name = self._read_tag_name()
        end = self._src.find(">", self._pos)
        if end == -1:
            self._pos = self._len

        else:
            self._pos = end + 1

        return _Token(type=_TokenType.CLOSE_TAG, tag_name=tag_name.lower())

    def _read_raw_text(self, tag_name: str) -> _Token:
        """
        Read raw text content until the matching close tag.

        Args:
            tag_name: The tag whose close tag terminates the raw block.
        """
        close_pattern = f"</{tag_name}"
        end = self._src.lower().find(close_pattern, self._pos)
        if end == -1:
            content = self._src[self._pos:]
            self._pos = self._len

        else:
            content = self._src[self._pos:end]
            self._pos = end  # leave the close tag to be read normally

        return _Token(type=_TokenType.RAW_TEXT, content=content)

    def _read_tag_name(self) -> str:
        """Read and return a tag name from the current position."""
        m = _RE_TAG_NAME.match(self._src, self._pos)
        if not m:
            return ""

        self._pos = m.end()
        return m.group()

    def _read_attributes(self) -> Dict[str, str]:
        """Read all attributes from the current position inside a tag."""
        attrs: Dict[str, str] = {}
        while self._pos < self._len:
            self._skip_whitespace()
            if self._pos >= self._len or self._src[self._pos] in (">", "/"):
                break

            name = self._read_attr_name()
            if not name:
                # Unrecognised character — skip it to avoid infinite loop.
                self._pos += 1
                continue

            self._skip_whitespace()
            if self._pos < self._len and self._src[self._pos] == "=":
                self._pos += 1  # skip '='
                self._skip_whitespace()
                value = self._read_attr_value()

            else:
                value = ""

            attrs[name.lower()] = value

        return attrs

    def _read_attr_name(self) -> str:
        """Read an attribute name."""
        m = _RE_ATTR_NAME.match(self._src, self._pos)
        if not m:
            return ""

        self._pos = m.end()
        return m.group()

    def _read_attr_value(self) -> str:
        """Read an attribute value (quoted or unquoted)."""
        if self._pos >= self._len:
            return ""

        ch = self._src[self._pos]
        if ch in ('"', "'"):
            return self._read_quoted_value(ch)

        return self._read_unquoted_value()

    def _read_quoted_value(self, quote: str) -> str:
        """Read a quoted attribute value."""
        self._pos += 1  # skip opening quote
        start = self._pos
        while self._pos < self._len and self._src[self._pos] != quote:
            self._pos += 1

        value = self._src[start:self._pos]
        if self._pos < self._len:
            self._pos += 1  # skip closing quote

        return _decode_entities(value)

    def _read_unquoted_value(self) -> str:
        """Read an unquoted attribute value."""
        start = self._pos
        while self._pos < self._len and self._src[self._pos] not in (" ", "\t", "\n", "\r", ">", "/"):
            self._pos += 1

        return _decode_entities(self._src[start:self._pos])

    def _skip_whitespace(self) -> None:
        """Advance past any whitespace at the current position."""
        m = _RE_WHITESPACE.match(self._src, self._pos)
        if m:
            self._pos = m.end()


def _decode_entities(text: str) -> str:
    """
    Decode HTML entities in a string using the stdlib html module.

    Args:
        text: Raw text that may contain HTML entities.

    Returns:
        Text with all entities decoded.
    """
    return _html_unescape(text)


class HtmlASTBuilder:
    """
    Builds an HTML AST from a source string.

    Usage::

        builder = HtmlASTBuilder()
        document = builder.build(html_source)
    """

    def build(self, source: str, source_path: str | None = None) -> HtmlASTDocumentNode:
        """
        Parse an HTML string and return the root document node.

        Args:
            source: The HTML source text.
            source_path: Optional path to the source file, stored on the document node.

        Returns:
            The root HtmlASTDocumentNode with the fully constructed DOM tree.

        Raises:
            HtmlParseError: If the source cannot be tokenised.
        """
        try:
            tokens = _Lexer(source).tokenise()
        except Exception as exc:
            raise HtmlParseError(f"Failed to tokenise HTML: {exc}") from exc

        document = HtmlASTDocumentNode(source_path=source_path)
        # The stack holds (element, stop_tags) pairs.  stop_tags is the set of
        # tag names that will cause this element to be implicitly closed.
        stack: List[Tuple[HtmlASTElementNode, frozenset]] = []

        def current_element() -> HtmlASTElementNode | None:
            return stack[-1][0] if stack else None

        def append_node(node: HtmlASTElementNode | HtmlASTTextNode | HtmlASTCommentNode) -> None:
            parent = current_element()
            if parent is not None:
                parent.add_child(node)

            else:
                document.add_child(node)

        for token in tokens:
            if token.type == _TokenType.DOCTYPE:
                document.has_doctype = True
                continue

            if token.type == _TokenType.COMMENT:
                append_node(HtmlASTCommentNode(content=token.content))
                continue

            if token.type == _TokenType.TEXT:
                text = token.content
                # Suppress whitespace-only text nodes at the document root.
                if text.strip() or stack:
                    append_node(HtmlASTTextNode(content=text))
                continue

            if token.type == _TokenType.RAW_TEXT:
                # Script/style content is attached as a text child but the
                # consumer (extractor, document_ir converter) will skip it.
                append_node(HtmlASTTextNode(content=token.content))
                continue

            if token.type == _TokenType.SELF_CLOSING_TAG:
                element = HtmlASTElementNode(
                    tag_name=token.tag_name,
                    attributes=token.attributes,
                )
                append_node(element)
                continue

            if token.type == _TokenType.OPEN_TAG:
                tag = token.tag_name

                # Apply implied-close rules: pop any open tags that this tag
                # implicitly closes before we push the new element.
                closers = _IMPLIED_CLOSE.get(tag, frozenset())
                while stack and stack[-1][0].tag_name in closers:
                    stack.pop()

                element = HtmlASTElementNode(
                    tag_name=tag,
                    attributes=token.attributes,
                )
                append_node(element)
                stack.append((element, frozenset()))
                continue

            if token.type == _TokenType.CLOSE_TAG:
                tag = token.tag_name

                # Walk the stack from top to find the matching open tag.
                # This handles misnested tags by discarding intervening elements.
                for i in range(len(stack) - 1, -1, -1):
                    if stack[i][0].tag_name == tag:
                        stack = stack[:i]
                        break

                # If no matching open tag was found, the close tag is ignored.

        return document


def parse_html(source: str, source_path: str | None = None) -> HtmlASTDocumentNode:
    """
    Parse an HTML string and return the root document node.

    Args:
        source: The HTML source text.
        source_path: Optional path to the source file.

    Returns:
        The root HtmlASTDocumentNode.

    Raises:
        HtmlParseError: If the source cannot be tokenised.
    """
    return HtmlASTBuilder().build(source, source_path)
