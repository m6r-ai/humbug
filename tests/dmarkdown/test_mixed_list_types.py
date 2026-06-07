"""
Tests for correct parsing of adjacent lists of different types.
"""
import pytest

from dmarkdown import (
    MarkdownASTBuilder,
    MarkdownASTOrderedListNode,
    MarkdownASTUnorderedListNode,
)


@pytest.fixture
def builder() -> MarkdownASTBuilder:
    """Fixture providing a markdown AST builder instance."""
    return MarkdownASTBuilder(no_underscores=False)


def test_unordered_then_ordered_with_blank_line(builder: MarkdownASTBuilder) -> None:
    """An unordered list followed by a blank line and then an ordered list produces two separate top-level list nodes."""
    markdown = """\
- Unordered item one
- Unordered item two
- Unordered item three

1. Ordered item one
2. Ordered item two
3. Ordered item three
"""
    doc = builder.build_ast(markdown)
    assert len(doc.children) == 2, (
        f"Expected 2 top-level nodes (one unordered list, one ordered list), "
        f"got {len(doc.children)}: {[c.__class__.__name__ for c in doc.children]}"
    )
    assert isinstance(doc.children[0], MarkdownASTUnorderedListNode), (
        f"Expected first child to be MarkdownASTUnorderedListNode, "
        f"got {doc.children[0].__class__.__name__}"
    )
    assert len(doc.children[0].children) == 3, (
        f"Expected 3 items in unordered list, got {len(doc.children[0].children)}"
    )
    assert isinstance(doc.children[1], MarkdownASTOrderedListNode), (
        f"Expected second child to be MarkdownASTOrderedListNode, "
        f"got {doc.children[1].__class__.__name__}"
    )
    assert len(doc.children[1].children) == 3, (
        f"Expected 3 items in ordered list, got {len(doc.children[1].children)}"
    )
    assert doc.children[0].tight, "Unordered list should be tight"
    assert doc.children[1].tight, "Ordered list should be tight"


def test_ordered_then_unordered_without_blank_line(builder: MarkdownASTBuilder) -> None:
    """An ordered list immediately followed (no blank line) by an unordered list produces two separate top-level list nodes."""
    markdown = """\
1. Ordered item one
2. Ordered item two
- Unordered item one
- Unordered item two
"""
    doc = builder.build_ast(markdown)
    assert len(doc.children) == 2, (
        f"Expected 2 top-level nodes (one ordered list, one unordered list), "
        f"got {len(doc.children)}: {[c.__class__.__name__ for c in doc.children]}"
    )
    assert isinstance(doc.children[0], MarkdownASTOrderedListNode), (
        f"Expected first child to be MarkdownASTOrderedListNode, "
        f"got {doc.children[0].__class__.__name__}"
    )
    assert len(doc.children[0].children) == 2, (
        f"Expected 2 items in ordered list, got {len(doc.children[0].children)}"
    )
    assert isinstance(doc.children[1], MarkdownASTUnorderedListNode), (
        f"Expected second child to be MarkdownASTUnorderedListNode, "
        f"got {doc.children[1].__class__.__name__}"
    )
    assert len(doc.children[1].children) == 2, (
        f"Expected 2 items in unordered list, got {len(doc.children[1].children)}"
    )
    assert doc.children[0].tight, "Ordered list should be tight"
    assert doc.children[1].tight, "Unordered list should be tight"
