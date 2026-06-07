"""
Tests for the exact content of test.md: a tight unordered list (no blank lines
between items) where one item contains an indented blockquote, and a loose
ordered list where every item contains an indented blockquote.

These tests check whether misrendering of test.md is a parser bug (wrong AST)
or a rendering bug (correct AST but wrong visual output).
"""
import pytest

from dmarkdown import MarkdownASTBuilder


TIGHT_UNORDERED_MD = """\
- Item one
- Item two with a blockquote:
  > This blockquote is nested inside a list item. It should have its bar
  > indented to match the list content.
- Item three
"""

ORDERED_WITH_BLOCKQUOTES_MD = """\
1. First item
   > Blockquote inside first ordered item.
2. Second item
   > Blockquote inside second ordered item.
"""


@pytest.fixture
def builder():
    """Fixture providing a markdown AST builder instance."""
    return MarkdownASTBuilder(no_underscores=False)


@pytest.fixture
def tight_doc(builder):
    """Parsed AST for the tight unordered list with an embedded blockquote."""
    return builder.build_ast(TIGHT_UNORDERED_MD)


@pytest.fixture
def ordered_doc(builder):
    """Parsed AST for the loose ordered list with embedded blockquotes."""
    return builder.build_ast(ORDERED_WITH_BLOCKQUOTES_MD)


# ---------------------------------------------------------------------------
# Tight unordered list
# ---------------------------------------------------------------------------

class TestTightUnorderedList:
    """The tight unordered list from test.md (no blank lines between items)."""

    def test_top_level_is_single_unordered_list(self, tight_doc):
        """The document contains exactly one top-level unordered list."""
        assert len(tight_doc.children) == 1
        assert tight_doc.children[0].__class__.__name__ == "MarkdownASTUnorderedListNode"

    def test_list_has_three_items(self, tight_doc):
        """The tight unordered list contains exactly three list items."""
        list_node = tight_doc.children[0]
        assert len(list_node.children) == 3, (
            f"Expected 3 list items, got {len(list_node.children)}: "
            f"{[c.__class__.__name__ for c in list_node.children]}"
        )
        for child in list_node.children:
            assert child.__class__.__name__ == "MarkdownASTListItemNode"

    def test_first_item_text(self, tight_doc):
        """The first list item contains the text 'Item one'."""
        first_item = tight_doc.children[0].children[0]
        assert len(first_item.children) >= 1
        para = first_item.children[0]
        assert para.__class__.__name__ == "MarkdownASTParagraphNode"
        assert para.children[0].content == "Item one"

    def test_third_item_text(self, tight_doc):
        """The third list item contains the text 'Item three'."""
        third_item = tight_doc.children[0].children[2]
        assert len(third_item.children) >= 1
        para = third_item.children[0]
        assert para.__class__.__name__ == "MarkdownASTParagraphNode"
        assert para.children[0].content == "Item three"

    def test_second_item_has_paragraph_and_blockquote(self, tight_doc):
        """The second list item contains a paragraph followed by a blockquote.

        The blockquote content must not be promoted to a bare paragraph directly
        inside the list item.
        """
        second_item = tight_doc.children[0].children[1]
        child_types = [c.__class__.__name__ for c in second_item.children]
        assert len(second_item.children) == 2, (
            f"Second list item has {len(second_item.children)} children "
            f"({child_types}), expected 2 (paragraph + blockquote)"
        )
        assert second_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
        assert second_item.children[1].__class__.__name__ == "MarkdownASTBlockquoteNode"

    def test_second_item_paragraph_text(self, tight_doc):
        """The paragraph in the second list item reads 'Item two with a blockquote:'."""
        second_item = tight_doc.children[0].children[1]
        para = second_item.children[0]
        assert para.__class__.__name__ == "MarkdownASTParagraphNode"
        assert para.children[0].content == "Item two with a blockquote:"

    def test_blockquote_contains_paragraph(self, tight_doc):
        """The blockquote inside the second list item contains a paragraph with the
        expected prose text."""
        blockquote = tight_doc.children[0].children[1].children[1]
        assert len(blockquote.children) >= 1
        para = blockquote.children[0]
        assert para.__class__.__name__ == "MarkdownASTParagraphNode"
        full_text = "".join(
            child.content for child in para.children if hasattr(child, "content")
        )
        assert "This blockquote is nested inside a list item" in full_text
        assert "indented to match the list content" in full_text

    def test_blockquote_paragraph_parent_is_blockquote(self, tight_doc):
        """The paragraph inside the blockquote must have the blockquote as its
        direct parent, not the list item.

        The renderer uses the parent type to decide whether to attach a paragraph
        to a Qt list object, so an incorrect parent causes misrendering.
        """
        blockquote = tight_doc.children[0].children[1].children[1]
        para = blockquote.children[0]
        assert para.parent is blockquote, (
            f"Blockquote paragraph's parent is {para.parent.__class__.__name__}, "
            f"expected MarkdownASTBlockquoteNode"
        )


# ---------------------------------------------------------------------------
# Ordered list with blockquotes (tight — no blank lines between marker and quote)
# ---------------------------------------------------------------------------

class TestTightOrderedListWithBlockquotes:
    """The ordered list from test.md where blockquotes immediately follow each
    list marker with no separating blank line."""

    def test_top_level_is_single_ordered_list(self, ordered_doc):
        """The document contains exactly one top-level ordered list."""
        assert len(ordered_doc.children) == 1
        assert ordered_doc.children[0].__class__.__name__ == "MarkdownASTOrderedListNode"

    def test_list_has_two_items(self, ordered_doc):
        """The ordered list contains exactly two list items."""
        list_node = ordered_doc.children[0]
        assert len(list_node.children) == 2, (
            f"Expected 2 list items, got {len(list_node.children)}: "
            f"{[c.__class__.__name__ for c in list_node.children]}"
        )
        for child in list_node.children:
            assert child.__class__.__name__ == "MarkdownASTListItemNode"

    def test_each_item_has_paragraph_and_blockquote(self, ordered_doc):
        """Each ordered list item contains a paragraph followed by a blockquote.

        The blockquote content must not be promoted to a bare paragraph directly
        inside the list item.
        """
        list_node = ordered_doc.children[0]
        for i, item in enumerate(list_node.children):
            child_types = [c.__class__.__name__ for c in item.children]
            assert len(item.children) == 2, (
                f"Item {i + 1} has {len(item.children)} children ({child_types}), "
                f"expected 2 (paragraph + blockquote)"
            )
            assert item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
            assert item.children[1].__class__.__name__ == "MarkdownASTBlockquoteNode"

    def test_first_item_paragraph_text(self, ordered_doc):
        """The paragraph in the first ordered list item reads 'First item'."""
        first_item = ordered_doc.children[0].children[0]
        para = first_item.children[0]
        assert para.children[0].content == "First item"

    def test_second_item_paragraph_text(self, ordered_doc):
        """The paragraph in the second ordered list item reads 'Second item'."""
        second_item = ordered_doc.children[0].children[1]
        para = second_item.children[0]
        assert para.children[0].content == "Second item"

    def test_first_item_blockquote_text(self, ordered_doc):
        """The blockquote in the first ordered list item contains the expected text."""
        first_item = ordered_doc.children[0].children[0]
        blockquote = first_item.children[1]
        assert len(blockquote.children) >= 1
        para = blockquote.children[0]
        assert para.__class__.__name__ == "MarkdownASTParagraphNode"
        assert para.children[0].content == "Blockquote inside first ordered item."

    def test_second_item_blockquote_text(self, ordered_doc):
        """The blockquote in the second ordered list item contains the expected text."""
        second_item = ordered_doc.children[0].children[1]
        blockquote = second_item.children[1]
        assert len(blockquote.children) >= 1
        para = blockquote.children[0]
        assert para.__class__.__name__ == "MarkdownASTParagraphNode"
        assert para.children[0].content == "Blockquote inside second ordered item."

    def test_blockquote_paragraphs_parent_is_blockquote(self, ordered_doc):
        """Paragraphs inside each blockquote must have the blockquote as their
        direct parent, not the list item.

        The renderer uses the parent type to decide whether to attach a paragraph
        to a Qt list object, so an incorrect parent causes misrendering.
        """
        list_node = ordered_doc.children[0]
        for i, item in enumerate(list_node.children):
            blockquote = item.children[1]
            for bq_child in blockquote.children:
                assert bq_child.__class__.__name__ == "MarkdownASTParagraphNode"
                assert bq_child.parent is blockquote, (
                    f"Item {i + 1}: blockquote paragraph's parent is "
                    f"{bq_child.parent.__class__.__name__}, "
                    f"expected MarkdownASTBlockquoteNode"
                )
