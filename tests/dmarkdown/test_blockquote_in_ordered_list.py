"""
Tests for blockquotes nested inside ordered list items.

Covers the specific structure in test.md: an ordered list where each item
contains an indented blockquote as a continuation block.
"""
import pytest

from dmarkdown import MarkdownASTBuilder


@pytest.fixture
def ast_builder():
    """Fixture providing a markdown AST builder instance."""
    return MarkdownASTBuilder(no_underscores=False)


def test_blockquote_in_ordered_list_item(ast_builder):
    """Each ordered list item that contains a blockquote continuation block produces
    a list item with a paragraph child and a blockquote child."""
    markdown = """\
1. First item

   > Blockquote inside first ordered item.

2. Second item

   > Blockquote inside second ordered item.
"""

    doc = ast_builder.build_ast(markdown)

    # Top level: one ordered list
    assert len(doc.children) == 1
    list_node = doc.children[0]
    assert list_node.__class__.__name__ == "MarkdownASTOrderedListNode"

    # Two list items
    assert len(list_node.children) == 2

    first_item = list_node.children[0]
    assert first_item.__class__.__name__ == "MarkdownASTListItemNode"
    # Each item should have a paragraph and a blockquote — not two separate paragraphs
    assert len(first_item.children) == 2, (
        f"First list item has {len(first_item.children)} children "
        f"({[c.__class__.__name__ for c in first_item.children]}), expected 2 "
        f"(paragraph + blockquote)"
    )
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[1].__class__.__name__ == "MarkdownASTBlockquoteNode"

    first_blockquote = first_item.children[1]
    assert len(first_blockquote.children) == 1
    assert first_blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_blockquote.children[0].children[0].content == "Blockquote inside first ordered item."

    second_item = list_node.children[1]
    assert second_item.__class__.__name__ == "MarkdownASTListItemNode"
    assert len(second_item.children) == 2, (
        f"Second list item has {len(second_item.children)} children "
        f"({[c.__class__.__name__ for c in second_item.children]}), expected 2 "
        f"(paragraph + blockquote)"
    )
    assert second_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert second_item.children[1].__class__.__name__ == "MarkdownASTBlockquoteNode"

    second_blockquote = second_item.children[1]
    assert len(second_blockquote.children) == 1
    assert second_blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert second_blockquote.children[0].children[0].content == "Blockquote inside second ordered item."


def test_blockquote_text_not_treated_as_list_item_paragraph(ast_builder):
    """The blockquote content must appear inside a blockquote node, not as a bare
    paragraph directly inside the list item."""
    markdown = """\
1. First item

   > Blockquote inside first ordered item.

2. Second item

   > Blockquote inside second ordered item.
"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]

    for item in list_node.children:
        # Collect all paragraph text directly under the list item (not inside blockquotes)
        direct_paragraph_texts = []
        for child in item.children:
            if child.__class__.__name__ == "MarkdownASTParagraphNode":
                for inline in child.children:
                    if hasattr(inline, "content"):
                        direct_paragraph_texts.append(inline.content)

        for text in direct_paragraph_texts:
            assert "Blockquote" not in text, (
                f"Blockquote text '{text}' appeared as a bare paragraph inside the "
                f"list item instead of being wrapped in a blockquote node"
            )


def test_blockquote_paragraph_parent_is_blockquote_not_list_item(ast_builder):
    """Paragraphs inside a blockquote that is itself inside an ordered list item must
    have the blockquote as their direct parent, not the list item.  The renderer uses
    the parent type to decide whether to attach a paragraph to a Qt list object, so
    an incorrect parent would cause blockquote content to be rendered as a list item."""
    markdown = """\
1. First item

   > Blockquote inside first ordered item.

2. Second item

   > Blockquote inside second ordered item.
"""

    doc = ast_builder.build_ast(markdown)
    list_node = doc.children[0]

    for item in list_node.children:
        blockquote = None
        for child in item.children:
            if child.__class__.__name__ == "MarkdownASTBlockquoteNode":
                blockquote = child
                break

        assert blockquote is not None, "Expected a blockquote child in each list item"

        for bq_child in blockquote.children:
            assert bq_child.__class__.__name__ == "MarkdownASTParagraphNode"
            assert bq_child.parent is blockquote, (
                f"Blockquote paragraph's parent is {bq_child.parent.__class__.__name__}, "
                f"expected MarkdownASTBlockquoteNode. The renderer checks parent type to "
                f"determine list attachment, so a wrong parent causes misrendering."
            )
