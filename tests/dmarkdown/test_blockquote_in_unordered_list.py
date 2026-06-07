"""
Tests for the structure described in test.md: a blockquote nested inside an
unordered list item, where the blockquote itself contains a nested list and a
further nested blockquote.

These tests check whether misrendering of test.md is a parser bug (wrong AST)
or a rendering bug (correct AST but wrong visual output).
"""
import pytest

from dmarkdown import MarkdownASTBuilder


TEST_MD = """\
- Item one

- Item two with a blockquote:

  > This blockquote is nested inside a list item. It should have its bar
  > indented to match the list content.
  >
  > - Then we have a list
  > - inside
  > - and a third item
  > - > the fourth is a blockquote
"""


@pytest.fixture
def ast_builder():
    """Fixture providing a markdown AST builder instance."""
    return MarkdownASTBuilder(no_underscores=False)


@pytest.fixture
def doc(ast_builder):
    """Parsed AST for the test.md content."""
    return ast_builder.build_ast(TEST_MD)


def test_top_level_is_single_unordered_list(doc):
    """The document contains exactly one top-level unordered list."""
    assert len(doc.children) == 1
    assert doc.children[0].__class__.__name__ == "MarkdownASTUnorderedListNode"


def test_list_has_two_items(doc):
    """The unordered list contains exactly two list items."""
    list_node = doc.children[0]
    assert len(list_node.children) == 2
    for child in list_node.children:
        assert child.__class__.__name__ == "MarkdownASTListItemNode"


def test_first_item_has_single_paragraph(doc):
    """The first list item contains a single paragraph with the text 'Item one'."""
    first_item = doc.children[0].children[0]
    assert len(first_item.children) == 1
    assert first_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert first_item.children[0].children[0].content == "Item one"


def test_second_item_has_paragraph_and_blockquote(doc):
    """The second list item contains a paragraph followed by a blockquote — not two
    paragraphs.  The blockquote content must not be promoted to a bare paragraph."""
    second_item = doc.children[0].children[1]
    assert len(second_item.children) == 2, (
        f"Second list item has {len(second_item.children)} children "
        f"({[c.__class__.__name__ for c in second_item.children]}), expected 2 "
        f"(paragraph + blockquote)"
    )
    assert second_item.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert second_item.children[1].__class__.__name__ == "MarkdownASTBlockquoteNode"


def test_second_item_paragraph_text(doc):
    """The paragraph in the second list item reads 'Item two with a blockquote:'."""
    second_item = doc.children[0].children[1]
    para = second_item.children[0]
    assert para.__class__.__name__ == "MarkdownASTParagraphNode"
    assert para.children[0].content == "Item two with a blockquote:"


def test_blockquote_has_paragraph_and_list(doc):
    """The blockquote inside the second list item contains a paragraph (the two
    prose lines) followed by an unordered list (the four '> -' items)."""
    blockquote = doc.children[0].children[1].children[1]
    assert len(blockquote.children) == 2, (
        f"Blockquote has {len(blockquote.children)} children "
        f"({[c.__class__.__name__ for c in blockquote.children]}), expected 2 "
        f"(paragraph + unordered_list)"
    )
    assert blockquote.children[0].__class__.__name__ == "MarkdownASTParagraphNode"
    assert blockquote.children[1].__class__.__name__ == "MarkdownASTUnorderedListNode"


def test_blockquote_paragraph_text(doc):
    """The paragraph inside the blockquote carries the two prose lines joined as
    inline text."""
    blockquote = doc.children[0].children[1].children[1]
    para = blockquote.children[0]
    full_text = "".join(
        child.content for child in para.children if hasattr(child, "content")
    )
    assert "This blockquote is nested inside a list item" in full_text
    assert "indented to match the list content" in full_text


def test_blockquote_paragraph_parent_is_blockquote(doc):
    """The paragraph inside the blockquote must have the blockquote as its direct
    parent.  If the parent is the list item instead, the renderer will attach the
    paragraph to the Qt list object and misrender it."""
    blockquote = doc.children[0].children[1].children[1]
    para = blockquote.children[0]
    assert para.parent is blockquote, (
        f"Blockquote paragraph's parent is {para.parent.__class__.__name__}, "
        f"expected MarkdownASTBlockquoteNode"
    )


def test_nested_list_has_four_items(doc):
    """The unordered list inside the blockquote has exactly four items."""
    nested_list = doc.children[0].children[1].children[1].children[1]
    assert len(nested_list.children) == 4, (
        f"Nested list has {len(nested_list.children)} items, expected 4"
    )
    for child in nested_list.children:
        assert child.__class__.__name__ == "MarkdownASTListItemNode"


def test_nested_list_first_three_items_are_plain_text(doc):
    """The first three items of the nested list are plain text paragraphs."""
    nested_list = doc.children[0].children[1].children[1].children[1]
    expected_texts = ["Then we have a list", "inside", "and a third item"]
    for i, expected in enumerate(expected_texts):
        item = nested_list.children[i]
        assert len(item.children) >= 1
        para = item.children[0]
        assert para.__class__.__name__ == "MarkdownASTParagraphNode"
        assert para.children[0].content == expected, (
            f"Item {i + 1}: expected text '{expected}', "
            f"got '{para.children[0].content}'"
        )


def test_nested_list_fourth_item_contains_blockquote(doc):
    """The fourth item of the nested list ('> the fourth is a blockquote') contains
    a blockquote node, not a bare paragraph with '>' in its text."""
    nested_list = doc.children[0].children[1].children[1].children[1]
    fourth_item = nested_list.children[3]

    blockquote_children = [
        c for c in fourth_item.children
        if c.__class__.__name__ == "MarkdownASTBlockquoteNode"
    ]
    assert len(blockquote_children) == 1, (
        f"Fourth nested list item has {len(fourth_item.children)} children "
        f"({[c.__class__.__name__ for c in fourth_item.children]}); "
        f"expected exactly one MarkdownASTBlockquoteNode"
    )

    inner_blockquote = blockquote_children[0]
    assert len(inner_blockquote.children) >= 1
    inner_para = inner_blockquote.children[0]
    assert inner_para.__class__.__name__ == "MarkdownASTParagraphNode"
    assert inner_para.children[0].content == "the fourth is a blockquote"
