import pytest

from doc_ir import (
    DocIRNode,
    DocIRVisitor,
    DocIRDocumentNode,
    DocIRHeadingNode,
    DocIRParagraphNode,
    DocIRBlockquoteNode,
    DocIRCodeBlockNode,
    DocIRUnorderedListNode,
    DocIROrderedListNode,
    DocIRListItemNode,
    DocIRTableNode,
    DocIRTableHeaderNode,
    DocIRTableBodyNode,
    DocIRTableRowNode,
    DocIRTableCellNode,
    DocIRHorizontalRuleNode,
    DocIRTextSpanNode,
    DocIRLinkNode,
    DocIRImageNode,
    DocIRLineBreakNode,
)


class TestDocIRNode:
    def test_initial_state(self):
        node = DocIRParagraphNode()
        assert node.parent is None
        assert node.children == []

    def test_add_child_sets_parent(self):
        parent = DocIRParagraphNode()
        child = DocIRTextSpanNode("hello")
        parent.add_child(child)
        assert child.parent is parent
        assert child in parent.children

    def test_add_child_returns_child(self):
        parent = DocIRParagraphNode()
        child = DocIRTextSpanNode("hello")
        result = parent.add_child(child)
        assert result is child

    def test_remove_child(self):
        parent = DocIRParagraphNode()
        child = DocIRTextSpanNode("hello")
        parent.add_child(child)
        parent.remove_child(child)
        assert child not in parent.children
        assert child.parent is None

    def test_remove_child_not_present_raises(self):
        parent = DocIRParagraphNode()
        child = DocIRTextSpanNode("hello")
        with pytest.raises(ValueError):
            parent.remove_child(child)

    def test_remove_children(self):
        parent = DocIRParagraphNode()
        child_a = DocIRTextSpanNode("a")
        child_b = DocIRTextSpanNode("b")
        parent.add_child(child_a)
        parent.add_child(child_b)
        parent.remove_children()
        assert parent.children == []
        assert child_a.parent is None
        assert child_b.parent is None


class TestDocIRVisitor:
    def test_dispatches_to_specific_method(self):
        visited = []

        class TestVisitor(DocIRVisitor):
            def visit_DocIRParagraphNode(self, node):  # pylint: disable=invalid-name
                visited.append(node)

        visitor = TestVisitor()
        node = DocIRParagraphNode()
        visitor.visit(node)
        assert node in visited

    def test_generic_visit_recurses(self):
        visited = []

        class TestVisitor(DocIRVisitor):
            def visit_DocIRTextSpanNode(self, node):  # pylint: disable=invalid-name
                visited.append(node.content)

        visitor = TestVisitor()
        para = DocIRParagraphNode()
        span_a = DocIRTextSpanNode("a")
        span_b = DocIRTextSpanNode("b")
        para.add_child(span_a)
        para.add_child(span_b)
        visitor.visit(para)
        assert visited == ["a", "b"]


class TestDocIRDocumentNode:
    def test_default_source_path(self):
        node = DocIRDocumentNode()
        assert node.source_path is None

    def test_source_path(self):
        node = DocIRDocumentNode("/path/to/file.md")
        assert node.source_path == "/path/to/file.md"


class TestDocIRHeadingNode:
    def test_level_stored(self):
        node = DocIRHeadingNode(2)
        assert node.level == 2

    def test_level_clamped_low(self):
        node = DocIRHeadingNode(0)
        assert node.level == 1

    def test_level_clamped_high(self):
        node = DocIRHeadingNode(9)
        assert node.level == 6


class TestDocIRCodeBlockNode:
    def test_attributes(self):
        node = DocIRCodeBlockNode("python", "print('hello')")
        assert node.language == "python"
        assert node.content == "print('hello')"

    def test_empty_language(self):
        node = DocIRCodeBlockNode("", "some code")
        assert node.language == ""


class TestDocIROrderedListNode:
    def test_default_start(self):
        node = DocIROrderedListNode()
        assert node.start == 1

    def test_custom_start(self):
        node = DocIROrderedListNode(start=3)
        assert node.start == 3


class TestDocIRTableCellNode:
    def test_defaults(self):
        node = DocIRTableCellNode()
        assert node.is_header is False
        assert node.alignment == "left"

    def test_header_cell(self):
        node = DocIRTableCellNode(is_header=True, alignment="center")
        assert node.is_header is True
        assert node.alignment == "center"


class TestDocIRTextSpanNode:
    def test_plain_text(self):
        node = DocIRTextSpanNode("hello")
        assert node.content == "hello"
        assert node.bold is False
        assert node.italic is False
        assert node.strikethrough is False
        assert node.code is False

    def test_all_flags(self):
        node = DocIRTextSpanNode("x", bold=True, italic=True, strikethrough=True, code=True)
        assert node.bold is True
        assert node.italic is True
        assert node.strikethrough is True
        assert node.code is True

    def test_combined_bold_italic(self):
        node = DocIRTextSpanNode("x", bold=True, italic=True)
        assert node.bold is True
        assert node.italic is True
        assert node.strikethrough is False
        assert node.code is False


class TestDocIRLinkNode:
    def test_url_only(self):
        node = DocIRLinkNode("https://example.com")
        assert node.url == "https://example.com"
        assert node.title is None

    def test_with_title(self):
        node = DocIRLinkNode("https://example.com", title="Example")
        assert node.title == "Example"

    def test_children_are_display_text(self):
        node = DocIRLinkNode("https://example.com")
        span = DocIRTextSpanNode("click here")
        node.add_child(span)
        assert span in node.children


class TestDocIRImageNode:
    def test_defaults(self):
        node = DocIRImageNode("image.png")
        assert node.url == "image.png"
        assert node.alt_text == ""
        assert node.title is None

    def test_full(self):
        node = DocIRImageNode("image.png", alt_text="A photo", title="Photo")
        assert node.alt_text == "A photo"
        assert node.title == "Photo"
