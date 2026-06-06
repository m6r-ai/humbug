import pytest

from document_ir import (
    DocumentIRNode,
    DocumentIRVisitor,
    DocumentIRDocumentNode,
    DocumentIRHeadingNode,
    DocumentIRParagraphNode,
    DocumentIRBlockquoteNode,
    DocumentIRCodeBlockNode,
    DocumentIRUnorderedListNode,
    DocumentIROrderedListNode,
    DocumentIRListItemNode,
    DocumentIRTableNode,
    DocumentIRTableHeaderNode,
    DocumentIRTableBodyNode,
    DocumentIRTableRowNode,
    DocumentIRTableCellNode,
    DocumentIRHorizontalRuleNode,
    DocumentIRTextSpanNode,
    DocumentIRLinkNode,
    DocumentIRImageNode,
    DocumentIRLineBreakNode,
)


class TestDocumentIRNode:
    def test_initial_state(self):
        node = DocumentIRParagraphNode()
        assert node.parent is None
        assert node.children == []

    def test_add_child_sets_parent(self):
        parent = DocumentIRParagraphNode()
        child = DocumentIRTextSpanNode("hello")
        parent.add_child(child)
        assert child.parent is parent
        assert child in parent.children

    def test_add_child_returns_child(self):
        parent = DocumentIRParagraphNode()
        child = DocumentIRTextSpanNode("hello")
        result = parent.add_child(child)
        assert result is child

    def test_remove_child(self):
        parent = DocumentIRParagraphNode()
        child = DocumentIRTextSpanNode("hello")
        parent.add_child(child)
        parent.remove_child(child)
        assert child not in parent.children
        assert child.parent is None

    def test_remove_child_not_present_raises(self):
        parent = DocumentIRParagraphNode()
        child = DocumentIRTextSpanNode("hello")
        with pytest.raises(ValueError):
            parent.remove_child(child)

    def test_remove_children(self):
        parent = DocumentIRParagraphNode()
        child_a = DocumentIRTextSpanNode("a")
        child_b = DocumentIRTextSpanNode("b")
        parent.add_child(child_a)
        parent.add_child(child_b)
        parent.remove_children()
        assert parent.children == []
        assert child_a.parent is None
        assert child_b.parent is None


class TestDocumentIRVisitor:
    def test_dispatches_to_specific_method(self):
        visited = []

        class TestVisitor(DocumentIRVisitor):
            def visit_DocumentIRParagraphNode(self, node):  # pylint: disable=invalid-name
                visited.append(node)

        visitor = TestVisitor()
        node = DocumentIRParagraphNode()
        visitor.visit(node)
        assert node in visited

    def test_generic_visit_recurses(self):
        visited = []

        class TestVisitor(DocumentIRVisitor):
            def visit_DocumentIRTextSpanNode(self, node):  # pylint: disable=invalid-name
                visited.append(node.content)

        visitor = TestVisitor()
        para = DocumentIRParagraphNode()
        span_a = DocumentIRTextSpanNode("a")
        span_b = DocumentIRTextSpanNode("b")
        para.add_child(span_a)
        para.add_child(span_b)
        visitor.visit(para)
        assert visited == ["a", "b"]


class TestDocumentIRDocumentNode:
    def test_default_source_path(self):
        node = DocumentIRDocumentNode()
        assert node.source_path is None

    def test_source_path(self):
        node = DocumentIRDocumentNode("/path/to/file.md")
        assert node.source_path == "/path/to/file.md"


class TestDocumentIRHeadingNode:
    def test_level_stored(self):
        node = DocumentIRHeadingNode(2)
        assert node.level == 2

    def test_level_clamped_low(self):
        node = DocumentIRHeadingNode(0)
        assert node.level == 1

    def test_level_clamped_high(self):
        node = DocumentIRHeadingNode(9)
        assert node.level == 6


class TestDocumentIRCodeBlockNode:
    def test_attributes(self):
        node = DocumentIRCodeBlockNode("python", "print('hello')")
        assert node.language == "python"
        assert node.content == "print('hello')"

    def test_empty_language(self):
        node = DocumentIRCodeBlockNode("", "some code")
        assert node.language == ""


class TestDocumentIROrderedListNode:
    def test_default_start(self):
        node = DocumentIROrderedListNode()
        assert node.start == 1

    def test_custom_start(self):
        node = DocumentIROrderedListNode(start=3)
        assert node.start == 3


class TestDocumentIRTableCellNode:
    def test_defaults(self):
        node = DocumentIRTableCellNode()
        assert node.is_header is False
        assert node.alignment == "left"

    def test_header_cell(self):
        node = DocumentIRTableCellNode(is_header=True, alignment="center")
        assert node.is_header is True
        assert node.alignment == "center"


class TestDocumentIRTextSpanNode:
    def test_plain_text(self):
        node = DocumentIRTextSpanNode("hello")
        assert node.content == "hello"
        assert node.bold is False
        assert node.italic is False
        assert node.strikethrough is False
        assert node.code is False

    def test_all_flags(self):
        node = DocumentIRTextSpanNode("x", bold=True, italic=True, strikethrough=True, code=True)
        assert node.bold is True
        assert node.italic is True
        assert node.strikethrough is True
        assert node.code is True

    def test_combined_bold_italic(self):
        node = DocumentIRTextSpanNode("x", bold=True, italic=True)
        assert node.bold is True
        assert node.italic is True
        assert node.strikethrough is False
        assert node.code is False


class TestDocumentIRLinkNode:
    def test_url_only(self):
        node = DocumentIRLinkNode("https://example.com")
        assert node.url == "https://example.com"
        assert node.title is None

    def test_with_title(self):
        node = DocumentIRLinkNode("https://example.com", title="Example")
        assert node.title == "Example"

    def test_children_are_display_text(self):
        node = DocumentIRLinkNode("https://example.com")
        span = DocumentIRTextSpanNode("click here")
        node.add_child(span)
        assert span in node.children


class TestDocumentIRImageNode:
    def test_defaults(self):
        node = DocumentIRImageNode("image.png")
        assert node.url == "image.png"
        assert node.alt_text == ""
        assert node.title is None

    def test_full(self):
        node = DocumentIRImageNode("image.png", alt_text="A photo", title="Photo")
        assert node.alt_text == "A photo"
        assert node.title == "Photo"
