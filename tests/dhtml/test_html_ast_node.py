import pytest

from dhtml.html_ast_node import (
    HtmlASTCommentNode,
    HtmlASTDocumentNode,
    HtmlASTElementNode,
    HtmlASTNode,
    HtmlASTTextNode,
    HtmlASTVisitor,
)


class TestHtmlASTNodeBase:
    """HtmlASTNode provides parent/child relationship management."""

    def test_new_node_has_no_parent(self) -> None:
        node = HtmlASTDocumentNode()
        assert node.parent is None

    def test_new_node_has_empty_children(self) -> None:
        node = HtmlASTDocumentNode()
        assert node.children == []

    def test_add_child_appends_to_children(self) -> None:
        parent = HtmlASTDocumentNode()
        child = HtmlASTTextNode("hello")
        parent.add_child(child)
        assert child in parent.children

    def test_add_child_sets_parent(self) -> None:
        parent = HtmlASTDocumentNode()
        child = HtmlASTTextNode("hello")
        parent.add_child(child)
        assert child.parent is parent

    def test_add_child_returns_child(self) -> None:
        parent = HtmlASTDocumentNode()
        child = HtmlASTTextNode("hello")
        result = parent.add_child(child)
        assert result is child

    def test_add_multiple_children_preserves_order(self) -> None:
        parent = HtmlASTDocumentNode()
        a = HtmlASTTextNode("a")
        b = HtmlASTTextNode("b")
        c = HtmlASTTextNode("c")
        parent.add_child(a)
        parent.add_child(b)
        parent.add_child(c)
        assert parent.children == [a, b, c]

    def test_remove_child_removes_from_children(self) -> None:
        parent = HtmlASTDocumentNode()
        child = HtmlASTTextNode("hello")
        parent.add_child(child)
        parent.remove_child(child)
        assert child not in parent.children

    def test_remove_child_clears_parent(self) -> None:
        parent = HtmlASTDocumentNode()
        child = HtmlASTTextNode("hello")
        parent.add_child(child)
        parent.remove_child(child)
        assert child.parent is None

    def test_remove_child_raises_for_non_child(self) -> None:
        parent = HtmlASTDocumentNode()
        stranger = HtmlASTTextNode("stranger")
        with pytest.raises(ValueError):
            parent.remove_child(stranger)

    def test_remove_children_clears_all(self) -> None:
        parent = HtmlASTDocumentNode()
        parent.add_child(HtmlASTTextNode("a"))
        parent.add_child(HtmlASTTextNode("b"))
        parent.remove_children()
        assert parent.children == []

    def test_remove_children_clears_parent_on_each(self) -> None:
        parent = HtmlASTDocumentNode()
        child = HtmlASTTextNode("a")
        parent.add_child(child)
        parent.remove_children()
        assert child.parent is None


class TestHtmlASTDocumentNode:
    """HtmlASTDocumentNode stores source_path and has_doctype."""

    def test_default_source_path_is_none(self) -> None:
        doc = HtmlASTDocumentNode()
        assert doc.source_path is None

    def test_source_path_stored(self) -> None:
        doc = HtmlASTDocumentNode(source_path="/tmp/doc.html")
        assert doc.source_path == "/tmp/doc.html"

    def test_has_doctype_defaults_false(self) -> None:
        doc = HtmlASTDocumentNode()
        assert doc.has_doctype is False

    def test_has_doctype_can_be_set(self) -> None:
        doc = HtmlASTDocumentNode()
        doc.has_doctype = True
        assert doc.has_doctype is True


class TestHtmlASTElementNode:
    """HtmlASTElementNode stores tag_name and attributes."""

    def test_tag_name_stored(self) -> None:
        elem = HtmlASTElementNode(tag_name="div")
        assert elem.tag_name == "div"

    def test_default_attributes_is_empty_dict(self) -> None:
        elem = HtmlASTElementNode(tag_name="p")
        assert elem.attributes == {}

    def test_attributes_stored(self) -> None:
        elem = HtmlASTElementNode(tag_name="a", attributes={"href": "https://example.com"})
        assert elem.attributes["href"] == "https://example.com"

    def test_none_attributes_becomes_empty_dict(self) -> None:
        elem = HtmlASTElementNode(tag_name="span", attributes=None)
        assert elem.attributes == {}

    def test_is_html_ast_node(self) -> None:
        elem = HtmlASTElementNode(tag_name="div")
        assert isinstance(elem, HtmlASTNode)


class TestHtmlASTTextNode:
    """HtmlASTTextNode stores its text content."""

    def test_content_stored(self) -> None:
        node = HtmlASTTextNode("hello world")
        assert node.content == "hello world"

    def test_empty_content(self) -> None:
        node = HtmlASTTextNode("")
        assert node.content == ""

    def test_is_html_ast_node(self) -> None:
        node = HtmlASTTextNode("text")
        assert isinstance(node, HtmlASTNode)


class TestHtmlASTCommentNode:
    """HtmlASTCommentNode stores its comment content."""

    def test_content_stored(self) -> None:
        node = HtmlASTCommentNode(" a comment ")
        assert node.content == " a comment "

    def test_is_html_ast_node(self) -> None:
        node = HtmlASTCommentNode("x")
        assert isinstance(node, HtmlASTNode)


class TestHtmlASTVisitor:
    """HtmlASTVisitor dispatches to visit_ClassName or generic_visit."""

    def test_generic_visit_recurses_into_children(self) -> None:
        doc = HtmlASTDocumentNode()
        doc.add_child(HtmlASTTextNode("a"))
        doc.add_child(HtmlASTTextNode("b"))

        visited: list[str] = []

        class CollectVisitor(HtmlASTVisitor):
            def visit_HtmlASTTextNode(self, node: HtmlASTTextNode) -> None:
                visited.append(node.content)

        CollectVisitor().visit(doc)
        assert visited == ["a", "b"]

    def test_specific_visit_method_called(self) -> None:
        called: list[str] = []

        class TrackVisitor(HtmlASTVisitor):
            def visit_HtmlASTDocumentNode(self, node: HtmlASTDocumentNode) -> str:
                called.append("document")
                return "ok"

        doc = HtmlASTDocumentNode()
        result = TrackVisitor().visit(doc)
        assert called == ["document"]
        assert result == "ok"

    def test_generic_visit_returns_list_of_child_results(self) -> None:
        doc = HtmlASTDocumentNode()
        text_a = HtmlASTTextNode("a")
        text_b = HtmlASTTextNode("b")
        doc.add_child(text_a)
        doc.add_child(text_b)

        class ConstVisitor(HtmlASTVisitor):
            def visit_HtmlASTTextNode(self, node: HtmlASTTextNode) -> int:
                return 1

        results = ConstVisitor().visit(doc)
        assert results == [1, 1]
