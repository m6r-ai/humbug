import pytest

from docx import (
    DocxASTNode,
    DocxASTVisitor,
    DocxASTDocumentNode,
    DocxASTBodyNode,
    DocxASTSectionPropertiesNode,
    DocxASTParagraphNode,
    DocxASTParagraphPropertiesNode,
    DocxASTNumberingPropertiesNode,
    DocxASTRunNode,
    DocxASTRunPropertiesNode,
    DocxASTTextNode,
    DocxASTTabNode,
    DocxASTBreakNode,
    DocxASTLastRenderedPageBreakNode,
    DocxASTBookmarkStartNode,
    DocxASTBookmarkEndNode,
    DocxASTDrawingNode,
    DocxASTTableNode,
    DocxASTTablePropertiesNode,
    DocxASTTableGridNode,
    DocxASTTableRowNode,
    DocxASTTableRowPropertiesNode,
    DocxASTTableCellNode,
    DocxASTTableCellPropertiesNode,
    DocxASTStylesNode,
    DocxASTStyleNode,
    DocxASTNumberingNode,
    DocxASTAbstractNumNode,
    DocxASTNumNode,
    DocxASTNumLevelNode,
)


class TestDocxASTNode:
    def test_initial_state(self):
        node = DocxASTParagraphNode()
        assert node.parent is None
        assert node.children == []

    def test_add_child_sets_parent(self):
        parent = DocxASTParagraphNode()
        child = DocxASTRunNode()
        parent.add_child(child)
        assert child.parent is parent
        assert child in parent.children

    def test_add_child_returns_child(self):
        parent = DocxASTParagraphNode()
        child = DocxASTRunNode()
        result = parent.add_child(child)
        assert result is child

    def test_remove_child(self):
        parent = DocxASTParagraphNode()
        child = DocxASTRunNode()
        parent.add_child(child)
        parent.remove_child(child)
        assert child not in parent.children
        assert child.parent is None

    def test_remove_child_not_present_raises(self):
        parent = DocxASTParagraphNode()
        child = DocxASTRunNode()
        with pytest.raises(ValueError):
            parent.remove_child(child)

    def test_remove_children(self):
        parent = DocxASTParagraphNode()
        child_a = DocxASTRunNode()
        child_b = DocxASTRunNode()
        parent.add_child(child_a)
        parent.add_child(child_b)
        parent.remove_children()
        assert parent.children == []
        assert child_a.parent is None
        assert child_b.parent is None


class TestDocxASTVisitor:
    def test_dispatches_to_specific_method(self):
        visited = []

        class TestVisitor(DocxASTVisitor):
            def visit_DocxASTParagraphNode(self, node):  # pylint: disable=invalid-name
                visited.append(node)

        visitor = TestVisitor()
        node = DocxASTParagraphNode()
        visitor.visit(node)
        assert node in visited

    def test_generic_visit_recurses(self):
        visited_texts = []

        class TestVisitor(DocxASTVisitor):
            def visit_DocxASTTextNode(self, node):  # pylint: disable=invalid-name
                visited_texts.append(node.content)

        visitor = TestVisitor()
        para = DocxASTParagraphNode()
        run = DocxASTRunNode()
        text = DocxASTTextNode("hello")
        para.add_child(run)
        run.add_child(text)
        visitor.visit(para)
        assert "hello" in visited_texts


class TestDocxASTDocumentNode:
    def test_defaults(self):
        node = DocxASTDocumentNode()
        assert node.source_path is None
        assert node.relationships == {}

    def test_source_path(self):
        node = DocxASTDocumentNode("/path/to/doc.docx")
        assert node.source_path == "/path/to/doc.docx"

    def test_relationships_mutable(self):
        node = DocxASTDocumentNode()
        node.relationships["rId6"] = "word/media/image1.png"
        assert node.relationships["rId6"] == "word/media/image1.png"


class TestDocxASTSectionPropertiesNode:
    def test_defaults(self):
        node = DocxASTSectionPropertiesNode()
        assert node.page_width is None
        assert node.page_height is None
        assert node.margin_top is None

    def test_values(self):
        node = DocxASTSectionPropertiesNode(
            page_width=12240, page_height=15840,
            margin_top=1440, margin_right=1440,
            margin_bottom=1440, margin_left=1440,
        )
        assert node.page_width == 12240
        assert node.page_height == 15840
        assert node.margin_top == 1440


class TestDocxASTParagraphPropertiesNode:
    def test_defaults(self):
        node = DocxASTParagraphPropertiesNode()
        assert node.style_id is None
        assert node.justification is None
        assert node.outline_level is None
        assert node.keep_next is False
        assert node.page_break_before is False

    def test_values(self):
        node = DocxASTParagraphPropertiesNode(
            style_id="Heading1",
            justification="center",
            outline_level=0,
            spacing_before=240,
            indent_left=720,
            keep_next=True,
        )
        assert node.style_id == "Heading1"
        assert node.justification == "center"
        assert node.outline_level == 0
        assert node.spacing_before == 240
        assert node.indent_left == 720
        assert node.keep_next is True


class TestDocxASTNumberingPropertiesNode:
    def test_values(self):
        node = DocxASTNumberingPropertiesNode(num_id="1", ilvl=0)
        assert node.num_id == "1"
        assert node.ilvl == 0


class TestDocxASTRunPropertiesNode:
    def test_defaults(self):
        node = DocxASTRunPropertiesNode()
        assert node.bold is False
        assert node.italic is False
        assert node.underline is None
        assert node.strike is False
        assert node.double_strike is False
        assert node.vertical_align is None
        assert node.style_id is None
        assert node.sz is None
        assert node.color is None

    def test_bold_italic(self):
        node = DocxASTRunPropertiesNode(bold=True, italic=True, sz=48)
        assert node.bold is True
        assert node.italic is True
        assert node.sz == 48

    def test_underline_and_strike(self):
        node = DocxASTRunPropertiesNode(underline="single", strike=True)
        assert node.underline == "single"
        assert node.strike is True

    def test_superscript(self):
        node = DocxASTRunPropertiesNode(vertical_align="superscript")
        assert node.vertical_align == "superscript"


class TestDocxASTTextNode:
    def test_content(self):
        node = DocxASTTextNode("hello world")
        assert node.content == "hello world"
        assert node.preserve_space is False

    def test_preserve_space(self):
        node = DocxASTTextNode("  spaced  ", preserve_space=True)
        assert node.preserve_space is True


class TestDocxASTBreakNode:
    def test_default_type(self):
        node = DocxASTBreakNode()
        assert node.break_type == "textWrapping"

    def test_page_break(self):
        node = DocxASTBreakNode(break_type="page")
        assert node.break_type == "page"


class TestDocxASTBookmarkNodes:
    def test_start(self):
        node = DocxASTBookmarkStartNode(bookmark_id="0", name="intro")
        assert node.bookmark_id == "0"
        assert node.name == "intro"

    def test_end(self):
        node = DocxASTBookmarkEndNode(bookmark_id="0")
        assert node.bookmark_id == "0"


class TestDocxASTDrawingNode:
    def test_defaults(self):
        node = DocxASTDrawingNode()
        assert node.relationship_id is None
        assert node.resolved_path is None
        assert node.description is None
        assert node.width_emu is None
        assert node.height_emu is None

    def test_values(self):
        node = DocxASTDrawingNode(
            relationship_id="rId6",
            resolved_path="word/media/image1.png",
            description="A photo",
            width_emu=914400,
            height_emu=685800,
        )
        assert node.relationship_id == "rId6"
        assert node.resolved_path == "word/media/image1.png"
        assert node.width_emu == 914400


class TestDocxASTTableCellPropertiesNode:
    def test_defaults(self):
        node = DocxASTTableCellPropertiesNode()
        assert node.grid_span == 1
        assert node.vertical_merge is None
        assert node.vertical_alignment is None

    def test_grid_span(self):
        node = DocxASTTableCellPropertiesNode(
            width=3355, width_type="dxa", grid_span=3
        )
        assert node.grid_span == 3
        assert node.width == 3355


class TestDocxASTTableGridNode:
    def test_empty(self):
        node = DocxASTTableGridNode()
        assert node.column_widths == []

    def test_widths(self):
        node = DocxASTTableGridNode(column_widths=[1188, 1170, 5490])
        assert node.column_widths == [1188, 1170, 5490]


class TestDocxASTTableRowPropertiesNode:
    def test_defaults(self):
        node = DocxASTTableRowPropertiesNode()
        assert node.is_header is False
        assert node.cant_split is False
        assert node.height is None

    def test_cant_split(self):
        node = DocxASTTableRowPropertiesNode(cant_split=True)
        assert node.cant_split is True


class TestDocxASTStyleNode:
    def test_values(self):
        node = DocxASTStyleNode(
            style_type="paragraph",
            style_id="Heading1",
            name="heading 1",
            based_on="Normal",
            next_style="Normal",
        )
        assert node.style_type == "paragraph"
        assert node.style_id == "Heading1"
        assert node.name == "heading 1"
        assert node.based_on == "Normal"
        assert node.is_default is False
        assert node.is_custom is False


class TestDocxASTAbstractNumNode:
    def test_values(self):
        node = DocxASTAbstractNumNode(abstract_num_id="0")
        assert node.abstract_num_id == "0"
        assert node.multi_level_type is None

    def test_multi_level_type(self):
        node = DocxASTAbstractNumNode(abstract_num_id="1")
        node.multi_level_type = "multilevel"
        assert node.multi_level_type == "multilevel"


class TestDocxASTNumNode:
    def test_values(self):
        node = DocxASTNumNode(num_id="1", abstract_num_id="0")
        assert node.num_id == "1"
        assert node.abstract_num_id == "0"


class TestDocxASTNumLevelNode:
    def test_defaults(self):
        node = DocxASTNumLevelNode(ilvl=0)
        assert node.ilvl == 0
        assert node.start == 1
        assert node.num_fmt == "bullet"
        assert node.lvl_text == ""
        assert node.lvl_jc == "left"
        assert node.indent_left is None
        assert node.font_ascii is None

    def test_decimal_level(self):
        node = DocxASTNumLevelNode(
            ilvl=0,
            start=1,
            num_fmt="decimal",
            lvl_text="%1.",
            indent_left=720,
            indent_hanging=360,
        )
        assert node.num_fmt == "decimal"
        assert node.lvl_text == "%1."
        assert node.indent_left == 720
        assert node.indent_hanging == 360

    def test_bullet_with_symbol_font(self):
        node = DocxASTNumLevelNode(
            ilvl=0,
            num_fmt="bullet",
            lvl_text="\uf0b7",
            font_ascii="Symbol",
            font_h_ansi="Symbol",
        )
        assert node.font_ascii == "Symbol"
        assert node.font_h_ansi == "Symbol"
