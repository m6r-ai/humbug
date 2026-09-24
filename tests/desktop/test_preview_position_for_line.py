"""Tests for mapping a source line to a scrollable position in a preview widget."""

import syntax.parser_imports  # noqa: F401  # registers all syntax parsers

from PySide6.QtCore import QPoint
from PySide6.QtGui import QTextCursor

from markdown_ import MarkdownConverter

from desktop.preview_tab.preview_file_content import PreviewFileContent
from desktop.preview_tab.preview_markdown_content_section import PreviewMarkdownContentSection


def _point_for_block(text_area, block_number: int, column: int = 0) -> QPoint:
    """Return a point in the text area's coordinates over the given block and column."""
    block = text_area.document().findBlockByNumber(block_number)
    cursor = QTextCursor(block)
    cursor.movePosition(
        QTextCursor.MoveOperation.Right,
        QTextCursor.MoveMode.MoveAnchor,
        column,
    )
    rect = text_area.cursorRect(cursor)
    return rect.center()


class TestPreviewFileContentPositionForLine:
    """Tests for PreviewFileContent.position_for_line."""

    def test_first_line_maps_to_first_block(self, qapp) -> None:
        """Line 1 maps to the start of the first block."""
        widget = PreviewFileContent()
        widget.set_content("alpha\nbeta\ngamma\n", "/tmp/example.txt")
        widget.resize(400, 300)

        result = widget.position_for_line(1)

        assert result is not None
        assert result == (0, 0)

    def test_third_line_maps_to_third_block(self, qapp) -> None:
        """Line 3 maps to the start of the third block."""
        widget = PreviewFileContent()
        widget.set_content("alpha\nbeta\ngamma\n", "/tmp/example.txt")
        widget.resize(400, 300)

        result = widget.position_for_line(3)

        assert result is not None
        section, position = result
        assert section == 0
        assert widget._text_area.document().findBlock(position).blockNumber() == 2

    def test_line_beyond_document_returns_none(self, qapp) -> None:
        """A line past the end of the document yields no mapping."""
        widget = PreviewFileContent()
        widget.set_content("alpha\nbeta\n", "/tmp/example.txt")
        widget.resize(400, 300)

        assert widget.position_for_line(99) is None

    def test_round_trips_with_line_column_at(self, qapp) -> None:
        """A line mapped to a position maps back to the same line."""
        widget = PreviewFileContent()
        widget.set_content("alpha\nbeta\ngamma\n", "/tmp/example.txt")
        widget.resize(400, 300)

        result = widget.position_for_line(2)
        assert result is not None

        block = widget._text_area.document().findBlock(result[1])
        point = _point_for_block(widget._text_area, block.blockNumber())
        round_tripped = widget.line_column_at(point)

        assert round_tripped is not None
        assert round_tripped[0] == 2


class TestPreviewMarkdownSectionPositionForLine:
    """Tests for PreviewMarkdownContentSection.position_for_line on rendered markdown."""

    def _make_section(self, node, syntax) -> PreviewMarkdownContentSection:
        section = PreviewMarkdownContentSection(syntax)
        section.set_content(node)
        section.resize(600, 400)
        return section

    def _sections_for(self, text: str):
        converter = MarkdownConverter()
        return converter.extract_sections(text, "/tmp/example.md")

    def test_heading_line_maps_to_heading_block(self, qapp) -> None:
        """The second heading's source line maps to the block rendering it."""
        sections = self._sections_for("# First\n\nSome text.\n\n## Second\n\nMore text.\n")
        node, syntax = sections[0]
        section = self._make_section(node, syntax)

        result = section.position_for_line(5)

        assert result is not None
        block = section.text_area().document().findBlock(result[1])
        assert block.text().startswith("Second")

    def test_paragraph_line_maps_to_paragraph_block(self, qapp) -> None:
        """The paragraph's source line maps to the block rendering it."""
        sections = self._sections_for("# Heading\n\nParagraph body.\n")
        node, syntax = sections[0]
        section = self._make_section(node, syntax)

        result = section.position_for_line(3)

        assert result is not None
        block = section.text_area().document().findBlock(result[1])
        assert block.text().startswith("Paragraph")

    def test_code_block_line_maps_to_code_block(self, qapp) -> None:
        """A line inside a fenced code block maps to the corresponding code line."""
        sections = self._sections_for("Intro.\n\n```python\nprint('one')\nprint('two')\n```\n")

        code_section = None
        for node, syntax in sections:
            if syntax is not None:
                code_section = self._make_section(node, syntax)
                break

        assert code_section is not None

        result = code_section.position_for_line(5)

        assert result is not None
        block = code_section.text_area().document().findBlock(result[1])
        assert "print('two')" in block.text()
