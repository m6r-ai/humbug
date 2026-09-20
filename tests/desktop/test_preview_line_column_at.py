"""Tests for mapping a clicked preview position back to a source line and column."""

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


def _find_block_starting_with(text_area, prefix: str) -> int | None:
    """Return the block number of the first block whose text starts with prefix."""
    for block_number in range(text_area.document().blockCount()):
        block = text_area.document().findBlockByNumber(block_number)
        if block.text().startswith(prefix):
            return block_number

    return None


class TestPreviewFileContentLineColumnAt:
    """Tests for PreviewFileContent.line_column_at."""

    def test_maps_first_line(self, qapp) -> None:
        """A click on the first line maps to line 1."""
        widget = PreviewFileContent()
        widget.set_content("alpha\nbeta\ngamma\n", "/tmp/example.txt")
        widget.resize(400, 300)

        point = _point_for_block(widget._text_area, 0)
        result = widget.line_column_at(point)

        assert result is not None
        assert result[0] == 1

    def test_maps_third_line(self, qapp) -> None:
        """A click on the third line maps to line 3."""
        widget = PreviewFileContent()
        widget.set_content("alpha\nbeta\ngamma\n", "/tmp/example.txt")
        widget.resize(400, 300)

        point = _point_for_block(widget._text_area, 2)
        result = widget.line_column_at(point)

        assert result is not None
        assert result[0] == 3

    def test_maps_column_within_line(self, qapp) -> None:
        """A click part way along a line maps to the matching column."""
        widget = PreviewFileContent()
        widget.set_content("abcdef\n", "/tmp/example.txt")
        widget.resize(400, 300)

        point = _point_for_block(widget._text_area, 0, column=3)
        result = widget.line_column_at(point)

        assert result is not None
        assert result[0] == 1
        assert result[1] >= 1

    def test_returns_none_outside_text_area(self, qapp) -> None:
        """A point outside the text area yields no mapping."""
        widget = PreviewFileContent()
        widget.set_content("alpha\n", "/tmp/example.txt")
        widget.resize(400, 300)

        result = widget.line_column_at(QPoint(-50, -50))

        assert result is None


class TestPreviewMarkdownSectionLineColumnAt:
    """Tests for PreviewMarkdownContentSection.line_column_at on rendered markdown."""

    def _make_section(self, node, syntax) -> PreviewMarkdownContentSection:
        section = PreviewMarkdownContentSection(syntax)
        section.set_content(node)
        section.resize(600, 400)
        return section

    def _sections_for(self, text: str):
        converter = MarkdownConverter()
        return converter.extract_sections(text, "/tmp/example.md")

    def test_maps_second_heading_to_its_source_line(self, qapp) -> None:
        """A click on the second heading maps to that heading's source line."""
        sections = self._sections_for("# First\n\nSome text.\n\n## Second\n\nMore text.\n")
        node, syntax = sections[0]
        section = self._make_section(node, syntax)

        text_area = section.text_area()
        target_block = _find_block_starting_with(text_area, "Second")
        assert target_block is not None

        result = section.line_column_at(_point_for_block(text_area, target_block))

        assert result is not None
        assert result[0] == 5

    def test_maps_first_paragraph_to_its_source_line(self, qapp) -> None:
        """A click on the first paragraph maps to the paragraph's source line."""
        sections = self._sections_for("# Heading\n\nParagraph body.\n")
        node, syntax = sections[0]
        section = self._make_section(node, syntax)

        text_area = section.text_area()
        target_block = _find_block_starting_with(text_area, "Paragraph")
        assert target_block is not None

        result = section.line_column_at(_point_for_block(text_area, target_block))

        assert result is not None
        assert result[0] == 3

    def test_maps_list_items_to_their_source_lines(self, qapp) -> None:
        """A click on each list item maps to that item's source line."""
        sections = self._sections_for("# Title\n\n- first\n- second\n- third\n")
        node, syntax = sections[0]
        section = self._make_section(node, syntax)

        text_area = section.text_area()
        expected = {"first": 3, "second": 4, "third": 5}

        for text, line in expected.items():
            target_block = _find_block_starting_with(text_area, text)
            assert target_block is not None

            result = section.line_column_at(_point_for_block(text_area, target_block))

            assert result is not None
            assert result[0] == line

    def test_code_block_maps_to_source_lines(self, qapp) -> None:
        """A click on a code block line maps to the matching source line."""
        sections = self._sections_for("Intro.\n\n```python\nprint('one')\nprint('two')\n```\n")

        code_section = None
        for node, syntax in sections:
            if syntax is not None:
                code_section = self._make_section(node, syntax)
                break

        assert code_section is not None
        text_area = code_section.text_area()
        result = code_section.line_column_at(_point_for_block(text_area, 1))

        assert result is not None
        assert result[0] == 5
