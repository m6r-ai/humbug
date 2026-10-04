"""
Tests for right-to-left table positioning in the markdown renderer.

Qt shapes RTL glyph runs correctly on its own, so the only direction-specific
behaviour the renderer must provide is block-level positioning: a table whose
content is predominantly RTL should be anchored to the right edge of the
document rather than the left.
"""

import pytest

from PySide6.QtGui import QTextDocument, QTextFrame, QTextTable

from markdown_ import MarkdownASTBuilder, MarkdownASTTableNode
from desktop.markdown.markdown_renderer import MarkdownRenderer


@pytest.fixture(autouse=True)
def _qt(qapp):
    """Ensure a QApplication exists for the font database and rendering."""


def _build_table(markdown: str) -> MarkdownASTTableNode:
    """Parse markdown and return its first table node."""
    document = MarkdownASTBuilder(no_underscores=False).build_ast(markdown)
    for child in document.children:
        if isinstance(child, MarkdownASTTableNode):
            return child

    raise AssertionError("markdown did not produce a table node")


def _render_table(markdown: str, width: float = 600.0) -> QTextDocument:
    """Render markdown into a document and return the document.

    The document is returned rather than the table because a QTextTable is
    owned by the QTextDocument that contains it: once the document is garbage
    collected the table's C++ object is destroyed and any access to it raises
    a libshiboken RuntimeError.  Keeping the document alive for the duration
    of the test keeps the table valid.
    """
    document = QTextDocument()
    document.setTextWidth(width)
    renderer = MarkdownRenderer(document)
    renderer.visit(MarkdownASTBuilder(no_underscores=False).build_ast(markdown))

    return document


def _table_format(document: QTextDocument):
    """Return the table format of the first table in a rendered document."""
    return _find_table(document.rootFrame()).format().toTableFormat()


def _find_table(frame: QTextFrame) -> QTextTable:
    """Recursively find the first table frame within a frame."""
    for child in frame.childFrames():
        if isinstance(child, QTextTable):
            return child

        nested = _find_table(child)
        if nested is not None:
            return nested

    raise AssertionError("no table found in document")


LTR_TABLE = (
    "| Header | Value |\n"
    "|--------|-------|\n"
    "| Alpha  | 1     |\n"
    "| Beta   | 2     |\n"
)

RTL_TABLE = (
    "| العنوان | القيمة |\n"
    "|---------|--------|\n"
    "| أحمد    | ١      |\n"
    "| محمد    | ٢      |\n"
)


class TestIsRtlTable:
    def test_arabic_table_is_rtl(self):
        assert MarkdownRenderer._is_rtl_table(_build_table(RTL_TABLE)) is True

    def test_latin_table_is_not_rtl(self):
        assert MarkdownRenderer._is_rtl_table(_build_table(LTR_TABLE)) is False

    def test_leading_digits_do_not_decide_direction(self):
        table = (
            "| 2024 | القيمة |\n"
            "|------|--------|\n"
            "| 1    | نص     |\n"
        )
        assert MarkdownRenderer._is_rtl_table(_build_table(table)) is True

    def test_latin_first_strong_character_wins(self):
        table = (
            "| ID   | الاسم |\n"
            "|------|-------|\n"
            "| 1    | أحمد  |\n"
        )
        assert MarkdownRenderer._is_rtl_table(_build_table(table)) is False


class TestRtlTablePositioning:
    def test_rtl_table_is_anchored_right(self):
        table_format = _table_format(_render_table(RTL_TABLE))
        assert table_format.leftMargin() > 0
        assert table_format.rightMargin() == 0

    def test_ltr_table_stays_anchored_left(self):
        table_format = _table_format(_render_table(LTR_TABLE))
        assert table_format.leftMargin() == 0
        assert table_format.rightMargin() == 0

    def test_rtl_table_slack_matches_remaining_width(self):
        width = 600.0
        table_format = _table_format(_render_table(RTL_TABLE, width))
        table_width = table_format.width().rawValue()
        overhead = MarkdownRenderer._table_frame_overhead
        assert table_format.leftMargin() == pytest.approx(width - table_width - overhead, abs=1.0)
        assert table_format.rightMargin() == 0
