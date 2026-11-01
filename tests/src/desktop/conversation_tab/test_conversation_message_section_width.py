"""
Tests for how a message section reacts to its text width changing.

A section's rendered content is measured against the document text width only
for tables (whose column and frame widths are computed at render time) and for
local images (which are pre-scaled to that width).  Everything else re-wraps
within the document on its own.  These tests pin the behaviour that has to
survive that distinction: a table follows the new width, and prose still wraps
at it.
"""

from collections.abc import Callable

import pytest

from PySide6.QtGui import QTextDocument, QTextFrame, QTextTable
from PySide6.QtWidgets import QApplication

from markdown_ import MarkdownASTBuilder
from desktop.conversation_tab.conversation_message_section import ConversationMessageSection

TABLE = (
    "| Operation | Description |\n"
    "|-----------|-------------|\n"
    "| read_file | Returns the complete contents of the named file |\n"
    "| write     | Replaces the complete contents of the named file |\n"
)

PROSE = (
    "This paragraph is long enough that it needs several lines once the view "
    "becomes narrow, which is what makes the wrapping behaviour observable. "
    "It repeats itself to be sure. This paragraph is long enough that it needs "
    "several lines once the view becomes narrow, which is what makes the "
    "wrapping behaviour observable."
)


def _find_table(frame: QTextFrame) -> QTextTable:
    """Return the first table frame within a frame."""
    for child in frame.childFrames():
        if isinstance(child, QTextTable):
            return child

        nested = _find_table(child)
        if nested is not None:
            return nested

    raise AssertionError("no table found in document")


@pytest.fixture
def make_section(qapp: QApplication) -> Callable[[str], ConversationMessageSection]:
    """Return a factory for shown, laid-out sections holding rendered markdown."""
    def _make(markdown: str) -> ConversationMessageSection:
        section = ConversationMessageSection(is_input=False)
        section.resize(600, 400)
        section.show()
        qapp.processEvents()
        section.set_content(MarkdownASTBuilder(no_underscores=False).build_ast(markdown))
        qapp.processEvents()
        return section

    return _make


class TestSectionWidthChange:
    def test_table_is_remeasured_for_the_new_width(
        self, qapp: QApplication, make_section: Callable[[str], ConversationMessageSection]
    ) -> None:
        """A table section is rendered again so its table fits the narrowed document."""
        section = make_section(TABLE)
        document = section.text_area().document()
        wide_table_width = _find_table(document.rootFrame()).format().toTableFormat().width().rawValue()

        section.resize(320, 400)
        qapp.processEvents()

        table = _find_table(document.rootFrame())
        table_width = table.format().toTableFormat().width().rawValue()

        assert table_width < wide_table_width
        assert table_width <= document.textWidth()

    def test_prose_wraps_at_the_new_width(
        self, qapp: QApplication, make_section: Callable[[str], ConversationMessageSection]
    ) -> None:
        """A prose section re-wraps within the document rather than being rendered again."""
        section = make_section(PROSE)
        text_area = section.text_area()
        wide_height = text_area.document().size().height()

        section.resize(320, 400)
        qapp.processEvents()

        assert text_area.document().textWidth() == pytest.approx(text_area.viewport().width())
        assert text_area.document().size().height() > wide_height


class TestPrimedWidth:
    def test_content_is_rendered_at_the_primed_width(self, qapp: QApplication) -> None:
        """Content wraps at the width the section will keep, not at its unlaid-out size."""
        section = ConversationMessageSection(is_input=False)
        margins = section._layout.contentsMargins()  # pylint: disable=protected-access
        expected = 942 - 2 * section.frameWidth() - margins.left() - margins.right()

        section.prime_width(942)
        section.set_content(MarkdownASTBuilder(no_underscores=False).build_ast(PROSE))

        assert section.text_area().document().textWidth() == expected

        section.resize(942, 400)
        section.show()
        qapp.processEvents()

        assert section.text_area().document().textWidth() == pytest.approx(
            section.text_area().viewport().width()
        )
