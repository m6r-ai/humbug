"""
Integration tests for DocumentConverterAITool using real temporary files.

These tests exercise the full conversion pipeline end-to-end: real Markdown and
DOCX content is read, converted through document_ir, and written to disk.  They verify
that the tool produces non-empty output and that the round-trip is coherent, without
asserting exact byte-for-byte equality (which is the responsibility of the dmarkdown
and docx module tests).

The fixture file test_tables_and_lists.md covers:
  - Simple tables
  - Tables with column alignment
  - Tables with inline formatting (bold, italic, inline code, strikethrough)
  - Wide tables (many columns)
  - Two-level nested bullet lists
  - Three-level nested bullet lists
  - Inline formatting within list items
  - Ordered lists with nested bullets
  - Transitions between block types (table then list)
  - Horizontal rules
"""

import asyncio
import tempfile
import zipfile
from pathlib import Path
from unittest.mock import MagicMock

import pytest

from document_converter_ai_tool.document_converter_ai_tool import DocumentConverterAITool

import syntax.parser_imports  # noqa: F401


_FIXTURES = Path(__file__).parent / "fixtures"
_ROUND_TRIP_MD = _FIXTURES / "test_tables_and_lists.md"


@pytest.fixture
def temp_dir():
    """Provide a real temporary directory that is cleaned up after the test."""
    with tempfile.TemporaryDirectory() as d:
        yield Path(d)


@pytest.fixture
def real_tool(temp_dir):
    """DocumentConverterAITool wired to a real temp directory as the mindspace."""
    def resolver(path: str) -> tuple[Path, str]:
        if not path:
            raise ValueError("Path cannot be empty")
        if ".." in path:
            raise ValueError(f"Path is outside the mindspace: {path}")
        p = Path(path)
        if p.is_absolute():
            if not str(p).startswith(str(temp_dir)):
                raise ValueError(f"Path is outside the mindspace: {path}")
            abs_path = p
        else:
            abs_path = temp_dir / path
        display = str(abs_path.relative_to(temp_dir))
        return abs_path, display

    mock_mindspace = MagicMock()
    mock_mindspace.add_interaction.return_value = None
    return DocumentConverterAITool(resolve_path=resolver, mindspace=mock_mindspace)


@pytest.fixture
def real_authorization():
    """Authorization callback that always approves."""
    mock = MagicMock()

    async def _approve(_tool_name, _arguments, _context, _requester_ref, _destructive):
        return True

    mock.side_effect = _approve
    return mock


@pytest.fixture
def md_input(temp_dir) -> Path:
    """Copy the round-trip fixture into the temp mindspace and return its path."""
    dest = temp_dir / "test_tables_and_lists.md"
    dest.write_text(_ROUND_TRIP_MD.read_text(encoding="utf-8"), encoding="utf-8")
    return dest


@pytest.fixture
def docx_from_fixture(real_tool, real_authorization, md_input, temp_dir, make_tool_call) -> Path:
    """Produce a DOCX from the round-trip fixture and return its path."""
    tool_call = make_tool_call("document_converter", {
        "operation": "convert",
        "input_path": str(md_input),
        "to_format": "docx",
    })
    asyncio.run(real_tool.execute(tool_call, "", real_authorization))
    return temp_dir / "test_tables_and_lists.docx"


class TestMdToDocxConversion:
    """Integration tests for Markdown → DOCX conversion."""

    def test_produces_non_empty_output(
        self, real_tool, real_authorization, md_input, make_tool_call
    ):
        """Converting the fixture produces a non-empty DOCX file on disk."""
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": str(md_input),
            "to_format": "docx",
        })
        asyncio.run(real_tool.execute(tool_call, "", real_authorization))

        output = md_input.with_suffix(".docx")
        assert output.exists()
        assert output.stat().st_size > 0

    def test_output_is_valid_zip(
        self, real_tool, real_authorization, md_input, make_tool_call
    ):
        """The DOCX output is a valid ZIP archive (DOCX files are ZIP-based)."""
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": str(md_input),
            "to_format": "docx",
        })
        asyncio.run(real_tool.execute(tool_call, "", real_authorization))

        assert zipfile.is_zipfile(md_input.with_suffix(".docx"))

    def test_explicit_output_path_is_used(
        self, real_tool, real_authorization, md_input, temp_dir, make_tool_call
    ):
        """An explicit output_path is used instead of the default stem+extension."""
        output = temp_dir / "custom.docx"
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": str(md_input),
            "output_path": str(output),
        })
        asyncio.run(real_tool.execute(tool_call, "", real_authorization))

        assert output.exists()
        assert not md_input.with_suffix(".docx").exists()

    def test_default_output_placed_alongside_input(
        self, real_tool, real_authorization, md_input, make_tool_call
    ):
        """The default output file is placed in the same directory as the input."""
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": str(md_input),
            "to_format": "docx",
        })
        asyncio.run(real_tool.execute(tool_call, "", real_authorization))

        assert (md_input.parent / "test_tables_and_lists.docx").exists()


class TestDocxToMdConversion:
    """Integration tests for DOCX → Markdown conversion."""

    def test_produces_non_empty_output(
        self, real_tool, real_authorization, docx_from_fixture, make_tool_call
    ):
        """Converting the DOCX back to Markdown produces a non-empty file."""
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": str(docx_from_fixture),
            "to_format": "md",
        })
        asyncio.run(real_tool.execute(tool_call, "", real_authorization))

        output = docx_from_fixture.with_suffix(".md")
        assert output.exists()
        assert output.stat().st_size > 0

    def test_output_is_valid_utf8_text(
        self, real_tool, real_authorization, docx_from_fixture, make_tool_call
    ):
        """The Markdown output is valid UTF-8 text."""
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": str(docx_from_fixture),
            "to_format": "md",
        })
        asyncio.run(real_tool.execute(tool_call, "", real_authorization))

        content = docx_from_fixture.with_suffix(".md").read_text(encoding="utf-8")
        assert len(content) > 0


class TestRoundTrip:
    """Round-trip tests: MD → DOCX → MD, checking content survival."""

    @pytest.fixture
    def roundtrip_md(
        self, real_tool, real_authorization, docx_from_fixture, temp_dir, make_tool_call
    ) -> str:
        """Perform the full round-trip and return the resulting Markdown text."""
        output = temp_dir / "roundtrip.md"
        tool_call = make_tool_call("document_converter", {
            "operation": "convert",
            "input_path": str(docx_from_fixture),
            "output_path": str(output),
        })
        asyncio.run(real_tool.execute(tool_call, "", real_authorization))
        return output.read_text(encoding="utf-8")

    def test_section_headings_survive(self, roundtrip_md):
        """All section headings from the fixture survive the round-trip."""
        assert "Table and Nested List Test Document" in roundtrip_md
        assert "Simple Table" in roundtrip_md
        assert "Table with Alignment" in roundtrip_md
        assert "Table with Inline Formatting" in roundtrip_md
        assert "Simple Nested Bullet List" in roundtrip_md
        assert "Deeply Nested Bullet List" in roundtrip_md
        assert "Nested List with Inline Formatting" in roundtrip_md
        assert "Mixed Content" in roundtrip_md
        assert "Numbered List with Nested Bullets" in roundtrip_md
        assert "Wide Table" in roundtrip_md

    def test_simple_table_content_survives(self, roundtrip_md):
        """Cell content from the simple table survives the round-trip."""
        assert "Alice" in roundtrip_md
        assert "Developer" in roundtrip_md
        assert "Carol" in roundtrip_md
        assert "On Leave" in roundtrip_md

    def test_table_with_alignment_content_survives(self, roundtrip_md):
        """Cell content from the alignment table survives the round-trip."""
        assert "Widget A" in roundtrip_md
        assert "Gadget Pro" in roundtrip_md

    def test_table_with_inline_formatting_content_survives(self, roundtrip_md):
        """Cell content from the inline-formatting table survives the round-trip."""
        assert "Bold text" in roundtrip_md
        assert "Italic text" in roundtrip_md
        assert "Inline code" in roundtrip_md

    def test_wide_table_content_survives(self, roundtrip_md):
        """Cell content from the wide table survives the round-trip."""
        assert "Engineering" in roundtrip_md
        assert "Manchester" in roundtrip_md

    def test_two_level_nested_list_survives(self, roundtrip_md):
        """Items from the two-level nested bullet list survive the round-trip."""
        assert "Apple" in roundtrip_md
        assert "Banana" in roundtrip_md
        assert "Broccoli" in roundtrip_md

    def test_three_level_nested_list_survives(self, roundtrip_md):
        """Items from the three-level nested bullet list survive the round-trip."""
        assert "Level 1 Item A" in roundtrip_md
        assert "Level 2 Item A1" in roundtrip_md
        assert "Level 3 Item A1a" in roundtrip_md
        assert "Level 3 Item A2a" in roundtrip_md

    def test_inline_formatting_in_lists_survives(self, roundtrip_md):
        """Text content of inline-formatted list items survives the round-trip."""
        assert "Bold top-level item" in roundtrip_md
        assert "Italic sub-item" in roundtrip_md
        assert "config.yaml" in roundtrip_md

    def test_ordered_list_content_survives(self, roundtrip_md):
        """Items from the ordered list survive the round-trip."""
        assert "First ordered item" in roundtrip_md
        assert "Second ordered item" in roundtrip_md
        assert "Third ordered item" in roundtrip_md

    def test_mixed_table_and_list_content_survives(self, roundtrip_md):
        """Content from the mixed table-then-list section survives the round-trip."""
        assert "Parse input" in roundtrip_md
        assert "Transform AST" in roundtrip_md
        assert "Serialise IR" in roundtrip_md
