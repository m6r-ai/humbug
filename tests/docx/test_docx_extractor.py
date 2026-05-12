import pytest

from docx.docx_errors import DocxExtractionError, DocxParseError, DocxUnsupportedError
from docx.docx_extractor import extract_text

from .conftest import (
    make_docx,
    make_docx_bad_xml,
    make_docx_missing_document,
    make_docx_no_body,
    make_encrypted_docx,
)


class TestExtractText:
    def test_single_paragraph(self) -> None:
        data = make_docx(["Hello World"])
        text = extract_text(data)
        assert "Hello World" in text

    def test_multiple_paragraphs(self) -> None:
        data = make_docx(["First paragraph", "Second paragraph", "Third paragraph"])
        text = extract_text(data)
        assert "First paragraph" in text
        assert "Second paragraph" in text
        assert "Third paragraph" in text

    def test_paragraphs_separated_by_newlines(self) -> None:
        data = make_docx(["Line one", "Line two"])
        text = extract_text(data)
        lines = text.split("\n")
        assert any("Line one" in line for line in lines)
        assert any("Line two" in line for line in lines)

    def test_empty_document(self) -> None:
        data = make_docx([])
        text = extract_text(data)
        assert isinstance(text, str)
        assert text == ""

    def test_empty_paragraph(self) -> None:
        data = make_docx(["Before", "", "After"])
        text = extract_text(data)
        assert "Before" in text
        assert "After" in text

    def test_special_xml_characters(self) -> None:
        data = make_docx(["Price < 100 & cost > 50"])
        text = extract_text(data)
        assert "Price" in text
        assert "100" in text
        assert "50" in text

    def test_unicode_text(self) -> None:
        data = make_docx(["Héllo Wörld", "日本語テスト"])
        text = extract_text(data)
        assert "Héllo Wörld" in text
        assert "日本語テスト" in text

    def test_returns_string(self) -> None:
        data = make_docx(["Some text"])
        result = extract_text(data)
        assert isinstance(result, str)

    def test_trailing_blank_paragraphs_stripped(self) -> None:
        data = make_docx(["Content", "", ""])
        text = extract_text(data)
        assert not text.endswith("\n\n")

    def test_not_a_zip_raises_parse_error(self) -> None:
        with pytest.raises(DocxParseError):
            extract_text(b"this is not a zip file")

    def test_empty_bytes_raises_parse_error(self) -> None:
        with pytest.raises(DocxParseError):
            extract_text(b"")

    def test_encrypted_raises_unsupported_error(self) -> None:
        data = make_encrypted_docx()
        with pytest.raises(DocxUnsupportedError):
            extract_text(data)

    def test_missing_document_xml_raises_parse_error(self) -> None:
        data = make_docx_missing_document()
        with pytest.raises(DocxParseError):
            extract_text(data)

    def test_malformed_xml_raises_parse_error(self) -> None:
        data = make_docx_bad_xml()
        with pytest.raises(DocxParseError):
            extract_text(data)

    def test_no_body_raises_extraction_error(self) -> None:
        data = make_docx_no_body()
        with pytest.raises(DocxExtractionError):
            extract_text(data)
