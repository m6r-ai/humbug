import pytest

from pdf.pdf_errors import PDFExtractionError
from pdf.pdf_extractor import extract_text
from pdf.pdf_parser import parse

from .conftest import make_pdf, make_compressed_pdf


class TestExtractText:
    def test_single_page_text(self) -> None:
        pdf = make_pdf(["Hello World"])
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Hello World" in text

    def test_multi_page_text(self) -> None:
        pdf = make_pdf(["First page", "Second page"])
        doc = parse(pdf)
        text = extract_text(doc)
        assert "First page" in text
        assert "Second page" in text

    def test_pages_separated_by_form_feed(self) -> None:
        pdf = make_pdf(["Page A", "Page B"])
        doc = parse(pdf)
        text = extract_text(doc)
        pages = text.split("\f")
        assert len(pages) == 2
        assert "Page A" in pages[0]
        assert "Page B" in pages[1]

    def test_compressed_content(self) -> None:
        pdf = make_compressed_pdf("Compressed text content")
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Compressed text content" in text

    def test_empty_page(self) -> None:
        pdf = make_pdf([""])
        doc = parse(pdf)
        text = extract_text(doc)
        # Should not raise; result may be empty or whitespace
        assert isinstance(text, str)

    def test_special_characters(self) -> None:
        pdf = make_pdf(["Price: 100 euros"])
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Price" in text
        assert "100" in text

    def test_no_root_raises(self) -> None:
        from pdf.pdf_types import PDFDocument
        doc = PDFDocument()
        with pytest.raises(PDFExtractionError):
            extract_text(doc)

    def test_three_pages(self) -> None:
        pdf = make_pdf(["Alpha", "Beta", "Gamma"])
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Alpha" in text
        assert "Beta" in text
        assert "Gamma" in text
        assert text.count("\f") == 2
