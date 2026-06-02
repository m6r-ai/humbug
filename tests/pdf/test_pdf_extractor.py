import pytest

from pdf.pdf_errors import PDFExtractionError
from pdf.pdf_extractor import extract_text
from pdf.pdf_parser import parse

from .conftest import make_pdf, make_compressed_pdf, make_form_xobject_pdf


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


class TestFormXObjectExtraction:
    def test_text_in_form_xobject_is_extracted(self) -> None:
        pdf = make_form_xobject_pdf("Form text here")
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Form text here" in text

    def test_form_xobject_and_page_text_combined(self) -> None:
        pdf = make_form_xobject_pdf("From the form", page_text="From the page")
        doc = parse(pdf)
        text = extract_text(doc)
        assert "From the form" in text
        assert "From the page" in text

    def test_image_xobject_do_does_not_raise(self) -> None:
        # A Do referencing an Image XObject (not a Form) must be silently ignored,
        # not raise an exception.
        pdf = make_pdf(["Page with image Do"])
        # Patch in a Do call for a non-existent XObject name — extractor must not crash.
        doc = parse(pdf)
        from pdf.pdf_types import PDFStream
        page_stream = PDFStream(data=b"/NonExistent Do BT /F1 12 Tf (Safe) Tj ET", attrs={"Length": 42})
        # Replace the content stream of the first page's object
        for obj_num, obj in doc.objects.items():
            if isinstance(obj, PDFStream) and b"BT" in obj.data:
                doc.objects[obj_num] = page_stream
                break
        text = extract_text(doc)
        assert isinstance(text, str)
