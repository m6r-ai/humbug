import zlib

import pytest

from pdf.pdf_errors import PDFParseError, PDFUnsupportedError
from pdf.pdf_parser import parse
from pdf.pdf_types import PDFObjectRef, PDFStream

from .conftest import make_pdf, make_compressed_pdf


class TestParse:
    def test_rejects_non_pdf(self) -> None:
        with pytest.raises(PDFParseError):
            parse(b"This is not a PDF")

    def test_rejects_encrypted(self) -> None:
        # Build a minimal PDF with an Encrypt entry in the trailer
        pdf = make_pdf(["hello"])
        # Inject /Encrypt into trailer by patching the bytes
        pdf = pdf.replace(b"/Root 1 0 R", b"/Root 1 0 R /Encrypt 99 0 R")
        with pytest.raises(PDFUnsupportedError):
            parse(pdf)

    def test_single_page(self) -> None:
        pdf = make_pdf(["Hello World"])
        doc = parse(pdf)
        assert doc.trailer.get("Root") is not None
        assert len(doc.objects) > 0

    def test_multi_page(self) -> None:
        pdf = make_pdf(["Page one", "Page two", "Page three"])
        doc = parse(pdf)
        assert doc.trailer.get("Root") is not None

    def test_xref_entries_loaded(self) -> None:
        pdf = make_pdf(["test"])
        doc = parse(pdf)
        assert len(doc.xref) > 0

    def test_compressed_stream(self) -> None:
        pdf = make_compressed_pdf("Compressed content")
        doc = parse(pdf)
        assert doc.trailer.get("Root") is not None

    def test_object_resolution(self) -> None:
        pdf = make_pdf(["Hello"])
        doc = parse(pdf)
        root_ref = doc.trailer.get("Root")
        assert isinstance(root_ref, PDFObjectRef)
        root = doc.get_object(root_ref)
        assert isinstance(root, dict)
        assert root.get("Type") == "Catalog"

    def test_pages_accessible(self) -> None:
        pdf = make_pdf(["Hello"])
        doc = parse(pdf)
        root_ref = doc.trailer.get("Root")
        root = doc.get_object(root_ref)
        pages_ref = root.get("Pages")
        pages = doc.get_object(pages_ref)
        assert isinstance(pages, dict)
        assert pages.get("Type") == "Pages"
        assert pages.get("Count") == 1

    def test_content_stream_accessible(self) -> None:
        pdf = make_pdf(["Hello"])
        doc = parse(pdf)
        root_ref = doc.trailer.get("Root")
        root = doc.get_object(root_ref)
        pages = doc.get_object(root.get("Pages"))
        page_ref = pages.get("Kids")[0]
        page = doc.get_object(page_ref)
        content_ref = page.get("Contents")
        content = doc.get_object(content_ref)
        assert isinstance(content, PDFStream)
        assert b"Hello" in content.data


class TestGetObject:
    def test_resolves_ref(self) -> None:
        pdf = make_pdf(["test"])
        doc = parse(pdf)
        root_ref = doc.trailer.get("Root")
        assert isinstance(root_ref, PDFObjectRef)
        result = doc.get_object(root_ref)
        assert isinstance(result, dict)

    def test_returns_none_for_missing(self) -> None:
        pdf = make_pdf(["test"])
        doc = parse(pdf)
        result = doc.get_object(PDFObjectRef(obj_num=9999, gen_num=0))
        assert result is None

    def test_cycle_protection(self) -> None:
        pdf = make_pdf(["test"])
        doc = parse(pdf)
        # Manually create a cycle
        doc.objects[100] = PDFObjectRef(obj_num=101, gen_num=0)
        doc.objects[101] = PDFObjectRef(obj_num=100, gen_num=0)
        result = doc.get_object(PDFObjectRef(obj_num=100, gen_num=0))
        assert result is None
