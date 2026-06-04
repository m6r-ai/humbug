import pytest

from pdf.pdf_errors import PDFExtractionError
from pdf.pdf_extractor import extract_text
from pdf.pdf_parser import parse

from .conftest import make_pdf, make_compressed_pdf, make_form_xobject_pdf, make_tounicode_pdf


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


class TestLigatureExpansion:
    """Ligature codepoints (U+FB00–U+FB06) must be expanded to ASCII pairs."""

    def test_ff_ligature_via_bfchar(self) -> None:
        pdf = make_tounicode_pdf([(0x01, "\uFB00")], b"\x01")
        doc = parse(pdf)
        assert "ff" in extract_text(doc)

    def test_fi_ligature_via_bfchar(self) -> None:
        pdf = make_tounicode_pdf([(0x01, "\uFB01")], b"\x01")
        doc = parse(pdf)
        assert "fi" in extract_text(doc)

    def test_fl_ligature_via_bfchar(self) -> None:
        pdf = make_tounicode_pdf([(0x01, "\uFB02")], b"\x01")
        doc = parse(pdf)
        assert "fl" in extract_text(doc)

    def test_ffi_ligature_via_bfchar(self) -> None:
        pdf = make_tounicode_pdf([(0x01, "\uFB03")], b"\x01")
        doc = parse(pdf)
        assert "ffi" in extract_text(doc)

    def test_ffl_ligature_via_bfchar(self) -> None:
        pdf = make_tounicode_pdf([(0x01, "\uFB04")], b"\x01")
        doc = parse(pdf)
        assert "ffl" in extract_text(doc)

    def test_long_st_ligature_via_bfchar(self) -> None:
        pdf = make_tounicode_pdf([(0x01, "\uFB05")], b"\x01")
        doc = parse(pdf)
        assert "st" in extract_text(doc)

    def test_st_ligature_via_bfchar(self) -> None:
        pdf = make_tounicode_pdf([(0x01, "\uFB06")], b"\x01")
        doc = parse(pdf)
        assert "st" in extract_text(doc)

    def test_no_ligature_codepoints_remain_via_bfchar(self) -> None:
        # All seven ligature codepoints in one document — none should survive.
        entries = [
            (0x01, "\uFB00"),
            (0x02, "\uFB01"),
            (0x03, "\uFB02"),
            (0x04, "\uFB03"),
            (0x05, "\uFB04"),
            (0x06, "\uFB05"),
            (0x07, "\uFB06"),
        ]
        pdf = make_tounicode_pdf(entries, bytes(range(1, 8)))
        doc = parse(pdf)
        text = extract_text(doc)
        ligature_codepoints = set(range(0xFB00, 0xFB07))
        assert not any(ord(ch) in ligature_codepoints for ch in text)

    def test_bfrange_ligature_expansion(self) -> None:
        # Build a CMap with a bfrange that spans the ligature block, exercising
        # the chr()-based path in _parse_to_unicode_cmap.
        cmap_stream = (
            "begincmap\n"
            "2 beginbfrange\n"
            "<01> <01> <FB01>\n"   # fi
            "<02> <02> <FB03>\n"   # ffi
            "endbfrange\n"
            "endcmap\n"
        ).encode("latin-1")
        from pdf.pdf_extractor import _parse_to_unicode_cmap  # pylint: disable=import-outside-toplevel
        cmap = _parse_to_unicode_cmap(cmap_stream)
        assert cmap[b"\x01"] == "fi"
        assert cmap[b"\x02"] == "ffi"

    def test_non_ligature_cmap_entry_unchanged(self) -> None:
        # Ordinary characters must not be affected by ligature expansion.
        pdf = make_tounicode_pdf([(0x41, "A"), (0x42, "B")], b"\x41\x42")
        doc = parse(pdf)
        assert "AB" in extract_text(doc)


class TestUnmappedBytes:
    """Bytes with no CMap entry must produce U+FFFD, not raw ASCII."""

    def test_unmapped_byte_produces_replacement_character(self) -> None:
        # CMap only covers 0x41 ('A'); byte 0x40 has no entry and must give U+FFFD.
        pdf = make_tounicode_pdf([(0x41, "A")], b"\x40\x41")
        doc = parse(pdf)
        text = extract_text(doc)
        assert "\uFFFD" in text
        assert "A" in text

    def test_unmapped_byte_does_not_produce_raw_ascii(self) -> None:
        # The raw ASCII value of 0x40 is '@'; it must not appear in the output.
        pdf = make_tounicode_pdf([(0x41, "A")], b"\x40\x41")
        doc = parse(pdf)
        text = extract_text(doc)
        assert "@" not in text
