import pytest

from pdf.pdf_errors import PDFExtractionError
from pdf.pdf_extractor import extract_text
from pdf.pdf_parser import parse

from .conftest import make_pdf, make_compressed_pdf, make_form_xobject_pdf, make_tounicode_pdf, make_raw_content_pdf


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


class TestTmPositioning:
    """Tm operator sets absolute text position; spacing between chunks is geometry-driven."""

    def test_tm_same_line_word_gap_inserts_space(self) -> None:
        # Two Tm-positioned chunks on the same Y with a gap larger than one
        # character width.  With font size 12 the word-gap threshold is
        # 12 * 0.5 * 0.3 = 1.8 pts.  A gap of 10 pts is well above that.
        # "Hello" is 5 chars -> estimated width 5 * 12 * 0.5 = 30 pts.
        # Second chunk starts at x = 72 + 30 + 10 = 112.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (Hello) Tj ET "
            "BT /F1 12 Tf 1 0 0 1 112 720 Tm (world) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Hello world" in text

    def test_tm_same_line_no_gap_no_separator(self) -> None:
        # Two chunks whose X positions are contiguous: no gap, no separator.
        # "Hello" width estimate = 30 pts; second chunk at x = 72 + 30 = 102.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (Hello) Tj ET "
            "BT /F1 12 Tf 1 0 0 1 102 720 Tm (world) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Helloworld" in text
        assert "Hello world" not in text

    def test_tm_y_change_inserts_newline(self) -> None:
        # A Tm that moves to a lower Y (new line) produces a newline between chunks.
        # Y change of 14 pts > threshold of 12 * 0.2 = 2.4 pts.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (First) Tj ET "
            "BT /F1 12 Tf 1 0 0 1 72 706 Tm (Second) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "First" in text
        assert "Second" in text
        assert "First Second" not in text

    def test_tm_large_forward_x_jump_inserts_newline(self) -> None:
        # A Tm that jumps far forward on the same Y signals a column boundary.
        # Large forward threshold = 12 * 0.5 * 3.0 = 18 pts.
        # "Hi" width = 2 * 12 * 0.5 = 12 pts; second chunk at x = 72 + 12 + 200 = 284.
        # Gap = 284 - (72 + 12) = 200 pts >> 18 pts threshold.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (Hi) Tj ET "
            "BT /F1 12 Tf 1 0 0 1 284 720 Tm (there) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Hi" in text
        assert "there" in text
        assert "Hi there" not in text

    def test_tm_large_backward_x_jump_inserts_newline(self) -> None:
        # A Tm that jumps far backward on the same Y signals a line wrap.
        # Large backward threshold = 12 * 0.5 * 2.0 = 12 pts.
        # "end" width = 3 * 12 * 0.5 = 18 pts; chunk starts at x = 400.
        # After emitting, tx advances to 418.  Next Tm at x = 72.
        # Gap = 72 - (400 + 0) = -328 pts; abs > 12 threshold -> newline.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 400 720 Tm (end) Tj ET "
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (start) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "end" in text
        assert "start" in text
        assert "endstart" not in text
        assert "end start" not in text

    def test_tm_matrix_scale_used_for_thresholds(self) -> None:
        # When Tf sets size=1 and the Tm matrix provides the scale (a=12),
        # the effective font size is 12 and thresholds behave identically to
        # the Tf-size=12 case.  A word gap of 10 pts should produce a space.
        # "Hello" width = 5 * (12*1) * 0.5 = 30 pts; second chunk at x = 112.
        pdf = make_raw_content_pdf(
            "BT /F1 1 Tf 12 0 0 12 72 720 Tm (Hello) Tj ET "
            "BT /F1 1 Tf 12 0 0 12 112 720 Tm (world) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Hello world" in text

    def test_multiple_bt_et_blocks_same_line_join_correctly(self) -> None:
        # Text split across multiple BT...ET blocks on the same line must not
        # produce a spurious newline at the ET boundary.
        # Chunks are contiguous: "Hello" width = 30 pts, next at x = 102.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (Hello) Tj ET "
            "BT /F1 12 Tf 1 0 0 1 102 720 Tm (world) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "\n" not in text.strip()


class TestTdPositioning:
    """Td operator moves text position relatively; spacing is geometry-driven."""

    def test_td_horizontal_word_gap_inserts_space(self) -> None:
        # Td with a horizontal move larger than one character width produces a space.
        # After "Hello" (width 30), Td moves +10 pts -> gap = 10 > 1.8 threshold.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (Hello) Tj 10 0 Td (world) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Hello world" in text

    def test_td_zero_horizontal_no_separator(self) -> None:
        # Td with zero horizontal offset: chunks are contiguous, no separator.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (Hello) Tj 0 0 Td (world) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "Helloworld" in text
        assert "Hello world" not in text

    def test_td_negative_y_inserts_newline(self) -> None:
        # Td with a negative Y offset (moving down a line) produces a newline.
        # Y move of -14 pts; threshold is 12 * 0.2 = 2.4 pts.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (First) Tj 0 -14 Td (Second) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "First" in text
        assert "Second" in text
        assert "First Second" not in text


class TestTDAndTStar:
    """TD sets leading; T* advances by that leading."""

    def test_TD_sets_leading_and_moves_position(self) -> None:
        # TD tx ty: moves by (tx, ty) and sets leading to -ty.
        # A move of (0, -14) is a line break; threshold is 2.4 pts.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (First) Tj 0 -14 TD (Second) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "First" in text
        assert "Second" in text
        assert "First Second" not in text

    def test_T_star_uses_leading_to_insert_newline(self) -> None:
        # TD sets leading; T* moves down by that leading.
        # TD 0 -14 sets leading=14; T* then moves ty by -14.
        pdf = make_raw_content_pdf(
            "BT /F1 12 Tf 1 0 0 1 72 720 Tm (First) Tj 0 -14 TD (Second) Tj T* (Third) Tj ET"
        )
        doc = parse(pdf)
        text = extract_text(doc)
        assert "First" in text
        assert "Second" in text
        assert "Third" in text
        assert "First Second" not in text
        assert "Second Third" not in text
