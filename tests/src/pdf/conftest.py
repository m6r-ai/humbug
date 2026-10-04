"""Shared fixtures and helpers for pdf module tests."""

import zlib
from typing import Any


def make_pdf(pages: list[str]) -> bytes:
    """Build a minimal but valid PDF containing the given page text strings.

    Each string in `pages` becomes one page. Text is embedded as uncompressed
    literal content streams so tests are easy to reason about.
    """
    objects: list[tuple[int, bytes]] = []
    obj_num = 1

    def next_num() -> int:
        nonlocal obj_num
        n = obj_num
        obj_num += 1
        return n

    catalog_num = next_num()   # 1
    pages_num = next_num()     # 2

    page_nums: list[int] = []
    content_nums: list[int] = []

    for text in pages:
        content_num = next_num()
        page_num = next_num()
        content_nums.append(content_num)
        page_nums.append(page_num)

    # Build content streams
    content_streams: list[tuple[int, bytes]] = []
    for i, text in enumerate(pages):
        stream_body = (
            f"BT /F1 12 Tf ({_escape_pdf_string(text)}) Tj ET"
        ).encode("latin-1")
        content_streams.append((content_nums[i], stream_body))

    # Assemble all objects in order
    all_objects: list[tuple[int, bytes]] = []

    # Catalog
    kids_refs = " ".join(f"{n} 0 R" for n in page_nums)
    all_objects.append((catalog_num, f"<< /Type /Catalog /Pages {pages_num} 0 R >>".encode()))
    all_objects.append((pages_num, f"<< /Type /Pages /Kids [{kids_refs}] /Count {len(pages)} >>".encode()))

    for i, text in enumerate(pages):
        stream_body = content_streams[i][1]
        stream_obj = (
            f"<< /Length {len(stream_body)} >>\nstream\n".encode() +
            stream_body +
            b"\nendstream"
        )
        all_objects.append((content_nums[i], stream_obj))

        font_dict = "<< /F1 << /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >> >>"
        page_obj = (
            f"<< /Type /Page /Parent {pages_num} 0 R "
            f"/Contents {content_nums[i]} 0 R "
            f"/Resources << /Font {font_dict} >> >>"
        ).encode()
        all_objects.append((page_nums[i], page_obj))

    # Build the PDF binary
    body = b"%PDF-1.4\n"
    offsets: dict[int, int] = {}

    for num, obj_body in all_objects:
        offsets[num] = len(body)
        body += f"{num} 0 obj\n".encode() + obj_body + b"\nendobj\n"

    xref_offset = len(body)
    max_obj = max(offsets.keys())
    xref = f"xref\n0 {max_obj + 1}\n"
    xref += "0000000000 65535 f \n"
    for i in range(1, max_obj + 1):
        off = offsets.get(i, 0)
        in_use = "n" if i in offsets else "f"
        xref += f"{off:010d} 00000 {in_use} \n"

    trailer = f"trailer\n<< /Size {max_obj + 1} /Root {catalog_num} 0 R >>\n"
    tail = f"startxref\n{xref_offset}\n%%EOF\n"

    return body + xref.encode() + trailer.encode() + tail.encode()


def make_compressed_pdf(text: str) -> bytes:
    """Build a minimal PDF with a FlateDecode-compressed content stream."""
    stream_body = f"BT /F1 12 Tf ({_escape_pdf_string(text)}) Tj ET".encode("latin-1")
    compressed = zlib.compress(stream_body)

    body = b"%PDF-1.4\n"
    offsets: dict[int, int] = {}

    objects: list[tuple[int, bytes]] = [
        (1, b"<< /Type /Catalog /Pages 2 0 R >>"),
        (2, b"<< /Type /Pages /Kids [4 0 R] /Count 1 >>"),
        (3, (
            f"<< /Length {len(compressed)} /Filter /FlateDecode >>\nstream\n".encode() +
            compressed +
            b"\nendstream"
        )),
        (4, (
            b"<< /Type /Page /Parent 2 0 R /Contents 3 0 R "
            b"/Resources << /Font << /F1 << /Type /Font /Subtype /Type1 "
            b"/BaseFont /Helvetica /Encoding /WinAnsiEncoding >> >> >> >>"
        )),
    ]

    for num, obj_body in objects:
        offsets[num] = len(body)
        body += f"{num} 0 obj\n".encode() + obj_body + b"\nendobj\n"

    xref_offset = len(body)
    xref = "xref\n0 5\n"
    xref += "0000000000 65535 f \n"
    for i in range(1, 5):
        xref += f"{offsets[i]:010d} 00000 n \n"

    trailer = f"trailer\n<< /Size 5 /Root 1 0 R >>\nstartxref\n{xref_offset}\n%%EOF\n"
    return body + xref.encode() + trailer.encode()


def _escape_pdf_string(text: str) -> str:
    """Escape special characters for use inside a PDF literal string."""
    return text.replace("\\", "\\\\").replace("(", "\\(").replace(")", "\\)")


def make_form_xobject_pdf(form_text: str, page_text: str = "") -> bytes:
    """Build a PDF where text lives inside a Form XObject invoked via the Do operator.

    This mirrors the structure used by OCR-over-image PDFs: the page content
    stream contains only a Do call, and the actual text operators live inside
    the referenced Form XObject.  An optional page_text string is also placed
    directly in the page stream to verify both sources are combined.
    """
    form_stream_body = (
        f"BT /F1 12 Tf ({_escape_pdf_string(form_text)}) Tj ET"
    ).encode("latin-1")

    font_dict = (
        b"<< /F1 << /Type /Font /Subtype /Type1 "
        b"/BaseFont /Helvetica /Encoding /WinAnsiEncoding >> >>"
    )

    body = b"%PDF-1.4\n"
    offsets: dict[int, int] = {}

    # Object layout:
    #   1 = Catalog
    #   2 = Pages
    #   3 = Form XObject stream
    #   4 = Page content stream (invokes /Frm via Do, plus optional page_text)
    #   5 = Page
    # 3: Form XObject
    form_obj = (
        f"<< /Type /XObject /Subtype /Form /Length {len(form_stream_body)}"
        f" /Resources << /Font {font_dict.decode()} >> >>\nstream\n"
    ).encode() + form_stream_body + b"\nendstream"

    # 4: Page content stream — invoke the form, then optionally emit page_text
    if page_text:
        page_stream_body = (
            f"/Frm Do BT /F1 12 Tf ({_escape_pdf_string(page_text)}) Tj ET"
        ).encode("latin-1")
    else:
        page_stream_body = b"/Frm Do"

    page_content_obj = (
        f"<< /Length {len(page_stream_body)} >>\nstream\n".encode()
        + page_stream_body
        + b"\nendstream"
    )

    objects: list[tuple[int, bytes]] = [
        (1, b"<< /Type /Catalog /Pages 2 0 R >>"),
        (2, b"<< /Type /Pages /Kids [5 0 R] /Count 1 >>"),
        (3, form_obj),
        (4, page_content_obj),
        (5, (
            b"<< /Type /Page /Parent 2 0 R /Contents 4 0 R"
            b" /Resources << /Font " + font_dict +
            b" /XObject << /Frm 3 0 R >> >> >>"
        )),
    ]

    for num, obj_body in objects:
        offsets[num] = len(body)
        body += f"{num} 0 obj\n".encode() + obj_body + b"\nendobj\n"

    xref_offset = len(body)
    xref = "xref\n0 6\n"
    xref += "0000000000 65535 f \n"
    for i in range(1, 6):
        xref += f"{offsets[i]:010d} 00000 n \n"

    trailer = f"trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n{xref_offset}\n%%EOF\n"
    return body + xref.encode() + trailer.encode()


def make_tounicode_pdf(cmap_entries: list[tuple[int, str]], glyph_bytes: bytes) -> bytes:
    """Build a minimal PDF whose font uses a ToUnicode CMap.

    cmap_entries is a list of (src_byte_value, unicode_string) pairs that are
    written as bfchar entries in the CMap stream.  glyph_bytes is the raw byte
    sequence placed inside the Tj string operator; the CMap is the sole source
    of truth for how those bytes decode to text.
    """
    bfchar_lines = "\n".join(
        f"<{src:02X}> <{''.join(f'{ord(ch):04X}' for ch in dst)}>"
        for src, dst in cmap_entries
    )
    cmap_stream = (
        "/CIDInit /ProcSet findresource begin\n"
        "12 dict begin\n"
        "begincmap\n"
        "/CMapType 2 def\n"
        f"1 beginbfchar\n"
        f"{len(cmap_entries)} beginbfchar\n"
        f"{bfchar_lines}\n"
        "endbfchar\n"
        "endcmap\n"
        "CMapName currentdict /CMap defineresource pop\n"
        "end\nend\n"
    ).encode("latin-1")

    # Escape the raw glyph bytes for use in a PDF literal string
    escaped = b""
    for b in glyph_bytes:
        if b in (ord("("), ord(")"), ord("\\")):
            escaped += b"\\" + bytes([b])
        else:
            escaped += bytes([b])

    content_stream = b"BT /F1 12 Tf (" + escaped + b") Tj ET"

    body = b"%PDF-1.4\n"
    offsets: dict[int, int] = {}

    # Object layout:
    #   1 = Catalog
    #   2 = Pages
    #   3 = ToUnicode CMap stream
    #   4 = Font dictionary
    #   5 = Content stream
    #   6 = Page

    cmap_obj = (
        f"<< /Length {len(cmap_stream)} >>\nstream\n".encode()
        + cmap_stream
        + b"\nendstream"
    )
    font_obj = b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /ToUnicode 3 0 R >>"
    content_obj = (
        f"<< /Length {len(content_stream)} >>\nstream\n".encode()
        + content_stream
        + b"\nendstream"
    )
    page_obj = (
        b"<< /Type /Page /Parent 2 0 R /Contents 5 0 R"
        b" /Resources << /Font << /F1 4 0 R >> >> >>"
    )

    objects: list[tuple[int, bytes]] = [
        (1, b"<< /Type /Catalog /Pages 2 0 R >>"),
        (2, b"<< /Type /Pages /Kids [6 0 R] /Count 1 >>"),
        (3, cmap_obj),
        (4, font_obj),
        (5, content_obj),
        (6, page_obj),
    ]

    for num, obj_body in objects:
        offsets[num] = len(body)
        body += f"{num} 0 obj\n".encode() + obj_body + b"\nendobj\n"

    xref_offset = len(body)
    xref = "xref\n0 7\n"
    xref += "0000000000 65535 f \n"
    for i in range(1, 7):
        xref += f"{offsets[i]:010d} 00000 n \n"

    trailer = f"trailer\n<< /Size 7 /Root 1 0 R >>\nstartxref\n{xref_offset}\n%%EOF\n"
    return body + xref.encode() + trailer.encode()


def make_raw_content_pdf(content_stream: str) -> bytes:
    """Build a minimal single-page PDF with a caller-supplied content stream.

    The content stream is embedded verbatim, allowing tests to exercise specific
    PDF operators (Tm, Td, TD, T*, TJ, etc.) with precise control over text
    positioning.  A standard WinAnsiEncoding Type1 font is registered as /F1
    so that Tf and text-show operators work without a ToUnicode CMap.
    """
    font_dict = (
        b"<< /F1 << /Type /Font /Subtype /Type1 "
        b"/BaseFont /Helvetica /Encoding /WinAnsiEncoding >> >>"
    )
    stream_body = content_stream.encode("latin-1")

    body = b"%PDF-1.4\n"
    offsets: dict[int, int] = {}

    content_obj = (
        f"<< /Length {len(stream_body)} >>\nstream\n".encode()
        + stream_body
        + b"\nendstream"
    )
    page_obj = (
        b"<< /Type /Page /Parent 2 0 R /Contents 3 0 R"
        b" /Resources << /Font " + font_dict + b" >> >>"
    )

    objects: list[tuple[int, bytes]] = [
        (1, b"<< /Type /Catalog /Pages 2 0 R >>"),
        (2, b"<< /Type /Pages /Kids [4 0 R] /Count 1 >>"),
        (3, content_obj),
        (4, page_obj),
    ]

    for num, obj_body in objects:
        offsets[num] = len(body)
        body += f"{num} 0 obj\n".encode() + obj_body + b"\nendobj\n"

    xref_offset = len(body)
    xref = "xref\n0 5\n0000000000 65535 f \n"
    for i in range(1, 5):
        xref += f"{offsets[i]:010d} 00000 n \n"

    trailer = f"trailer\n<< /Size 5 /Root 1 0 R >>\nstartxref\n{xref_offset}\n%%EOF\n"
    return body + xref.encode() + trailer.encode()
