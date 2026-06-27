import re
from typing import Any

from pdf.pdf_errors import PDFExtractionError
from pdf.pdf_tokenizer import PDFTokenizer, TokenType
from pdf.pdf_types import PDFDocument, PDFObjectRef, PDFStream
from pdf.pdf_parser import _parse_value


# Unicode Alphabetic Presentation Forms (U+FB00–U+FB06) that PDF fonts commonly
# encode as single ligature glyphs.  Expand them to their ASCII equivalents so
# downstream consumers see plain text.
_LIGATURE_MAP: dict[str, str] = {
    "\uFB00": "ff",
    "\uFB01": "fi",
    "\uFB02": "fl",
    "\uFB03": "ffi",
    "\uFB04": "ffl",
    "\uFB05": "st",
    "\uFB06": "st",
}


def extract_text(doc: PDFDocument) -> str:
    """
    Extract all text from a parsed PDFDocument, in page order.

    Returns a single string with pages separated by form-feed characters.

    Raises:
        PDFExtractionError: if the page tree cannot be traversed.
    """
    pages = _collect_pages(doc)
    page_texts = []

    for page in pages:
        text = _extract_page_text(doc, page)
        page_texts.append(text)

    return "\f".join(page_texts)


def _collect_pages(doc: PDFDocument) -> list[dict[str, Any]]:
    """Walk the page tree and return a list of page dictionaries in order."""
    root_ref = doc.trailer.get("Root")
    if root_ref is None:
        raise PDFExtractionError("PDF trailer has no Root entry")

    root = doc.get_object(root_ref) if isinstance(root_ref, PDFObjectRef) else root_ref
    if not isinstance(root, dict):
        raise PDFExtractionError("PDF Root is not a dictionary")

    pages_ref = root.get("Pages")
    if pages_ref is None:
        raise PDFExtractionError("PDF Root has no Pages entry")

    pages_node = doc.get_object(pages_ref) if isinstance(pages_ref, PDFObjectRef) else pages_ref
    if not isinstance(pages_node, dict):
        raise PDFExtractionError("PDF Pages is not a dictionary")

    result: list[dict[str, Any]] = []
    _walk_page_tree(doc, pages_node, result, depth=0)
    return result


def _walk_page_tree(doc: PDFDocument, node: dict[str, Any], result: list[dict[str, Any]], depth: int) -> None:
    """Recursively collect leaf Page nodes from a Pages tree node."""
    if depth > 64:
        return

    node_type = node.get("Type")
    if node_type == "Page":
        result.append(node)
        return

    kids = node.get("Kids")
    if not isinstance(kids, list):
        return

    for kid_ref in kids:
        kid = doc.get_object(kid_ref) if isinstance(kid_ref, PDFObjectRef) else kid_ref
        if isinstance(kid, dict):
            _walk_page_tree(doc, kid, result, depth + 1)


def _extract_page_text(doc: PDFDocument, page: dict[str, Any]) -> str:
    """Extract text from a single page dictionary."""
    contents = page.get("Contents")
    if contents is None:
        return ""

    # Contents can be a single ref or an array of refs
    if isinstance(contents, (PDFObjectRef, PDFStream)):
        content_streams = [contents]

    elif isinstance(contents, list):
        content_streams = contents

    else:
        return ""

    combined = bytearray()
    for item in content_streams:
        stream = doc.get_object(item) if isinstance(item, PDFObjectRef) else item
        if isinstance(stream, PDFStream):
            combined.extend(stream.data)
            combined.extend(b" ")

    resources = _resolve_resources(doc, page)
    return _interpret_content_stream(bytes(combined), resources, doc)


def _resolve_resources(doc: PDFDocument, page: dict[str, Any]) -> dict[str, Any]:
    """Resolve the Resources dictionary for a page, following indirect refs."""
    resources = page.get("Resources")
    if isinstance(resources, PDFObjectRef):

        resources = doc.get_object(resources)
    return resources if isinstance(resources, dict) else {}


def _interpret_content_stream(data: bytes, resources: dict[str, Any], doc: PDFDocument) -> str:
    """Interpret a PDF content stream and return extracted text."""
    tokenizer = PDFTokenizer(data)
    text_parts: list[str] = []

    in_text_block = False
    operand_stack: list[Any] = []

    # Current font info
    current_font_name: str = ""
    current_font_size: float = 12.0
    font_cache: dict[str, dict[str, Any]] = {}
    # Text position state: absolute coordinates in text space
    tx: float = 0.0
    ty: float = 0.0
    # tm_scale captures the effective rendered font size from the Tm matrix (a component).
    tm_scale: float = 1.0
    prev_tx: float = 0.0
    prev_ty: float = 0.0
    prev_width: float = 0.0
    line_leading: float = 0.0

    while True:
        tokenizer.skip_whitespace_and_comments()
        if tokenizer.pos >= len(data):
            break

        saved = tokenizer.pos
        token = tokenizer.next_token()

        if token.type == TokenType.EOF:
            break

        # Accumulate operands
        if token.type in (
            TokenType.INTEGER, TokenType.REAL, TokenType.BOOLEAN,
            TokenType.STRING, TokenType.HEX_STRING, TokenType.NAME,
            TokenType.NULL
        ):
            operand_stack.append(token.value)
            continue

        if token.type == TokenType.ARRAY_START:
            tokenizer.pos = saved
            operand_stack.append(_parse_value(tokenizer))
            continue

        if token.type == TokenType.DICT_START:
            tokenizer.pos = saved
            operand_stack.append(_parse_value(tokenizer))
            continue

        if token.type not in (TokenType.KEYWORD, TokenType.REF_KW):
            operand_stack.clear()
            continue

        op = str(token.value)

        if op == "BT":
            in_text_block = True
            operand_stack.clear()
            continue

        if op == "ET":
            in_text_block = False
            operand_stack.clear()
            continue

        if op == "BI":
            # Inline image — skip everything up to and including EI
            ei_pos = data.find(b"EI", tokenizer.pos)
            if ei_pos == -1:
                break

            tokenizer.pos = ei_pos + 2
            operand_stack.clear()
            continue

        if op == "Do":
            # Invoke a named XObject. If it is a Form XObject (Subtype == Form)
            # recursively interpret its content stream to extract any text it contains.
            if operand_stack:
                xobject_name = str(operand_stack[-1])
                xobjects = resources.get("XObject", {})
                if isinstance(xobjects, PDFObjectRef):
                    xobjects = doc.get_object(xobjects)

                if isinstance(xobjects, dict):
                    xobj_ref = xobjects.get(xobject_name)
                    xobj = doc.get_object(xobj_ref) if isinstance(xobj_ref, PDFObjectRef) else xobj_ref
                    if isinstance(xobj, PDFStream) and xobj.attrs.get("Subtype") == "Form":
                        form_resources = xobj.attrs.get("Resources", {})
                        if isinstance(form_resources, PDFObjectRef):
                            form_resources = doc.get_object(form_resources)

                        if not isinstance(form_resources, dict):
                            form_resources = resources

                        text_parts.append(_interpret_content_stream(xobj.data, form_resources, doc))

            operand_stack.clear()
            continue

        if not in_text_block:
            operand_stack.clear()
            continue

        if op == "Tf":
            # Tf: fontname size
            if len(operand_stack) >= 1:
                current_font_name = str(operand_stack[-2]) if len(operand_stack) >= 2 else ""
                try:
                    current_font_size = float(operand_stack[-1])

                except (TypeError, ValueError):
                    pass

                font_cache_key = current_font_name
                if font_cache_key not in font_cache:
                    font_cache[font_cache_key] = _resolve_font(doc, resources, current_font_name)

            operand_stack.clear()
            continue

        if op in ("Tj", "'"):
            if operand_stack:
                text = _decode_string(operand_stack[-1], _get_font(font_cache, current_font_name))
                effective_size = tm_scale * current_font_size
                prev_tx, prev_ty, prev_width = _emit_text(
                    text, text_parts, tx, ty, prev_tx, prev_ty, prev_width, effective_size
                )
                tx += len(text) * effective_size * 0.5

            operand_stack.clear()
            continue

        if op == '"':
            # word-spacing char-spacing string
            if operand_stack:
                text = _decode_string(operand_stack[-1], _get_font(font_cache, current_font_name))
                effective_size = tm_scale * current_font_size
                prev_tx, prev_ty, prev_width = _emit_text(
                    text, text_parts, tx, ty, prev_tx, prev_ty, prev_width, effective_size
                )
                tx += len(text) * effective_size * 0.5

            operand_stack.clear()
            continue

        if op == "TJ":
            # Array of strings and spacing adjustments
            if operand_stack and isinstance(operand_stack[-1], list):
                font = _get_font(font_cache, current_font_name)
                parts: list[str] = []
                for item in operand_stack[-1]:
                    if isinstance(item, (bytes, str)):
                        parts.append(_decode_string(item, font))

                    elif isinstance(item, (int, float)) and item < -100:
                        parts.append(" ")

                text = "".join(parts)
                effective_size = tm_scale * current_font_size
                prev_tx, prev_ty, prev_width = _emit_text(
                    text, text_parts, tx, ty, prev_tx, prev_ty, prev_width, effective_size
                )
                tx += len(text) * effective_size * 0.5

            operand_stack.clear()
            continue

        if op == "Tm":
            # Tm sets the text matrix absolutely: [a b c d e f]
            # e is the new x position, f is the new y position in user space.
            # a is the x-scale component; abs(a) gives the effective rendered font size
            # when Tf sets size=1 and scaling is done entirely via the matrix.
            # prev_tx/prev_ty/prev_width are left unchanged: _emit_text uses
            # prev_tx + prev_width as the end of the previous chunk, so the gap
            # to the new tx is computed correctly whether Tm jumps far or lands
            # contiguously after the previous chunk.
            if len(operand_stack) >= 6:
                tx = float(operand_stack[-2])
                ty = float(operand_stack[-1])
                tm_scale = abs(float(operand_stack[-6]))

            operand_stack.clear()
            continue

        if op == "Td":
            # Td moves the text position relatively by (dtx, dty).
            if len(operand_stack) >= 2:
                tx += float(operand_stack[-2])
                ty += float(operand_stack[-1])

            operand_stack.clear()
            continue

        if op == "TD":
            # TD moves the text position and sets the leading to -dty.
            if len(operand_stack) >= 2:
                dty = float(operand_stack[-1])
                tx += float(operand_stack[-2])
                ty += dty
                line_leading = -dty

            operand_stack.clear()
            continue

        if op == "T*":
            # T* moves to the start of the next line using the current leading.
            tx = 0.0
            ty -= line_leading
            operand_stack.clear()
            continue

        operand_stack.clear()

    return _clean_text("".join(text_parts))


def _emit_text(
    text: str,
    text_parts: list[str],
    tx: float,
    ty: float,
    prev_tx: float,
    prev_ty: float,
    prev_width: float,
    font_size: float,
) -> tuple[float, float, float]:
    """
    Append text to text_parts with a position-aware separator, return updated prev state.

    Uses the current and previous text positions to decide whether to insert a
    newline, a space, or nothing between the previous chunk and this one.
    """
    # One average character width, used as the word-gap threshold.
    char_width = font_size * 0.5
    # A forward X jump larger than ~3 characters, or any significant backward
    # X jump, signals a column boundary or line wrap rather than a word space.
    large_forward_threshold = char_width * 3.0
    large_backward_threshold = char_width * 2.0

    if text_parts:
        dy = abs(ty - prev_ty)
        if dy > font_size * 0.2:
            text_parts.append("\n")

        else:
            gap = tx - (prev_tx + prev_width)
            if gap >= large_forward_threshold or gap <= -large_backward_threshold:
                # Large forward jump on the same line: column wrap or line wrap.
                text_parts.append("\n")

            elif gap >= char_width * 0.3:
                text_parts.append(" ")

            # Negative or very small gap: continuation of same word, no separator.

    text_parts.append(text)
    new_width = len(text) * font_size * 0.5
    return tx, ty, new_width


def _resolve_font(doc: PDFDocument, resources: dict[str, Any], font_name: str) -> dict[str, Any]:
    """Resolve a font resource dictionary by name."""
    font_dict = resources.get("Font", {})

    if isinstance(font_dict, PDFObjectRef):
        font_dict = doc.get_object(font_dict)
    if not isinstance(font_dict, dict):
        return {}

    font_ref = font_dict.get(font_name)
    if font_ref is None:
        return {}

    font = doc.get_object(font_ref) if isinstance(font_ref, PDFObjectRef) else font_ref
    if not isinstance(font, dict):
        return {}

    # Resolve ToUnicode CMap if present
    to_unicode_ref = font.get("ToUnicode")
    if to_unicode_ref is not None:
        to_unicode_stream = doc.get_object(to_unicode_ref) if isinstance(to_unicode_ref, PDFObjectRef) else to_unicode_ref
        if isinstance(to_unicode_stream, PDFStream):
            cmap = _parse_to_unicode_cmap(to_unicode_stream.data)
            return {"ToUnicode": cmap, "Encoding": font.get("Encoding")}

    return {"Encoding": font.get("Encoding")}


def _get_font(font_cache: dict[str, dict[str, Any]], font_name: str) -> dict[str, Any]:
    """Return the cached font info, or an empty dict if not found."""
    return font_cache.get(font_name, {})


def _decode_string(value: Any, font: dict[str, Any]) -> str:
    """Decode a PDF string value to Unicode text using font information."""
    if isinstance(value, str):
        return value

    if not isinstance(value, bytes):
        return ""

    to_unicode = font.get("ToUnicode")
    if isinstance(to_unicode, dict):
        return _apply_to_unicode(value, to_unicode)

    encoding = font.get("Encoding")
    if encoding == "MacRomanEncoding":
        return value.decode("mac_roman", errors="replace")

    if encoding in ("WinAnsiEncoding", "StandardEncoding", None):
        return _decode_pdf_doc_encoding(value)

    return _decode_pdf_doc_encoding(value)


def _decode_pdf_doc_encoding(data: bytes) -> str:
    """Decode bytes using PDFDocEncoding (close to latin-1 with some differences)."""
    # For the common case this is equivalent to latin-1
    return data.decode("latin-1", errors="replace")


def _apply_to_unicode(data: bytes, cmap: dict[bytes, str]) -> str:
    """
    Apply a ToUnicode CMap to decode a byte string.

    Bytes that have no CMap entry are replaced with U+FFFD (REPLACEMENT CHARACTER)
    rather than being passed through as raw ASCII, since their true value is unknown.
    """
    result: list[str] = []
    i = 0
    while i < len(data):
        # Try 2-byte lookup first, then 1-byte
        if i + 1 < len(data):
            two = data[i:i + 2]
            if two in cmap:
                result.append(cmap[two])
                i += 2
                continue

        one = data[i:i + 1]
        if one in cmap:
            result.append(cmap[one])

        else:
            result.append("\uFFFD")

        i += 1

    return "".join(result)


def _parse_to_unicode_cmap(data: bytes) -> dict[bytes, str]:
    """Parse a ToUnicode CMap stream into a bytes→str mapping."""
    cmap: dict[bytes, str] = {}
    text = data.decode("latin-1", errors="replace")

    # Parse bfchar blocks only — avoid matching across block boundaries
    for block in re.finditer(r"beginbfchar\s*(.*?)\s*endbfchar", text, re.DOTALL):
        for match in re.finditer(r"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>", block.group(1)):
            try:
                src = bytes.fromhex(match.group(1))
                cmap[src] = _hex_to_unicode_str(match.group(2))

            except (ValueError, OverflowError):
                pass

    # Parse bfrange blocks only
    for block in re.finditer(r"beginbfrange\s*(.*?)\s*endbfrange", text, re.DOTALL):
        for match in re.finditer(r"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>", block.group(1)):
            try:
                start = int(match.group(1), 16)
                end = int(match.group(2), 16)
                dst_start = int(match.group(3), 16)
                src_len = len(match.group(1)) // 2
                for offset in range(end - start + 1):
                    src = (start + offset).to_bytes(src_len, "big")
                    cmap[src] = _LIGATURE_MAP.get(chr(dst_start + offset), chr(dst_start + offset))

            except (ValueError, OverflowError):
                pass

    return cmap


def _hex_to_unicode_str(hex_str: str) -> str:
    """
    Convert a CMap destination hex string to a Unicode string.

    Handles single codepoints (<0041> -> 'A') and multi-codepoint sequences
    (<00660069> -> 'fi') used for ligature mappings.

    """
    raw = bytes.fromhex(hex_str)
    if len(raw) % 2 != 0:
        raise ValueError(f"Odd-length hex sequence in CMap: {hex_str}")
    result = []

    for i in range(0, len(raw), 2):
        cp = (raw[i] << 8) | raw[i + 1]
        ch = chr(cp)
        result.append(_LIGATURE_MAP.get(ch, ch))
    return "".join(result)


def _clean_text(text: str) -> str:
    """Normalise whitespace in extracted text."""
    # Collapse runs of spaces (but preserve newlines)
    lines = text.split("\n")
    cleaned = []
    for line in lines:
        line = re.sub(r"[ \t]+", " ", line).strip()
        cleaned.append(line)

    # Remove runs of blank lines
    result = re.sub(r"\n{3,}", "\n\n", "\n".join(cleaned))
    return result.strip()
