from typing import Any

from pdf.pdf_errors import PDFParseError, PDFUnsupportedError
from pdf.pdf_filters import decode_stream_filters
from pdf.pdf_tokenizer import PDFTokenizer, TokenType
from pdf.pdf_types import PDFDocument, PDFObjectRef, PDFStream, PDFXRefEntry


def _int_value(value: object) -> int:
    """Extract an int from a token value that mypy types as object."""
    assert isinstance(value, int)
    return value


def parse(data: bytes) -> PDFDocument:
    """
    Parse a PDF byte stream into a PDFDocument.

    Raises:
        PDFParseError: if the file structure is invalid.
        PDFUnsupportedError: if the file uses unsupported features (e.g. encryption).
    """
    if not data.startswith(b"%PDF-"):
        raise PDFParseError("Not a PDF file: missing %PDF- header")

    doc = PDFDocument()
    tokenizer = PDFTokenizer(data)

    xref_offset = _find_xref_offset(data)
    _load_xref(tokenizer, doc, xref_offset)

    if "Encrypt" in doc.trailer:
        raise PDFUnsupportedError("Encrypted PDFs are not supported")

    _load_objects(tokenizer, doc)

    return doc


def _find_xref_offset(data: bytes) -> int:
    """Locate the startxref offset by scanning backwards from the end of the file."""
    # Search in the last 1024 bytes for 'startxref'
    tail = data[-1024:]
    idx = tail.rfind(b"startxref")
    if idx == -1:
        raise PDFParseError("Cannot find 'startxref' in PDF")

    tokenizer = PDFTokenizer(tail)
    tokenizer.pos = idx + len(b"startxref")
    tokenizer.skip_whitespace_and_comments()

    token = tokenizer.next_token()
    if token.type != TokenType.INTEGER:
        raise PDFParseError("'startxref' not followed by an integer offset")

    return _int_value(token.value)


def _load_xref(tokenizer: PDFTokenizer, doc: PDFDocument, offset: int) -> None:
    """
    Load cross-reference table(s) and trailer(s) starting from the given offset.

    Handles chained xref tables via the /Prev trailer entry.
    """
    visited: set[int] = set()

    while offset not in visited:
        visited.add(offset)
        tokenizer.pos = offset
        tokenizer.skip_whitespace_and_comments()

        # Peek at the first token to decide between traditional xref and xref stream
        saved_pos = tokenizer.pos
        token = tokenizer.next_token()

        if token.type == TokenType.KEYWORD and token.value == "xref":
            _parse_xref_table(tokenizer, doc)
            trailer = _parse_trailer_dict(tokenizer)

        elif token.type == TokenType.INTEGER:
            # Could be an xref stream object: "N G obj"
            tokenizer.pos = saved_pos
            trailer = _parse_xref_stream(tokenizer, doc)

        else:
            raise PDFParseError(f"Unexpected token at xref offset {offset}: {token}")

        # Merge trailer — earlier (chained) trailers don't override the first one
        for key, value in trailer.items():
            if key not in doc.trailer:
                doc.trailer[key] = value

        prev = trailer.get("Prev")
        if isinstance(prev, int):
            offset = prev

        else:
            break


def _parse_xref_table(tokenizer: PDFTokenizer, doc: PDFDocument) -> None:
    """Parse a traditional cross-reference table."""
    while True:
        tokenizer.skip_whitespace_and_comments()
        saved = tokenizer.pos
        token = tokenizer.next_token()

        if token.type == TokenType.KEYWORD and token.value == "trailer":
            tokenizer.pos = saved  # leave 'trailer' for _parse_trailer_dict
            return

        if token.type != TokenType.INTEGER:
            tokenizer.pos = saved
            return

        first_obj = _int_value(token.value)
        count_token = tokenizer.next_token()
        if count_token.type != TokenType.INTEGER:
            raise PDFParseError("Invalid xref subsection header")

        count = _int_value(count_token.value)

        # Consume the rest of the subsection header line before reading entries
        tokenizer.read_line()

        for i in range(count):
            line = tokenizer.read_line()
            parts = line.split()
            if len(parts) < 3:
                continue

            try:
                entry_offset = int(parts[0])
                gen_num = int(parts[1])
                in_use = parts[2] == b"n"

            except (ValueError, IndexError):
                continue

            obj_num = first_obj + i
            # Earlier xref tables (from later /Prev chains) take precedence
            if obj_num not in doc.xref:
                doc.xref[obj_num] = PDFXRefEntry(offset=entry_offset, gen_num=gen_num, in_use=in_use)


def _parse_trailer_dict(tokenizer: PDFTokenizer) -> dict[str, Any]:
    """Parse the 'trailer <<...>>' block and return the dictionary."""
    tokenizer.skip_whitespace_and_comments()
    token = tokenizer.next_token()
    if not (token.type == TokenType.KEYWORD and token.value == "trailer"):
        raise PDFParseError("Expected 'trailer' keyword")

    tokenizer.skip_whitespace_and_comments()
    value = _parse_value(tokenizer)
    if not isinstance(value, dict):
        raise PDFParseError("Trailer is not a dictionary")

    return value


def _parse_xref_stream(tokenizer: PDFTokenizer, doc: PDFDocument) -> dict[str, Any]:
    """Parse a cross-reference stream object (PDF 1.5+)."""
    obj_num_token = tokenizer.next_token()
    gen_token = tokenizer.next_token()
    obj_kw = tokenizer.next_token()

    if (obj_num_token.type != TokenType.INTEGER or gen_token.type != TokenType.INTEGER
            or obj_kw.type != TokenType.OBJ_KW):
        raise PDFParseError("Invalid xref stream object header")

    stream = _parse_object_body(tokenizer)
    if not isinstance(stream, PDFStream):
        raise PDFParseError("Xref stream object does not contain a stream")

    attrs = stream.attrs
    w_entry = attrs.get("W")
    index_entry = attrs.get("Index")
    size = attrs.get("Size", 0)

    if not isinstance(w_entry, list) or len(w_entry) != 3:
        raise PDFUnsupportedError("Xref stream W entry must be a 3-element array")

    w = [int(x) for x in w_entry]
    if index_entry is None:
        index_pairs = [(0, int(size))]

    else:
        pairs_flat = [int(x) for x in index_entry]
        index_pairs = [(pairs_flat[i], pairs_flat[i + 1]) for i in range(0, len(pairs_flat), 2)]

    entry_size = sum(w)
    data = stream.data
    pos = 0

    for first_obj, count in index_pairs:
        for i in range(count):
            if pos + entry_size > len(data):
                break

            fields = []
            for width in w:
                if width == 0:
                    fields.append(0)

                else:
                    val = 0

                    for byte in data[pos:pos + width]:
                        val = (val << 8) | byte

                    fields.append(val)
                    pos += width

            entry_type = fields[0] if w[0] > 0 else 1
            obj_num = first_obj + i

            if obj_num in doc.xref:
                continue

            if entry_type == 0:
                doc.xref[obj_num] = PDFXRefEntry(offset=fields[1], gen_num=fields[2], in_use=False)

            elif entry_type == 1:
                doc.xref[obj_num] = PDFXRefEntry(offset=fields[1], gen_num=fields[2], in_use=True)

            elif entry_type == 2:
                # Compressed object: field1=stream obj num, field2=index within stream
                # Store as negative offset to distinguish from regular offsets
                doc.xref[obj_num] = PDFXRefEntry(offset=-(fields[1] * 65536 + fields[2]), gen_num=0, in_use=True)

    return dict(attrs)


def _load_objects(tokenizer: PDFTokenizer, doc: PDFDocument) -> None:
    """Load all in-use indirect objects referenced by the xref table."""
    for obj_num, entry in doc.xref.items():
        if not entry.in_use:
            continue

        if entry.offset < 0:
            # Compressed object — handled lazily via object streams
            continue

        try:
            tokenizer.pos = entry.offset
            _parse_indirect_object(tokenizer, doc, obj_num)

        except (PDFParseError, PDFUnsupportedError):
            pass  # Skip unreadable objects; partial extraction is better than none

        except Exception:  # pylint: disable=broad-except
            pass

    # Second pass: resolve object streams (type 2 xref entries)
    _load_object_streams(doc)


def _parse_indirect_object(tokenizer: PDFTokenizer, doc: PDFDocument, expected_obj_num: int) -> None:
    """Parse one indirect object at the current tokenizer position."""
    tokenizer.skip_whitespace_and_comments()
    obj_num_token = tokenizer.next_token()
    _ = tokenizer.next_token()
    obj_kw = tokenizer.next_token()

    if obj_num_token.type != TokenType.INTEGER or obj_kw.type != TokenType.OBJ_KW:
        return

    value = _parse_object_body(tokenizer)
    doc.objects[expected_obj_num] = value


def _parse_object_body(tokenizer: PDFTokenizer) -> Any:
    """Parse the value of an indirect object, including an optional stream."""
    value = _parse_value(tokenizer)

    tokenizer.skip_whitespace_and_comments()
    saved = tokenizer.pos
    token = tokenizer.next_token()

    if token.type == TokenType.STREAM_KW:
        if not isinstance(value, dict):
            raise PDFParseError("'stream' keyword without preceding dictionary")

        stream_data = _read_stream_data(tokenizer, value)
        return PDFStream(data=stream_data, attrs=value)

    tokenizer.pos = saved
    return value


def _read_stream_data(tokenizer: PDFTokenizer, attrs: dict[str, Any]) -> bytes:
    """Read and decode the raw bytes of a stream."""
    # The stream data starts after the newline following 'stream'
    data = tokenizer._data  # pylint: disable=protected-access
    pos = tokenizer.pos

    # Consume the line ending after 'stream'
    if pos < len(data) and data[pos] == ord("\r"):
        pos += 1

    if pos < len(data) and data[pos] == ord("\n"):
        pos += 1

    length = attrs.get("Length")
    if isinstance(length, PDFObjectRef):
        # We can't resolve this yet — fall back to scanning for endstream
        length = None

    if isinstance(length, int) and length >= 0:
        raw = data[pos:pos + length]
        tokenizer.pos = pos + length

    else:
        end = tokenizer.find(b"endstream", pos)
        if end == -1:
            raise PDFParseError("Cannot find 'endstream' marker")

        raw = data[pos:end]
        tokenizer.pos = end

    # Decode filters
    filters_raw = attrs.get("Filter")
    parms_raw = attrs.get("DecodeParms")

    if filters_raw is None:
        return raw

    if isinstance(filters_raw, str):
        filters = [filters_raw]
        parms_list: list[dict[str, Any] | None] = [parms_raw if isinstance(parms_raw, dict) else None]

    elif isinstance(filters_raw, list):
        filters = [str(f) for f in filters_raw]
        if isinstance(parms_raw, list):
            parms_list = [p if isinstance(p, dict) else None for p in parms_raw]

        else:
            parms_list = [None] * len(filters)

    else:
        return raw

    return decode_stream_filters(raw, filters, parms_list)


def _load_object_streams(doc: PDFDocument) -> None:
    """Resolve type-2 xref entries by parsing object streams."""
    # Collect which objects live in which object stream
    stream_map: dict[int, list[tuple[int, int]]] = {}  # stream_obj_num -> [(obj_num, index)]

    for obj_num, entry in doc.xref.items():
        if entry.in_use and entry.offset < 0:
            packed = -entry.offset
            stream_obj_num = packed // 65536
            index_in_stream = packed % 65536
            stream_map.setdefault(stream_obj_num, []).append((obj_num, index_in_stream))

    for stream_obj_num, _ in stream_map.items():
        stream_obj = doc.objects.get(stream_obj_num)
        if not isinstance(stream_obj, PDFStream):
            continue

        attrs = stream_obj.attrs
        if attrs.get("Type") != "ObjStm":
            continue

        n = int(attrs.get("N", 0))
        first = int(attrs.get("First", 0))

        stm_data = stream_obj.data
        header_tokenizer = PDFTokenizer(stm_data[:first])
        offsets: list[tuple[int, int]] = []

        for _ in range(n):
            num_tok = header_tokenizer.next_token()
            off_tok = header_tokenizer.next_token()
            if num_tok.type == TokenType.INTEGER and off_tok.type == TokenType.INTEGER:
                offsets.append((_int_value(num_tok.value), _int_value(off_tok.value)))

        body_data = stm_data[first:]
        body_tokenizer = PDFTokenizer(body_data)

        for embedded_obj_num, offset in offsets:
            if embedded_obj_num in doc.objects:
                continue

            body_tokenizer.pos = offset
            try:
                value = _parse_value(body_tokenizer)
                doc.objects[embedded_obj_num] = value

            except Exception:  # pylint: disable=broad-except
                pass


def _parse_value(tokenizer: PDFTokenizer) -> Any:
    """Parse a single PDF value (recursive for arrays and dicts)."""
    tokenizer.skip_whitespace_and_comments()
    token = tokenizer.next_token()

    if token.type == TokenType.NULL:
        return None

    if token.type == TokenType.BOOLEAN:
        return token.value

    if token.type == TokenType.INTEGER:
        # Could be the start of an indirect reference: "N G R"
        saved = tokenizer.pos
        gen_token = tokenizer.next_token()
        if gen_token.type == TokenType.INTEGER:
            ref_token = tokenizer.next_token()
            if ref_token.type == TokenType.REF_KW:
                return PDFObjectRef(obj_num=_int_value(token.value), gen_num=_int_value(gen_token.value))

            tokenizer.pos = saved

        else:
            tokenizer.pos = saved

        return token.value

    if token.type == TokenType.REAL:
        return token.value

    if token.type in (TokenType.STRING, TokenType.HEX_STRING):
        return token.value

    if token.type == TokenType.NAME:
        return token.value

    if token.type == TokenType.ARRAY_START:
        return _parse_array(tokenizer)

    if token.type == TokenType.DICT_START:
        return _parse_dict(tokenizer)

    return token.value


def _parse_array(tokenizer: PDFTokenizer) -> list[Any]:
    """Parse a PDF array, assuming '[' has already been consumed."""
    result: list[Any] = []

    while True:
        tokenizer.skip_whitespace_and_comments()
        saved = tokenizer.pos
        token = tokenizer.next_token()

        if token.type in (TokenType.ARRAY_END, TokenType.EOF):
            break

        tokenizer.pos = saved
        result.append(_parse_value(tokenizer))

    return result


def _parse_dict(tokenizer: PDFTokenizer) -> dict[str, Any]:
    """Parse a PDF dictionary, assuming '<<' has already been consumed."""
    result: dict[str, Any] = {}

    while True:
        tokenizer.skip_whitespace_and_comments()
        token = tokenizer.next_token()

        if token.type in (TokenType.DICT_END, TokenType.EOF):
            break

        if token.type != TokenType.NAME:
            # Malformed dict — skip and try to recover
            continue

        key = str(token.value)
        value = _parse_value(tokenizer)
        result[key] = value

    return result
