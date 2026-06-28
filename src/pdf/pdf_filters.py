import base64
import zlib
from typing import Any

from pdf.pdf_errors import PDFParseError, PDFUnsupportedError


def decode_stream(data: bytes, filter_name: str, decode_parms: dict[str, Any] | None = None) -> bytes:
    """
    Decode a PDF stream using the named filter.

    Raises:
        PDFParseError: if the data is malformed for the given filter.
        PDFUnsupportedError: if the filter is not implemented.
    """
    if filter_name == "FlateDecode":
        return _decode_flate(data, decode_parms)

    if filter_name in ("JBIG2Decode", "CCITTFaxDecode", "DCTDecode", "JPXDecode"):
        # Image-only filters. We cannot decode the image data, but we must not
        # raise here: doing so would cause the entire containing object to be
        # silently dropped by the parser, which can prevent Form XObjects and
        # other non-image objects from loading. Return the raw bytes unchanged.
        return data

    if filter_name == "ASCIIHexDecode":
        return _decode_ascii_hex(data)

    if filter_name == "ASCII85Decode":
        return _decode_ascii85(data)

    raise PDFUnsupportedError(f"Unsupported stream filter: {filter_name}")


def decode_stream_filters(
    data: bytes,
    filters: list[str],
    decode_parms_list: list[dict[str, Any] | None]
) -> bytes:
    """
    Apply a chain of filters in sequence, as PDF allows.

    Raises:
        PDFParseError: if any stage produces malformed data.
        PDFUnsupportedError: if any filter is not implemented.
    """
    for filter_name, parms in zip(filters, decode_parms_list):
        data = decode_stream(data, filter_name, parms)

    return data


def _decode_flate(data: bytes, decode_parms: dict[str, Any] | None) -> bytes:
    """Decompress FlateDecode data, applying PNG predictor if specified."""
    try:
        decompressed = zlib.decompress(data)

    except zlib.error as e:
        raise PDFParseError(f"FlateDecode decompression failed: {e}") from e

    if decode_parms is None:
        return decompressed

    predictor = decode_parms.get("Predictor", 1)
    if predictor == 1:
        return decompressed

    if predictor >= 10:
        return _apply_png_predictor(decompressed, decode_parms)

    # Predictor 2 is TIFF predictor — not commonly needed for text PDFs
    raise PDFUnsupportedError(f"FlateDecode Predictor {predictor} is not supported")


def _apply_png_predictor(data: bytes, decode_parms: dict[str, Any]) -> bytes:
    """Undo PNG predictor filtering (Predictors 10-15) from a decompressed stream."""
    columns = decode_parms.get("Columns", 1)
    colors = decode_parms.get("Colors", 1)
    bits = decode_parms.get("BitsPerComponent", 8)

    # Bytes per pixel (rounded up)
    bytes_per_pixel = max(1, (colors * bits + 7) // 8)
    stride = (columns * colors * bits + 7) // 8

    # Each row is preceded by a 1-byte filter type
    row_size = stride + 1
    if len(data) % row_size != 0:
        raise PDFParseError(
            f"PNG predictor data length {len(data)} is not a multiple of row size {row_size}"
        )

    num_rows = len(data) // row_size
    output = bytearray()
    prev_row = bytes(stride)

    for i in range(num_rows):
        row_start = i * row_size
        filter_type = data[row_start]
        row = bytearray(data[row_start + 1: row_start + 1 + stride])

        if filter_type == 0:
            pass  # None

        elif filter_type == 1:
            for j in range(bytes_per_pixel, stride):
                row[j] = (row[j] + row[j - bytes_per_pixel]) & 0xFF

        elif filter_type == 2:
            for j in range(stride):
                row[j] = (row[j] + prev_row[j]) & 0xFF

        elif filter_type == 3:
            for j in range(stride):
                left = row[j - bytes_per_pixel] if j >= bytes_per_pixel else 0
                up = prev_row[j]
                row[j] = (row[j] + (left + up) // 2) & 0xFF

        elif filter_type == 4:
            for j in range(stride):
                left = row[j - bytes_per_pixel] if j >= bytes_per_pixel else 0
                up = prev_row[j]
                up_left = prev_row[j - bytes_per_pixel] if j >= bytes_per_pixel else 0
                row[j] = (row[j] + _paeth(left, up, up_left)) & 0xFF

        else:
            raise PDFParseError(f"Unknown PNG predictor filter type: {filter_type}")

        output.extend(row)
        prev_row = bytes(row)

    return bytes(output)


def _paeth(a: int, b: int, c: int) -> int:
    """Paeth predictor function as defined in the PNG specification."""
    p = a + b - c
    pa = abs(p - a)
    pb = abs(p - b)
    pc = abs(p - c)

    if pa <= pb and pa <= pc:
        return a

    if pb <= pc:
        return b

    return c


def _decode_ascii_hex(data: bytes) -> bytes:
    """Decode an ASCII hex-encoded PDF stream into raw bytes."""
    try:
        # Strip whitespace, stop at '>'
        hex_str = data.split(b">")[0]

        hex_str = hex_str.replace(b" ", b"").replace(b"\n", b"").replace(b"\r", b"").replace(b"\t", b"")
        return bytes.fromhex(hex_str.decode("ascii"))

    except (ValueError, UnicodeDecodeError) as e:
        raise PDFParseError(f"ASCIIHexDecode failed: {e}") from e


def _decode_ascii85(data: bytes) -> bytes:
    """Decode an ASCII85-encoded PDF stream into raw bytes."""
    try:
        return base64.a85decode(data, adobe=True, ignorechars=b" \t\n\r")

    except ValueError as e:
        raise PDFParseError(f"ASCII85Decode failed: {e}") from e
