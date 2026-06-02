import zlib

import pytest

from pdf.pdf_errors import PDFParseError, PDFUnsupportedError
from pdf.pdf_filters import decode_stream, decode_stream_filters


class TestASCIIHexDecode:
    def test_basic(self) -> None:
        assert decode_stream(b"48656c6c6f>", "ASCIIHexDecode") == b"Hello"

    def test_uppercase(self) -> None:
        assert decode_stream(b"48656C6C6F>", "ASCIIHexDecode") == b"Hello"

    def test_with_whitespace(self) -> None:
        assert decode_stream(b"48 65 6c 6c 6f>", "ASCIIHexDecode") == b"Hello"

    def test_empty(self) -> None:
        assert decode_stream(b">", "ASCIIHexDecode") == b""

    def test_malformed(self) -> None:
        with pytest.raises(PDFParseError):
            decode_stream(b"ZZ>", "ASCIIHexDecode")


class TestASCII85Decode:
    def test_basic(self) -> None:
        # "Man " encoded in ASCII85 with adobe framing
        encoded = b"<~9jqo^~>"
        result = decode_stream(encoded, "ASCII85Decode")
        assert result == b"Man "

    def test_malformed(self) -> None:
        # 'v' is not a valid ASCII85 character
        with pytest.raises(PDFParseError):
            decode_stream(b"<~vvvv~>", "ASCII85Decode")


class TestFlateDecode:
    def test_basic(self) -> None:
        original = b"Hello, world!"
        compressed = zlib.compress(original)
        assert decode_stream(compressed, "FlateDecode") == original

    def test_no_predictor(self) -> None:
        data = b"Some PDF content stream data"
        compressed = zlib.compress(data)
        result = decode_stream(compressed, "FlateDecode", {"Predictor": 1})
        assert result == data

    def test_malformed(self) -> None:
        with pytest.raises(PDFParseError):
            decode_stream(b"not compressed data", "FlateDecode")

    def test_png_predictor_up(self) -> None:
        # Predictor 2 (Up filter): each byte is delta from byte above
        # 2 rows, 3 columns, 1 color, 8 bits
        # Row 0: filter=2(Up), bytes=[10, 20, 30]  -> decoded [10, 20, 30]
        # Row 1: filter=2(Up), bytes=[1, 2, 3]    -> decoded [11, 22, 33]
        raw = bytes([2, 10, 20, 30, 2, 1, 2, 3])
        compressed = zlib.compress(raw)
        result = decode_stream(compressed, "FlateDecode", {
            "Predictor": 12, "Columns": 3, "Colors": 1, "BitsPerComponent": 8
        })
        assert result == bytes([10, 20, 30, 11, 22, 33])

    def test_png_predictor_none(self) -> None:
        # Predictor 0 (None): row is passed through unchanged
        raw = bytes([0, 1, 2, 3, 0, 4, 5, 6])
        compressed = zlib.compress(raw)
        result = decode_stream(compressed, "FlateDecode", {
            "Predictor": 10, "Columns": 3, "Colors": 1, "BitsPerComponent": 8
        })
        assert result == bytes([1, 2, 3, 4, 5, 6])

    def test_unsupported_predictor(self) -> None:
        data = zlib.compress(b"hello")
        with pytest.raises(PDFUnsupportedError):
            decode_stream(data, "FlateDecode", {"Predictor": 2})


class TestUnsupportedFilter:
    def test_lzw(self) -> None:
        with pytest.raises(PDFUnsupportedError):
            decode_stream(b"data", "LZWDecode")

    def test_unknown(self) -> None:
        with pytest.raises(PDFUnsupportedError):
            decode_stream(b"data", "SomeFutureFilter")


class TestImageOnlyFilters:
    def test_jbig2decode_returns_raw(self) -> None:
        raw = b"\x00\x01\x02\x03some image data"
        assert decode_stream(raw, "JBIG2Decode") == raw

    def test_ccittfaxdecode_returns_raw(self) -> None:
        raw = b"\xff\xd8\xff some fax data"
        assert decode_stream(raw, "CCITTFaxDecode") == raw

    def test_dctdecode_returns_raw(self) -> None:
        raw = b"\xff\xd8\xff\xe0 jpeg data"
        assert decode_stream(raw, "DCTDecode") == raw

    def test_jpxdecode_returns_raw(self) -> None:
        raw = b"\x00\x00\x00\x0cjP  jpeg2000 data"
        assert decode_stream(raw, "JPXDecode") == raw


class TestDecodeStreamFilters:
    def test_single_filter(self) -> None:
        original = b"test data"
        compressed = zlib.compress(original)
        result = decode_stream_filters(compressed, ["FlateDecode"], [None])
        assert result == original

    def test_chained_filters(self) -> None:
        original = b"Hello"
        # ASCII85 then FlateDecode (contrived but valid chain)
        import base64
        ascii85 = base64.a85encode(original, adobe=True)
        compressed = zlib.compress(ascii85)
        result = decode_stream_filters(compressed, ["FlateDecode", "ASCII85Decode"], [None, None])
        assert result == original

    def test_empty_chain(self) -> None:
        data = b"unchanged"
        result = decode_stream_filters(data, [], [])
        assert result == data
