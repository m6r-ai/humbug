"""Tests for the bytes type — construction, conversion, access, and comparison."""

import pytest

from menai import Menai, MenaiEvalError, VMErrorCode


class TestBytesTypePredicate:
    """Test bytes? type predicate."""

    @pytest.mark.parametrize("expr,expected", [
        ('(bytes? (string-hex->bytes "504b0304"))', True),
        ('(bytes? (string->bytes "hello"))', True),
        ('(bytes? (string-hex->bytes ""))', True),
        ('(bytes? "hello")', False),
        ('(bytes? 42)', False),
        ('(bytes? (list 1 2 3))', False),
        ('(bytes? #t)', False),
        ('(bytes? #none)', False),
    ])
    def test_bytes_predicate(self, menai, expr, expected):
        """bytes? correctly identifies bytes values."""
        assert menai.evaluate(expr) == expected


class TestBytesConstruction:
    """Test bytes construction from hex, strings, and lists."""

    @pytest.mark.parametrize("hex_str,expected_hex", [
        ("504b0304", "504b0304"),
        ("", ""),
        ("00", "00"),
        ("ff", "ff"),
        ("deadbeef", "deadbeef"),
        ("0123456789abcdef", "0123456789abcdef"),
    ])
    def test_hex_round_trip(self, menai, hex_str, expected_hex):
        """string-hex->bytes and bytes->string-hex round-trip correctly."""
        result = menai.evaluate(f'(bytes->string-hex (string-hex->bytes "{hex_str}"))')
        assert result == expected_hex

    @pytest.mark.parametrize("text,expected_hex", [
        ("hello", "68656c6c6f"),
        ("", ""),
        ("A", "41"),
        ("AB", "4142"),
        ("cafe", "63616665"),
    ])
    def test_string_to_bytes_utf8(self, menai, text, expected_hex):
        """string->bytes encodes as UTF-8."""
        result = menai.evaluate(f'(bytes->string-hex (string->bytes "{text}"))')
        assert result == expected_hex

    @pytest.mark.parametrize("hex_str,expected_text", [
        ("68656c6c6f", "hello"),
        ("", ""),
        ("41", "A"),
        ("4142", "AB"),
    ])
    def test_bytes_to_string_utf8(self, menai, hex_str, expected_text):
        """bytes->string decodes UTF-8."""
        result = menai.evaluate(f'(bytes->string (string-hex->bytes "{hex_str}"))')
        assert result == expected_text

    def test_string_to_bytes_unicode(self, menai):
        """string->bytes correctly encodes multi-byte UTF-8."""
        # cafe with accents: c3 a9 c3 a0
        assert menai.evaluate('(bytes->string-hex (string->bytes "caf\u00e9"))') == "636166c3a9"
        # Greek alpha: ce b1
        assert menai.evaluate('(bytes->string-hex (string->bytes "\u03b1"))') == "ceb1"
        # Emoji: f0 9f 91 8b
        assert menai.evaluate('(bytes->string-hex (string->bytes "\U0001f44b"))') == "f09f918b"

    def test_bytes_to_string_unicode(self, menai):
        """bytes->string correctly decodes multi-byte UTF-8."""
        assert menai.evaluate('(bytes->string (string-hex->bytes "636166c3a9"))') == "caf\u00e9"
        assert menai.evaluate('(bytes->string (string-hex->bytes "ceb1"))') == "\u03b1"
        assert menai.evaluate('(bytes->string (string-hex->bytes "f09f918b"))') == "\U0001f44b"

    def test_bytes_to_string_invalid_utf8(self, menai):
        """bytes->string raises error on invalid UTF-8."""
        with pytest.raises(MenaiEvalError, match="invalid UTF-8"):
            menai.evaluate('(bytes->string (string-hex->bytes "ff"))')

        with pytest.raises(MenaiEvalError, match="invalid UTF-8"):
            menai.evaluate('(bytes->string (string-hex->bytes "c0"))')

        with pytest.raises(MenaiEvalError, match="invalid UTF-8"):
            menai.evaluate('(bytes->string (string-hex->bytes "e0a0"))')

    @pytest.mark.parametrize("hex_str", [
        "5",       # Odd length
        "gg",      # Invalid hex char
        "5g",      # Mixed valid/invalid
    ])
    def test_string_hex_to_bytes_invalid(self, menai, hex_str):
        """string-hex->bytes raises error on invalid hex."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(string-hex->bytes "{hex_str}")')

    @pytest.mark.parametrize("integers,expected_hex", [
        ([80, 75, 3, 4], "504b0304"),
        ([0], "00"),
        ([255], "ff"),
        ([], ""),
        ([1, 2, 3], "010203"),
    ])
    def test_list_to_bytes(self, menai, integers, expected_hex):
        """list->bytes constructs bytes from a list of integers."""
        lst = "(list " + " ".join(str(i) for i in integers) + ")"
        result = menai.evaluate(f'(bytes->string-hex (list->bytes {lst}))')
        assert result == expected_hex

    def test_list_to_bytes_out_of_range(self, menai):
        """list->bytes raises error on values outside 0-255."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate('(list->bytes (list 256))')

        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate('(list->bytes (list -1))')

    def test_list_to_bytes_non_integer(self, menai):
        """list->bytes raises error on non-integer elements."""
        with pytest.raises(MenaiEvalError, match="must be integers"):
            menai.evaluate('(list->bytes (list "a"))')


class TestBytesLength:
    """Test bytes-length."""

    @pytest.mark.parametrize("hex_str,expected", [
        ("", 0),
        ("50", 1),
        ("504b", 2),
        ("504b0304", 4),
        ("0102030405060708", 8),
    ])
    def test_bytes_length(self, menai, hex_str, expected):
        """bytes-length returns the correct byte count."""
        assert menai.evaluate(f'(bytes-length (string-hex->bytes "{hex_str}"))') == expected


class TestBytesRef:
    """Test bytes-ref."""

    @pytest.mark.parametrize("hex_str,offset,expected", [
        ("504b0304", 0, 80),
        ("504b0304", 1, 75),
        ("504b0304", 2, 3),
        ("504b0304", 3, 4),
        ("ff", 0, 255),
        ("00", 0, 0),
    ])
    def test_bytes_ref(self, menai, hex_str, offset, expected):
        """bytes-ref returns the byte value at the given offset."""
        assert menai.evaluate(f'(bytes-ref (string-hex->bytes "{hex_str}") {offset})') == expected

    @pytest.mark.parametrize("offset", [-1, 4, 100])
    def test_bytes_ref_out_of_bounds(self, menai, offset):
        """bytes-ref raises error on out-of-bounds offset."""
        with pytest.raises(MenaiEvalError, match="out of bounds"):
            menai.evaluate(f'(bytes-ref (string-hex->bytes "504b0304") {offset})')


class TestBytesAppendU8:
    """Test bytes-append-u8."""

    @pytest.mark.parametrize("hex_str,value,expected_hex", [
        ("504b", 3, "504b03"),
        ("", 255, "ff"),
        ("00", 1, "0001"),
        ("504b0304", 0, "504b030400"),
    ])
    def test_bytes_append_u8(self, menai, hex_str, value, expected_hex):
        """bytes-append-u8 appends a single byte."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u8 (string-hex->bytes "{hex_str}") {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value", [-1, 256, 1000])
    def test_bytes_append_u8_out_of_range(self, menai, value):
        """bytes-append-u8 raises error on values outside 0-255."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-u8 (string-hex->bytes "504b") {value})')


class TestBytesSlice:
    """Test bytes-slice."""

    @pytest.mark.parametrize("hex_str,start,end,expected", [
        ("504b0304", 0, 4, "504b0304"),
        ("504b0304", 0, 2, "504b"),
        ("504b0304", 2, 4, "0304"),
        ("504b0304", 1, 3, "4b03"),
        ("504b0304", 0, 0, ""),
        ("504b0304", 4, 4, ""),
    ])
    def test_bytes_slice_with_end(self, menai, hex_str, start, end, expected):
        """bytes-slice with explicit end offset."""
        result = menai.evaluate(
            f'(bytes->string-hex (bytes-slice (string-hex->bytes "{hex_str}") {start} {end}))'
        )
        assert result == expected

    @pytest.mark.parametrize("hex_str,start,expected", [
        ("504b0304", 0, "504b0304"),
        ("504b0304", 2, "0304"),
        ("504b0304", 4, ""),
    ])
    def test_bytes_slice_without_end(self, menai, hex_str, start, expected):
        """bytes-slice with omitted end defaults to end of bytes."""
        result = menai.evaluate(
            f'(bytes->string-hex (bytes-slice (string-hex->bytes "{hex_str}") {start}))'
        )
        assert result == expected

    def test_bytes_slice_clamping(self, menai):
        """bytes-slice clamps start and end to valid range."""
        assert menai.evaluate('(bytes->string-hex (bytes-slice (string-hex->bytes "504b0304") -1 2))') == "504b"
        assert menai.evaluate('(bytes->string-hex (bytes-slice (string-hex->bytes "504b0304") 0 100))') == "504b0304"
        assert menai.evaluate('(bytes->string-hex (bytes-slice (string-hex->bytes "504b0304") 3 1))') == ""


class TestBytesConcat:
    """Test bytes-concat."""

    @pytest.mark.parametrize("parts,expected", [
        (["504b", "0304"], "504b0304"),
        (["", ""], ""),
        (["504b", ""], "504b"),
        (["", "504b"], "504b"),
    ])
    def test_bytes_concat_binary(self, menai, parts, expected):
        """bytes-concat with two arguments."""
        expr = f'(bytes-concat (string-hex->bytes "{parts[0]}") (string-hex->bytes "{parts[1]}"))'
        assert menai.evaluate(f'(bytes->string-hex {expr})') == expected

    def test_bytes_concat_variadic(self, menai):
        """bytes-concat with variadic arguments."""
        expr = '(bytes-concat (string-hex->bytes "de") (string-hex->bytes "ad") (string-hex->bytes "beef"))'
        assert menai.evaluate(f'(bytes->string-hex {expr})') == "deadbeef"

    def test_bytes_concat_empty(self, menai):
        """bytes-concat with zero arguments returns empty bytes."""
        assert menai.evaluate('(bytes->string-hex (bytes-concat))') == ""

    def test_bytes_concat_single(self, menai):
        """bytes-concat with one argument returns it unchanged."""
        assert menai.evaluate('(bytes->string-hex (bytes-concat (string-hex->bytes "504b")))') == "504b"


class TestBytesToList:
    """Test bytes->list."""

    @pytest.mark.parametrize("hex_str,expected", [
        ("504b", [80, 75]),
        ("", []),
        ("ff", [255]),
        ("010203", [1, 2, 3]),
    ])
    def test_bytes_to_list(self, menai, hex_str, expected):
        """bytes->list converts bytes to a list of integers."""
        assert menai.evaluate(f'(bytes->list (string-hex->bytes "{hex_str}"))') == expected


class TestBytesEquality:
    """Test bytes=? and bytes!=?."""

    @pytest.mark.parametrize("a,b,expected", [
        ("504b0304", "504b0304", True),
        ("504b0304", "504b0305", False),
        ("", "", True),
        ("504b", "504b0304", False),
        ("deadbeef", "deadbeef", True),
    ])
    def test_bytes_eq(self, menai, a, b, expected):
        """bytes=? compares byte sequences."""
        expr = f'(bytes=? (string-hex->bytes "{a}") (string-hex->bytes "{b}"))'
        assert menai.evaluate(expr) == expected

    @pytest.mark.parametrize("a,b,expected", [
        ("504b0304", "504b0304", False),
        ("504b0304", "504b0305", True),
        ("", "", False),
        ("504b", "504b0304", True),
    ])
    def test_bytes_neq(self, menai, a, b, expected):
        """bytes!=? returns negation of bytes=?."""
        expr = f'(bytes!=? (string-hex->bytes "{a}") (string-hex->bytes "{b}"))'
        assert menai.evaluate(expr) == expected

    def test_bytes_eq_variadic(self, menai):
        """bytes=? with variadic arguments checks all equal."""
        assert menai.evaluate('(bytes=? (string-hex->bytes "ab") (string-hex->bytes "ab") (string-hex->bytes "ab"))') is True
        assert menai.evaluate('(bytes=? (string-hex->bytes "ab") (string-hex->bytes "ab") (string-hex->bytes "cd"))') is False

    def test_bytes_eq_min_args(self, menai):
        """bytes=? requires at least 2 arguments."""
        with pytest.raises(MenaiEvalError, match="bytes=.*has wrong number of arguments"):
            menai.evaluate('(bytes=? (string-hex->bytes "ab"))')


class TestBytesComparison:
    """Test bytes<? bytes>? bytes<=? bytes>=?."""

    @pytest.mark.parametrize("a,b,expected", [
        ("504b", "504c", True),
        ("504c", "504b", False),
        ("504b", "504b", False),
        ("504b", "504b0304", True),
        ("504b0304", "504b", False),
        ("", "504b", True),
        ("504b", "", False),
    ])
    def test_bytes_lt(self, menai, a, b, expected):
        """bytes<? lexicographic comparison."""
        expr = f'(bytes<? (string-hex->bytes "{a}") (string-hex->bytes "{b}"))'
        assert menai.evaluate(expr) == expected

    @pytest.mark.parametrize("a,b,expected", [
        ("504c", "504b", True),
        ("504b", "504c", False),
        ("504b", "504b", False),
        ("504b0304", "504b", True),
        ("504b", "504b0304", False),
    ])
    def test_bytes_gt(self, menai, a, b, expected):
        """bytes>? lexicographic comparison."""
        expr = f'(bytes>? (string-hex->bytes "{a}") (string-hex->bytes "{b}"))'
        assert menai.evaluate(expr) == expected

    @pytest.mark.parametrize("a,b,expected", [
        ("504b", "504c", True),
        ("504b", "504b", True),
        ("504c", "504b", False),
    ])
    def test_bytes_lte(self, menai, a, b, expected):
        """bytes<=? lexicographic comparison."""
        expr = f'(bytes<=? (string-hex->bytes "{a}") (string-hex->bytes "{b}"))'
        assert menai.evaluate(expr) == expected

    @pytest.mark.parametrize("a,b,expected", [
        ("504c", "504b", True),
        ("504b", "504b", True),
        ("504b", "504c", False),
    ])
    def test_bytes_gte(self, menai, a, b, expected):
        """bytes>=? lexicographic comparison."""
        expr = f'(bytes>=? (string-hex->bytes "{a}") (string-hex->bytes "{b}"))'
        assert menai.evaluate(expr) == expected


class TestBytesIndex:
    """Test bytes-index and bytes-index-int."""

    @pytest.mark.parametrize("needle,haystack,expected", [
        ("0304", "504b0304", 2),
        ("504b", "504b0304", 0),
        ("504b0304", "504b0304", 0),
        ("ffff", "504b0304", None),
        ("", "504b0304", 0),
    ])
    def test_bytes_index(self, menai, needle, haystack, expected):
        """bytes-index finds the offset of the first occurrence."""
        result = menai.evaluate(f'(bytes-index (string-hex->bytes "{needle}") (string-hex->bytes "{haystack}"))')
        assert result == expected

    @pytest.mark.parametrize("byte_val,haystack,expected", [
        (75, "504b0304", 1),
        (80, "504b0304", 0),
        (4, "504b0304", 3),
        (255, "504b0304", None),
        (0, "00ff00", 0),
    ])
    def test_bytes_index_int(self, menai, byte_val, haystack, expected):
        """bytes-index-int finds the offset of the first matching byte."""
        result = menai.evaluate(f'(bytes-index-int {byte_val} (string-hex->bytes "{haystack}"))')
        assert result == expected

    def test_bytes_index_int_out_of_range(self, menai):
        """bytes-index-int raises error on values outside 0-255."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate('(bytes-index-int 256 (string-hex->bytes "504b"))')

        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate('(bytes-index-int -1 (string-hex->bytes "504b"))')


class TestBytesMultiByteReads:
    """Test multi-byte integer reads."""

    @pytest.mark.parametrize("hex_str,offset,expected", [
        ("50", 0, 80),
        ("ff", 0, 255),
        ("00", 0, 0),
    ])
    def test_bytes_read_u8(self, menai, hex_str, offset, expected):
        """bytes-read-u8 reads a single unsigned byte."""
        assert menai.evaluate(f'(bytes-read-u8 (string-hex->bytes "{hex_str}") {offset})') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ff", -1),
        ("7f", 127),
        ("80", -128),
        ("00", 0),
    ])
    def test_bytes_read_i8(self, menai, hex_str, expected):
        """bytes-read-i8 reads a single signed byte."""
        assert menai.evaluate(f'(bytes-read-i8 (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("4b50", 0x504b),
        ("ffff", 0xffff),
        ("0000", 0),
        ("0100", 0x0001),
    ])
    def test_bytes_read_u16_le(self, menai, hex_str, expected):
        """bytes-read-u16-le reads unsigned 16-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-u16-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("504b", 0x504b),
        ("ffff", 0xffff),
        ("0000", 0),
        ("0001", 0x0001),
    ])
    def test_bytes_read_u16_be(self, menai, hex_str, expected):
        """bytes-read-u16-be reads unsigned 16-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-u16-be (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("03040a", 0x0a0403),
        ("ffffff", 0xffffff),
        ("000000", 0),
    ])
    def test_bytes_read_u24_le(self, menai, hex_str, expected):
        """bytes-read-u24-le reads unsigned 24-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-u24-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("03040a", 0x03040a),
        ("ffffff", 0xffffff),
        ("000000", 0),
    ])
    def test_bytes_read_u24_be(self, menai, hex_str, expected):
        """bytes-read-u24-be reads unsigned 24-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-u24-be (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("04034b50", 0x504b0304),
        ("ffffffff", 0xffffffff),
        ("00000000", 0),
    ])
    def test_bytes_read_u32_le(self, menai, hex_str, expected):
        """bytes-read-u32-le reads unsigned 32-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-u32-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("504b0304", 0x504b0304),
        ("ffffffff", 0xffffffff),
        ("00000000", 0),
    ])
    def test_bytes_read_u32_be(self, menai, hex_str, expected):
        """bytes-read-u32-be reads unsigned 32-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-u32-be (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("0102030405060708", 0x0807060504030201),
        ("ffffffffffffffff", 0xffffffffffffffff),
        ("0000000000000000", 0),
    ])
    def test_bytes_read_u64_le(self, menai, hex_str, expected):
        """bytes-read-u64-le reads unsigned 64-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-u64-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("0102030405060708", 0x0102030405060708),
        ("ffffffffffffffff", 0xffffffffffffffff),
        ("0000000000000000", 0),
    ])
    def test_bytes_read_u64_be(self, menai, hex_str, expected):
        """bytes-read-u64-be reads unsigned 64-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-u64-be (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffff", -1),
        ("ff7f", 32767),
        ("0080", -32768),
        ("0000", 0),
    ])
    def test_bytes_read_i16_le(self, menai, hex_str, expected):
        """bytes-read-i16-le reads signed 16-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-i16-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffff", -1),
        ("7fff", 0x7fff),
        ("8000", -0x8000),
        ("0000", 0),
    ])
    def test_bytes_read_i16_be(self, menai, hex_str, expected):
        """bytes-read-i16-be reads signed 16-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-i16-be (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffffff", -1),
        ("ffff7f", 8388607),
        ("000080", -8388608),
    ])
    def test_bytes_read_i24_le(self, menai, hex_str, expected):
        """bytes-read-i24-le reads signed 24-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-i24-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffffff", -1),
        ("7fffff", 0x7fffff),
        ("800000", -0x800000),
    ])
    def test_bytes_read_i24_be(self, menai, hex_str, expected):
        """bytes-read-i24-be reads signed 24-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-i24-be (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffffffff", -1),
        ("ffffff7f", 2147483647),
        ("00000080", -2147483648),
    ])
    def test_bytes_read_i32_le(self, menai, hex_str, expected):
        """bytes-read-i32-le reads signed 32-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-i32-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffffffff", -1),
        ("7fffffff", 0x7fffffff),
        ("80000000", -0x80000000),
    ])
    def test_bytes_read_i32_be(self, menai, hex_str, expected):
        """bytes-read-i32-be reads signed 32-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-i32-be (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffffffffffffffff", -1),
        ("ffffffffffffff7f", 9223372036854775807),
        ("0000000000000080", -9223372036854775808),
    ])
    def test_bytes_read_i64_le(self, menai, hex_str, expected):
        """bytes-read-i64-le reads signed 64-bit little-endian."""
        assert menai.evaluate(f'(bytes-read-i64-le (string-hex->bytes "{hex_str}") 0)') == expected

    @pytest.mark.parametrize("hex_str,expected", [
        ("ffffffffffffffff", -1),
        ("7fffffffffffffff", 0x7fffffffffffffff),
        ("8000000000000000", -0x8000000000000000),
    ])
    def test_bytes_read_i64_be(self, menai, hex_str, expected):
        """bytes-read-i64-be reads signed 64-bit big-endian."""
        assert menai.evaluate(f'(bytes-read-i64-be (string-hex->bytes "{hex_str}") 0)') == expected

    def test_multi_byte_read_with_offset(self, menai):
        """Multi-byte reads work at non-zero offsets."""
        b = '(string-hex->bytes "504b03040a0b0c0d")'
        assert menai.evaluate(f'(bytes-read-u16-le {b} 2)') == 0x0403
        assert menai.evaluate(f'(bytes-read-u16-be {b} 2)') == 0x0304
        assert menai.evaluate(f'(bytes-read-u32-le {b} 4)') == 0x0d0c0b0a

    def test_multi_byte_read_out_of_bounds(self, menai):
        """Multi-byte reads raise error when not enough bytes."""
        with pytest.raises(MenaiEvalError, match="out of bounds"):
            menai.evaluate('(bytes-read-u16-le (string-hex->bytes "50") 0)')

        with pytest.raises(MenaiEvalError, match="out of bounds"):
            menai.evaluate('(bytes-read-u32-be (string-hex->bytes "504b") 0)')

        with pytest.raises(MenaiEvalError, match="out of bounds"):
            menai.evaluate('(bytes-read-u16-le (string-hex->bytes "504b0304") 3)')


class TestBytesDisplayFormat:
    """Test the describe/display format for bytes."""

    def test_display_short(self, menai):
        """describe produces #bytes\"hex\" format."""
        assert menai.evaluate_and_format('(string-hex->bytes "504b0304")') == '#bytes"504b0304"'

    def test_display_empty(self, menai):
        """describe of empty bytes produces #bytes\"\"."""
        assert menai.evaluate_and_format('(string-hex->bytes "")') == '#bytes""'

    def test_display_truncation(self, menai):
        """describe truncates at 64 bytes (128 hex chars)."""
        hex_str = "ab" * 100
        result = menai.evaluate_and_format(f'(string-hex->bytes "{hex_str}")')
        assert result.startswith('#bytes"' + "ab" * 64)
        assert result.endswith('..."')
        assert len(result) == len('#bytes"') + 128 + len('..."')


class TestBytesAsDictKey:
    """Test bytes as dictionary keys."""

    def test_bytes_dict_key(self, menai):
        """Bytes can be used as dict keys."""
        result = menai.evaluate('(dict-get (dict (string-hex->bytes "504b") "zip") (string-hex->bytes "504b"))')
        assert result == "zip"

    def test_bytes_dict_key_miss(self, menai):
        """Dict lookup with non-matching bytes key returns #none."""
        result = menai.evaluate('(dict-get (dict (string-hex->bytes "504b") "zip") (string-hex->bytes "ffff"))')
        assert result is None

    def test_bytes_dict_key_round_trip(self, menai):
        """Bytes dict keys work with structural sharing (slices)."""
        original = '(string-hex->bytes "504b0304")'
        sliced = '(bytes-slice (string-hex->bytes "504b03040000") 0 4)'
        expr = f'(dict-get (dict {original} "found") {sliced})'
        result = menai.evaluate(expr)
        assert result == "found"


class TestBytesConstantFolding:
    """Test that string->bytes and string-hex->bytes are folded at compile time."""

    def test_string_hex_to_bytes_folds(self, menai):
        """string-hex->bytes with literal argument is folded to a constant."""
        # If folded, the result is a single bytes constant, not a function call at runtime
        result = menai.evaluate('(bytes->string-hex (string-hex->bytes "deadbeef"))')
        assert result == "deadbeef"

    def test_string_to_bytes_folds(self, menai):
        """string->bytes with literal argument is folded to a constant."""
        result = menai.evaluate('(bytes->string-hex (string->bytes "hello"))')
        assert result == "68656c6c6f"

    def test_folded_bytes_in_nested_expression(self, menai):
        """Folded bytes constants work in nested expressions."""
        result = menai.evaluate('(bytes-length (bytes-concat (string-hex->bytes "dead") (string-hex->bytes "beef")))')
        assert result == 4


class TestBytesTypeErrors:
    """Test type error handling for bytes operations."""

    @pytest.mark.parametrize("expr", [
        '(bytes-length "hello")',
        '(bytes-length 42)',
        '(bytes-length (list 1 2 3))',
    ])
    def test_bytes_length_type_error(self, menai, expr):
        """bytes-length raises error on non-bytes arguments."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate(expr)

        assert exc_info.value.error_code == VMErrorCode.TYPE_MISMATCH

    @pytest.mark.parametrize("expr", [
        '(bytes-ref "hello" 0)',
        '(bytes-ref 42 0)',
    ])
    def test_bytes_ref_type_error(self, menai, expr):
        """bytes-ref raises error on non-bytes arguments."""
        with pytest.raises(MenaiEvalError) as exc_info:
            menai.evaluate(expr)

        assert exc_info.value.error_code == VMErrorCode.TYPE_MISMATCH

    def test_bytes_ref_non_integer_offset(self, menai):
        """bytes-ref raises error when offset is not an integer."""
        with pytest.raises(MenaiEvalError, match="offset must be"):
            menai.evaluate('(bytes-ref (string-hex->bytes "504b") "x")')

    def test_bytes_slice_non_integer_args(self, menai):
        """bytes-slice raises error when start/end are not integers."""
        with pytest.raises(MenaiEvalError, match="start must be"):
            menai.evaluate('(bytes-slice (string-hex->bytes "504b0304") "x" 2)')

        with pytest.raises(MenaiEvalError, match="end must be"):
            menai.evaluate('(bytes-slice (string-hex->bytes "504b0304") 0 "x")')


class TestBytesZipHeaderExample:
    """Integration test: parsing a zip local file header."""

    def test_parse_zip_local_file_header(self, menai):
        """Parse a zip local file header and extract fields."""
        # Zip local file header: signature + version + flags + compression + ...
        header_hex = "504b0304" + "1400" + "0000" + "0800" + "5000" + "7801" + \
                      "00000000" + "00000000" + "00000000" + "0400" + "0000"
        expr = f'''
          (let ((b (string-hex->bytes "{header_hex}")))
            (if (bytes!=? (bytes-slice b 0 4) (string-hex->bytes "504b0304"))
                (error "not a zip local file header")
                (dict
                  "signature"         (bytes->string-hex (bytes-slice b 0 4))
                  "version-needed"    (bytes-read-u16-le b 4)
                  "flags"             (bytes-read-u16-le b 6)
                  "compression"       (bytes-read-u16-le b 8)
                  "mod-time"          (bytes-read-u16-le b 10)
                  "mod-date"          (bytes-read-u16-le b 12)
                  "crc32"             (bytes-read-u32-le b 14)
                  "compressed-size"   (bytes-read-u32-le b 18)
                  "uncompressed-size" (bytes-read-u32-le b 22)
                  "filename-length"   (bytes-read-u16-le b 26)
                  "extra-length"      (bytes-read-u16-le b 28))))
        '''
        result = menai.evaluate(expr)
        assert result == {
            "signature": "504b0304",
            "version-needed": 20,
            "flags": 0,
            "compression": 8,
            "mod-time": 80,
            "mod-date": 376,
            "crc32": 0,
            "compressed-size": 0,
            "uncompressed-size": 0,
            "filename-length": 4,
            "extra-length": 0,
        }

class TestBytesMultiByteAppend:
    """Test multi-byte integer append operations."""

    EMPTY = '(string-hex->bytes "")'

    @pytest.mark.parametrize("value,expected_hex", [
        (0x1234, "3412"),
        (0x0000, "0000"),
        (0xffff, "ffff"),
    ])
    def test_bytes_append_u16_le_encoding(self, menai, value, expected_hex):
        """bytes-append-u16-le produces correct byte order for little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u16-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x1234, "1234"),
        (0x0000, "0000"),
        (0xffff, "ffff"),
    ])
    def test_bytes_append_u16_be(self, menai, value, expected_hex):
        """bytes-append-u16-be encodes unsigned 16-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u16-be {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x123456, "563412"),
        (0x000000, "000000"),
        (0xffffff, "ffffff"),
    ])
    def test_bytes_append_u24_le(self, menai, value, expected_hex):
        """bytes-append-u24-le encodes unsigned 24-bit little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u24-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x123456, "123456"),
        (0x000000, "000000"),
        (0xffffff, "ffffff"),
    ])
    def test_bytes_append_u24_be(self, menai, value, expected_hex):
        """bytes-append-u24-be encodes unsigned 24-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u24-be {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x12345678, "78563412"),
        (0x00000000, "00000000"),
        (0xffffffff, "ffffffff"),
    ])
    def test_bytes_append_u32_le(self, menai, value, expected_hex):
        """bytes-append-u32-le encodes unsigned 32-bit little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u32-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x12345678, "12345678"),
        (0x00000000, "00000000"),
        (0xffffffff, "ffffffff"),
    ])
    def test_bytes_append_u32_be(self, menai, value, expected_hex):
        """bytes-append-u32-be encodes unsigned 32-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u32-be {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x0102030405060708, "0807060504030201"),
        (0x0000000000000000, "0000000000000000"),
        (0x8000000000000000, "0000000000000080"),
        (0xffffffffffffffff, "ffffffffffffffff"),
    ])
    def test_bytes_append_u64_le(self, menai, value, expected_hex):
        """bytes-append-u64-le encodes unsigned 64-bit little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u64-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x0102030405060708, "0102030405060708"),
        (0x0000000000000000, "0000000000000000"),
        (0x8000000000000000, "8000000000000000"),
        (0xffffffffffffffff, "ffffffffffffffff"),
    ])
    def test_bytes_append_u64_be(self, menai, value, expected_hex):
        """bytes-append-u64-be encodes unsigned 64-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-u64-be {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0, "00"),
        (127, "7f"),
        (-1, "ff"),
        (-128, "80"),
    ])
    def test_bytes_append_i8(self, menai, value, expected_hex):
        """bytes-append-i8 encodes signed 8-bit."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i8 {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "0100"),
        (-1, "ffff"),
        (32767, "ff7f"),
        (-32768, "0080"),
    ])
    def test_bytes_append_i16_le(self, menai, value, expected_hex):
        """bytes-append-i16-le encodes signed 16-bit little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i16-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "0001"),
        (-1, "ffff"),
        (32767, "7fff"),
        (-32768, "8000"),
    ])
    def test_bytes_append_i16_be(self, menai, value, expected_hex):
        """bytes-append-i16-be encodes signed 16-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i16-be {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "010000"),
        (-1, "ffffff"),
        (8388607, "ffff7f"),
        (-8388608, "000080"),
    ])
    def test_bytes_append_i24_le(self, menai, value, expected_hex):
        """bytes-append-i24-le encodes signed 24-bit little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i24-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "000001"),
        (-1, "ffffff"),
        (8388607, "7fffff"),
        (-8388608, "800000"),
    ])
    def test_bytes_append_i24_be(self, menai, value, expected_hex):
        """bytes-append-i24-be encodes signed 24-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i24-be {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "01000000"),
        (-1, "ffffffff"),
        (2147483647, "ffffff7f"),
        (-2147483648, "00000080"),
    ])
    def test_bytes_append_i32_le(self, menai, value, expected_hex):
        """bytes-append-i32-le encodes signed 32-bit little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i32-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "00000001"),
        (-1, "ffffffff"),
        (2147483647, "7fffffff"),
        (-2147483648, "80000000"),
    ])
    def test_bytes_append_i32_be(self, menai, value, expected_hex):
        """bytes-append-i32-be encodes signed 32-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i32-be {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "0100000000000000"),
        (-1, "ffffffffffffffff"),
    ])
    def test_bytes_append_i64_le(self, menai, value, expected_hex):
        """bytes-append-i64-le encodes signed 64-bit little-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i64-le {self.EMPTY} {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (1, "0000000000000001"),
        (-1, "ffffffffffffffff"),
    ])
    def test_bytes_append_i64_be(self, menai, value, expected_hex):
        """bytes-append-i64-be encodes signed 64-bit big-endian."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-i64-be {self.EMPTY} {value}))')
        assert result == expected_hex

    def test_append_to_non_empty(self, menai):
        """Multi-byte append extends existing bytes."""
        result = menai.evaluate('(bytes->string-hex (bytes-append-u16-le (string-hex->bytes "aabb") 1027))')
        assert result == "aabb0304"

    def test_append_chained(self, menai):
        """Chaining multiple appends builds bytes incrementally."""
        expr = '(bytes->string-hex (bytes-append-u32-be (bytes-append-u16-le (string-hex->bytes "") 1) 2))'
        assert menai.evaluate(expr) == "010000000002"

    @pytest.mark.parametrize("expr", [
        '(bytes-append-u16-le (string-hex->bytes "") 65536)',
        '(bytes-append-u8 (string-hex->bytes "") 256)',
        '(bytes-append-u8 (string-hex->bytes "") -1)',
    ])
    def test_append_out_of_range(self, menai, expr):
        """Append raises error on values outside the valid range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(expr)

    @pytest.mark.parametrize("expr", [
        '(bytes-append-i8 (string-hex->bytes "") 128)',
        '(bytes-append-i8 (string-hex->bytes "") -129)',
        '(bytes-append-i16-le (string-hex->bytes "") 32768)',
    ])
    def test_append_signed_out_of_range(self, menai, expr):
        """Signed append raises error on values outside the signed range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(expr)


class TestBytesMultiByteWrite:
    """Test multi-byte integer write (patch at offset) operations."""

    def test_write_u16_le(self, menai):
        """bytes-write-u16-le writes unsigned 16-bit at offset, little-endian."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-u16-le (string-hex->bytes "00000000") 1 4660))')
        assert result == "00341200"

    def test_write_u16_be(self, menai):
        """bytes-write-u16-be writes unsigned 16-bit at offset, big-endian."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-u16-be (string-hex->bytes "00000000") 1 4660))')
        assert result == "00123400"

    def test_write_u32_le(self, menai):
        """bytes-write-u32-le writes unsigned 32-bit at offset, little-endian."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-u32-le (string-hex->bytes "0000000000") 1 305419896))')
        assert result == "0078563412"

    def test_write_u32_be(self, menai):
        """bytes-write-u32-be writes unsigned 32-bit at offset, big-endian."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-u32-be (string-hex->bytes "0000000000") 1 305419896))')
        assert result == "0012345678"

    def test_write_i16_le_negative(self, menai):
        """bytes-write-i16-le writes signed 16-bit at offset."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-i16-le (string-hex->bytes "0000") 0 -1))')
        assert result == "ffff"

    def test_write_u8(self, menai):
        """bytes-write-u8 writes a single byte at offset."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-u8 (string-hex->bytes "000000") 1 255))')
        assert result == "00ff00"

    def test_write_u64_le(self, menai):
        """bytes-write-u64-le writes unsigned 64-bit at offset, little-endian."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-u64-le (string-hex->bytes "000000000000000000") 1 72623859790382856))')
        assert result == "000807060504030201"

    def test_write_u64_be(self, menai):
        """bytes-write-u64-be writes unsigned 64-bit at offset, big-endian."""
        result = menai.evaluate('(bytes->string-hex (bytes-write-u64-be (string-hex->bytes "000000000000000000") 1 72623859790382856))')
        assert result == "000102030405060708"

    @pytest.mark.parametrize("value,expected_hex", [
        (0x8000000000000000, "000000000000000080"),
        (0xffffffffffffffff, "00ffffffffffffffff"),
    ])
    def test_write_u64_le_large(self, menai, value, expected_hex):
        """bytes-write-u64-le writes unsigned 64-bit values above LONG_MAX."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-write-u64-le (string-hex->bytes "000000000000000000") 1 {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("value,expected_hex", [
        (0x8000000000000000, "008000000000000000"),
        (0xffffffffffffffff, "00ffffffffffffffff"),
    ])
    def test_write_u64_be_large(self, menai, value, expected_hex):
        """bytes-write-u64-be writes unsigned 64-bit values above LONG_MAX."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-write-u64-be (string-hex->bytes "000000000000000000") 1 {value}))')
        assert result == expected_hex

    def test_write_immutability(self, menai):
        """bytes-write returns new bytes; original is unchanged."""
        original = '(string-hex->bytes "00000000")'
        result = menai.evaluate(f'(bytes->string-hex (bytes-write-u16-le {original} 0 4660))')
        assert result == "34120000"
        assert menai.evaluate(f'(bytes->string-hex {original})') == "00000000"

    def test_write_out_of_bounds(self, menai):
        """Write raises error when not enough bytes from offset."""
        with pytest.raises(MenaiEvalError, match="out of bounds"):
            menai.evaluate('(bytes-write-u16-le (string-hex->bytes "00") 0 4660)')

    def test_write_out_of_range(self, menai):
        """Write raises error on values outside the valid range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate('(bytes-write-u8 (string-hex->bytes "00") 0 256)')


class TestBytesLeb128:
    """Test LEB128 variable-length integer encode/decode."""

    @pytest.mark.parametrize("value,expected_hex", [
        (0, "00"),
        (1, "01"),
        (127, "7f"),
        (128, "8001"),
        (300, "ac02"),
        (16383, "ff7f"),
        (16384, "808001"),
        (9223372036854775807, "ffffffffffffffff7f"),  # 2^63 - 1 = LONG_MAX
        (18446744073709551615, "ffffffffffffffffff01"),  # 2^64 - 1 = UINT64_MAX
    ])
    def test_append_uleb128(self, menai, value, expected_hex):
        """bytes-append-uleb128 encodes unsigned LEB128."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-uleb128 (string-hex->bytes "") {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("hex_str,expected_value,expected_next", [
        ("00", 0, 1),
        ("01", 1, 1),
        ("7f", 127, 1),
        ("8001", 128, 2),
        ("ac02", 300, 2),
        ("ff7f", 16383, 2),
        ("ffffffffffffffff7f", 9223372036854775807, 9),   # 2^63 - 1 = LONG_MAX
        ("ffffffffffffffffff01", 18446744073709551615, 10), # 2^64 - 1 = UINT64_MAX
    ])
    def test_read_uleb128(self, menai, hex_str, expected_value, expected_next):
        """bytes-read-uleb128 decodes unsigned LEB128 and returns (value next-offset)."""
        expr = f'''
            (match (bytes-read-uleb128 (string-hex->bytes "{hex_str}") 0)
              ((val next) (list val next)))
        '''
        assert menai.evaluate(expr) == [expected_value, expected_next]

    def test_read_uleb128_with_offset(self, menai):
        """bytes-read-uleb128 works at non-zero offset."""
        expr = '''
            (match (bytes-read-uleb128 (string-hex->bytes "ffac02ff") 1)
              ((val next) (list val next)))
        '''
        assert menai.evaluate(expr) == [300, 3]

    def test_uleb128_round_trip(self, menai):
        """Encode then decode recovers the original value."""
        expr = '''
            (match (bytes-read-uleb128 (bytes-append-uleb128 (string-hex->bytes "") 624485) 0)
              ((val next) val))
        '''
        assert menai.evaluate(expr) == 624485

    def test_read_uleb128_truncated(self, menai):
        """Reading truncated ULEB128 raises error."""
        with pytest.raises(MenaiEvalError, match="truncated"):
            menai.evaluate('(bytes-read-uleb128 (string-hex->bytes "80") 0)')

    def test_append_uleb128_negative_error(self, menai):
        """ULEB128 append rejects negative values."""
        with pytest.raises(MenaiEvalError, match="non-negative"):
            menai.evaluate('(bytes-append-uleb128 (string-hex->bytes "") -1)')

    @pytest.mark.parametrize("value,expected_hex", [
        (0, "00"),
        (1, "01"),
        (-1, "7f"),
        (63, "3f"),
        (-64, "40"),
        (64, "c000"),
        (-65, "bf7f"),
        (127, "ff00"),
        (-127, "817f"),
        (128, "8001"),
        (-128, "807f"),
    ])
    def test_append_sleb128(self, menai, value, expected_hex):
        """bytes-append-sleb128 encodes signed LEB128."""
        result = menai.evaluate(f'(bytes->string-hex (bytes-append-sleb128 (string-hex->bytes "") {value}))')
        assert result == expected_hex

    @pytest.mark.parametrize("hex_str,expected_value", [
        ("00", 0),
        ("01", 1),
        ("7f", -1),
        ("3f", 63),
        ("40", -64),
        ("c000", 64),
        ("ff00", 127),
        ("817f", -127),
    ])
    def test_read_sleb128(self, menai, hex_str, expected_value):
        """bytes-read-sleb128 decodes signed LEB128."""
        expr = f'''
            (match (bytes-read-sleb128 (string-hex->bytes "{hex_str}") 0)
              ((val next) val))
        '''
        assert menai.evaluate(expr) == expected_value

    def test_sleb128_round_trip(self, menai):
        """Encode then decode recovers the original signed value."""
        expr = '''
            (match (bytes-read-sleb128 (bytes-append-sleb128 (string-hex->bytes "") -123456) 0)
              ((val next) val))
        '''
        assert menai.evaluate(expr) == -123456

    def test_read_sleb128_truncated(self, menai):
        """Reading truncated SLEB128 raises error."""
        with pytest.raises(MenaiEvalError, match="truncated"):
            menai.evaluate('(bytes-read-sleb128 (string-hex->bytes "80") 0)')


class TestBytesPreludeHigherOrder:
    """Test higher-order prelude functions for bytes."""

    def test_map_bytes(self, menai):
        """map-bytes applies a function to each byte."""
        result = menai.evaluate('(bytes->string-hex (map-bytes (lambda (b) (integer-bit-xor b 255)) (string-hex->bytes "00ff4231")))')
        assert result == "ff00bdce"

    def test_map_bytes_empty(self, menai):
        """map-bytes on empty bytes returns empty bytes."""
        result = menai.evaluate('(bytes->string-hex (map-bytes (lambda (b) (integer+ b 1)) (string-hex->bytes "")))')
        assert result == ""

    def test_filter_bytes(self, menai):
        """filter-bytes keeps bytes satisfying the predicate."""
        result = menai.evaluate('(bytes->string-hex (filter-bytes (lambda (b) (integer<? b 128)) (string-hex->bytes "41ff4280ff43")))')
        assert result == "414243"

    def test_filter_bytes_all_removed(self, menai):
        """filter-bytes returns empty when no bytes match."""
        result = menai.evaluate('(bytes->string-hex (filter-bytes (lambda (b) (integer>? b 255)) (string-hex->bytes "4142")))')
        assert result == ""

    def test_fold_bytes_sum(self, menai):
        """fold-bytes accumulates a result over all bytes."""
        result = menai.evaluate('(fold-bytes (lambda (acc b) (integer+ acc b)) 0 (string-hex->bytes "01020304"))')
        assert result == 10

    def test_fold_bytes_to_list(self, menai):
        """fold-bytes can build a list from bytes."""
        result = menai.evaluate('(fold-bytes (lambda (acc b) (list-append acc b)) (list) (string-hex->bytes "010203"))')
        assert result == [1, 2, 3]

class TestBytesPreludePredicates:
    """Test convenience predicate prelude functions."""

    def test_bytes_empty_true(self, menai):
        """bytes-empty? returns true for zero-length bytes."""
        assert menai.evaluate('(bytes-empty? (string-hex->bytes ""))') is True

    def test_bytes_empty_false(self, menai):
        """bytes-empty? returns false for non-empty bytes."""
        assert menai.evaluate('(bytes-empty? (string-hex->bytes "00"))') is False

    @pytest.mark.parametrize("source_hex,prefix_hex,expected", [
        ("504b0304", "504b", True),
        ("504b0304", "504b0304", True),
        ("504b0304", "504b03", True),
        ("504b0304", "504c", False),
        ("504b0304", "504b030400", False),
    ])
    def test_bytes_prefix(self, menai, source_hex, prefix_hex, expected):
        """bytes-prefix? checks if bytes starts with the given prefix."""
        result = menai.evaluate(f'(bytes-prefix? (string-hex->bytes "{source_hex}") (string-hex->bytes "{prefix_hex}"))')
        assert result == expected

    @pytest.mark.parametrize("source_hex,suffix_hex,expected", [
        ("504b0304", "0304", True),
        ("504b0304", "504b0304", True),
        ("504b0304", "0304", True),
        ("504b0304", "0305", False),
        ("00504b0304", "504b0304", True),
    ])
    def test_bytes_suffix(self, menai, source_hex, suffix_hex, expected):
        """bytes-suffix? checks if bytes ends with the given suffix."""
        result = menai.evaluate(f'(bytes-suffix? (string-hex->bytes "{source_hex}") (string-hex->bytes "{suffix_hex}"))')
        assert result == expected


class TestBytesPreludeSplit:
    """Test splitting prelude functions."""

    def test_bytes_split_basic(self, menai):
        """bytes-split divides bytes on a delimiter."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split (string-hex->bytes "41424344") (string-hex->bytes "42")))')
        assert result == ["41", "4344"]

    def test_bytes_split_multiple(self, menai):
        """bytes-split handles multiple delimiter occurrences."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split (string-hex->bytes "012e342e56") (string-hex->bytes "2e")))')
        assert result == ["01", "34", "56"]

    def test_bytes_split_no_match(self, menai):
        """bytes-split returns a single-element list when delimiter is absent."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split (string-hex->bytes "414243") (string-hex->bytes "ff")))')
        assert result == ["414243"]

    def test_bytes_split_consecutive(self, menai):
        """bytes-split produces empty segments for consecutive delimiters."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split (string-hex->bytes "410042") (string-hex->bytes "00")))')
        assert result == ["41", "42"]

    def test_bytes_split_empty_delimiter_error(self, menai):
        """bytes-split raises error on empty delimiter."""
        with pytest.raises(MenaiEvalError, match="non-empty"):
            menai.evaluate('(bytes-split (string-hex->bytes "4142") (string-hex->bytes ""))')

    def test_bytes_split_int_basic(self, menai):
        """bytes-split-int divides bytes on a single byte value."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split-int (string-hex->bytes "4100420043") 0))')
        assert result == ["41", "42", "43"]

    def test_bytes_split_int_no_match(self, menai):
        """bytes-split-int returns a single-element list when byte is absent."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split-int (string-hex->bytes "414243") 0))')
        assert result == ["414243"]

    def test_bytes_split_int_leading_delimiter(self, menai):
        """bytes-split-int produces an empty first segment for a leading delimiter."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split-int (string-hex->bytes "004142") 0))')
        assert result == ["", "4142"]

    def test_bytes_split_int_trailing_delimiter(self, menai):
        """bytes-split-int produces an empty last segment for a trailing delimiter."""
        result = menai.evaluate('(map-list bytes->string-hex (bytes-split-int (string-hex->bytes "414200") 0))')
        assert result == ["4142", ""]

class TestBytesU64RoundTrip:
    """Test round-trip symmetry for unsigned 64-bit values above LONG_MAX.

    Values in the range [2^63, 2^64-1] require the MenaiBigInt path on both
    the write side (append/write) and the read side.  These tests verify
    that encoding and decoding are symmetric for the full unsigned 64-bit range.
    """

    EMPTY = '(string-hex->bytes "")'

    @pytest.mark.parametrize("value", [
        0,
        1,
        9223372036854775807,   # 2^63 - 1 = LONG_MAX (boundary, still fits in long)
        9223372036854775808,   # 2^63     = LONG_MAX + 1 (first bigint value)
        18446744073709551614,  # 2^64 - 2
        18446744073709551615,  # 2^64 - 1 = UINT64_MAX
    ])
    def test_append_read_u64_le_round_trip(self, menai, value):
        """bytes-append-u64-le then bytes-read-u64-le recovers the original value."""
        expr = f'(bytes-read-u64-le (bytes-append-u64-le {self.EMPTY} {value}) 0)'
        assert menai.evaluate(expr) == value

    @pytest.mark.parametrize("value", [
        0,
        1,
        9223372036854775807,   # 2^63 - 1 = LONG_MAX
        9223372036854775808,   # 2^63     = LONG_MAX + 1
        18446744073709551614,  # 2^64 - 2
        18446744073709551615,  # 2^64 - 1 = UINT64_MAX
    ])
    def test_append_read_u64_be_round_trip(self, menai, value):
        """bytes-append-u64-be then bytes-read-u64-be recovers the original value."""
        expr = f'(bytes-read-u64-be (bytes-append-u64-be {self.EMPTY} {value}) 0)'
        assert menai.evaluate(expr) == value

    @pytest.mark.parametrize("value", [
        0,
        1,
        9223372036854775807,   # 2^63 - 1 = LONG_MAX
        9223372036854775808,   # 2^63     = LONG_MAX + 1
        18446744073709551614,  # 2^64 - 2
        18446744073709551615,  # 2^64 - 1 = UINT64_MAX
    ])
    def test_write_read_u64_le_round_trip(self, menai, value):
        """bytes-write-u64-le then bytes-read-u64-le recovers the original value."""
        expr = f'(bytes-read-u64-le (bytes-write-u64-le (string-hex->bytes "0000000000000000") 0 {value}) 0)'
        assert menai.evaluate(expr) == value

    @pytest.mark.parametrize("value", [
        0,
        1,
        9223372036854775807,   # 2^63 - 1 = LONG_MAX
        9223372036854775808,   # 2^63     = LONG_MAX + 1
        18446744073709551614,  # 2^64 - 2
        18446744073709551615,  # 2^64 - 1 = UINT64_MAX
    ])
    def test_write_read_u64_be_round_trip(self, menai, value):
        """bytes-write-u64-be then bytes-read-u64-be recovers the original value."""
        expr = f'(bytes-read-u64-be (bytes-write-u64-be (string-hex->bytes "0000000000000000") 0 {value}) 0)'
        assert menai.evaluate(expr) == value


class TestBytesUleb128RoundTrip:
    """Test round-trip symmetry for ULEB128 values above LONG_MAX.

    ULEB128 encoding of values in [2^63, 2^64-1] requires the MenaiBigInt path
    on both the append and read sides.
    """

    @pytest.mark.parametrize("value", [
        0,
        1,
        127,
        128,
        624485,
        9223372036854775807,   # 2^63 - 1 = LONG_MAX
        9223372036854775808,   # 2^63     = LONG_MAX + 1
        18446744073709551614,  # 2^64 - 2
        18446744073709551615,  # 2^64 - 1 = UINT64_MAX
    ])
    def test_uleb128_round_trip(self, menai, value):
        """bytes-append-uleb128 then bytes-read-uleb128 recovers the original value."""
        expr = f'''
            (match (bytes-read-uleb128 (bytes-append-uleb128 (string-hex->bytes "") {value}) 0)
              ((val next) val))
        '''
        assert menai.evaluate(expr) == value


class TestIntegerToLongOverflow:
    """Test that bytes operations correctly handle MenaiInteger values too large for a C long.

    The VM's integer_to_long helper silently discards overflow when a MenaiInteger
    holds a bignum exceeding LONG_MAX.  These tests verify that every bytes operation
    that extracts an integer value raises a proper error instead of returning
    garbage or silently truncating.
    """

    BIG_POS = 9223372036854775808  # 2^63 — first value that does not fit in a C long
    BIG_NEG = -9223372036854775809  # -(2^63) - 1
    EMPTY = '(string-hex->bytes "")'


class TestIntegerToLongOverflowValues(TestIntegerToLongOverflow):
    """Value operands that are too large for a C long must raise value-out-of-range."""

    def test_bytes_append_u8_bigint(self, menai):
        """bytes-append-u8 rejects a bigint value that exceeds the 0–255 range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-u8 {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_i8_bigint(self, menai):
        """bytes-append-i8 rejects a bigint value that exceeds the signed 8-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i8 {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_i16_le_bigint(self, menai):
        """bytes-append-i16-le rejects a bigint value that exceeds the signed 16-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i16-le {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_i16_be_bigint(self, menai):
        """bytes-append-i16-be rejects a bigint value that exceeds the signed 16-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i16-be {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_i32_le_bigint(self, menai):
        """bytes-append-i32-le rejects a bigint value that exceeds the signed 32-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i32-le {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_i32_be_bigint(self, menai):
        """bytes-append-i32-be rejects a bigint value that exceeds the signed 32-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i32-be {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_i64_le_bigint(self, menai):
        """bytes-append-i64-le rejects a bigint value that exceeds the signed 64-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i64-le {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_i64_be_bigint(self, menai):
        """bytes-append-i64-be rejects a bigint value that exceeds the signed 64-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i64-be {self.EMPTY} {self.BIG_POS})')

    def test_bytes_append_sleb128_bigint(self, menai):
        """bytes-append-sleb128 rejects a bigint value that exceeds C long range."""
        with pytest.raises((MenaiEvalError, OverflowError), match="overflow"):
            menai.evaluate(f'(bytes-append-sleb128 {self.EMPTY} {self.BIG_POS})')

    def test_list_to_bytes_bigint(self, menai):
        """list->bytes rejects a bigint element that exceeds the 0–255 range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(list->bytes (list {self.BIG_POS}))')

    def test_bytes_index_int_bigint(self, menai):
        """bytes-index-int rejects a bigint byte value that exceeds the 0–255 range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-index-int {self.BIG_POS} {self.EMPTY})')

    def test_bytes_write_i16_le_bigint(self, menai):
        """bytes-write-i16-le rejects a bigint value that exceeds the signed 16-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-write-i16-le (string-hex->bytes "0000") 0 {self.BIG_POS})')

    def test_bytes_write_i64_le_bigint(self, menai):
        """bytes-write-i64-le rejects a bigint value that exceeds the signed 64-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-write-i64-le (string-hex->bytes "0000000000000000") 0 {self.BIG_POS})')

    def test_bytes_append_i8_bigint_negative(self, menai):
        """bytes-append-i8 rejects a negative bigint value that exceeds the signed 8-bit range."""
        with pytest.raises(MenaiEvalError, match="out of range"):
            menai.evaluate(f'(bytes-append-i8 {self.EMPTY} {self.BIG_NEG})')

    def test_bytes_append_sleb128_bigint_negative(self, menai):
        """bytes-append-sleb128 rejects a negative bigint value that exceeds C long range."""
        with pytest.raises((MenaiEvalError, OverflowError), match="overflow"):
            menai.evaluate(f'(bytes-append-sleb128 {self.EMPTY} {self.BIG_NEG})')


class TestIntegerToLongOverflowOffsets(TestIntegerToLongOverflow):
    """Offset operands that are too large for a C long must raise a proper error."""

    FOUR_BYTES = '(string-hex->bytes "01020304")'
    EIGHT_BYTES = '(string-hex->bytes "0102030405060708")'

    def test_bytes_ref_bigint_offset(self, menai):
        """bytes-ref rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-ref {self.FOUR_BYTES} {self.BIG_POS})')

    def test_bytes_slice_bigint_start(self, menai):
        """bytes-slice rejects a bigint start index that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-slice {self.FOUR_BYTES} {self.BIG_POS} 4)')

    def test_bytes_slice_bigint_end(self, menai):
        """bytes-slice rejects a bigint end index that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-slice {self.FOUR_BYTES} 0 {self.BIG_POS})')

    def test_bytes_read_u8_bigint_offset(self, menai):
        """bytes-read-u8 rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-read-u8 {self.FOUR_BYTES} {self.BIG_POS})')

    def test_bytes_read_i8_bigint_offset(self, menai):
        """bytes-read-i8 rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-read-i8 {self.FOUR_BYTES} {self.BIG_POS})')

    def test_bytes_read_u16_le_bigint_offset(self, menai):
        """bytes-read-u16-le rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-read-u16-le {self.FOUR_BYTES} {self.BIG_POS})')

    def test_bytes_read_u64_le_bigint_offset(self, menai):
        """bytes-read-u64-le rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-read-u64-le {self.EIGHT_BYTES} {self.BIG_POS})')

    def test_bytes_write_u16_le_bigint_offset(self, menai):
        """bytes-write-u16-le rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-write-u16-le {self.FOUR_BYTES} {self.BIG_POS} 1)')

    def test_bytes_read_uleb128_bigint_offset(self, menai):
        """bytes-read-uleb128 rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-read-uleb128 {self.FOUR_BYTES} {self.BIG_POS})')

    def test_bytes_read_sleb128_bigint_offset(self, menai):
        """bytes-read-sleb128 rejects a bigint offset that does not fit in a C long."""
        with pytest.raises(MenaiEvalError):
            menai.evaluate(f'(bytes-read-sleb128 {self.FOUR_BYTES} {self.BIG_POS})')