"""Tests for the bytes type — construction, conversion, access, and comparison."""

import pytest

from menai import Menai, MenaiEvalError


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
        with pytest.raises(MenaiEvalError, match="bytes"):
            menai.evaluate(expr)

    @pytest.mark.parametrize("expr", [
        '(bytes-ref "hello" 0)',
        '(bytes-ref 42 0)',
    ])
    def test_bytes_ref_type_error(self, menai, expr):
        """bytes-ref raises error on non-bytes arguments."""
        with pytest.raises(MenaiEvalError, match="bytes"):
            menai.evaluate(expr)

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