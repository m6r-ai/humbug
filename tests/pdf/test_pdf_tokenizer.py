import pytest

from pdf.pdf_tokenizer import PDFTokenizer, TokenType


class TestSkipWhitespaceAndComments:
    def test_skips_spaces(self) -> None:
        t = PDFTokenizer(b"   hello")
        t.skip_whitespace_and_comments()
        assert t.pos == 3

    def test_skips_comment(self) -> None:
        t = PDFTokenizer(b"% this is a comment\nhello")
        t.skip_whitespace_and_comments()
        assert t.peek() == ord("h")

    def test_skips_mixed(self) -> None:
        t = PDFTokenizer(b"  % comment\n  % another\nX")
        t.skip_whitespace_and_comments()
        assert t.peek() == ord("X")


class TestNextTokenBasicTypes:
    def test_eof(self) -> None:
        t = PDFTokenizer(b"")
        tok = t.next_token()
        assert tok.type == TokenType.EOF

    def test_integer(self) -> None:
        tok = PDFTokenizer(b"42").next_token()
        assert tok.type == TokenType.INTEGER
        assert tok.value == 42

    def test_negative_integer(self) -> None:
        tok = PDFTokenizer(b"-7").next_token()
        assert tok.type == TokenType.INTEGER
        assert tok.value == -7

    def test_real(self) -> None:
        tok = PDFTokenizer(b"3.14").next_token()
        assert tok.type == TokenType.REAL
        assert abs(tok.value - 3.14) < 1e-9  # type: ignore[operator]

    def test_boolean_true(self) -> None:
        tok = PDFTokenizer(b"true").next_token()
        assert tok.type == TokenType.BOOLEAN
        assert tok.value is True

    def test_boolean_false(self) -> None:
        tok = PDFTokenizer(b"false").next_token()
        assert tok.type == TokenType.BOOLEAN
        assert tok.value is False

    def test_null(self) -> None:
        tok = PDFTokenizer(b"null").next_token()
        assert tok.type == TokenType.NULL
        assert tok.value is None

    def test_name(self) -> None:
        tok = PDFTokenizer(b"/FlateDecode").next_token()
        assert tok.type == TokenType.NAME
        assert tok.value == "FlateDecode"

    def test_name_with_hex_escape(self) -> None:
        tok = PDFTokenizer(b"/F#23ont").next_token()
        assert tok.type == TokenType.NAME
        assert tok.value == "F#ont"


class TestNextTokenStrings:
    def test_literal_string(self) -> None:
        tok = PDFTokenizer(b"(Hello World)").next_token()
        assert tok.type == TokenType.STRING
        assert tok.value == b"Hello World"

    def test_literal_string_escapes(self) -> None:
        tok = PDFTokenizer(b"(line1\\nline2)").next_token()
        assert tok.type == TokenType.STRING
        assert tok.value == b"line1\nline2"

    def test_literal_string_nested_parens(self) -> None:
        tok = PDFTokenizer(b"(a (b) c)").next_token()
        assert tok.type == TokenType.STRING
        assert tok.value == b"a (b) c"

    def test_hex_string(self) -> None:
        tok = PDFTokenizer(b"<48656c6c6f>").next_token()
        assert tok.type == TokenType.HEX_STRING
        assert tok.value == b"Hello"

    def test_hex_string_uppercase(self) -> None:
        tok = PDFTokenizer(b"<48656C6C6F>").next_token()
        assert tok.type == TokenType.HEX_STRING
        assert tok.value == b"Hello"


class TestNextTokenStructure:
    def test_dict_start(self) -> None:
        tok = PDFTokenizer(b"<<").next_token()
        assert tok.type == TokenType.DICT_START

    def test_dict_end(self) -> None:
        tok = PDFTokenizer(b">>").next_token()
        assert tok.type == TokenType.DICT_END

    def test_array_start(self) -> None:
        tok = PDFTokenizer(b"[").next_token()
        assert tok.type == TokenType.ARRAY_START

    def test_array_end(self) -> None:
        tok = PDFTokenizer(b"]").next_token()
        assert tok.type == TokenType.ARRAY_END


class TestNextTokenKeywords:
    def test_obj(self) -> None:
        tok = PDFTokenizer(b"obj").next_token()
        assert tok.type == TokenType.OBJ_KW

    def test_endobj(self) -> None:
        tok = PDFTokenizer(b"endobj").next_token()
        assert tok.type == TokenType.ENDOBJ_KW

    def test_stream(self) -> None:
        tok = PDFTokenizer(b"stream").next_token()
        assert tok.type == TokenType.STREAM_KW

    def test_endstream(self) -> None:
        tok = PDFTokenizer(b"endstream").next_token()
        assert tok.type == TokenType.ENDSTREAM_KW

    def test_ref(self) -> None:
        tok = PDFTokenizer(b"R").next_token()
        assert tok.type == TokenType.REF_KW

    def test_unknown_keyword(self) -> None:
        tok = PDFTokenizer(b"Tf").next_token()
        assert tok.type == TokenType.KEYWORD
        assert tok.value == "Tf"


class TestReadLine:
    def test_reads_to_newline(self) -> None:
        t = PDFTokenizer(b"hello\nworld")
        line = t.read_line()
        assert line == b"hello"
        assert t.peek() == ord("w")

    def test_handles_crlf(self) -> None:
        t = PDFTokenizer(b"hello\r\nworld")
        line = t.read_line()
        assert line == b"hello"
        assert t.peek() == ord("w")


class TestFind:
    def test_finds_needle(self) -> None:
        t = PDFTokenizer(b"abcdefgh")
        assert t.find(b"cde") == 2

    def test_not_found(self) -> None:
        t = PDFTokenizer(b"abcdefgh")
        assert t.find(b"xyz") == -1
