from enum import Enum, auto
from typing import NamedTuple


class TokenType(Enum):
    """The type of a scanned PDF token."""

    BOOLEAN = auto()
    INTEGER = auto()
    REAL = auto()
    STRING = auto()       # literal string (...)
    HEX_STRING = auto()   # hex string <...>
    NAME = auto()         # /Name
    ARRAY_START = auto()  # [
    ARRAY_END = auto()    # ]
    DICT_START = auto()   # <<
    DICT_END = auto()     # >>
    STREAM_KW = auto()    # stream keyword
    ENDSTREAM_KW = auto() # endstream keyword
    OBJ_KW = auto()       # obj keyword
    ENDOBJ_KW = auto()    # endobj keyword
    REF_KW = auto()       # R keyword (indirect reference)
    NULL = auto()         # null
    KEYWORD = auto()      # any other bare keyword
    EOF = auto()


class Token(NamedTuple):
    """A single scanned token with its type and value."""

    type: TokenType
    value: object  # bool | int | float | str | bytes | None


# Bytes that delimit tokens
_DELIMITERS = b"()<>[]{}/%"
_WHITESPACE = b" \t\n\r\f\x00"


class PDFTokenizer:
    """
    Scans a PDF byte stream into tokens.

    Operates on a bytes buffer with an integer position cursor.
    """

    def __init__(self, data: bytes) -> None:
        self._data = data
        self.pos = 0

    def peek(self) -> int | None:
        """Return the byte at the current position without advancing."""
        if self.pos >= len(self._data):
            return None

        return self._data[self.pos]

    def skip_whitespace_and_comments(self) -> None:
        """Advance past whitespace and % comment lines."""
        data = self._data
        length = len(data)
        pos = self.pos

        while pos < length:
            b = data[pos]
            if b in _WHITESPACE:
                pos += 1

            elif b == ord("%"):
                while pos < length and data[pos] not in (ord("\n"), ord("\r")):
                    pos += 1

            else:
                break

        self.pos = pos

    def next_token(self) -> Token:
        """Scan and return the next token, advancing the position."""
        self.skip_whitespace_and_comments()

        if self.pos >= len(self._data):
            return Token(TokenType.EOF, None)

        b = self._data[self.pos]

        if b == ord("/"):
            return self._scan_name()

        if b == ord("("):
            return self._scan_literal_string()

        if b == ord("<"):
            if self.pos + 1 < len(self._data) and self._data[self.pos + 1] == ord("<"):
                self.pos += 2
                return Token(TokenType.DICT_START, None)

            return self._scan_hex_string()

        if b == ord(">"):
            if self.pos + 1 < len(self._data) and self._data[self.pos + 1] == ord(">"):
                self.pos += 2
                return Token(TokenType.DICT_END, None)

            # Lone '>' is malformed but we skip it gracefully
            self.pos += 1
            return self.next_token()

        if b == ord("["):
            self.pos += 1
            return Token(TokenType.ARRAY_START, None)

        if b == ord("]"):
            self.pos += 1
            return Token(TokenType.ARRAY_END, None)

        if b in (ord("+"), ord("-")) or b == ord(".") or (ord("0") <= b <= ord("9")):
            return self._scan_number()

        return self._scan_keyword()

    def read_line(self) -> bytes:
        """Read bytes up to and including the next newline."""
        data = self._data
        length = len(data)
        start = self.pos

        while self.pos < length and data[self.pos] not in (ord("\n"), ord("\r")):
            self.pos += 1

        line = data[start:self.pos]

        # Consume the line ending (handle \r\n)
        if self.pos < length and data[self.pos] == ord("\r"):
            self.pos += 1

        if self.pos < length and data[self.pos] == ord("\n"):
            self.pos += 1

        return line

    def read_exact(self, n: int) -> bytes:
        """Read exactly n bytes, advancing the position."""
        chunk = self._data[self.pos: self.pos + n]
        self.pos += n
        return chunk

    def find(self, needle: bytes, start: int | None = None) -> int:
        """Return the position of needle in the buffer, or -1 if not found."""
        search_from = start if start is not None else self.pos
        idx = self._data.find(needle, search_from)
        return idx

    def _scan_name(self) -> Token:
        """Scan a PDF name token starting from the current position."""
        self.pos += 1  # skip '/'
        start = self.pos
        data = self._data
        length = len(data)

        while self.pos < length and data[self.pos] not in _DELIMITERS and data[self.pos] not in _WHITESPACE:
            self.pos += 1

        raw = data[start:self.pos].decode("latin-1")
        name = _unescape_name(raw)
        return Token(TokenType.NAME, name)

    def _scan_literal_string(self) -> Token:
        """Scan a PDF literal string, handling escape sequences and nested parentheses."""
        self.pos += 1  # skip '('
        data = self._data
        length = len(data)
        result = bytearray()
        depth = 1

        while self.pos < length and depth > 0:
            b = data[self.pos]

            if b == ord("\\"):
                self.pos += 1
                if self.pos >= length:
                    break

                esc = data[self.pos]
                self.pos += 1
                if esc == ord("n"):
                    result.append(ord("\n"))

                elif esc == ord("r"):
                    result.append(ord("\r"))

                elif esc == ord("t"):
                    result.append(ord("\t"))

                elif esc == ord("b"):
                    result.append(ord("\b"))

                elif esc == ord("f"):
                    result.append(ord("\f"))

                elif esc in (ord("("), ord(")"), ord("\\")):
                    result.append(esc)

                elif ord("0") <= esc <= ord("7"):
                    # Octal escape — up to 3 digits
                    octal = bytes([esc])
                    for _ in range(2):
                        if self.pos < length and ord("0") <= data[self.pos] <= ord("7"):
                            octal += bytes([data[self.pos]])
                            self.pos += 1

                        else:
                            break

                    result.append(int(octal, 8) & 0xFF)

                elif esc in (ord("\n"), ord("\r")):
                    # Line continuation — consume \r\n if present
                    if esc == ord("\r") and self.pos < length and data[self.pos] == ord("\n"):
                        self.pos += 1

                else:
                    result.append(esc)

            elif b == ord("("):
                depth += 1
                result.append(b)
                self.pos += 1

            elif b == ord(")"):
                depth -= 1
                if depth > 0:
                    result.append(b)

                self.pos += 1

            else:
                result.append(b)
                self.pos += 1

        return Token(TokenType.STRING, bytes(result))

    def _scan_hex_string(self) -> Token:
        """Scan a PDF hex string enclosed in angle brackets."""
        self.pos += 1  # skip '<'
        data = self._data
        length = len(data)
        hex_chars = bytearray()

        while self.pos < length and data[self.pos] != ord(">"):
            b = data[self.pos]
            if b not in _WHITESPACE:
                hex_chars.append(b)

            self.pos += 1

        if self.pos < length:
            self.pos += 1  # skip '>'

        # Pad to even length
        if len(hex_chars) % 2 != 0:
            hex_chars.append(ord("0"))

        try:
            value = bytes.fromhex(hex_chars.decode("ascii"))

        except (ValueError, UnicodeDecodeError):
            value = b""

        return Token(TokenType.HEX_STRING, value)

    def _scan_number(self) -> Token:
        """Scan a PDF number (integer or real) or fall back to a keyword."""
        data = self._data
        length = len(data)
        start = self.pos
        is_real = False

        if self.pos < length and data[self.pos] in (ord("+"), ord("-")):
            self.pos += 1

        while self.pos < length and (ord("0") <= data[self.pos] <= ord("9")):
            self.pos += 1

        if self.pos < length and data[self.pos] == ord("."):
            is_real = True
            self.pos += 1
            while self.pos < length and (ord("0") <= data[self.pos] <= ord("9")):
                self.pos += 1

        raw = data[start:self.pos].decode("ascii")
        try:
            if is_real:
                return Token(TokenType.REAL, float(raw))

            return Token(TokenType.INTEGER, int(raw))

        except ValueError:
            return Token(TokenType.KEYWORD, raw)

    def _scan_keyword(self) -> Token:
        """Scan a PDF keyword, boolean, or null value."""
        data = self._data
        length = len(data)
        start = self.pos

        while self.pos < length and data[self.pos] not in _DELIMITERS and data[self.pos] not in _WHITESPACE:
            self.pos += 1

        word = data[start:self.pos].decode("latin-1")

        if word == "true":
            return Token(TokenType.BOOLEAN, True)

        if word == "false":
            return Token(TokenType.BOOLEAN, False)

        if word == "null":
            return Token(TokenType.NULL, None)

        if word == "obj":
            return Token(TokenType.OBJ_KW, None)

        if word == "endobj":
            return Token(TokenType.ENDOBJ_KW, None)

        if word == "stream":
            return Token(TokenType.STREAM_KW, None)

        if word == "endstream":
            return Token(TokenType.ENDSTREAM_KW, None)

        if word == "R":
            return Token(TokenType.REF_KW, None)

        return Token(TokenType.KEYWORD, word)


def _unescape_name(raw: str) -> str:
    """Resolve #XX hex escapes in a PDF name."""
    if "#" not in raw:
        return raw

    result = []
    i = 0
    while i < len(raw):
        if raw[i] == "#" and i + 2 < len(raw):
            try:
                result.append(chr(int(raw[i + 1:i + 3], 16)))
                i += 3
                continue

            except ValueError:
                pass

        result.append(raw[i])
        i += 1

    return "".join(result)
