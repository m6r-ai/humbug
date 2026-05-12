import pytest

from pdf.pdf_errors import PDFError, PDFExtractionError, PDFParseError, PDFUnsupportedError


class TestPDFError:
    def test_message_stored(self) -> None:
        e = PDFError("something went wrong")
        assert e.message == "something went wrong"

    def test_str(self) -> None:
        e = PDFError("something went wrong")
        assert str(e) == "something went wrong"

    def test_is_exception(self) -> None:
        with pytest.raises(PDFError):
            raise PDFError("boom")


class TestPDFParseError:
    def test_is_pdf_error(self) -> None:
        e = PDFParseError("bad structure")
        assert isinstance(e, PDFError)

    def test_message(self) -> None:
        e = PDFParseError("bad structure")
        assert e.message == "bad structure"
        assert str(e) == "bad structure"


class TestPDFUnsupportedError:
    def test_is_pdf_error(self) -> None:
        e = PDFUnsupportedError("encrypted")
        assert isinstance(e, PDFError)

    def test_message(self) -> None:
        e = PDFUnsupportedError("encrypted")
        assert str(e) == "encrypted"


class TestPDFExtractionError:
    def test_is_pdf_error(self) -> None:
        e = PDFExtractionError("no pages")
        assert isinstance(e, PDFError)

    def test_message(self) -> None:
        e = PDFExtractionError("no pages")
        assert str(e) == "no pages"
