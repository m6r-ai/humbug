from docx.docx_errors import DocxError, DocxExtractionError, DocxParseError, DocxUnsupportedError


class TestDocxErrorHierarchy:
    def test_docx_parse_error_is_docx_error(self) -> None:
        err = DocxParseError("bad zip")
        assert isinstance(err, DocxError)

    def test_docx_unsupported_error_is_docx_error(self) -> None:
        err = DocxUnsupportedError("encrypted")
        assert isinstance(err, DocxError)

    def test_docx_extraction_error_is_docx_error(self) -> None:
        err = DocxExtractionError("no body")
        assert isinstance(err, DocxError)

    def test_message_attribute(self) -> None:
        err = DocxParseError("something went wrong")
        assert err.message == "something went wrong"

    def test_str_representation(self) -> None:
        err = DocxUnsupportedError("encrypted file")
        assert str(err) == "encrypted file"

    def test_errors_are_not_interchangeable(self) -> None:
        parse_err = DocxParseError("parse")
        assert not isinstance(parse_err, DocxUnsupportedError)
        assert not isinstance(parse_err, DocxExtractionError)
