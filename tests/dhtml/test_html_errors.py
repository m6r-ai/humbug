from dhtml.html_errors import HtmlError, HtmlExtractionError, HtmlParseError


class TestHtmlErrorHierarchy:
    """HtmlError is the base; parse and extraction errors are subclasses."""

    def test_html_parse_error_is_html_error(self) -> None:
        err = HtmlParseError("bad input")
        assert isinstance(err, HtmlError)

    def test_html_extraction_error_is_html_error(self) -> None:
        err = HtmlExtractionError("no body")
        assert isinstance(err, HtmlError)

    def test_html_parse_error_is_not_extraction_error(self) -> None:
        err = HtmlParseError("parse")
        assert not isinstance(err, HtmlExtractionError)

    def test_html_extraction_error_is_not_parse_error(self) -> None:
        err = HtmlExtractionError("extract")
        assert not isinstance(err, HtmlParseError)

    def test_html_error_is_exception(self) -> None:
        err = HtmlError("base")
        assert isinstance(err, Exception)

    def test_parse_error_message_preserved(self) -> None:
        err = HtmlParseError("something went wrong")
        assert str(err) == "something went wrong"

    def test_extraction_error_message_preserved(self) -> None:
        err = HtmlExtractionError("extraction failed")
        assert str(err) == "extraction failed"

    def test_html_error_can_be_raised_and_caught(self) -> None:
        try:
            raise HtmlParseError("oops")
        except HtmlError as exc:
            assert str(exc) == "oops"
        else:
            assert False, "Exception was not raised"
