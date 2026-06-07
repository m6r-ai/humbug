from dhtml.html_errors import HtmlError, HtmlParseError


class TestHtmlErrorHierarchy:
    """HtmlError is the base; HtmlParseError is a subclass."""

    def test_html_parse_error_is_html_error(self) -> None:
        err = HtmlParseError("bad input")
        assert isinstance(err, HtmlError)

    def test_html_error_is_exception(self) -> None:
        err = HtmlError("base")
        assert isinstance(err, Exception)

    def test_parse_error_message_preserved(self) -> None:
        err = HtmlParseError("something went wrong")
        assert str(err) == "something went wrong"

    def test_html_error_can_be_raised_and_caught(self) -> None:
        try:
            raise HtmlParseError("oops")
        except HtmlError as exc:
            assert str(exc) == "oops"
        else:
            assert False, "Exception was not raised"
