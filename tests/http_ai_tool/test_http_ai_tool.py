"""Tests for the HTTP AI tool."""

import asyncio
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from ai_tool import AIToolCall, AIToolAuthorizationDenied, AIToolExecutionError
from http_ai_tool.http_ai_tool import HttpAITool
from mindspace.mindspace import Mindspace


def _make_mindspace_mock(mindspace_path: str = "/tmp/mindspace") -> MagicMock:
    """Create a mock Mindspace with the given path."""
    mock = MagicMock(spec=Mindspace)
    mock.mindspace_path.return_value = mindspace_path
    mock.MINDSPACE_DIR = ".humbug"
    return mock


def _make_tool_call(operation: str, **kwargs: Any) -> AIToolCall:
    """Create an AIToolCall for the given operation and arguments."""
    arguments: dict[str, Any] = {"operation": operation}
    arguments.update(kwargs)
    return AIToolCall(id="test-id", name="http", arguments=arguments)


def _make_mock_response(
    status: int = 200,
    headers: dict[str, str] | None = None,
    text: str = "",
    json_data: dict[str, Any] | None = None,
) -> MagicMock:
    """Create a mock HttpResponse."""
    response = MagicMock()
    response.status.return_value = status
    response.headers.return_value = headers or {}

    if json_data is not None:
        response.json = AsyncMock(return_value=json_data)

    response.text = AsyncMock(return_value=text)

    return response


def _make_auth_callback(authorized: bool = True) -> MagicMock:
    """Create a mocked authorization callback."""
    mock = MagicMock()

    async def mock_auth_callback(
        _tool_name: str,
        _arguments: dict[str, Any],
        _context: str,
        _requester_ref: Any,
        _destructive: bool
    ) -> bool:
        return authorized

    mock.side_effect = mock_auth_callback
    return mock


def _execute_tool(tool: HttpAITool, tool_call: AIToolCall, auth_callback: MagicMock | None = None) -> Any:
    """Execute a tool call synchronously and return the result."""
    if auth_callback is None:
        auth_callback = _make_auth_callback(authorized=True)
    return asyncio.run(tool.execute(tool_call, None, auth_callback))


class TestHttpAIToolDefinition:
    """Tests for tool definition and metadata."""

    def test_definition_has_correct_name(self) -> None:
        """Tool name should be 'http'."""
        tool = HttpAITool(_make_mindspace_mock())
        definition = tool.get_definition()
        assert definition.name == "http"

    def test_definition_has_operations(self) -> None:
        """Tool should define all expected operations."""
        tool = HttpAITool(_make_mindspace_mock())
        operations = tool.get_operation_definitions()
        assert "get" in operations
        assert "head" in operations
        assert "download" in operations
        assert "post" in operations
        assert "put" in operations
        assert "patch" in operations
        assert "delete" in operations

    def test_brief_description(self) -> None:
        """Brief description should mention HTTP operations."""
        tool = HttpAITool(_make_mindspace_mock())
        brief = tool.get_brief_description()
        assert "HTTP" in brief
        assert "GET" in brief
        assert "POST" in brief

    def test_get_operation_requires_url(self) -> None:
        """GET operation should require url parameter."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert "url" in ops["get"].required_parameters

    def test_post_operation_requires_url(self) -> None:
        """POST operation should require url parameter."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert "url" in ops["post"].required_parameters

    def test_get_operation_allows_format(self) -> None:
        """GET operation should allow format parameter."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert "format" in ops["get"].allowed_parameters

    def test_post_operation_allows_json_and_data(self) -> None:
        """POST operation should allow json and data parameters."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert "json" in ops["post"].allowed_parameters
        assert "data" in ops["post"].allowed_parameters

    def test_format_parameter_has_enum(self) -> None:
        """Format parameter should have markdown and raw enum values."""
        tool = HttpAITool(_make_mindspace_mock())
        definition = tool.get_definition()
        format_param = next(p for p in definition.parameters if p.name == "format")
        assert format_param.enum == ["markdown", "raw"]


class TestHttpAIToolGet:
    """Tests for the GET operation."""

    def test_get_requires_url(self) -> None:
        """GET without url should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get")
        with pytest.raises(AIToolExecutionError, match="url"):
            _execute_tool(tool, tool_call)

    def test_get_requires_authorization(self) -> None:
        """GET should raise AIToolAuthorizationDenied when denied."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        auth = _make_auth_callback(authorized=False)
        with pytest.raises(AIToolAuthorizationDenied):
            _execute_tool(tool, tool_call, auth)

    def test_get_validates_url_scheme(self) -> None:
        """GET with missing scheme should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="example.com")
        with pytest.raises(AIToolExecutionError, match="scheme"):
            _execute_tool(tool, tool_call)

    def test_get_rejects_non_http_scheme(self) -> None:
        """GET with non-http scheme should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="ftp://example.com")
        with pytest.raises(AIToolExecutionError, match="http.*https"):
            _execute_tool(tool, tool_call)

    def test_get_rejects_missing_hostname(self) -> None:
        """GET with missing hostname should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://")
        with pytest.raises(AIToolExecutionError, match="hostname"):
            _execute_tool(tool, tool_call)

    def test_get_returns_status_and_content_type(self) -> None:
        """GET should return status code and content type in the result."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            status=200,
            headers={"content-type": "text/plain"},
            text="Hello, world!"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Status: 200" in result.content
        assert "Content-Type: text/plain" in result.content
        assert "Hello, world!" in result.content

    def test_get_passes_headers(self) -> None:
        """GET should pass headers to the HTTP client."""
        tool = HttpAITool(_make_mindspace_mock())
        headers = {"Authorization": "Bearer token123"}
        tool_call = _make_tool_call("get", url="https://example.com", headers=headers)
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.get.assert_called_once_with("https://example.com", headers=headers)

    def test_get_html_converts_to_markdown(self) -> None:
        """GET with text/html should convert to markdown."""
        tool = HttpAITool(_make_mindspace_mock())
        html = "<html><body><h1>Title</h1><p>Hello</p></body></html>"
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            headers={"content-type": "text/html"},
            text=html
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "# Title" in result.content
        assert "Hello" in result.content
        assert "<html>" not in result.content

    def test_get_html_raw_format_returns_html(self) -> None:
        """GET with format=raw should return raw HTML."""
        tool = HttpAITool(_make_mindspace_mock())
        html = "<html><body><h1>Title</h1></body></html>"
        tool_call = _make_tool_call("get", url="https://example.com", format="raw")
        mock_response = _make_mock_response(
            headers={"content-type": "text/html"},
            text=html
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "<html>" in result.content
        assert "<h1>Title</h1>" in result.content

    def test_get_json_pretty_printed(self) -> None:
        """GET with application/json should pretty-print the JSON."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com/api")
        mock_response = _make_mock_response(
            headers={"content-type": "application/json"},
            json_data={"name": "test", "value": 42}
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert '"name": "test"' in result.content
        assert '"value": 42' in result.content

    def test_get_error_status_returns_body(self) -> None:
        """GET with 404 should still return the response body."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com/missing")
        mock_response = _make_mock_response(
            status=404,
            headers={"content-type": "text/plain"},
            text="Not Found"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Status: 404" in result.content
        assert "Not Found" in result.content

    def test_get_truncates_large_response(self) -> None:
        """GET should truncate responses over 64KB."""
        tool = HttpAITool(_make_mindspace_mock())
        large_text = "A" * (70 * 1024)
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(text=large_text)
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "truncated" in result.content

    def test_get_connection_error_raises_execution_error(self) -> None:
        """GET with a connection error should raise AIToolExecutionError."""
        from http_client import ClientConnectorError

        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(side_effect=ClientConnectorError("Connection refused"))
            mock_client_class.return_value = mock_client

            with pytest.raises(AIToolExecutionError, match="HTTP request failed"):
                _execute_tool(tool, tool_call)


class TestHttpAIToolHead:
    """Tests for the HEAD operation."""

    def test_head_requires_url(self) -> None:
        """HEAD without url should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head")
        with pytest.raises(AIToolExecutionError, match="url"):
            _execute_tool(tool, tool_call)

    def test_head_requires_authorization(self) -> None:
        """HEAD should raise AIToolAuthorizationDenied when denied."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head", url="https://example.com")
        auth = _make_auth_callback(authorized=False)
        with pytest.raises(AIToolAuthorizationDenied):
            _execute_tool(tool, tool_call, auth)

    def test_head_validates_url_scheme(self) -> None:
        """HEAD with missing scheme should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head", url="example.com")
        with pytest.raises(AIToolExecutionError, match="scheme"):
            _execute_tool(tool, tool_call)

    def test_head_returns_status_and_headers(self) -> None:
        """HEAD should return status code and all response headers."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head", url="https://example.com")
        mock_response = MagicMock()
        mock_response.status.return_value = 200
        mock_response.headers.return_value = {
            "content-type": "text/html",
            "content-length": "1234",
            "last-modified": "Wed, 01 Jan 2025 00:00:00 GMT",
        }
        mock_response.content = AsyncMock(return_value=b"")

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.head = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Status: 200" in result.content
        assert "content-type: text/html" in result.content
        assert "content-length: 1234" in result.content
        assert "last-modified:" in result.content

    def test_head_passes_headers(self) -> None:
        """HEAD should pass headers to the HTTP client."""
        tool = HttpAITool(_make_mindspace_mock())
        headers = {"Authorization": "Bearer token"}
        tool_call = _make_tool_call("head", url="https://example.com", headers=headers)
        mock_response = MagicMock()
        mock_response.status.return_value = 200
        mock_response.headers.return_value = {}
        mock_response.content = AsyncMock(return_value=b"")

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.head = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.head.assert_called_once_with("https://example.com", headers=headers)

    def test_head_marks_non_destructive(self) -> None:
        """HEAD should mark the authorization request as non-destructive."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head", url="https://example.com")
        mock_response = MagicMock()
        mock_response.status.return_value = 200
        mock_response.headers.return_value = {}
        mock_response.content = AsyncMock(return_value=b"")

        captured_destructive: list[bool] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            _context: str,
            _requester_ref: Any,
            destructive: bool
        ) -> bool:
            captured_destructive.append(destructive)
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.head = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert captured_destructive == [False]

    def test_head_error_status_still_returns_headers(self) -> None:
        """HEAD with 404 should still return the status and headers."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head", url="https://example.com/missing")
        mock_response = MagicMock()
        mock_response.status.return_value = 404
        mock_response.headers.return_value = {"content-type": "text/plain"}
        mock_response.content = AsyncMock(return_value=b"")

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.head = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Status: 404" in result.content

    def test_head_context(self) -> None:
        """HEAD context should include the URL."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        result = ops["head"].extract_context({"url": "https://example.com"})
        assert result == "HEAD https://example.com"


class TestHttpAIToolPost:
    """Tests for the POST operation."""

    def test_post_requires_url(self) -> None:
        """POST without url should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("post")
        with pytest.raises(AIToolExecutionError, match="url"):
            _execute_tool(tool, tool_call)

    def test_post_requires_authorization(self) -> None:
        """POST should raise AIToolAuthorizationDenied when denied."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("post", url="https://example.com")
        auth = _make_auth_callback(authorized=False)
        with pytest.raises(AIToolAuthorizationDenied):
            _execute_tool(tool, tool_call, auth)

    def test_post_with_json_body(self) -> None:
        """POST should send JSON body when provided."""
        tool = HttpAITool(_make_mindspace_mock())
        json_body = {"name": "test", "value": 42}
        tool_call = _make_tool_call("post", url="https://example.com/api", json=json_body)
        mock_response = _make_mock_response(
            headers={"content-type": "application/json"},
            json_data={"success": True}
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        mock_client.post.assert_called_once_with(
            "https://example.com/api",
            headers=None,
            json=json_body,
            data=None,
        )
        assert "Status: 200" in result.content

    def test_post_with_raw_data(self) -> None:
        """POST should send raw data when provided."""
        tool = HttpAITool(_make_mindspace_mock())
        data_body = "raw text body"
        tool_call = _make_tool_call("post", url="https://example.com/api", data=data_body)
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.post.assert_called_once_with(
            "https://example.com/api",
            headers=None,
            json=None,
            data=b"raw text body",
        )

    def test_post_json_takes_precedence_over_data(self) -> None:
        """POST with both json and data should use json."""
        tool = HttpAITool(_make_mindspace_mock())
        json_body = {"key": "value"}
        data_body = "raw text"
        tool_call = _make_tool_call(
            "post",
            url="https://example.com/api",
            json=json_body,
            data=data_body
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.post.assert_called_once_with(
            "https://example.com/api",
            headers=None,
            json=json_body,
            data=None,
        )

    def test_post_without_body(self) -> None:
        """POST without json or data should send empty body."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("post", url="https://example.com/api")
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.post.assert_called_once_with(
            "https://example.com/api",
            headers=None,
            json=None,
            data=None,
        )

    def test_post_marks_destructive(self) -> None:
        """POST should mark the authorization request as destructive."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("post", url="https://example.com/api", json={"x": 1})
        mock_response = _make_mock_response(text="OK")

        captured_destructive: list[bool] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            _context: str,
            _requester_ref: Any,
            destructive: bool
        ) -> bool:
            captured_destructive.append(destructive)
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert captured_destructive == [True]

    def test_get_marks_non_destructive(self) -> None:
        """GET should mark the authorization request as non-destructive."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(text="OK")

        captured_destructive: list[bool] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            _context: str,
            _requester_ref: Any,
            destructive: bool
        ) -> bool:
            captured_destructive.append(destructive)
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert captured_destructive == [False]


class TestHttpAIToolPut:
    """Tests for the PUT operation."""

    def test_put_requires_url(self) -> None:
        """PUT without url should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("put")
        with pytest.raises(AIToolExecutionError, match="url"):
            _execute_tool(tool, tool_call)

    def test_put_requires_authorization(self) -> None:
        """PUT should raise AIToolAuthorizationDenied when denied."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("put", url="https://example.com")
        auth = _make_auth_callback(authorized=False)
        with pytest.raises(AIToolAuthorizationDenied):
            _execute_tool(tool, tool_call, auth)

    def test_put_with_json_body(self) -> None:
        """PUT should send JSON body when provided."""
        tool = HttpAITool(_make_mindspace_mock())
        json_body = {"name": "updated", "value": 99}
        tool_call = _make_tool_call("put", url="https://example.com/api/1", json=json_body)
        mock_response = _make_mock_response(
            headers={"content-type": "application/json"},
            json_data={"success": True}
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.put = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        mock_client.put.assert_called_once_with(
            "https://example.com/api/1",
            headers=None,
            json=json_body,
            data=None,
        )
        assert "Status: 200" in result.content

    def test_put_with_raw_data(self) -> None:
        """PUT should send raw data when provided."""
        tool = HttpAITool(_make_mindspace_mock())
        data_body = "raw put body"
        tool_call = _make_tool_call("put", url="https://example.com/api/1", data=data_body)
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.put = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.put.assert_called_once_with(
            "https://example.com/api/1",
            headers=None,
            json=None,
            data=b"raw put body",
        )

    def test_put_marks_destructive(self) -> None:
        """PUT should mark the authorization request as destructive."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("put", url="https://example.com/api/1", json={"x": 1})
        mock_response = _make_mock_response(text="OK")

        captured_destructive: list[bool] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            _context: str,
            _requester_ref: Any,
            destructive: bool
        ) -> bool:
            captured_destructive.append(destructive)
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.put = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert captured_destructive == [True]

    def test_put_context(self) -> None:
        """PUT context should include the URL."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        result = ops["put"].extract_context({"url": "https://example.com"})
        assert result == "PUT https://example.com"


class TestHttpAIToolPatch:
    """Tests for the PATCH operation."""

    def test_patch_requires_url(self) -> None:
        """PATCH without url should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("patch")
        with pytest.raises(AIToolExecutionError, match="url"):
            _execute_tool(tool, tool_call)

    def test_patch_requires_authorization(self) -> None:
        """PATCH should raise AIToolAuthorizationDenied when denied."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("patch", url="https://example.com")
        auth = _make_auth_callback(authorized=False)
        with pytest.raises(AIToolAuthorizationDenied):
            _execute_tool(tool, tool_call, auth)

    def test_patch_with_json_body(self) -> None:
        """PATCH should send JSON body when provided."""
        tool = HttpAITool(_make_mindspace_mock())
        json_body = {"op": "replace", "path": "/name", "value": "new"}
        tool_call = _make_tool_call("patch", url="https://example.com/api/1", json=json_body)
        mock_response = _make_mock_response(
            headers={"content-type": "application/json"},
            json_data={"success": True}
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.patch = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        mock_client.patch.assert_called_once_with(
            "https://example.com/api/1",
            headers=None,
            json=json_body,
            data=None,
        )
        assert "Status: 200" in result.content

    def test_patch_with_raw_data(self) -> None:
        """PATCH should send raw data when provided."""
        tool = HttpAITool(_make_mindspace_mock())
        data_body = "raw patch body"
        tool_call = _make_tool_call("patch", url="https://example.com/api/1", data=data_body)
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.patch = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.patch.assert_called_once_with(
            "https://example.com/api/1",
            headers=None,
            json=None,
            data=b"raw patch body",
        )

    def test_patch_marks_destructive(self) -> None:
        """PATCH should mark the authorization request as destructive."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("patch", url="https://example.com/api/1", json={"x": 1})
        mock_response = _make_mock_response(text="OK")

        captured_destructive: list[bool] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            _context: str,
            _requester_ref: Any,
            destructive: bool
        ) -> bool:
            captured_destructive.append(destructive)
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.patch = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert captured_destructive == [True]

    def test_patch_context(self) -> None:
        """PATCH context should include the URL."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        result = ops["patch"].extract_context({"url": "https://example.com"})
        assert result == "PATCH https://example.com"


class TestHttpAIToolDelete:
    """Tests for the DELETE operation."""

    def test_delete_requires_url(self) -> None:
        """DELETE without url should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("delete")
        with pytest.raises(AIToolExecutionError, match="url"):
            _execute_tool(tool, tool_call)

    def test_delete_requires_authorization(self) -> None:
        """DELETE should raise AIToolAuthorizationDenied when denied."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("delete", url="https://example.com")
        auth = _make_auth_callback(authorized=False)
        with pytest.raises(AIToolAuthorizationDenied):
            _execute_tool(tool, tool_call, auth)

    def test_delete_calls_client_delete(self) -> None:
        """DELETE should call the client's delete method."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("delete", url="https://example.com/api/1")
        mock_response = _make_mock_response(
            status=204,
            headers={"content-type": "application/json"},
            text=""
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        mock_client.delete.assert_called_once_with(
            "https://example.com/api/1",
            headers=None,
        )
        assert "Status: 204" in result.content

    def test_delete_passes_headers(self) -> None:
        """DELETE should pass headers to the HTTP client."""
        tool = HttpAITool(_make_mindspace_mock())
        headers = {"Authorization": "Bearer token123"}
        tool_call = _make_tool_call("delete", url="https://example.com/api/1", headers=headers)
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.delete.assert_called_once_with(
            "https://example.com/api/1",
            headers=headers,
        )

    def test_delete_marks_destructive(self) -> None:
        """DELETE should mark the authorization request as destructive."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("delete", url="https://example.com/api/1")
        mock_response = _make_mock_response(text="OK")

        captured_destructive: list[bool] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            _context: str,
            _requester_ref: Any,
            destructive: bool
        ) -> bool:
            captured_destructive.append(destructive)
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert captured_destructive == [True]

    def test_delete_context(self) -> None:
        """DELETE context should include the URL."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        result = ops["delete"].extract_context({"url": "https://example.com"})
        assert result == "DELETE https://example.com"

    def test_delete_validates_url_scheme(self) -> None:
        """DELETE with missing scheme should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("delete", url="example.com")
        with pytest.raises(AIToolExecutionError, match="scheme"):
            _execute_tool(tool, tool_call)


class TestHttpAIToolHtmlConversion:
    """Tests for HTML to markdown conversion."""

    def test_html_with_headings(self) -> None:
        """HTML headings should convert to markdown headings."""
        tool = HttpAITool(_make_mindspace_mock())
        html = "<html><body><h1>Title</h1><h2>Subtitle</h2></body></html>"
        result = tool._html_to_markdown(html)
        assert "# Title" in result
        assert "## Subtitle" in result

    def test_html_with_paragraphs(self) -> None:
        """HTML paragraphs should convert to markdown paragraphs."""
        tool = HttpAITool(_make_mindspace_mock())
        html = "<html><body><p>First paragraph.</p><p>Second paragraph.</p></body></html>"
        result = tool._html_to_markdown(html)
        assert "First paragraph." in result
        assert "Second paragraph." in result

    def test_html_strips_scripts_and_styles(self) -> None:
        """Script and style elements should be stripped."""
        tool = HttpAITool(_make_mindspace_mock())
        html = (
            "<html><head><style>body { color: red; }</style>"
            "<script>alert('hi');</script></head>"
            "<body><p>Content</p></body></html>"
        )
        result = tool._html_to_markdown(html)
        assert "Content" in result
        assert "color" not in result
        assert "alert" not in result

    def test_html_with_links(self) -> None:
        """HTML links should convert to markdown links."""
        tool = HttpAITool(_make_mindspace_mock())
        html = '<html><body><a href="https://example.com">Link text</a></body></html>'
        result = tool._html_to_markdown(html)
        assert "[Link text]" in result
        assert "https://example.com" in result

    def test_html_with_code_blocks(self) -> None:
        """HTML pre/code blocks should convert to markdown code blocks."""
        tool = HttpAITool(_make_mindspace_mock())
        html = "<html><body><pre><code>print('hello')</code></pre></body></html>"
        result = tool._html_to_markdown(html)
        assert "```" in result
        assert "print('hello')" in result

    def test_html_conversion_failure_returns_raw_html(self) -> None:
        """If HTML conversion fails, raw HTML should be returned."""
        tool = HttpAITool(_make_mindspace_mock())
        html = "not really html"
        with patch("http_ai_tool.http_ai_tool.parse_html", side_effect=Exception("Parse error")):
            result = tool._html_to_markdown(html)
        assert result == html


class TestHttpAIToolTruncation:
    """Tests for response truncation."""

    def test_short_content_not_truncated(self) -> None:
        """Content under 64KB should not be truncated."""
        tool = HttpAITool(_make_mindspace_mock())
        content = "Hello, world!"
        result = tool._truncate(content)
        assert result == content

    def test_large_content_truncated(self) -> None:
        """Content over 64KB should be truncated with a notice."""
        tool = HttpAITool(_make_mindspace_mock())
        content = "A" * (70 * 1024)
        result = tool._truncate(content)
        assert "truncated" in result
        assert "omitted" in result

    def test_truncation_at_exact_limit(self) -> None:
        """Content at exactly 64KB should not be truncated."""
        tool = HttpAITool(_make_mindspace_mock())
        content = "A" * (64 * 1024)
        result = tool._truncate(content)
        assert result == content


class TestHttpAIToolUrlValidation:
    """Tests for URL validation."""

    def test_valid_https_url(self) -> None:
        """Valid HTTPS URL should not raise."""
        tool = HttpAITool(_make_mindspace_mock())
        tool._validate_url("https://example.com/path")

    def test_valid_http_url(self) -> None:
        """Valid HTTP URL should not raise."""
        tool = HttpAITool(_make_mindspace_mock())
        tool._validate_url("http://example.com/path")

    def test_missing_scheme_raises(self) -> None:
        """URL without scheme should raise."""
        tool = HttpAITool(_make_mindspace_mock())
        with pytest.raises(AIToolExecutionError, match="scheme"):
            tool._validate_url("example.com")

    def test_invalid_scheme_raises(self) -> None:
        """Non-http(s) scheme should raise."""
        tool = HttpAITool(_make_mindspace_mock())
        with pytest.raises(AIToolExecutionError, match="http.*https"):
            tool._validate_url("ftp://example.com")

    def test_missing_hostname_raises(self) -> None:
        """URL without hostname should raise."""
        tool = HttpAITool(_make_mindspace_mock())
        with pytest.raises(AIToolExecutionError, match="hostname"):
            tool._validate_url("https://")


class TestHttpAIToolContextExtraction:
    """Tests for context extraction."""

    def test_get_context(self) -> None:
        """GET context should include the URL."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        result = ops["get"].extract_context({"url": "https://example.com"})
        assert result == "GET https://example.com"

    def test_post_context(self) -> None:
        """POST context should include the URL."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        result = ops["post"].extract_context({"url": "https://example.com"})
        assert result == "POST https://example.com"


class TestHttpAIToolDownload:
    """Tests for the download operation."""

    def test_download_requires_url(self) -> None:
        """Download without url should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("download", destination="file.txt")
        with pytest.raises(AIToolExecutionError, match="url"):
            _execute_tool(tool, tool_call)

    def test_download_requires_destination(self) -> None:
        """Download without destination should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("download", url="https://example.com/file.txt")
        with pytest.raises(AIToolExecutionError, match="destination"):
            _execute_tool(tool, tool_call)

    def test_download_requires_authorization(self) -> None:
        """Download should raise AIToolAuthorizationDenied when denied."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("download", url="https://example.com/file.txt", destination="file.txt")
        auth = _make_auth_callback(authorized=False)
        with pytest.raises(AIToolAuthorizationDenied):
            _execute_tool(tool, tool_call, auth)

    def test_download_rejects_humbug_directory(self) -> None:
        """Download to .humbug/ should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "download",
            url="https://example.com/file.txt",
            destination=".humbug/secrets.txt"
        )
        with pytest.raises(AIToolExecutionError, match="humbug"):
            _execute_tool(tool, tool_call)

    def test_download_writes_file(self) -> None:
        """Download should write the response body to the destination file."""
        import tempfile
        import os

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download",
                url="https://example.com/data.txt",
                destination="data.txt"
            )
            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {"content-type": "text/plain"}
            mock_response.content = AsyncMock(return_value=b"file contents here")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                result = _execute_tool(tool, tool_call)

            downloaded_path = os.path.join(tmp, "data.txt")
            assert os.path.exists(downloaded_path)

            with open(downloaded_path, "rb") as f:
                assert f.read() == b"file contents here"

            assert "Status: 200" in result.content
            assert "text/plain" in result.content
            assert "data.txt" in result.content

    def test_download_marks_destructive_when_overwriting(self) -> None:
        """Download to an existing file should be marked destructive."""
        import tempfile
        import os

        with tempfile.TemporaryDirectory() as tmp:
            existing = os.path.join(tmp, "existing.txt")
            with open(existing, "w") as f:
                f.write("old content")

            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download",
                url="https://example.com/file.txt",
                destination="existing.txt"
            )

            captured_destructive: list[bool] = []

            async def capturing_auth(
                _tool_name: str,
                _arguments: dict[str, Any],
                _context: str,
                _requester_ref: Any,
                destructive: bool
            ) -> bool:
                captured_destructive.append(destructive)
                return True

            auth = MagicMock(side_effect=capturing_auth)

            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {}
            mock_response.content = AsyncMock(return_value=b"new content")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call, auth)

            assert captured_destructive == [True]

    def test_download_marks_non_destructive_for_new_file(self) -> None:
        """Download to a new file should be marked non-destructive."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download",
                url="https://example.com/file.txt",
                destination="new_file.txt"
            )

            captured_destructive: list[bool] = []

            async def capturing_auth(
                _tool_name: str,
                _arguments: dict[str, Any],
                _context: str,
                _requester_ref: Any,
                destructive: bool
            ) -> bool:
                captured_destructive.append(destructive)
                return True

            auth = MagicMock(side_effect=capturing_auth)

            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {}
            mock_response.content = AsyncMock(return_value=b"content")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call, auth)

            assert captured_destructive == [False]

    def test_download_creates_parent_directories(self) -> None:
        """Download should create parent directories if they don't exist."""
        import tempfile
        import os

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download",
                url="https://example.com/file.txt",
                destination="subdir/nested/file.txt"
            )
            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {}
            mock_response.content = AsyncMock(return_value=b"nested content")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            assert os.path.exists(os.path.join(tmp, "subdir", "nested", "file.txt"))

    def test_download_error_status_raises(self) -> None:
        """Download with a 404 should raise AIToolExecutionError."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download",
                url="https://example.com/missing.txt",
                destination="file.txt"
            )
            mock_response = MagicMock()
            mock_response.status.return_value = 404
            mock_response.headers.return_value = {"content-type": "text/plain"}
            mock_response.text = AsyncMock(return_value="Not Found")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                with pytest.raises(AIToolExecutionError, match="404"):
                    _execute_tool(tool, tool_call)

    def test_download_context(self) -> None:
        """Download context should include URL and destination."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        result = ops["download"].extract_context({
            "url": "https://example.com/file.txt",
            "destination": "local.txt"
        })
        assert "https://example.com/file.txt" in result
        assert "local.txt" in result
