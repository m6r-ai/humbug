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

    def test_get_large_response_body_raises_execution_error(self) -> None:
        """GET should fail when the response body exceeds the inline size limit."""
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

            with pytest.raises(AIToolExecutionError, match="too large to return inline") as exc_info:
                _execute_tool(tool, tool_call)

        assert "download" in str(exc_info.value)
        assert str(70 * 1024) in str(exc_info.value)

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

    def test_head_has_no_extract_context(self) -> None:
        """HEAD should not attach extract_context (URL is in the approval reason)."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert ops["head"].extract_context is None


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

    def test_put_has_no_extract_context(self) -> None:
        """PUT should not attach extract_context (URL is in the approval reason)."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert ops["put"].extract_context is None


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

    def test_patch_has_no_extract_context(self) -> None:
        """PATCH should not attach extract_context (URL is in the approval reason)."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert ops["patch"].extract_context is None


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

    def test_delete_has_no_extract_context(self) -> None:
        """DELETE should not attach extract_context (URL is in the approval reason)."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert ops["delete"].extract_context is None

    def test_delete_validates_url_scheme(self) -> None:
        """DELETE with missing scheme should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("delete", url="example.com")
        with pytest.raises(AIToolExecutionError, match="scheme"):
            _execute_tool(tool, tool_call)


class TestHttpAIToolMultipart:
    """Tests for multipart/form-data support."""

    def test_multipart_with_text_parts(self) -> None:
        """POST with multipart text parts should build correct body."""
        tool = HttpAITool(_make_mindspace_mock())
        parts = [
            {"name": "field1", "content": "value1"},
            {"name": "field2", "content": "value2"},
        ]
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", multipart=parts
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client.post.assert_called_once()
        call_kwargs = mock_client.post.call_args
        headers = call_kwargs.kwargs["headers"]
        assert "multipart/form-data" in headers["Content-Type"]
        assert "boundary=" in headers["Content-Type"]

        body = call_kwargs.kwargs["data"]
        assert b'name="field1"' in body
        assert b'value1' in body
        assert b'name="field2"' in body
        assert b'value2' in body

    def test_multipart_with_file_part(self) -> None:
        """POST with multipart file part should read file and include it."""
        import tempfile
        import os

        with tempfile.TemporaryDirectory() as tmp:
            file_path = os.path.join(tmp, "upload.txt")
            with open(file_path, "wb") as f:
                f.write(b"file contents here")

            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            parts = [
                {"name": "file", "file_path": "upload.txt"},
            ]
            tool_call = _make_tool_call(
                "post", url="https://example.com/upload", multipart=parts
            )
            mock_response = _make_mock_response(text="Uploaded")
            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.post = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            call_kwargs = mock_client.post.call_args.kwargs
            body = call_kwargs["data"]
            assert b"file contents here" in body
            assert b'filename="upload.txt"' in body
            assert b"application/octet-stream" in body

    def test_multipart_file_part_with_explicit_filename_and_type(self) -> None:
        """Multipart file part should use explicit filename and content_type if provided."""
        import tempfile
        import os

        with tempfile.TemporaryDirectory() as tmp:
            file_path = os.path.join(tmp, "data.bin")
            with open(file_path, "wb") as f:
                f.write(b"\x00\x01\x02binary")

            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            parts = [
                {
                    "name": "attachment",
                    "file_path": "data.bin",
                    "filename": "report.pdf",
                    "content_type": "application/pdf",
                },
            ]
            tool_call = _make_tool_call(
                "post", url="https://example.com/upload", multipart=parts
            )
            mock_response = _make_mock_response(text="OK")
            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.post = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            body = mock_client.post.call_args.kwargs["data"]
            assert b'filename="report.pdf"' in body
            assert b"application/pdf" in body

    def test_multipart_takes_precedence_over_json(self) -> None:
        """Multipart should take precedence over json."""
        tool = HttpAITool(_make_mindspace_mock())
        parts = [{"name": "field", "content": "value"}]
        tool_call = _make_tool_call(
            "post",
            url="https://example.com/api",
            multipart=parts,
            json={"key": "value"},
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_kwargs = mock_client.post.call_args.kwargs
        assert "multipart/form-data" in call_kwargs["headers"]["Content-Type"]
        assert "json" not in call_kwargs
        assert call_kwargs["data"] is not None

    def test_multipart_mixed_text_and_file(self) -> None:
        """POST with mixed text and file parts should include both."""
        import tempfile
        import os

        with tempfile.TemporaryDirectory() as tmp:
            file_path = os.path.join(tmp, "doc.txt")
            with open(file_path, "wb") as f:
                f.write(b"document content")

            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            parts = [
                {"name": "title", "content": "My Document"},
                {"name": "file", "file_path": "doc.txt"},
            ]
            tool_call = _make_tool_call(
                "post", url="https://example.com/upload", multipart=parts
            )
            mock_response = _make_mock_response(text="OK")
            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.post = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            body = mock_client.post.call_args.kwargs["data"]
            assert b"My Document" in body
            assert b"document content" in body
            assert b'name="title"' in body
            assert b'name="file"' in body

    def test_multipart_empty_parts_raises(self) -> None:
        """Multipart with empty parts array should raise."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", multipart=[]
        )
        with pytest.raises(AIToolExecutionError, match="non-empty"):
            _execute_tool(tool, tool_call)

    def test_multipart_part_missing_name_raises(self) -> None:
        """Multipart part without name should raise."""
        tool = HttpAITool(_make_mindspace_mock())
        parts = [{"content": "value"}]
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", multipart=parts
        )
        with pytest.raises(AIToolExecutionError, match="name"):
            _execute_tool(tool, tool_call)

    def test_multipart_part_both_content_and_file_path_raises(self) -> None:
        """Multipart part with both content and file_path should raise."""
        tool = HttpAITool(_make_mindspace_mock())
        parts = [{"name": "field", "content": "val", "file_path": "some.txt"}]
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", multipart=parts
        )
        with pytest.raises(AIToolExecutionError, match="both"):
            _execute_tool(tool, tool_call)

    def test_multipart_part_neither_content_nor_file_path_raises(self) -> None:
        """Multipart part with neither content nor file_path should raise."""
        tool = HttpAITool(_make_mindspace_mock())
        parts = [{"name": "field"}]
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", multipart=parts
        )
        with pytest.raises(AIToolExecutionError, match="either"):
            _execute_tool(tool, tool_call)

    def test_multipart_file_outside_mindspace_raises(self) -> None:
        """Multipart file_path outside mindspace should raise."""
        tool = HttpAITool(_make_mindspace_mock("/tmp/mindspace"))
        parts = [{"name": "file", "file_path": "/etc/passwd"}]
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", multipart=parts
        )
        with pytest.raises(AIToolExecutionError, match="outside"):
            _execute_tool(tool, tool_call)

    def test_multipart_file_in_humbug_dir_raises(self) -> None:
        """Multipart file_path inside .humbug/ should raise."""
        tool = HttpAITool(_make_mindspace_mock("/tmp/mindspace"))
        parts = [{"name": "file", "file_path": ".humbug/secrets.txt"}]
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", multipart=parts
        )
        with pytest.raises(AIToolExecutionError, match="humbug"):
            _execute_tool(tool, tool_call)

    def test_multipart_file_not_found_raises(self) -> None:
        """Multipart file_path for nonexistent file should raise."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            parts = [{"name": "file", "file_path": "nonexistent.txt"}]
            tool_call = _make_tool_call(
                "post", url="https://example.com/api", multipart=parts
            )
            with pytest.raises(AIToolExecutionError, match="does not exist"):
                _execute_tool(tool, tool_call)

    def test_multipart_works_with_put(self) -> None:
        """PUT with multipart should also work."""
        tool = HttpAITool(_make_mindspace_mock())
        parts = [{"name": "field", "content": "value"}]
        tool_call = _make_tool_call(
            "put", url="https://example.com/api/1", multipart=parts
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.put = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_kwargs = mock_client.put.call_args.kwargs
        assert "multipart/form-data" in call_kwargs["headers"]["Content-Type"]
        assert call_kwargs["data"] is not None


class TestHttpAIToolBasicAuth:
    """Tests for HTTP Basic Authentication support."""

    def test_basic_auth_sets_authorization_header_on_get(self) -> None:
        """GET with username/password should set Basic Auth header."""
        import base64

        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com", username="user", password="pass"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        expected = base64.b64encode(b"user:pass").decode("ascii")
        assert call_headers["Authorization"] == f"Basic {expected}"

    def test_basic_auth_sets_authorization_header_on_post(self) -> None:
        """POST with username/password should set Basic Auth header."""
        import base64

        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "post", url="https://example.com/api",
            json={"x": 1}, username="admin", password="secret"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.post.call_args.kwargs["headers"]
        expected = base64.b64encode(b"admin:secret").decode("ascii")
        assert call_headers["Authorization"] == f"Basic {expected}"

    def test_basic_auth_sets_authorization_header_on_delete(self) -> None:
        """DELETE with username/password should set Basic Auth header."""
        import base64

        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "delete", url="https://example.com/api/1",
            username="user", password="pass"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.delete.call_args.kwargs["headers"]
        expected = base64.b64encode(b"user:pass").decode("ascii")
        assert call_headers["Authorization"] == f"Basic {expected}"

    def test_basic_auth_sets_authorization_header_on_download(self) -> None:
        """Download with username/password should set Basic Auth header."""
        import base64
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download", url="https://example.com/file.txt",
                destination="file.txt", username="user", password="pass"
            )
            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {}
            mock_response.content = AsyncMock(return_value=b"data")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            call_headers = mock_client.get.call_args.kwargs["headers"]
            expected = base64.b64encode(b"user:pass").decode("ascii")
            assert call_headers["Authorization"] == f"Basic {expected}"

    def test_basic_auth_sets_authorization_header_on_head(self) -> None:
        """HEAD with username/password should set Basic Auth header."""
        import base64

        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "head", url="https://example.com",
            username="user", password="pass"
        )
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

        call_headers = mock_client.head.call_args.kwargs["headers"]
        expected = base64.b64encode(b"user:pass").decode("ascii")
        assert call_headers["Authorization"] == f"Basic {expected}"

    def test_explicit_authorization_header_takes_precedence(self) -> None:
        """Explicit Authorization header should take precedence over Basic Auth."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            headers={"Authorization": "Bearer token123"},
            username="user", password="pass"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        assert call_headers["Authorization"] == "Bearer token123"

    def test_basic_auth_without_username_does_not_set_header(self) -> None:
        """Only password (no username) should not set Authorization header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com", password="pass"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"] or {}
        assert "Authorization" not in call_headers

    def test_basic_auth_without_password_does_not_set_header(self) -> None:
        """Only username (no password) should not set Authorization header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com", username="user"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"] or {}
        assert "Authorization" not in call_headers

    def test_basic_auth_no_credentials_does_not_set_header(self) -> None:
        """No username or password should not set Authorization header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"] or {}
        assert "Authorization" not in call_headers

    def test_basic_auth_preserves_other_headers(self) -> None:
        """Basic Auth should preserve other caller-supplied headers."""
        import base64

        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            headers={"X-Custom": "value", "Accept": "application/json"},
            username="user", password="pass"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        assert call_headers["X-Custom"] == "value"
        assert call_headers["Accept"] == "application/json"
        assert "Authorization" in call_headers

    def test_basic_auth_case_insensitive_header_check(self) -> None:
        """Explicit 'authorization' (lowercase) should take precedence too."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            headers={"authorization": "Bearer token123"},
            username="user", password="pass"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        assert call_headers["authorization"] == "Bearer token123"
        assert "Authorization" not in call_headers


class TestHttpAIToolTimeout:
    """Tests for configurable timeout support."""

    def test_timeout_passes_to_client_on_get(self) -> None:
        """GET with timeout should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com", timeout=10)
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(read_timeout=10.0, proxy=None)

    def test_timeout_passes_to_client_on_post(self) -> None:
        """POST with timeout should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", json={"x": 1}, timeout=30
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(read_timeout=30.0, proxy=None)

    def test_timeout_passes_to_client_on_delete(self) -> None:
        """DELETE with timeout should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "delete", url="https://example.com/api/1", timeout=5
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(read_timeout=5.0, proxy=None)

    def test_timeout_passes_to_client_on_head(self) -> None:
        """HEAD with timeout should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "head", url="https://example.com", timeout=15
        )
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

        mock_client_class.assert_called_once_with(read_timeout=15.0, proxy=None)

    def test_timeout_passes_to_client_on_download(self) -> None:
        """Download with timeout should pass it to the HttpClient constructor."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download", url="https://example.com/file.txt",
                destination="file.txt", timeout=60
            )
            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {}
            mock_response.content = AsyncMock(return_value=b"data")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            mock_client_class.assert_called_once_with(read_timeout=60.0, proxy=None)

    def test_default_timeout_when_not_specified(self) -> None:
        """GET without timeout should use the default 300s read timeout."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(read_timeout=300.0, proxy=None)

    def test_timeout_float_value(self) -> None:
        """GET with a float timeout should pass it as a float."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com", timeout=0.5)
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(read_timeout=0.5, proxy=None)

    def test_timeout_zero_raises(self) -> None:
        """GET with timeout=0 should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com", timeout=0)
        with pytest.raises(AIToolExecutionError, match="positive"):
            _execute_tool(tool, tool_call)

    def test_timeout_negative_raises(self) -> None:
        """GET with a negative timeout should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com", timeout=-5)
        with pytest.raises(AIToolExecutionError, match="positive"):
            _execute_tool(tool, tool_call)

    def test_timeout_non_number_raises(self) -> None:
        """GET with a non-numeric timeout should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com", timeout="10")
        with pytest.raises(AIToolExecutionError, match="number"):
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


class TestHttpAIToolInlineBodyLimit:
    """Tests for the inline response body size limit."""

    def test_short_content_accepted(self) -> None:
        """Content under 64KB should be accepted for inline return."""
        tool = HttpAITool(_make_mindspace_mock())
        content = "Hello, world!"
        tool._ensure_inline_body_fits(content)

    def test_large_content_raises_execution_error(self) -> None:
        """Content over 64KB should fail with AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        content = "A" * (70 * 1024)
        with pytest.raises(AIToolExecutionError, match="too large to return inline") as exc_info:
            tool._ensure_inline_body_fits(content)

        message = str(exc_info.value)
        assert "download" in message
        assert str(70 * 1024) in message
        assert str(64 * 1024) in message

    def test_content_at_exact_limit_accepted(self) -> None:
        """Content at exactly 64KB should be accepted for inline return."""
        tool = HttpAITool(_make_mindspace_mock())
        content = "A" * (64 * 1024)
        tool._ensure_inline_body_fits(content)


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

    def test_get_has_no_extract_context(self) -> None:
        """GET should not attach extract_context (URL is in the approval reason)."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert ops["get"].extract_context is None

    def test_post_has_no_extract_context(self) -> None:
        """POST should not attach extract_context (URL is in the approval reason)."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert ops["post"].extract_context is None


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

    def test_download_has_no_extract_context(self) -> None:
        """Download should not attach extract_context (URL is in the approval reason)."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        assert ops["download"].extract_context is None


class TestHttpAIToolCookies:
    """Tests for cookie support (sending cookies and displaying Set-Cookie headers)."""

    def test_cookies_sets_cookie_header_on_get(self) -> None:
        """GET with cookies should set the Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            cookies={"session": "abc123", "token": "xyz789"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        assert "Cookie" in call_headers
        assert "session=abc123" in call_headers["Cookie"]
        assert "token=xyz789" in call_headers["Cookie"]

    def test_cookies_sets_cookie_header_on_post(self) -> None:
        """POST with cookies should set the Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "post", url="https://example.com/api",
            json={"x": 1}, cookies={"session": "abc123"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.post.call_args.kwargs["headers"]
        assert "Cookie" in call_headers
        assert "session=abc123" in call_headers["Cookie"]

    def test_cookies_sets_cookie_header_on_delete(self) -> None:
        """DELETE with cookies should set the Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "delete", url="https://example.com/api/1",
            cookies={"session": "abc123"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.delete.call_args.kwargs["headers"]
        assert "Cookie" in call_headers
        assert "session=abc123" in call_headers["Cookie"]

    def test_cookies_sets_cookie_header_on_head(self) -> None:
        """HEAD with cookies should set the Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "head", url="https://example.com",
            cookies={"session": "abc123"}
        )
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

        call_headers = mock_client.head.call_args.kwargs["headers"]
        assert "Cookie" in call_headers
        assert "session=abc123" in call_headers["Cookie"]

    def test_cookies_sets_cookie_header_on_download(self) -> None:
        """Download with cookies should set the Cookie header."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download", url="https://example.com/file.txt",
                destination="file.txt", cookies={"session": "abc123"}
            )
            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {}
            mock_response.content = AsyncMock(return_value=b"data")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            call_headers = mock_client.get.call_args.kwargs["headers"]
            assert "Cookie" in call_headers
            assert "session=abc123" in call_headers["Cookie"]

    def test_cookies_sets_cookie_header_on_put(self) -> None:
        """PUT with cookies should set the Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "put", url="https://example.com/api/1",
            json={"x": 1}, cookies={"session": "abc123"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.put = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.put.call_args.kwargs["headers"]
        assert "Cookie" in call_headers
        assert "session=abc123" in call_headers["Cookie"]

    def test_cookies_sets_cookie_header_on_patch(self) -> None:
        """PATCH with cookies should set the Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "patch", url="https://example.com/api/1",
            json={"x": 1}, cookies={"session": "abc123"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.patch = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.patch.call_args.kwargs["headers"]
        assert "Cookie" in call_headers
        assert "session=abc123" in call_headers["Cookie"]

    def test_cookies_merges_with_explicit_cookie_header(self) -> None:
        """Cookies parameter should merge with an explicit Cookie header, not overwrite."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            headers={"Cookie": "existing=val"},
            cookies={"new": "cookie"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        assert "existing=val" in call_headers["Cookie"]
        assert "new=cookie" in call_headers["Cookie"]

    def test_cookies_merges_with_explicit_cookie_header_case_insensitive(self) -> None:
        """Cookies should merge with an explicit lowercase 'cookie' header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            headers={"cookie": "existing=val"},
            cookies={"new": "cookie"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        assert "existing=val" in call_headers["cookie"]
        assert "new=cookie" in call_headers["cookie"]
        assert "Cookie" not in call_headers

    def test_no_cookies_does_not_set_cookie_header(self) -> None:
        """No cookies parameter should not set a Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"] or {}
        assert "Cookie" not in call_headers

    def test_empty_cookies_dict_does_not_set_cookie_header(self) -> None:
        """Empty cookies dict should not set a Cookie header."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com", cookies={})
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"] or {}
        assert "Cookie" not in call_headers

    def test_cookies_preserves_other_headers(self) -> None:
        """Cookies should preserve other caller-supplied headers."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            headers={"X-Custom": "value", "Accept": "application/json"},
            cookies={"session": "abc123"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        assert call_headers["X-Custom"] == "value"
        assert call_headers["Accept"] == "application/json"
        assert "Cookie" in call_headers

    def test_cookies_non_dict_raises(self) -> None:
        """Non-dict cookies should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com", cookies="session=abc")
        with pytest.raises(AIToolExecutionError, match="cookies.*object"):
            _execute_tool(tool, tool_call)

    def test_cookies_allowed_on_all_operations(self) -> None:
        """All operations should allow the cookies parameter."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        for op_name in ("get", "head", "post", "put", "patch", "delete", "download"):
            assert "cookies" in ops[op_name].allowed_parameters, f"{op_name} should allow cookies"

    def test_cookies_in_additional_parameters(self) -> None:
        """cookies should be in the tool's additional parameters."""
        tool = HttpAITool(_make_mindspace_mock())
        definition = tool.get_definition()
        cookie_param = next((p for p in definition.parameters if p.name == "cookies"), None)
        assert cookie_param is not None
        assert cookie_param.type == "object"

    def test_set_cookie_displayed_in_get_result(self) -> None:
        """GET response with Set-Cookie should display it in the result."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            headers={"content-type": "text/plain", "set-cookie": "session=abc123; Path=/; HttpOnly"},
            text="Hello"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Set-Cookie: session=abc123; Path=/; HttpOnly" in result.content

    def test_multiple_set_cookies_displayed_separately_in_get_result(self) -> None:
        """Multiple Set-Cookie headers should be displayed on separate lines."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            headers={
                "content-type": "text/plain",
                "set-cookie": "session=abc123; Path=/, token=xyz789; HttpOnly"
            },
            text="Hello"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Set-Cookie: session=abc123; Path=/" in result.content
        assert "Set-Cookie: token=xyz789; HttpOnly" in result.content

    def test_set_cookie_displayed_in_head_result(self) -> None:
        """HEAD response with Set-Cookie should display it in the result."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head", url="https://example.com")
        mock_response = MagicMock()
        mock_response.status.return_value = 200
        mock_response.headers.return_value = {
            "content-type": "text/html",
            "set-cookie": "session=abc123; Path=/; HttpOnly",
        }
        mock_response.content = AsyncMock(return_value=b"")

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.head = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Set-Cookie: session=abc123; Path=/; HttpOnly" in result.content

    def test_multiple_set_cookies_displayed_separately_in_head_result(self) -> None:
        """Multiple Set-Cookie headers in HEAD should be on separate lines."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("head", url="https://example.com")
        mock_response = MagicMock()
        mock_response.status.return_value = 200
        mock_response.headers.return_value = {
            "set-cookie": "session=abc123; Path=/, token=xyz789; HttpOnly"
        }
        mock_response.content = AsyncMock(return_value=b"")

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.head = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Set-Cookie: session=abc123; Path=/" in result.content
        assert "Set-Cookie: token=xyz789; HttpOnly" in result.content

    def test_no_set_cookie_not_displayed_in_result(self) -> None:
        """Response without Set-Cookie should not include Set-Cookie in result."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            headers={"content-type": "text/plain"},
            text="Hello"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Set-Cookie" not in result.content

    def test_set_cookie_with_expires_date_not_split(self) -> None:
        """Set-Cookie with Expires containing a comma should not be split incorrectly."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            headers={
                "content-type": "text/plain",
                "set-cookie": "session=abc123; Expires=Wed, 01 Jan 2025 00:00:00 GMT; Path=/"
            },
            text="Hello"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Set-Cookie: session=abc123; Expires=Wed, 01 Jan 2025 00:00:00 GMT; Path=/" in result.content

    def test_multiple_set_cookies_with_expires_dates(self) -> None:
        """Multiple Set-Cookie headers with Expires dates should be split correctly."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            headers={
                "content-type": "text/plain",
                "set-cookie": (
                    "session=abc123; Expires=Wed, 01 Jan 2025 00:00:00 GMT; Path=/, "
                    "token=xyz789; Expires=Thu, 02 Jan 2025 00:00:00 GMT; HttpOnly"
                )
            },
            text="Hello"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Set-Cookie: session=abc123; Expires=Wed, 01 Jan 2025 00:00:00 GMT; Path=/" in result.content
        assert "Set-Cookie: token=xyz789; Expires=Thu, 02 Jan 2025 00:00:00 GMT; HttpOnly" in result.content

    def test_set_cookie_displayed_in_error_result(self) -> None:
        """Set-Cookie should be displayed even in error responses (status >= 400)."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(
            status=401,
            headers={"content-type": "text/plain", "set-cookie": "session=expired; Path=/"},
            text="Unauthorized"
        )
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            result = _execute_tool(tool, tool_call)

        assert "Status: 401" in result.content
        assert "Set-Cookie: session=expired; Path=/" in result.content

    def test_cookies_works_with_basic_auth(self) -> None:
        """Cookies and Basic Auth should work together."""
        import base64

        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            username="user", password="pass",
            cookies={"session": "abc123"}
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        call_headers = mock_client.get.call_args.kwargs["headers"]
        expected = base64.b64encode(b"user:pass").decode("ascii")
        assert call_headers["Authorization"] == f"Basic {expected}"
        assert "session=abc123" in call_headers["Cookie"]

class TestHttpAIToolDecompression:
    """Tests for transparent gzip/deflate decompression through the AI tool."""

    def test_get_handles_gzip_response(self) -> None:
        """GET with a gzip-compressed response returns decompressed content."""
        import gzip
        import asyncio

        original = "Hello, compressed world! " * 10
        compressed = gzip.compress(original.encode("utf-8"))

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            from tests.http_client.test_http_client import _write_response
            _write_response(
                writer, 200, compressed,
                headers={"Content-Encoding": "gzip", "Content-Type": "text/plain"},
            )

        async def run() -> Any:
            from tests.http_client.test_http_client import MockHTTPServer
            from http_client import HttpClient

            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    text = await response.text()
                    return text

        text = asyncio.run(run())
        assert text == original

    def test_get_handles_deflate_response(self) -> None:
        """GET with a deflate-compressed response returns decompressed content."""
        import zlib
        import asyncio

        original = "Deflate compressed content " * 10
        compressed = zlib.compress(original.encode("utf-8"))

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            from tests.http_client.test_http_client import _write_response
            _write_response(
                writer, 200, compressed,
                headers={"Content-Encoding": "deflate", "Content-Type": "text/plain"},
            )

        async def run() -> Any:
            from tests.http_client.test_http_client import MockHTTPServer
            from http_client import HttpClient

            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    text = await response.text()
                    return text

        text = asyncio.run(run())
        assert text == original

class TestHttpAIToolProxy:
    """Tests for proxy parameter support."""

    def test_proxy_passes_to_client_on_get(self) -> None:
        """GET with proxy should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com", proxy="http://proxy.corp:8080"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(
            read_timeout=300.0, proxy="http://proxy.corp:8080"
        )

    def test_proxy_passes_to_client_on_post(self) -> None:
        """POST with proxy should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "post", url="https://example.com/api", json={"x": 1},
            proxy="socks5://proxy:1080"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(
            read_timeout=300.0, proxy="socks5://proxy:1080"
        )

    def test_proxy_passes_to_client_on_delete(self) -> None:
        """DELETE with proxy should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "delete", url="https://example.com/api/1", proxy="http://proxy:3128"
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(
            read_timeout=300.0, proxy="http://proxy:3128"
        )

    def test_proxy_passes_to_client_on_head(self) -> None:
        """HEAD with proxy should pass it to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "head", url="https://example.com", proxy="socks5h://proxy:1080"
        )
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

        mock_client_class.assert_called_once_with(
            read_timeout=300.0, proxy="socks5h://proxy:1080"
        )

    def test_proxy_passes_to_client_on_download(self) -> None:
        """Download with proxy should pass it to the HttpClient constructor."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            mindspace = _make_mindspace_mock(tmp)
            tool = HttpAITool(mindspace)
            tool_call = _make_tool_call(
                "download", url="https://example.com/file.txt",
                destination="file.txt", proxy="http://proxy:8080"
            )
            mock_response = MagicMock()
            mock_response.status.return_value = 200
            mock_response.headers.return_value = {}
            mock_response.content = AsyncMock(return_value=b"data")

            with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
                mock_client = MagicMock()
                mock_client.__aenter__ = AsyncMock(return_value=mock_client)
                mock_client.__aexit__ = AsyncMock(return_value=None)
                mock_client.get = AsyncMock(return_value=mock_response)
                mock_client_class.return_value = mock_client

                _execute_tool(tool, tool_call)

            mock_client_class.assert_called_once_with(
                read_timeout=300.0, proxy="http://proxy:8080"
            )

    def test_no_proxy_when_not_specified(self) -> None:
        """GET without proxy should pass proxy=None to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com")
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(read_timeout=300.0, proxy=None)

    def test_proxy_invalid_scheme_raises(self) -> None:
        """GET with an invalid proxy scheme should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com", proxy="ftp://proxy:21"
        )
        with pytest.raises(AIToolExecutionError, match="scheme"):
            _execute_tool(tool, tool_call)

    def test_proxy_missing_host_raises(self) -> None:
        """GET with a proxy URL missing a host should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com", proxy="http://"
        )
        with pytest.raises(AIToolExecutionError, match="hostname"):
            _execute_tool(tool, tool_call)

    def test_proxy_non_string_raises(self) -> None:
        """GET with a non-string proxy should raise AIToolExecutionError."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com", proxy=123
        )
        with pytest.raises(AIToolExecutionError, match="string"):
            _execute_tool(tool, tool_call)

    def test_proxy_allowed_on_all_operations(self) -> None:
        """All operations should allow the proxy parameter."""
        tool = HttpAITool(_make_mindspace_mock())
        ops = tool.get_operation_definitions()
        for op_name in ("get", "head", "post", "put", "patch", "delete", "download"):
            assert "proxy" in ops[op_name].allowed_parameters, f"{op_name} should allow proxy"

    def test_proxy_in_additional_parameters(self) -> None:
        """proxy should be in the tool's additional parameters."""
        tool = HttpAITool(_make_mindspace_mock())
        definition = tool.get_definition()
        proxy_param = next((p for p in definition.parameters if p.name == "proxy"), None)
        assert proxy_param is not None
        assert proxy_param.type == "string"

    def test_proxy_with_timeout_passes_both(self) -> None:
        """GET with both proxy and timeout should pass both to the HttpClient constructor."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get", url="https://example.com",
            proxy="http://proxy:8080", timeout=15
        )
        mock_response = _make_mock_response(text="OK")
        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call)

        mock_client_class.assert_called_once_with(
            read_timeout=15.0, proxy="http://proxy:8080"
        )


class TestHttpAIToolAuthorizationDisplay:
    """Tests for authorization reason text and context block formatting."""

    def test_get_auth_reason_includes_url_and_hides_empty_context(self) -> None:
        """GET authorization reason should state the URL; empty headers hide the context block."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call("get", url="https://example.com/path")
        mock_response = _make_mock_response(text="OK")

        captured: list[tuple[str, str | None]] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            reason: str,
            context: str | None,
            _destructive: bool,
        ) -> bool:
            captured.append((reason, context))
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert len(captured) == 1
        reason, context = captured[0]
        assert "https://example.com/path" in reason
        assert "GET" in reason
        assert context is None

    def test_get_auth_context_formats_and_redacts_headers(self) -> None:
        """GET authorization context should list headers and redact secrets."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(
            "get",
            url="https://example.com",
            headers={
                "Authorization": "Bearer secret-token",
                "Accept": "application/json",
                "X-Api-Key": "key-123",
            },
        )
        mock_response = _make_mock_response(text="OK")

        captured: list[str | None] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            _reason: str,
            context: str | None,
            _destructive: bool,
        ) -> bool:
            captured.append(context)
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert len(captured) == 1
        context = captured[0]
        assert context is not None
        assert "Accept: application/json" in context
        assert "Authorization: [REDACTED]" in context
        assert "X-Api-Key: [REDACTED]" in context
        assert "secret-token" not in context
        assert "key-123" not in context

    def test_post_auth_context_includes_json_body(self) -> None:
        """POST authorization context should include redacted headers and JSON body."""
        tool = HttpAITool(_make_mindspace_mock())
        json_body = {"name": "test", "value": 42}
        tool_call = _make_tool_call(
            "post",
            url="https://example.com/api",
            headers={"Content-Type": "application/json", "Cookie": "session=abc"},
            json=json_body,
        )
        mock_response = _make_mock_response(text="OK")

        captured: list[tuple[str, str | None]] = []

        async def capturing_auth(
            _tool_name: str,
            _arguments: dict[str, Any],
            reason: str,
            context: str | None,
            _destructive: bool,
        ) -> bool:
            captured.append((reason, context))
            return True

        auth = MagicMock(side_effect=capturing_auth)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=mock_response)
            mock_client_class.return_value = mock_client

            _execute_tool(tool, tool_call, auth)

        assert len(captured) == 1
        reason, context = captured[0]
        assert "POST" in reason
        assert "https://example.com/api" in reason
        assert context is not None
        assert "Content-Type: application/json" in context
        assert "Cookie: [REDACTED]" in context
        assert "session=abc" not in context
        assert "--- body ---" in context
        assert '"name": "test"' in context
        assert '"value": 42' in context

    def test_build_authorization_context_block_body_only(self) -> None:
        """Context block with only a body and no headers should still be shown."""
        tool = HttpAITool(_make_mindspace_mock())
        block = tool._build_authorization_context_block(  # pylint: disable=protected-access
            None, data_body="plain body"
        )
        assert block == "plain body"
