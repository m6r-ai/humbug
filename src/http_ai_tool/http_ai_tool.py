"""HTTP AI tool for fetching URLs and making POST requests."""

import base64
import re
import json as json_module
import logging
import os
from pathlib import Path
import tempfile
from typing import Any
from urllib.parse import urlsplit

from ai_tool import (
    AITool,
    AIToolAuthorizationCallback,
    AIToolAuthorizationDenied,
    AIToolCall,
    AIToolDefinition,
    AIToolExecutionError,
    AIToolOperationDefinition,
    AIToolParameter,
    AIToolResult,
)
from mindspace.mindspace import Mindspace
from html_ import html_ast_to_document_ir, parse_html
from http_client import (
    ClientConnectorError,
    ClientResponseError,
    HttpClient,
    HttpClientError,
    ServerTimeoutError,
)
from markdown_ import document_ir_to_markdown


_MAX_INLINE_RESPONSE_BYTES = 64 * 1024

# Header names whose values are redacted in authorization UI (case-insensitive).
_SENSITIVE_HEADER_NAMES = frozenset({
    "authorization",
    "proxy-authorization",
    "cookie",
    "set-cookie",
    "x-api-key",
    "api-key",
    "x-auth-token",
})

_SET_COOKIE_SPLIT_PATTERN = re.compile(
    r", (?=[A-Za-z!#$%&'*+\-.^_`|~]+=)"
)

class HttpAITool(AITool):
    """
    HTTP tool for fetching URLs, making requests with all standard HTTP methods,
    and downloading files.

    All operations require user authorization. The user sees the method,
    URL, headers, and body before approving. GET/POST/PUT/PATCH/DELETE return
    response bodies inline only when the final body text is at most 64KB
    (measured after HTML-to-markdown or JSON formatting when those apply).
    Larger bodies fail the tool call with instructions to use download.

    HTML responses are converted to readable markdown by default. Use
    ``format="raw"`` to receive the original HTML.
    """

    def __init__(self, mindspace: Mindspace) -> None:
        """Initialize the HTTP tool."""
        self._mindspace = mindspace
        self._logger = logging.getLogger("HttpAITool")

    def get_definition(self) -> AIToolDefinition:
        """Get the tool definition."""
        return self._build_definition_from_operations(
            name="http",
            description_prefix=(
                "The http tool lets you (the AI) fetch URLs, make requests with all standard HTTP "
                "methods, and download files from external services. It supports GET, HEAD, POST, "
                "PUT, PATCH, DELETE, and download operations.\n\n"
                "All operations require user authorization — the user sees the method, URL, "
                "headers, and body before approving. GET/POST/PUT/PATCH/DELETE return the response "
                "body inline only when that final body text is at most 64KB (after HTML-to-markdown "
                "or JSON formatting when those apply); larger bodies fail the tool call and the "
                "full response should be fetched with download. Download writes directly to a file "
                "in the mindspace without an inline size limit."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="url",
                    type="string",
                    description="Full URL to fetch or post to (e.g. 'https://example.com/api').",
                    required=False
                ),
                AIToolParameter(
                    name="username",
                    type="string",
                    description="Username for HTTP Basic Authentication. If provided, 'password' "
                                "must also be set. An explicit 'Authorization' header takes precedence.",
                    required=False
                ),
                AIToolParameter(
                    name="password",
                    type="string",
                    description="Password for HTTP Basic Authentication. If provided, 'username' "
                                "must also be set.",
                    required=False
                ),
                AIToolParameter(
                    name="headers",
                    type="object",
                    description="Optional request headers as a key-value object "
                                "(e.g. {\"Authorization\": \"Bearer token\"}).",
                    required=False
                ),
                AIToolParameter(
                    name="json",
                    type="object",
                    description="JSON body for POST requests. Serialized automatically. "
                                "Used instead of 'data' if both are provided.",
                    required=False
                ),
                AIToolParameter(
                    name="data",
                    type="string",
                    description="Raw body string for POST/PUT/PATCH requests. Used instead of 'json' "
                                "if both are provided.",
                    required=False
                ),
                AIToolParameter(
                    name="multipart",
                    type="array",
                    description="Multipart form-data parts for file uploads. Each part is an object "
                                "with 'name' (field name), and either 'content' (text value) or "
                                "'file_path' (path to a file in the mindspace). Optional: 'filename' "
                                "and 'content_type' for file parts. Takes precedence over 'json' "
                                "and 'data'. Only for POST/PUT/PATCH.",
                    required=False
                ),
                AIToolParameter(
                    name="format",
                    type="string",
                    description="Response format for HTML content. 'markdown' (default) converts "
                                "HTML to readable markdown. 'raw' returns the original response "
                                "body without conversion. Has no effect on non-HTML responses.",
                    required=False,
                    enum=["markdown", "raw"]
                ),
                AIToolParameter(
                    name="timeout",
                    type="number",
                    description="Maximum time in seconds to wait for a response. Applies to "
                                "the read phase of each request (per-read-operation, not total). "
                                "Default is 300 seconds if not specified.",
                    required=False
                ),
                AIToolParameter(
                    name="cookies",
                    type="object",
                    description="Cookies to send with the request as a {name: value} object. "
                                "Merged with any explicit 'Cookie' header.",
                    required=False
                ),
                AIToolParameter(
                    name="proxy",
                    type="string",
                    description="Proxy URL to tunnel the connection through. "
                                "Supported schemes: 'http', 'https', 'socks5', 'socks5h' "
                                "(e.g. 'http://proxy.corp:8080' or 'socks5://proxy:1080').",
                    required=False
                ),
                AIToolParameter(
                    name="destination",
                    type="string",
                    description="Destination file path in the mindspace for the download operation "
                                "(relative to mindspace root or absolute within the mindspace).",
                    required=False
                )
            ]
        )

    def get_brief_description(self) -> str:
        """Get brief one-line description for system prompt."""
        return (
            "HTTP client operations (GET, HEAD, POST, PUT, PATCH, DELETE, download). Fetch URLs, "
            "check headers, make API requests, download files. HTML responses converted to markdown by default."
        )

    def get_operation_definitions(self) -> dict[str, AIToolOperationDefinition]:
        """Get operation definitions for this tool."""
        return {
            "get": AIToolOperationDefinition(
                name="get",
                handler=self._get,
                extract_context=None,
                allowed_parameters={"url", "headers", "username", "password", "timeout", "format", "cookies", "proxy"},
                required_parameters={"url"},
                description=(
                    "Fetch a URL via GET. Returns the HTTP status code, content type, "
                    "and response body as text. HTML responses are converted to readable "
                    "markdown by default; use format=\"raw\" for the original HTML. "
                    "The body is returned inline only when at most 64KB after any formatting; "
                    "larger bodies fail the tool call — use download for the full response."
                )
            ),
            "head": AIToolOperationDefinition(
                name="head",
                handler=self._head,
                extract_context=None,
                allowed_parameters={"url", "headers", "username", "password", "timeout", "cookies", "proxy"},
                required_parameters={"url"},
                description=(
                    "Send a HEAD request to a URL. Returns the HTTP status code and "
                    "response headers without the body. Useful for checking if a URL "
                    "exists, inspecting content-type and content-length before fetching "
                    "or downloading, and checking last-modified or etag headers."
                )
            ),
            "post": AIToolOperationDefinition(
                name="post",
                handler=self._post,
                extract_context=None,
                allowed_parameters={
                    "url", "headers", "username", "password", "json", "data",
                    "multipart", "timeout", "format", "cookies", "proxy",
                },
                required_parameters={"url"},
                description=(
                    "Send a POST request with an optional JSON, raw, or multipart body. Returns the HTTP "
                    "status code, content type, and response body as text. HTML responses are "
                    "converted to readable markdown by default; use format=\"raw\" for the "
                    "original HTML. The body is returned inline only when at most 64KB after any "
                    "formatting; larger bodies fail the tool call — use download for the full response."
                )
            ),
            "put": AIToolOperationDefinition(
                name="put",
                handler=self._put,
                extract_context=None,
                allowed_parameters={
                    "url", "headers", "username", "password", "json", "data",
                    "multipart", "timeout", "format", "cookies", "proxy",
                },
                required_parameters={"url"},
                description=(
                    "Send a PUT request with an optional JSON, raw, or multipart body. Returns the HTTP "
                    "status code, content type, and response body as text. HTML responses are "
                    "converted to readable markdown by default; use format=\"raw\" for the "
                    "original HTML. The body is returned inline only when at most 64KB after any "
                    "formatting; larger bodies fail the tool call — use download for the full response."
                )
            ),
            "patch": AIToolOperationDefinition(
                name="patch",
                handler=self._patch,
                extract_context=None,
                allowed_parameters={
                    "url", "headers", "username", "password", "json", "data",
                    "multipart", "timeout", "format", "cookies", "proxy",
                },
                required_parameters={"url"},
                description=(
                    "Send a PATCH request with an optional JSON, raw, or multipart body. Returns the HTTP "
                    "status code, content type, and response body as text. HTML responses are "
                    "converted to readable markdown by default; use format=\"raw\" for the "
                    "original HTML. The body is returned inline only when at most 64KB after any "
                    "formatting; larger bodies fail the tool call — use download for the full response."
                )
            ),
            "delete": AIToolOperationDefinition(
                name="delete",
                handler=self._delete,
                extract_context=None,
                allowed_parameters={"url", "headers", "username", "password", "timeout", "format", "cookies", "proxy"},
                required_parameters={"url"},
                description=(
                    "Send a DELETE request to a URL. Returns the HTTP status code, "
                    "content type, and response body as text. HTML responses are converted "
                    "to readable markdown by default; use format=\"raw\" for the original "
                    "HTML. The body is returned inline only when at most 64KB after any "
                    "formatting; larger bodies fail the tool call — use download for the full response."
                )
            ),
            "download": AIToolOperationDefinition(
                name="download",
                handler=self._download,
                extract_context=None,
                allowed_parameters={"url", "headers", "username", "password", "timeout", "destination", "cookies", "proxy"},
                required_parameters={"url", "destination"},
                description=(
                    "Download a file from a URL and save it to the mindspace. The file is "
                    "written directly to disk without passing through the AI's context window, "
                    "so there is no inline size limit. Returns the status code, content type, "
                    "and number of bytes written."
                )
            ),
        }

    async def _get(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """GET operation — fetch a URL."""
        arguments = tool_call.arguments
        url = self._get_required_str_value("url", arguments)
        headers = self._get_optional_dict_value("headers", arguments)
        timeout = self._get_timeout(arguments)
        proxy = self._get_proxy(arguments)
        format_type = self._get_optional_str_value("format", arguments, "markdown") or "markdown"

        headers = self._apply_basic_auth(headers, arguments)
        headers = self._apply_cookies(headers, arguments)
        self._validate_url(url)

        reason = f"The AI is requesting to GET {url}"
        auth_context = self._build_authorization_context_block(headers)
        authorized = await request_authorization(
            "http", arguments, reason, auth_context, False
        )

        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to fetch URL: {url}")

        try:
            async with HttpClient(read_timeout=timeout, proxy=proxy) as client:
                response = await client.get(url, headers=headers)
                return await self._build_result(tool_call, response, format_type)

        except (ClientConnectorError, ServerTimeoutError, ClientResponseError, HttpClientError) as e:
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

        except Exception as e:
            self._logger.error("Unexpected error in GET %s: %s", url, str(e), exc_info=True)
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

    async def _head(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """HEAD operation — fetch headers only."""
        arguments = tool_call.arguments
        url = self._get_required_str_value("url", arguments)
        headers = self._get_optional_dict_value("headers", arguments)
        timeout = self._get_timeout(arguments)
        proxy = self._get_proxy(arguments)

        headers = self._apply_basic_auth(headers, arguments)
        headers = self._apply_cookies(headers, arguments)
        self._validate_url(url)

        reason = f"The AI is requesting to HEAD {url}"
        auth_context = self._build_authorization_context_block(headers)
        authorized = await request_authorization(
            "http", arguments, reason, auth_context, False
        )

        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to check URL: {url}")

        try:
            async with HttpClient(read_timeout=timeout, proxy=proxy) as client:
                response = await client.head(url, headers=headers)
                return await self._build_head_result(tool_call, response)

        except (ClientConnectorError, ServerTimeoutError, ClientResponseError, HttpClientError) as e:
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

        except Exception as e:
            self._logger.error("Unexpected error in HEAD %s: %s", url, str(e), exc_info=True)
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

    async def _post(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """POST operation — send a POST request."""
        return await self._body_request("POST", tool_call, request_authorization)

    async def _put(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """PUT operation — send a PUT request."""
        return await self._body_request("PUT", tool_call, request_authorization)

    async def _patch(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """PATCH operation — send a PATCH request."""
        return await self._body_request("PATCH", tool_call, request_authorization)

    async def _delete(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """DELETE operation — send a DELETE request."""
        arguments = tool_call.arguments
        url = self._get_required_str_value("url", arguments)
        headers = self._get_optional_dict_value("headers", arguments)
        timeout = self._get_timeout(arguments)
        proxy = self._get_proxy(arguments)
        format_type = self._get_optional_str_value("format", arguments, "markdown") or "markdown"

        headers = self._apply_basic_auth(headers, arguments)
        headers = self._apply_cookies(headers, arguments)
        self._validate_url(url)

        reason = f"The AI is requesting to DELETE {url}"
        auth_context = self._build_authorization_context_block(headers)
        authorized = await request_authorization(
            "http", arguments, reason, auth_context, True
        )

        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to DELETE URL: {url}")

        try:
            async with HttpClient(read_timeout=timeout, proxy=proxy) as client:
                response = await client.delete(url, headers=headers)
                return await self._build_result(tool_call, response, format_type)

        except (ClientConnectorError, ServerTimeoutError, ClientResponseError, HttpClientError) as e:
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

        except Exception as e:
            self._logger.error("Unexpected error in DELETE %s: %s", url, str(e), exc_info=True)
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

    async def _body_request(
        self,
        method: str,
        tool_call: AIToolCall,
        request_authorization: AIToolAuthorizationCallback,
    ) -> AIToolResult:
        """
        Shared handler for body-carrying HTTP methods (POST, PUT, PATCH).

        Args:
            method: HTTP method name (POST, PUT, or PATCH).
            tool_call: The original tool call containing arguments.
            request_authorization: Authorization callback.

        Returns:
            AIToolResult with status, content type, and body.
        """
        arguments = tool_call.arguments
        url = self._get_required_str_value("url", arguments)
        headers = self._get_optional_dict_value("headers", arguments)
        multipart_parts = arguments.get("multipart")
        json_body = arguments.get("json")
        data_body = self._get_optional_str_value("data", arguments)
        timeout = self._get_timeout(arguments)
        proxy = self._get_proxy(arguments)
        format_type = self._get_optional_str_value("format", arguments, "markdown") or "markdown"

        headers = self._apply_basic_auth(headers, arguments)
        headers = self._apply_cookies(headers, arguments)
        self._validate_url(url)

        reason = f"The AI is requesting to {method} {url}"
        auth_context = self._build_authorization_context_block(
            headers,
            json_body=json_body,
            data_body=data_body,
            multipart_parts=multipart_parts if isinstance(multipart_parts, list) else None,
        )
        authorized = await request_authorization(
            "http", arguments, reason, auth_context, True
        )

        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to {method} to URL: {url}")

        try:
            async with HttpClient(read_timeout=timeout, proxy=proxy) as client:
                client_method = {
                    "POST": client.post,
                    "PUT": client.put,
                    "PATCH": client.patch,
                }[method]

                if multipart_parts is not None:
                    boundary, body = self._build_multipart_body(multipart_parts)
                    request_headers = dict(headers) if headers else {}
                    request_headers["Content-Type"] = f"multipart/form-data; boundary={boundary}"
                    response = await client_method(
                        url,
                        headers=request_headers,
                        data=body,
                    )

                else:
                    response = await client_method(
                        url,
                        headers=headers,
                        json=json_body if json_body is not None else None,
                        data=data_body.encode("utf-8") if data_body is not None and json_body is None else None,
                    )

                return await self._build_result(tool_call, response, format_type)

        except (ClientConnectorError, ServerTimeoutError, ClientResponseError, HttpClientError) as e:
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

        except Exception as e:
            self._logger.error("Unexpected error in %s %s: %s", method, url, str(e), exc_info=True)
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

    async def _download(
        self,
        tool_call: AIToolCall,
        _requester_ref: Any,
        request_authorization: AIToolAuthorizationCallback
    ) -> AIToolResult:
        """Download operation — fetch a URL and write it to a file in the mindspace."""
        arguments = tool_call.arguments
        url = self._get_required_str_value("url", arguments)
        destination = self._get_required_str_value("destination", arguments)
        headers = self._get_optional_dict_value("headers", arguments)
        timeout = self._get_timeout(arguments)
        proxy = self._get_proxy(arguments)

        headers = self._apply_basic_auth(headers, arguments)
        headers = self._apply_cookies(headers, arguments)
        self._validate_url(url)

        dest_path, display_path = self._resolve_destination_path(destination)

        if dest_path.exists():
            reason = (
                f"The AI is requesting to download {url}. "
                f"This will overwrite the existing file '{display_path}'. "
                f"The previous contents will be lost."
            )
            destructive = True

        else:
            reason = (
                f"The AI is requesting to download {url}. "
                f"This will create a new file '{display_path}'."
            )
            destructive = False

        auth_context = self._build_authorization_context_block(headers)
        authorized = await request_authorization(
            "http", arguments, reason, auth_context, destructive
        )

        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to download from URL: {url}"
            )

        try:
            async with HttpClient(read_timeout=timeout, proxy=proxy) as client:
                response = await client.get(url, headers=headers)

                status = response.status()

                if status >= 400:
                    body = await response.text()
                    raise AIToolExecutionError(
                        f"Download failed with HTTP {status}: "
                        f"{self._error_body_excerpt(body)}"
                    )

                content_type = response.headers().get("content-type", "")
                data = await response.content()

        except (ClientConnectorError, ServerTimeoutError, ClientResponseError, HttpClientError) as e:
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

        except AIToolExecutionError:
            raise

        except Exception as e:
            self._logger.error("Unexpected error in download %s: %s", url, str(e), exc_info=True)
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

        try:
            dest_path.parent.mkdir(parents=True, exist_ok=True)

            tmp_fd, tmp_path = tempfile.mkstemp(dir=dest_path.parent, suffix=".tmp")

            with os.fdopen(tmp_fd, "wb") as f:
                f.write(data)

            os.replace(tmp_path, dest_path)

        except OSError as e:
            raise AIToolExecutionError(f"Failed to write file '{display_path}': {e}") from e

        size = len(data)
        return AIToolResult(
            id=tool_call.id,
            name="http",
            content=(
                f"Downloaded {url}\n"
                f"Status: {status}\n"
                f"Content-Type: {content_type}\n"
                f"Saved to: {display_path} ({size:,} bytes)"
            )
        )

    async def _build_result(
        self,
        tool_call: AIToolCall,
        response: Any,
        format_type: str
    ) -> AIToolResult:
        """
        Build an AIToolResult from an HttpResponse.

        Reads the response body and converts it based on content type and the
        requested format. Raises AIToolExecutionError when the final body text
        exceeds _MAX_INLINE_RESPONSE_BYTES.

        Args:
            tool_call: The original tool call.
            response: The HttpResponse from the HTTP client.
            format_type: Response format — 'markdown' or 'raw'.

        Returns:
            AIToolResult with status, content type, and body.

        Raises:
            AIToolExecutionError: If the formatted body exceeds the inline limit.
        """
        headers = response.headers()
        content_type = response.headers().get("content-type", "")
        status = response.status()

        if status >= 400:
            try:
                body = await response.text()

            except Exception:
                body = ""

            self._ensure_inline_body_fits(body)
            return AIToolResult(
                id=tool_call.id,
                name="http",
                content=self._build_result_content(status, content_type, body, headers)
            )

        if "text/html" in content_type and format_type != "raw":
            body = await response.text()
            body = self._html_to_markdown(body)

        elif "application/json" in content_type:
            try:
                json_data = await response.json()
                body = json_module.dumps(json_data, indent=2)

            except Exception:
                body = await response.text()

        else:
            body = await response.text()

        self._ensure_inline_body_fits(body)
        return AIToolResult(
            id=tool_call.id,
            name="http",
            content=self._build_result_content(status, content_type, body, headers)
        )

    async def _build_head_result(
        self,
        tool_call: AIToolCall,
        response: Any
    ) -> AIToolResult:
        """
        Build an AIToolResult from a HEAD response (headers only, no body).

        Args:
            tool_call: The original tool call.
            response: The HttpResponse from the HTTP client.

        Returns:
            AIToolResult with status and all response headers.
        """
        status = response.status()
        headers = response.headers()

        # Consume any body data so the connection closes cleanly
        if response.headers().get("content-length") is not None:
            await response.content()

        lines = [f"Status: {status}"]

        for key, value in headers.items():
            if key.lower() == "set-cookie":
                for cookie in self._split_set_cookie(value):
                    lines.append(f"Set-Cookie: {cookie}")

            else:
                lines.append(f"{key}: {value}")

        return AIToolResult(
            id=tool_call.id,
            name="http",
            content="\n".join(lines)
        )

    def _build_result_content(
        self,
        status: int,
        content_type: str,
        body: str,
        headers: dict[str, str],
    ) -> str:
        """
        Build the content string for a standard HTTP result.

        Includes status, content type, Set-Cookie headers (if any), and body.

        Args:
            status: HTTP status code.
            content_type: Response content type.
            body: Response body text.
            headers: Response headers dict.

        Returns:
            Formatted content string.
        """
        parts = [f"Status: {status}", f"Content-Type: {content_type}"]

        set_cookie = headers.get("set-cookie")
        if set_cookie:
            for cookie in self._split_set_cookie(set_cookie):
                parts.append(f"Set-Cookie: {cookie}")

        parts.append("")
        parts.append(body)
        return "\n".join(parts)

    def _split_set_cookie(self, value: str) -> list[str]:
        """
        Split a comma-joined Set-Cookie header value into individual cookies.

        The HTTP client concatenates multiple same-name headers with ', '.
        A single Set-Cookie value can contain commas (e.g. in Expires dates),
        so we split on the pattern of a cookie name followed by '='.

        Args:
            value: The raw Set-Cookie header value (possibly comma-joined).

        Returns:
            List of individual cookie strings.
        """
        return _SET_COOKIE_SPLIT_PATTERN.split(value)

    def _html_to_markdown(self, html: str) -> str:
        """
        Convert HTML text to markdown.

        Parses the HTML, converts to document IR, then serialises to markdown.
        If conversion fails, returns the raw HTML so the caller still gets
        usable content.

        Args:
            html: Raw HTML string.

        Returns:
            Markdown text, or raw HTML if conversion fails.
        """
        try:
            ast = parse_html(html)
            ir = html_ast_to_document_ir(ast)
            return document_ir_to_markdown(ir)

        except Exception as e:
            self._logger.warning("HTML to markdown conversion failed: %s", str(e))
            return html

    def _ensure_inline_body_fits(self, content: str) -> None:
        """
        Reject response bodies that exceed the inline context size limit.

        Args:
            content: Full content string.

        Raises:
            AIToolExecutionError: If content exceeds _MAX_INLINE_RESPONSE_BYTES.
        """
        size = len(content.encode("utf-8"))
        if size <= _MAX_INLINE_RESPONSE_BYTES:
            return

        raise AIToolExecutionError(
            f"Response body is too large to return inline ({size} bytes; "
            f"limit is {_MAX_INLINE_RESPONSE_BYTES} bytes). Use the http download "
            f"operation to save the full response to a mindspace file, then read "
            f"or search it with the filesystem tools."
        )

    def _error_body_excerpt(self, content: str) -> str:
        """
        Return a short excerpt of an HTTP error body for exception messages.

        Args:
            content: Full error response body.

        Returns:
            The original content if small enough for an error message, otherwise
            a prefix with an omission note.
        """
        max_excerpt_bytes = 2 * 1024
        content_bytes = content.encode("utf-8")
        if len(content_bytes) <= max_excerpt_bytes:
            return content

        excerpt = content_bytes[:max_excerpt_bytes].decode("utf-8", errors="ignore")
        omitted = len(content_bytes) - max_excerpt_bytes
        return f"{excerpt}\n... error body truncated, {omitted} bytes omitted"

    def _redact_header_value(self, name: str, value: str) -> str:
        """
        Redact sensitive header values for authorization UI display.

        Args:
            name: Header name.
            value: Header value.

        Returns:
            Redacted value for sensitive headers, otherwise the original value.
        """
        if name.lower() in _SENSITIVE_HEADER_NAMES:
            return "[REDACTED]"

        return value

    def _format_headers_for_authorization(self, headers: dict[str, str] | None) -> str | None:
        """
        Format request headers for the authorization context block.

        Sensitive values are redacted. Returns None if there are no headers.

        Args:
            headers: Effective request headers after auth/cookie merging.

        Returns:
            Multi-line header text, or None if headers is empty/None.
        """
        if not headers:
            return None

        lines = [
            f"{name}: {self._redact_header_value(name, str(value))}"
            for name, value in headers.items()
        ]
        return "\n".join(lines)

    def _format_body_for_authorization(
        self,
        json_body: Any = None,
        data_body: str | None = None,
        multipart_parts: list[Any] | None = None,
    ) -> str | None:
        """
        Format a request body for the authorization context block.

        Multipart parts take precedence over json, which takes precedence over raw data
        (matching send order). Sensitive fields in multipart file content are not fully
        inspected; only header-style secrets are redacted elsewhere.

        Args:
            json_body: Optional JSON body object.
            data_body: Optional raw body string.
            multipart_parts: Optional multipart part list.

        Returns:
            Formatted body text, or None if no body is present.
        """
        if multipart_parts is not None:
            try:
                return json_module.dumps(multipart_parts, indent=2)

            except (TypeError, ValueError):
                return str(multipart_parts)

        if json_body is not None:
            try:
                return json_module.dumps(json_body, indent=2)

            except (TypeError, ValueError):
                return str(json_body)

        if data_body is not None:
            return data_body

        return None

    def _build_authorization_context_block(
        self,
        headers: dict[str, str] | None,
        json_body: Any = None,
        data_body: str | None = None,
        multipart_parts: list[Any] | None = None,
    ) -> str | None:
        """
        Build the optional authorization UI context code block.

        Includes redacted headers and, when present, the request body. Returns None
        when there is nothing to show so the UI hides the block.

        Args:
            headers: Effective request headers.
            json_body: Optional JSON body.
            data_body: Optional raw body.
            multipart_parts: Optional multipart parts.

        Returns:
            Context block text, or None if empty.
        """
        sections: list[str] = []

        header_text = self._format_headers_for_authorization(headers)
        if header_text is not None:
            sections.append(header_text)

        body_text = self._format_body_for_authorization(
            json_body=json_body,
            data_body=data_body,
            multipart_parts=multipart_parts,
        )
        if body_text is not None:
            if sections:
                sections.append("")
                sections.append("--- body ---")
                sections.append(body_text)

            else:
                sections.append(body_text)

        if not sections:
            return None

        return "\n".join(sections)

    def _apply_basic_auth(
        self,
        headers: dict[str, str] | None,
        arguments: dict[str, Any],
    ) -> dict[str, str] | None:
        """
        Apply HTTP Basic Authentication from username/password arguments.

        If both 'username' and 'password' are present in arguments, and the
        headers do not already contain an 'Authorization' key, the Basic Auth
        header is added. An explicit 'Authorization' header always takes
        precedence.

        Args:
            headers: Existing request headers, or None.
            arguments: Tool call arguments possibly containing username/password.

        Returns:
            Headers dict with Basic Auth applied if applicable, or the
            original headers unchanged if no credentials are provided.
        """
        username = arguments.get("username")
        password = arguments.get("password")

        if username is None or password is None:
            return headers

        result = dict(headers) if headers is not None else {}

        has_auth = any(k.lower() == "authorization" for k in result)
        if not has_auth:
            credentials = f"{username}:{password}"
            encoded = base64.b64encode(credentials.encode("utf-8")).decode("ascii")
            result["Authorization"] = f"Basic {encoded}"

        return result

    def _apply_cookies(
        self,
        headers: dict[str, str] | None,
        arguments: dict[str, Any],
    ) -> dict[str, str] | None:
        """
        Apply cookies from the 'cookies' argument to the request headers.

        If 'cookies' is provided (a dict of {name: value} pairs), they are
        merged into a Cookie header. If the caller already provided an
        explicit Cookie header (case-insensitive), the cookies are merged
        into it rather than overwriting it.

        Args:
            headers: Existing request headers, or None.
            arguments: Tool call arguments possibly containing 'cookies'.

        Returns:
            Headers dict with cookies applied if applicable, or the
            original headers unchanged if no cookies are provided.
        """
        cookies = arguments.get("cookies")
        if cookies is None:
            return headers

        if not isinstance(cookies, dict):
            raise AIToolExecutionError("'cookies' must be an object of {name: value} pairs")

        cookie_pairs = [f"{name}={value}" for name, value in cookies.items()]
        if not cookie_pairs:
            return headers

        cookie_str = "; ".join(cookie_pairs)
        result = dict(headers) if headers is not None else {}

        existing_key = next((k for k in result if k.lower() == "cookie"), None)
        if existing_key is not None:
            result[existing_key] = f"{result[existing_key]}; {cookie_str}"

        else:
            result["Cookie"] = cookie_str

        return result

    _DEFAULT_READ_TIMEOUT: float = 300.0

    def _get_timeout(self, arguments: dict[str, Any]) -> float:
        """
        Extract and validate the timeout parameter from arguments.

        Args:
            arguments: Tool call arguments possibly containing 'timeout'.

        Returns:
            Timeout in seconds, or the default of 300.0 if not specified.

        Raises:
            AIToolExecutionError: If the timeout is not a positive number.
        """
        timeout = arguments.get("timeout")

        if timeout is None:
            return self._DEFAULT_READ_TIMEOUT

        if isinstance(timeout, bool) or not isinstance(timeout, (int, float)):
            raise AIToolExecutionError("'timeout' must be a number (seconds)")

        if timeout <= 0:
            raise AIToolExecutionError("'timeout' must be a positive number of seconds")

        return float(timeout)

    _VALID_PROXY_SCHEMES: frozenset[str] = frozenset({"http", "https", "socks5", "socks5h"})

    def _get_proxy(self, arguments: dict[str, Any]) -> str | None:
        """
        Extract and validate the proxy parameter from arguments.

        Args:
            arguments: Tool call arguments possibly containing 'proxy'.

        Returns:
            Proxy URL string, or None if not specified.

        Raises:
            AIToolExecutionError: If the proxy URL has an invalid scheme or
                is missing a host.
        """
        proxy = arguments.get("proxy")

        if proxy is None:
            return None

        if not isinstance(proxy, str):
            raise AIToolExecutionError("'proxy' must be a string URL")

        parsed = urlsplit(proxy)
        if parsed.scheme.lower() not in self._VALID_PROXY_SCHEMES:
            raise AIToolExecutionError(
                f"'proxy' scheme must be one of {sorted(self._VALID_PROXY_SCHEMES)}: {proxy}"
            )

        if not parsed.hostname:
            raise AIToolExecutionError(f"'proxy' must include a hostname: {proxy}")

        return proxy

    def _validate_url(self, url: str) -> None:
        """
        Validate that a URL has a scheme and host.

        Args:
            url: URL string to validate.

        Raises:
            AIToolExecutionError: If the URL is missing a scheme or host.
        """
        parsed = urlsplit(url)
        if not parsed.scheme:
            raise AIToolExecutionError(
                f"URL must include a scheme (e.g. 'https://'): {url}"
            )

        if parsed.scheme not in ("http", "https"):
            raise AIToolExecutionError(
                f"URL scheme must be 'http' or 'https': {url}"
            )

        if not parsed.hostname:
            raise AIToolExecutionError(
                f"URL must include a hostname: {url}"
            )

    def _get_optional_dict_value(
        self,
        key: str,
        arguments: dict[str, Any],
    ) -> dict[str, str] | None:
        """
        Extract optional dict value from arguments dictionary.

        Args:
            key: Key to extract from arguments.
            arguments: Dictionary containing operation parameters.

        Returns:
            Dict value or None.

        Raises:
            AIToolExecutionError: If value exists but is not a dict.
        """
        if key not in arguments:
            return None

        value = arguments[key]
        if value is None:
            return None

        if not isinstance(value, dict):
            raise AIToolExecutionError(f"'{key}' must be an object")

        return value

    def _build_multipart_body(
        self,
        parts: list[dict[str, Any]],
    ) -> tuple[str, bytes]:
        """
        Build a multipart/form-data body from a list of part definitions.

        Args:
            parts: List of part objects. Each part must have a 'name' field,
                and either 'content' (text value) or 'file_path' (path to a
                file in the mindspace). Optional fields: 'filename' and
                'content_type' for file parts.

        Returns:
            Tuple of (boundary, body_bytes) where boundary is the MIME
            boundary string and body_bytes is the complete multipart body.

        Raises:
            AIToolExecutionError: If parts are malformed or a file cannot be read.
        """
        if not isinstance(parts, list) or len(parts) == 0:
            raise AIToolExecutionError("'multipart' must be a non-empty array of parts")

        boundary = f"----HumbugBoundary{os.urandom(16).hex()}"

        body = bytearray()
        for i, part in enumerate(parts):
            if not isinstance(part, dict):
                raise AIToolExecutionError(f"multipart part {i} must be an object")

            name = part.get("name")
            if not name or not isinstance(name, str):
                raise AIToolExecutionError(
                    f"multipart part {i} must have a 'name' field"
                )

            content = part.get("content")
            file_path = part.get("file_path")

            if content is not None and file_path is not None:
                raise AIToolExecutionError(
                    f"multipart part '{name}' cannot have both 'content' and 'file_path'"
                )

            if content is None and file_path is None:
                raise AIToolExecutionError(
                    f"multipart part '{name}' must have either 'content' or 'file_path'"
                )

            part_filename = None
            part_content_type = None
            part_data: bytes

            if file_path is not None:
                if not isinstance(file_path, str):
                    raise AIToolExecutionError(
                        f"multipart part '{name}' file_path must be a string"
                    )

                resolved_path = self._resolve_source_path(file_path)

                try:
                    with open(resolved_path, "rb") as f:
                        part_data = f.read()

                except OSError as e:
                    raise AIToolExecutionError(
                        f"Failed to read file for multipart part '{name}': {e}"
                    ) from e

                part_filename = part.get("filename") or os.path.basename(file_path)
                part_content_type = part.get("content_type") or "application/octet-stream"

            else:
                if not isinstance(content, str):
                    raise AIToolExecutionError(
                        f"multipart part '{name}' content must be a string"
                    )

                part_data = content.encode("utf-8")
                part_filename = part.get("filename")
                part_content_type = part.get("content_type")

            body.extend(f"--{boundary}\r\n".encode("latin-1"))

            header = f'Content-Disposition: form-data; name="{name}"'
            if part_filename:
                header += f'; filename="{part_filename}"'

            body.extend(f"{header}\r\n".encode("latin-1"))

            if part_content_type:
                body.extend(f"Content-Type: {part_content_type}\r\n".encode("latin-1"))

            body.extend(b"\r\n")
            body.extend(part_data)
            body.extend(b"\r\n")

        body.extend(f"--{boundary}--\r\n".encode("latin-1"))

        return boundary, bytes(body)

    def _resolve_source_path(self, source: str) -> str:
        """
        Resolve a source file path within the mindspace for reading.

        Args:
            source: Path relative to the mindspace root, or absolute
                within the mindspace.

        Returns:
            Resolved absolute path as a string.

        Raises:
            AIToolExecutionError: If no mindspace is open, the path resolves
                outside the mindspace boundary, inside .humbug/, or the file
                does not exist.
        """
        mindspace_path = self._mindspace.mindspace_path()

        if not mindspace_path:
            raise AIToolExecutionError("No mindspace is open")

        if os.path.isabs(source):
            abs_path = os.path.abspath(source)

        else:
            abs_path = os.path.join(mindspace_path, source)

        resolved = os.path.realpath(abs_path)
        mindspace_real = os.path.realpath(mindspace_path)

        if not (resolved == mindspace_real or resolved.startswith(mindspace_real + os.sep)):
            raise AIToolExecutionError(
                f"Source path is outside the mindspace: {source}"
            )

        humbug_dir = os.path.join(mindspace_real, Mindspace.MINDSPACE_DIR)
        if resolved == humbug_dir or resolved.startswith(humbug_dir + os.sep):
            raise AIToolExecutionError(
                "Cannot read from the .humbug/ directory — it is managed by Humbug internally."
            )

        if not os.path.isfile(resolved):
            raise AIToolExecutionError(f"Source file does not exist: {source}")

        return resolved

    def _resolve_destination_path(self, destination: str) -> tuple[Any, str]:
        """
        Resolve a destination path within the mindspace.

        Args:
            destination: Path relative to the mindspace root, or absolute
                within the mindspace.

        Returns:
            Tuple of (Path, display_path) where Path is the resolved
            pathlib.Path and display_path is the mindspace-relative path.

        Raises:
            AIToolExecutionError: If no mindspace is open, or if the path
                resolves outside the mindspace boundary or inside .humbug/.
        """
        mindspace_path = self._mindspace.mindspace_path()

        if not mindspace_path:
            raise AIToolExecutionError("No mindspace is open")

        if os.path.isabs(destination):
            abs_path = os.path.abspath(destination)

        else:
            abs_path = os.path.join(mindspace_path, destination)

        resolved = os.path.realpath(abs_path)
        mindspace_real = os.path.realpath(mindspace_path)

        if not (resolved == mindspace_real or resolved.startswith(mindspace_real + os.sep)):
            raise AIToolExecutionError(
                f"Destination path is outside the mindspace: {destination}"
            )

        humbug_dir = os.path.join(mindspace_real, Mindspace.MINDSPACE_DIR)
        if resolved == humbug_dir or resolved.startswith(humbug_dir + os.sep):
            raise AIToolExecutionError(
                "Cannot write to the .humbug/ directory — it is managed by Humbug internally."
            )

        return Path(resolved), os.path.relpath(resolved, mindspace_real)
