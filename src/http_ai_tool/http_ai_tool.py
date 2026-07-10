"""HTTP AI tool for fetching URLs and making POST requests."""

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


_MAX_RESPONSE_BYTES = 64 * 1024


class HttpAITool(AITool):
    """
    HTTP tool for fetching URLs, making POST requests, and downloading files.

    All operations require user authorization. The user sees the method,
    URL, headers, and body before approving. GET/POST responses are truncated
    to 64KB.

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
                "The http tool lets you (the AI) fetch URLs, make POST requests, and download "
                "files from external services. It supports GET, POST, and download operations.\n\n"
                "All operations require user authorization — the user sees the method, URL, "
                "headers, and body before approving. GET/POST responses are converted to readable "
                "markdown or truncated to 64KB. Download writes directly to a file in the mindspace "
                "without size limits."
            ),
            additional_parameters=[
                AIToolParameter(
                    name="url",
                    type="string",
                    description="Full URL to fetch or post to (e.g. 'https://example.com/api').",
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
                    description="Raw body string for POST requests. Used instead of 'json' "
                                "if both are provided.",
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
            "HTTP client operations (GET, HEAD, POST, download). Fetch URLs, check headers, "
            "make API requests, download files. HTML responses converted to markdown by default."
        )

    def get_operation_definitions(self) -> dict[str, AIToolOperationDefinition]:
        """Get operation definitions for this tool."""
        return {
            "get": AIToolOperationDefinition(
                name="get",
                handler=self._get,
                extract_context=self._extract_get_context,
                allowed_parameters={"url", "headers", "format"},
                required_parameters={"url"},
                description=(
                    "Fetch a URL via GET. Returns the HTTP status code, content type, "
                    "and response body as text. HTML responses are converted to readable "
                    "markdown by default; use format=\"raw\" for the original HTML. "
                    "Output is truncated to 64KB."
                )
            ),
            "head": AIToolOperationDefinition(
                name="head",
                handler=self._head,
                extract_context=self._extract_head_context,
                allowed_parameters={"url", "headers"},
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
                extract_context=self._extract_post_context,
                allowed_parameters={"url", "headers", "json", "data", "format"},
                required_parameters={"url"},
                description=(
                    "Send a POST request with an optional JSON or raw body. Returns the HTTP "
                    "status code, content type, and response body as text. HTML responses are "
                    "converted to readable markdown by default; use format=\"raw\" for the "
                    "original HTML. Output is truncated to 64KB."
                )
            ),
            "download": AIToolOperationDefinition(
                name="download",
                handler=self._download,
                extract_context=self._extract_download_context,
                allowed_parameters={"url", "headers", "destination"},
                required_parameters={"url", "destination"},
                description=(
                    "Download a file from a URL and save it to the mindspace. The file is "
                    "written directly to disk without passing through the AI's context window, "
                    "so there is no 64KB size limit. Returns the status code, content type, "
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
        format_type = self._get_optional_str_value("format", arguments, "markdown") or "markdown"

        self._validate_url(url)

        context = f"The AI is requesting to fetch URL:\n  GET {url}"
        if headers:
            context += f"\n  Headers: {headers}"

        authorized = await request_authorization("http", arguments, context, None, False)

        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to fetch URL: {url}")

        try:
            async with HttpClient() as client:
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

        self._validate_url(url)

        context = f"The AI is requesting to check URL:\n  HEAD {url}"
        if headers:
            context += f"\n  Headers: {headers}"

        authorized = await request_authorization("http", arguments, context, None, False)

        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to check URL: {url}")

        try:
            async with HttpClient() as client:
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
        arguments = tool_call.arguments
        url = self._get_required_str_value("url", arguments)
        headers = self._get_optional_dict_value("headers", arguments)
        json_body = arguments.get("json")
        data_body = self._get_optional_str_value("data", arguments)
        format_type = self._get_optional_str_value("format", arguments, "markdown") or "markdown"

        self._validate_url(url)

        context = f"The AI is requesting to POST to URL:\n  POST {url}"
        if headers:
            context += f"\n  Headers: {headers}"

        if json_body is not None:
            context += f"\n  JSON body: {json_module.dumps(json_body, indent=2)}"

        elif data_body is not None:
            context += f"\n  Body: {data_body}"

        authorized = await request_authorization("http", arguments, context, None, True)

        if not authorized:
            raise AIToolAuthorizationDenied(f"User denied permission to POST to URL: {url}")

        try:
            async with HttpClient() as client:
                response = await client.post(
                    url,
                    headers=headers,
                    json=json_body if json_body is not None else None,
                    data=data_body.encode("utf-8") if data_body is not None and json_body is None else None,
                )
                return await self._build_result(tool_call, response, format_type)

        except (ClientConnectorError, ServerTimeoutError, ClientResponseError, HttpClientError) as e:
            raise AIToolExecutionError(f"HTTP request failed: {e}") from e

        except Exception as e:
            self._logger.error("Unexpected error in POST %s: %s", url, str(e), exc_info=True)
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

        self._validate_url(url)

        dest_path, display_path = self._resolve_destination_path(destination)

        if dest_path.exists():
            context = (
                f"The AI is requesting to download:\n  {url}\n"
                f"This will overwrite the existing file '{display_path}'. "
                f"The previous contents will be lost."
            )

            destructive = True

        else:
            context = (
                f"The AI is requesting to download:\n  {url}\n"
                f"This will create a new file '{display_path}'."
            )

            destructive = False

        if headers:
            context += f"\n  Headers: {headers}"

        authorized = await request_authorization("http", arguments, context, None, destructive)

        if not authorized:
            raise AIToolAuthorizationDenied(
                f"User denied permission to download from URL: {url}"
            )

        try:
            async with HttpClient() as client:
                response = await client.get(url, headers=headers)

                status = response.status()

                if status >= 400:
                    body = await response.text()
                    raise AIToolExecutionError(
                        f"Download failed with HTTP {status}: {self._truncate(body)}"
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

        Reads the response body and converts it based on content type and
        the requested format. Truncates to _MAX_RESPONSE_BYTES.

        Args:
            tool_call: The original tool call.
            response: The HttpResponse from the HTTP client.
            format_type: Response format — 'markdown' or 'raw'.

        Returns:
            AIToolResult with status, content type, and body.
        """
        content_type = response.headers().get("content-type", "")
        status = response.status()

        if status >= 400:
            try:
                body = await response.text()

            except Exception:
                body = ""

            body = self._truncate(body)
            return AIToolResult(
                id=tool_call.id,
                name="http",
                content=f"Status: {status}\nContent-Type: {content_type}\n\n{body}"
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

        body = self._truncate(body)

        return AIToolResult(
            id=tool_call.id,
            name="http",
            content=f"Status: {status}\nContent-Type: {content_type}\n\n{body}"
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
            lines.append(f"{key}: {value}")

        return AIToolResult(
            id=tool_call.id,
            name="http",
            content="\n".join(lines)
        )

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

    def _truncate(self, content: str) -> str:
        """
        Truncate content if it exceeds the response size limit.

        Args:
            content: Full content string.

        Returns:
            Content truncated to _MAX_RESPONSE_BYTES with a notice, or the
            original content if within the limit.
        """
        content_bytes = content.encode("utf-8")
        if len(content_bytes) <= _MAX_RESPONSE_BYTES:
            return content

        truncated = content_bytes[:_MAX_RESPONSE_BYTES].decode("utf-8", errors="ignore")
        omitted = len(content_bytes) - _MAX_RESPONSE_BYTES
        return f"{truncated}\n... output truncated, {omitted} bytes omitted"

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

    def _extract_get_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for GET operation."""
        url = arguments.get("url", "?")
        return f"GET {url}"

    def _extract_post_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for POST operation."""
        url = arguments.get("url", "?")
        return f"POST {url}"

    def _extract_head_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for HEAD operation."""
        url = arguments.get("url", "?")
        return f"HEAD {url}"

    def _extract_download_context(self, arguments: dict[str, Any]) -> str | None:
        """Extract context for download operation."""
        url = arguments.get("url", "?")
        destination = arguments.get("destination", "?")
        return f"download {url} -> {destination}"
