"""Tests for the async HTTP client."""

import asyncio
import json
import socket
import ssl
import tempfile
from pathlib import Path

import pytest

from http_client import (
    ClientConnectorError,
    ClientResponseError,
    HttpClient,
    HttpClientError,
    ServerTimeoutError,
)


class MockHTTPServer:
    """A minimal HTTP server for testing, built on asyncio.start_server."""

    def __init__(
        self,
        handler: asyncio.Callable[[dict, asyncio.StreamWriter], asyncio.Awaitable[None]],
        use_tls: bool = False,
    ) -> None:
        """
        Initialize the mock server.

        Args:
            handler: Async function called with (request_dict, writer).
                     request_dict has keys: method, path, headers, body.
            use_tls: Whether to use TLS.
        """
        self._handler = handler
        self._use_tls = use_tls
        self._server: asyncio.base_events.Server | None = None
        self._host = "127.0.0.1"
        self._port = 0
        self._ssl_context: ssl.SSLContext | None = None
        self._cert_dir: tempfile.TemporaryDirectory | None = None

    def host(self) -> str:
        """Return the server host."""
        return self._host

    def port(self) -> int:
        """Return the server port."""
        return self._port

    def url(self, path: str = "/") -> str:
        """Return a full URL for the given path."""
        scheme = "https" if self._use_tls else "http"
        return f"{scheme}://{self._host}:{self._port}{path}"

    def ssl_context(self) -> ssl.SSLContext | None:
        """Return the client SSL context for TLS servers."""
        return self._ssl_context

    async def __aenter__(self) -> 'MockHTTPServer':
        """Start the server."""
        ssl_param = None
        if self._use_tls:
            self._cert_dir = tempfile.TemporaryDirectory()
            cert_path, key_path = _generate_self_signed_cert(Path(self._cert_dir.name))
            server_ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            server_ctx.load_cert_chain(cert_path, key_path)

            client_ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
            client_ctx.load_verify_locations(cert_path)
            client_ctx.check_hostname = False
            client_ctx.verify_mode = ssl.CERT_NONE
            self._ssl_context = client_ctx

            ssl_param = server_ctx

        self._server = await asyncio.start_server(
            self._handle_client,
            host=self._host,
            port=0,
            ssl=ssl_param,
        )

        sock = self._server.sockets[0]
        self._port = sock.getsockname()[1]

        return self

    async def __aexit__(self, *args: object) -> None:
        """Stop the server."""
        if self._server:
            self._server.close()
            await self._server.wait_closed()

        if self._cert_dir:
            self._cert_dir.cleanup()

    async def _handle_client(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> None:
        """Handle a single client connection."""
        try:
            request = await self._read_request(reader)
            await self._handler(request, writer)

        except (ConnectionError, asyncio.IncompleteReadError):
            pass

        finally:
            try:
                writer.write_eof()
            except (ConnectionError, OSError):
                pass
            await writer.drain()
            writer.close()
            try:
                await writer.wait_closed()

            except (ConnectionError, OSError):
                pass

    async def _read_request(self, reader: asyncio.StreamReader) -> dict:
        """Read an HTTP request from the client."""
        request_line = await reader.readline()
        parts = request_line.decode("latin-1").strip().split(" ")
        method = parts[0]
        path = parts[1] if len(parts) > 1 else "/"

        headers: dict[str, str] = {}
        while True:
            line = await reader.readline()
            if not line or line in (b"\r\n", b"\n"):
                break

            decoded = line.decode("latin-1").rstrip("\r\n")
            if ":" in decoded:
                key, value = decoded.split(":", 1)
                headers[key.strip().lower()] = value.strip()

        body = b""
        content_length = headers.get("content-length")
        if content_length:
            body = await reader.readexactly(int(content_length))

        return {
            "method": method,
            "path": path,
            "headers": headers,
            "body": body,
        }


def _generate_self_signed_cert(dir_path: Path) -> tuple[str, str]:
    """Generate a self-signed certificate and private key for testing."""
    import subprocess

    cert_path = str(dir_path / "cert.pem")
    key_path = str(dir_path / "key.pem")

    subprocess.run(
        [
            "openssl", "req", "-x509", "-newkey", "rsa:2048",
            "-keyout", key_path, "-out", cert_path,
            "-days", "1", "-nodes",
            "-subj", "/CN=localhost",
        ],
        check=True,
        capture_output=True,
    )

    return cert_path, key_path


def _write_response(
    writer: asyncio.StreamWriter,
    status: int,
    body: bytes,
    headers: dict[str, str] | None = None,
) -> None:
    """Write a complete HTTP response with Content-Length."""
    reason = {
        200: "OK", 204: "No Content", 301: "Moved Permanently",
        302: "Found", 303: "See Other", 307: "Temporary Redirect",
        308: "Permanent Redirect", 400: "Bad Request", 401: "Unauthorized",
        403: "Forbidden", 404: "Not Found", 429: "Too Many Requests",
        500: "Internal Server Error",
    }.get(status, "Unknown")

    response_headers = {
        "Content-Length": str(len(body)),
        "Connection": "close",
    }
    if headers:
        response_headers.update(headers)

    lines = [f"HTTP/1.1 {status} {reason}"]
    for k, v in response_headers.items():
        lines.append(f"{k}: {v}")

    response = "\r\n".join(lines) + "\r\n\r\n"
    writer.write(response.encode("latin-1") + body)


def _write_chunked_response(
    writer: asyncio.StreamWriter,
    status: int,
    chunks: list[bytes],
    headers: dict[str, str] | None = None,
) -> None:
    """Write a chunked HTTP response."""
    reason = "OK" if status == 200 else "Unknown"

    response_headers = {
        "Transfer-Encoding": "chunked",
        "Connection": "close",
    }
    if headers:
        response_headers.update(headers)

    lines = [f"HTTP/1.1 {status} {reason}"]
    for k, v in response_headers.items():
        lines.append(f"{k}: {v}")

    response = "\r\n".join(lines) + "\r\n\r\n"
    data = response.encode("latin-1")

    for chunk in chunks:
        data += f"{len(chunk):x}\r\n".encode("latin-1") + chunk + b"\r\n"

    data += b"0\r\n\r\n"
    writer.write(data)


async def _write_streaming_response(
    writer: asyncio.StreamWriter,
    status: int,
    lines: list[bytes],
    headers: dict[str, str] | None = None,
) -> None:
    """Write a streaming HTTP response without Content-Length (connection-close framed)."""
    reason = "OK" if status == 200 else "Unknown"

    response_headers = {
        "Connection": "close",
    }
    if headers:
        response_headers.update(headers)

    header_lines = [f"HTTP/1.1 {status} {reason}"]
    for k, v in response_headers.items():
        header_lines.append(f"{k}: {v}")

    response = "\r\n".join(header_lines) + "\r\n\r\n"
    writer.write(response.encode("latin-1"))

    for line in lines:
        writer.write(line)

    await writer.drain()


# --- Tests: basic GET ---

class TestHttpGet:
    """Tests for HTTP GET requests."""

    def test_get_returns_response_with_status(self) -> None:
        """GET request returns a response with the correct status code."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 200, b"hello")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/test"))
                    assert response.status() == 200
                    await response.text()

        asyncio.run(run())

    def test_get_returns_response_body_as_text(self) -> None:
        """GET request body can be read as text."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 200, b"hello world")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    text = await response.text()
                    assert text == "hello world"

        asyncio.run(run())

    def test_get_returns_response_body_as_json(self) -> None:
        """GET request body can be parsed as JSON."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            body = json.dumps({"key": "value", "num": 42}).encode()
            _write_response(writer, 200, body)

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    data = await response.json()
                    assert data == {"key": "value", "num": 42}

        asyncio.run(run())

    def test_get_sends_headers(self) -> None:
        """GET request sends caller-supplied headers."""
        received_headers: dict[str, str] = {}

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            received_headers.update(request["headers"])
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"), headers={"X-Custom": "test-value"})
                    await response.text()

        asyncio.run(run())
        assert received_headers.get("x-custom") == "test-value"

    def test_get_sends_correct_method_and_path(self) -> None:
        """GET request sends the correct method and path."""
        received: dict = {}

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            received["method"] = request["method"]
            received["path"] = request["path"]
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/api/v1/models"))
                    await response.text()

        asyncio.run(run())
        assert received["method"] == "GET"
        assert received["path"] == "/api/v1/models"

    def test_get_with_query_string(self) -> None:
        """GET request preserves query string in the path."""
        received: dict = {}

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            received["path"] = request["path"]
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/search?q=hello&limit=10"))
                    await response.text()

        asyncio.run(run())
        assert received["path"] == "/search?q=hello&limit=10"


# --- Tests: basic POST ---

class TestHttpPost:
    """Tests for HTTP POST requests."""

    def test_post_with_json_body(self) -> None:
        """POST request sends JSON body with correct Content-Type."""
        received: dict = {}

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            received["method"] = request["method"]
            received["body"] = request["body"]
            received["content_type"] = request["headers"].get("content-type")
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.post(server.url("/api"), json={"name": "test", "value": 123})
                    await response.text()

        asyncio.run(run())
        assert received["method"] == "POST"
        assert json.loads(received["body"]) == {"name": "test", "value": 123}
        assert received["content_type"] == "application/json"

    def test_post_with_raw_data(self) -> None:
        """POST request sends raw byte body."""
        received: dict = {}
        raw = b'{"raw": "data"}'

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            received["body"] = request["body"]
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.post(
                        server.url("/"),
                        data=raw,
                        headers={"Content-Type": "application/json"},
                    )
                    await response.text()

        asyncio.run(run())
        assert received["body"] == raw

    def test_post_with_no_body(self) -> None:
        """POST request with no body sends Content-Length: 0."""
        received: dict = {}

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            received["content_length"] = request["headers"].get("content-length")
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.post(server.url("/"))
                    await response.text()

        asyncio.run(run())
        assert received["content_length"] == "0"


# --- Tests: error handling ---

class TestHttpErrors:
    """Tests for HTTP error handling."""

    def test_raise_for_status_raises_on_4xx(self) -> None:
        """raise_for_status raises ClientResponseError on 4xx responses."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 404, b"not found")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    with pytest.raises(ClientResponseError) as exc_info:
                        response.raise_for_status()
                    assert exc_info.value.status() == 404
                    response.close()

        asyncio.run(run())

    def test_raise_for_status_raises_on_5xx(self) -> None:
        """raise_for_status raises ClientResponseError on 5xx responses."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 500, b"server error")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    with pytest.raises(ClientResponseError) as exc_info:
                        response.raise_for_status()
                    assert exc_info.value.status() == 500
                    response.close()

        asyncio.run(run())

    def test_raise_for_status_does_not_raise_on_2xx(self) -> None:
        """raise_for_status does not raise on 2xx responses."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    response.raise_for_status()
                    await response.text()

        asyncio.run(run())

    def test_connection_refused_raises_connector_error(self) -> None:
        """Connection to a closed port raises ClientConnectorError."""
        async def run() -> None:
            async with HttpClient() as client:
                with pytest.raises(ClientConnectorError):
                    await client.get("http://127.0.0.1:1/")

        asyncio.run(run())

    def test_response_error_has_status(self) -> None:
        """ClientResponseError has a status attribute."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 401, b"unauthorized")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    with pytest.raises(ClientResponseError) as exc_info:
                        response.raise_for_status()
                    assert exc_info.value.status() == 401
                    response.close()

        asyncio.run(run())

    def test_204_response_has_empty_body(self) -> None:
        """204 No Content response returns empty body."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 204, b"")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    assert response.status() == 204
                    text = await response.text()
                    assert text == ""

        asyncio.run(run())


# --- Tests: 100 Continue ---

class TestContinue:
    """Tests for 100 Continue informational response handling."""

    def test_skips_100_continue_response(self) -> None:
        """Client skips 100 Continue and returns the real response."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            # Send 100 Continue, then the real 200 response
            writer.write(b"HTTP/1.1 100 Continue\r\n\r\n")
            _write_response(writer, 200, b"real body")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    assert response.status() == 200
                    text = await response.text()
                    assert text == "real body"

        asyncio.run(run())

    def test_skips_multiple_informational_responses(self) -> None:
        """Client skips multiple 1xx responses and returns the real response."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            writer.write(b"HTTP/1.1 100 Continue\r\n\r\n")
            writer.write(b"HTTP/1.1 103 Early Hints\r\nLink: </style.css>; rel=preload\r\n\r\n")
            _write_response(writer, 200, b"final")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    assert response.status() == 200
                    text = await response.text()
                    assert text == "final"

        asyncio.run(run())


# --- Tests: multiple headers ---

class TestMultipleHeaders:
    """Tests for handling multiple headers with the same name."""

    def test_multiple_same_name_headers_concatenated(self) -> None:
        """Multiple headers with the same name are concatenated with ', '."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            response = (
                b"HTTP/1.1 200 OK\r\n"
                b"Set-Cookie: a=1\r\n"
                b"Set-Cookie: b=2\r\n"
                b"Content-Length: 2\r\n"
                b"Connection: close\r\n"
                b"\r\n"
                b"ok"
            )
            writer.write(response)

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    cookies = response.headers().get("set-cookie")
                    assert cookies == "a=1, b=2"
                    await response.text()

        asyncio.run(run())

    def test_single_header_not_affected(self) -> None:
        """Single headers are not affected by the concatenation logic."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 200, b"ok", headers={"X-Custom": "value"})

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    assert response.headers().get("x-custom") == "value"
                    await response.text()

        asyncio.run(run())


# --- Tests: streaming ---

class TestStreaming:
    """Tests for streaming response bodies."""

    def test_content_lines_yields_lines(self) -> None:
        """content_lines yields each line from the response body."""
        lines_data = [b"data: first\n", b"data: second\n", b"data: third\n"]

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            await _write_streaming_response(writer, 200, lines_data)

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    lines = []
                    async for line in response.content_lines():
                        lines.append(line)
                    assert lines == lines_data

        asyncio.run(run())

    def test_content_lines_with_chunked_encoding(self) -> None:
        """content_lines transparently decodes chunked transfer encoding."""
        chunks = [b"data: hello\n", b"data: world\n", b"data: done\n"]

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_chunked_response(writer, 200, chunks)

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    lines = []
                    async for line in response.content_lines():
                        lines.append(line)
                    assert lines == chunks

        asyncio.run(run())

    def test_text_with_chunked_encoding(self) -> None:
        """text() correctly reassembles chunked response body."""
        chunks = [b"hello ", b"world", b"!"]

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_chunked_response(writer, 200, chunks)

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    text = await response.text()
                    assert text == "hello world!"

        asyncio.run(run())


# --- Tests: redirects ---

class TestRedirects:
    """Tests for redirect following."""

    def test_follows_301_redirect(self) -> None:
        """Client follows a 301 redirect to the new location."""
        request_count = 0

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            nonlocal request_count
            request_count += 1
            if request_count == 1:
                _write_response(writer, 301, b"", headers={"Location": "/final"})
            else:
                _write_response(writer, 200, b"final content")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/start"))
                    assert response.status() == 200
                    assert await response.text() == "final content"

        asyncio.run(run())

    def test_follows_302_redirect(self) -> None:
        """Client follows a 302 redirect."""
        request_count = 0

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            nonlocal request_count
            request_count += 1
            if request_count == 1:
                _write_response(writer, 302, b"", headers={"Location": "/destination"})
            else:
                _write_response(writer, 200, b"arrived")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/"))
                    assert response.status() == 200
                    assert await response.text() == "arrived"

        asyncio.run(run())

    def test_303_redirect_changes_post_to_get(self) -> None:
        """303 redirect converts POST to GET and drops the body."""
        requests: list[dict] = []

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            requests.append(request)
            if len(requests) == 1:
                _write_response(writer, 303, b"", headers={"Location": "/result"})
            else:
                _write_response(writer, 200, b"result")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.post(
                        server.url("/submit"),
                        json={"data": "value"},
                    )
                    assert response.status() == 200
                    await response.text()

        asyncio.run(run())
        assert len(requests) == 2
        assert requests[0]["method"] == "POST"
        assert requests[1]["method"] == "GET"
        assert requests[1]["body"] == b""

    def test_307_redirect_preserves_post(self) -> None:
        """307 redirect preserves the POST method and body."""
        requests: list[dict] = []

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            requests.append(request)
            if len(requests) == 1:
                _write_response(writer, 307, b"", headers={"Location": "/retry"})
            else:
                _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.post(
                        server.url("/submit"),
                        json={"data": "value"},
                    )
                    assert response.status() == 200
                    await response.text()

        asyncio.run(run())
        assert len(requests) == 2
        assert requests[1]["method"] == "POST"
        assert json.loads(requests[1]["body"]) == {"data": "value"}

    def test_follows_relative_redirect(self) -> None:
        """Client follows a relative path redirect."""
        request_count = 0

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            nonlocal request_count
            request_count += 1
            if request_count == 1:
                _write_response(writer, 302, b"", headers={"Location": "/relative/path"})
            else:
                _write_response(writer, 200, b"relative ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    response = await client.get(server.url("/start"))
                    assert response.status() == 200
                    assert await response.text() == "relative ok"

        asyncio.run(run())

    def test_max_redirects_raises_error(self) -> None:
        """Exceeding max redirects raises ClientResponseError."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 302, b"", headers={"Location": "/loop"})

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient() as client:
                    with pytest.raises(ClientResponseError):
                        await client.get(server.url("/loop"))

        asyncio.run(run())


# --- Tests: TLS ---

class TestTls:
    """Tests for TLS connections."""

    def test_https_get_with_ssl_context(self) -> None:
        """HTTPS GET works with a caller-supplied SSL context."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 200, b"secure hello")

        async def run() -> None:
            async with MockHTTPServer(handler, use_tls=True) as server:
                async with HttpClient(ssl_context=server.ssl_context()) as client:
                    response = await client.get(server.url("/"))
                    text = await response.text()
                    assert text == "secure hello"

        asyncio.run(run())

    def test_https_post_with_ssl_context(self) -> None:
        """HTTPS POST works with a caller-supplied SSL context."""
        received: dict = {}

        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            received["body"] = request["body"]
            _write_response(writer, 200, b"ok")

        async def run() -> None:
            async with MockHTTPServer(handler, use_tls=True) as server:
                async with HttpClient(ssl_context=server.ssl_context()) as client:
                    response = await client.post(server.url("/api"), json={"secure": True})
                    await response.text()

        asyncio.run(run())
        assert json.loads(received["body"]) == {"secure": True}


# --- Tests: IPv4 forcing ---

class TestIPv4:
    """Tests for IPv4 family forcing."""

    def test_ipv4_family_connects_to_localhost(self) -> None:
        """Client with family=AF_INET connects to an IPv4 server."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            _write_response(writer, 200, b"ipv4 ok")

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient(family=socket.AF_INET) as client:
                    response = await client.get(f"http://localhost:{server.port()}/")
                    text = await response.text()
                    assert text == "ipv4 ok"

        asyncio.run(run())


# --- Tests: timeout ---

class TestTimeouts:
    """Tests for connect and read timeouts."""

    def test_connect_timeout_raises_server_timeout_error(self) -> None:
        """Connect timeout raises ServerTimeoutError."""
        async def run() -> None:
            async with HttpClient(connect_timeout=0.001) as client:
                with pytest.raises(ServerTimeoutError):
                    await client.get("http://10.255.255.1:80/")

        asyncio.run(run())

    def test_read_timeout_raises_server_timeout_error(self) -> None:
        """Read timeout raises ServerTimeoutError."""
        async def handler(request: dict, writer: asyncio.StreamWriter) -> None:
            writer.write(b"HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\n")
            await writer.drain()
            # Sleep long enough that the client's read timeout fires first.
            # The test server's finally block will close the connection
            # when the handler is cancelled after the test completes.
            await asyncio.sleep(5)

        async def run() -> None:
            async with MockHTTPServer(handler) as server:
                async with HttpClient(read_timeout=0.1) as client:
                    response = await client.get(server.url("/"))
                    with pytest.raises(ServerTimeoutError):
                        await response.text()

        asyncio.run(run())


# --- Tests: exception hierarchy ---

class TestExceptionHierarchy:
    """Tests for the exception class hierarchy."""

    def test_client_connector_error_is_http_client_error(self) -> None:
        """ClientConnectorError is a subclass of HttpClientError."""
        assert issubclass(ClientConnectorError, HttpClientError)

    def test_client_response_error_is_http_client_error(self) -> None:
        """ClientResponseError is a subclass of HttpClientError."""
        assert issubclass(ClientResponseError, HttpClientError)

    def test_server_timeout_error_is_http_client_error(self) -> None:
        """ServerTimeoutError is a subclass of HttpClientError."""
        assert issubclass(ServerTimeoutError, HttpClientError)

    def test_client_response_error_has_status_and_message(self) -> None:
        """ClientResponseError stores status and message."""
        error = ClientResponseError(404, "Not Found")
        assert error.status() == 404
        assert error.message() == "Not Found"