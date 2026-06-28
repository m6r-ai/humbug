"""Async HTTP client built on the Python standard library."""

import asyncio
import json as json_module
import socket
import ssl
from collections.abc import AsyncGenerator
from typing import Any
from urllib.parse import urlsplit, urlunsplit

from http_client.http_exceptions import (
    ClientConnectorError,
    ClientResponseError,
    HttpClientError,
    ServerTimeoutError,
)

_MAX_REDIRECTS = 5
_DEFAULT_CONNECT_TIMEOUT = 20.0
_DEFAULT_SSL_HANDSHAKE_TIMEOUT = 20.0
_DEFAULT_READ_TIMEOUT = 300.0
_DEFAULT_USER_AGENT = "Humbug"


class HttpResponse:
    """
    HTTP response with streaming and full-body access.

    The response body is read lazily — text() and json() consume the entire
    body, while content_lines() streams it line by line.  Only one body
    access method should be called per response.
    """

    def __init__(
        self,
        status_code: int,
        headers: dict[str, str],
        reader: asyncio.StreamReader | None,
        protocol: str = "HTTP/1.1",
        writer: asyncio.StreamWriter | None = None,
        read_timeout: float = _DEFAULT_READ_TIMEOUT,
    ) -> None:
        """
        Initialize the response.

        Args:
            status_code: HTTP status code
            headers: Response headers (lowercased keys)
            reader: Stream reader for the response body, or None if no body
            protocol: HTTP protocol version from the status line
            writer: Writer to close when the response body is fully consumed
            read_timeout: Seconds to wait for each read operation
        """
        self._status = status_code
        self._headers = headers
        self._reader = reader
        self._writer = writer
        self._protocol = protocol
        self._read_timeout = read_timeout
        self._body_read = False

    def status(self) -> int:
        """Return the HTTP status code."""
        return self._status

    def headers(self) -> dict[str, str]:
        """Return response headers with lowercased keys."""
        return self._headers

    def raise_for_status(self) -> None:
        """Raise ClientResponseError if the status code is >= 400."""
        if self._status >= 400:
            raise ClientResponseError(self._status, self._headers.get("reason", ""))

    async def text(self) -> str:
        """
        Read the entire response body as a string.

        Returns:
            Response body decoded as UTF-8
        """
        data = await self._read_full_body()
        return data.decode("utf-8")

    async def json(self) -> dict[str, Any]:
        """
        Read the entire response body and parse as JSON.

        Returns:
            Parsed JSON object
        """
        data = await self._read_full_body()
        return json_module.loads(data.decode("utf-8"))

    async def content_lines(self) -> AsyncGenerator[bytes, None]:
        """
        Stream the response body line by line as bytes.

        Each yielded item includes the trailing newline character(s), matching
        the behavior of asyncio.StreamReader.readline().

        Yields:
            Lines from the response body as bytes
        """
        if self._reader is None:
            return

        self._body_read = True
        try:
            chunked = self._headers.get("transfer-encoding", "").lower() == "chunked"

            if chunked:
                async for line in self._read_chunked_lines():
                    yield line

            else:
                while True:
                    line = await self._timed_readline()
                    if not line:
                        break

                    yield line

        finally:
            self._close_writer()

    async def _read_full_body(self) -> bytes:
        """Read the complete response body as bytes."""
        if self._body_read:
            raise HttpClientError("Response body has already been read")

        if self._reader is None:
            self._body_read = True
            self._close_writer()
            return b""

        self._body_read = True
        chunked = self._headers.get("transfer-encoding", "").lower() == "chunked"

        try:
            if chunked:
                chunks: list[bytes] = []
                async for chunk in self._read_chunked_data():
                    chunks.append(chunk)

                return b"".join(chunks)

            content_length = self._headers.get("content-length")
            if content_length is not None:
                length = int(content_length)
                if length == 0:
                    return b""

                data = await self._timed_readexactly(length)
                return data

            # No Content-Length and no chunked encoding — read until close
            data = await self._timed_read(-1)
            return data

        finally:
            self._close_writer()

    async def _read_chunked_data(self) -> AsyncGenerator[bytes, None]:
        """Yield decoded chunk data from a chunked transfer-encoding response."""
        if self._reader is None:
            return

        while True:
            size_line = await self._timed_readline()
            if not size_line:
                return

            size_str = size_line.strip().split(b";")[0]
            try:
                chunk_size = int(size_str, 16)

            except ValueError:
                return

            if chunk_size == 0:
                # Read and discard trailing headers until empty line
                while True:
                    trailer = await self._timed_readline()
                    if not trailer or trailer == b"\r\n":
                        break

                return

            chunk = await self._timed_readexactly(chunk_size)
            yield chunk

            # Consume trailing CRLF after chunk data
            await self._timed_readexactly(2)

    async def _read_chunked_lines(self) -> AsyncGenerator[bytes, None]:
        """Yield lines from a chunked response, transparently decoding chunks."""
        buffer = bytearray()

        async for chunk in self._read_chunked_data():
            buffer.extend(chunk)
            while b"\n" in buffer:
                idx = buffer.index(b"\n") + 1
                yield bytes(buffer[:idx])
                buffer = buffer[idx:]

        if buffer:
            yield bytes(buffer)

    async def _timed_readline(self) -> bytes:
        """Read a line from the stream with the configured read timeout."""
        assert self._reader is not None
        try:
            return await asyncio.wait_for(
                self._reader.readline(),
                timeout=self._read_timeout,
            )

        except asyncio.TimeoutError as e:
            raise ServerTimeoutError(f"Read timed out after {self._read_timeout}s") from e

    async def _timed_readexactly(self, n: int) -> bytes:
        """Read exactly n bytes from the stream with the configured read timeout."""
        assert self._reader is not None
        try:
            return await asyncio.wait_for(
                self._reader.readexactly(n),
                timeout=self._read_timeout,
            )

        except asyncio.TimeoutError as e:
            raise ServerTimeoutError(f"Read timed out after {self._read_timeout}s") from e

    async def _timed_read(self, n: int) -> bytes:
        """Read up to n bytes from the stream with the configured read timeout."""
        assert self._reader is not None
        try:
            return await asyncio.wait_for(
                self._reader.read(n),
                timeout=self._read_timeout,
            )

        except asyncio.TimeoutError as e:
            raise ServerTimeoutError(f"Read timed out after {self._read_timeout}s") from e

    def _close_writer(self) -> None:
        """Close the underlying writer if one is associated with this response."""
        if self._writer is not None:
            self._writer.close()
            self._writer = None

    def close(self) -> None:
        """Close the underlying connection if the body has not been fully read."""
        self._close_writer()


class HttpClient:
    """
    Minimal async HTTP client built on asyncio streams.

    Each request creates a new TCP/TLS connection.  No connection pooling.
    Supports GET and POST, JSON and raw byte bodies, TLS with caller-supplied
    SSL contexts, connect/read timeouts, and redirect following.
    """

    def __init__(
        self,
        ssl_context: ssl.SSLContext | None = None,
        family: socket.AddressFamily | None = None,
        connect_timeout: float = _DEFAULT_CONNECT_TIMEOUT,
        ssl_handshake_timeout: float = _DEFAULT_SSL_HANDSHAKE_TIMEOUT,
        read_timeout: float = _DEFAULT_READ_TIMEOUT,
    ) -> None:
        """
        Initialize the HTTP client.

        Args:
            ssl_context: SSL context for TLS.  If None, uses system default.
            family: Force IPv4 (socket.AF_INET) or IPv6.  None for default.
            connect_timeout: Seconds to wait for TCP connection.
            ssl_handshake_timeout: Seconds to wait for TLS handshake.
            read_timeout: Seconds to wait for each read operation.
        """
        self._ssl_context = ssl_context
        self._family = family
        self._connect_timeout = connect_timeout
        self._ssl_handshake_timeout = ssl_handshake_timeout
        self._read_timeout = read_timeout

    async def get(self, url: str, headers: dict[str, str] | None = None) -> HttpResponse:
        """
        Send a GET request.

        Args:
            url: Full URL to fetch
            headers: Optional request headers

        Returns:
            HttpResponse instance
        """
        return await self._request("GET", url, headers=headers)

    async def post(
        self,
        url: str,
        headers: dict[str, str] | None = None,
        json: dict[str, Any] | None = None,
        data: bytes | None = None,
    ) -> HttpResponse:
        """
        Send a POST request.

        Args:
            url: Full URL to post to
            headers: Optional request headers
            json: JSON body (serialized automatically)
            data: Raw byte body (used instead of json if both provided)

        Returns:
            HttpResponse instance
        """
        return await self._request("POST", url, headers=headers, json=json, data=data)

    async def __aenter__(self) -> 'HttpClient':
        """Enter async context."""
        return self

    async def __aexit__(self, *args: Any) -> None:
        """Exit async context — no resources to clean up."""

    async def _request(
        self,
        method: str,
        url: str,
        headers: dict[str, str] | None = None,
        json: dict[str, Any] | None = None,
        data: bytes | None = None,
        redirect_count: int = 0,
    ) -> HttpResponse:
        """
        Send an HTTP request and handle redirects.

        Args:
            method: HTTP method (GET or POST)
            url: Full URL
            headers: Optional request headers
            json: Optional JSON body
            data: Optional raw byte body
            redirect_count: Current redirect depth (internal)

        Returns:
            HttpResponse instance
        """
        parsed = urlsplit(url)
        scheme = parsed.scheme.lower()
        host = parsed.hostname or ""
        use_tls = scheme == "https"
        port = parsed.port or (443 if use_tls else 80)

        path = parsed.path or "/"
        if parsed.query:
            path = f"{path}?{parsed.query}"

        # Build request headers
        request_headers: dict[str, str] = {
            "Host": host if port in (80, 443) else f"{host}:{port}",
            "User-Agent": _DEFAULT_USER_AGENT,
            "Accept": "*/*",
            "Connection": "close",
        }

        if headers:
            request_headers.update(headers)

        # Build request body
        body: bytes | None = None
        if data is not None:
            body = data
            request_headers.setdefault("Content-Type", "application/octet-stream")

        elif json is not None:
            body = json_module.dumps(json).encode("utf-8")
            request_headers.setdefault("Content-Type", "application/json")

        if method == "POST":
            request_headers["Content-Length"] = str(len(body) if body is not None else 0)

        # Connect
        try:
            ssl_param: ssl.SSLContext | bool | None = None
            if use_tls:
                ssl_param = self._ssl_context if self._ssl_context else True

            reader, writer = await asyncio.wait_for(
                asyncio.open_connection(
                    host=host,
                    port=port,
                    ssl=ssl_param,
                    ssl_handshake_timeout=self._ssl_handshake_timeout if use_tls else None,
                    server_hostname=host if use_tls else None,
                    family=self._family if self._family else 0,
                ),
                timeout=self._connect_timeout,
            )

        except asyncio.TimeoutError as e:
            raise ServerTimeoutError(f"Connection to {host}:{port} timed out") from e

        except (ConnectionError, OSError) as e:
            raise ClientConnectorError(f"Failed to connect to {host}:{port}: {e}") from e

        try:
            # Send request
            request_line = f"{method} {path} HTTP/1.1\r\n"
            header_lines = "".join(f"{k}: {v}\r\n" for k, v in request_headers.items())
            request_data = (request_line + header_lines + "\r\n").encode("latin-1")

            if body:
                request_data += body

            writer.write(request_data)
            await writer.drain()

            # Read response
            response = await self._read_response(reader, writer)

            # Handle redirects
            if response.status() in (301, 302, 303, 307, 308):
                if redirect_count >= _MAX_REDIRECTS:
                    await response.text()
                    raise ClientResponseError(
                        response.status(),
                        f"Too many redirects (max {_MAX_REDIRECTS})",
                    )

                location = response.headers().get("location")
                if location:
                    # Consume the redirect response body so the connection can close cleanly
                    await response.text()

                    redirect_url = self._resolve_redirect(url, location)
                    redirect_method = method
                    redirect_json = json
                    redirect_data = data

                    # 301, 302, 303: POST becomes GET, body dropped
                    if response.status() in (301, 302, 303) and method == "POST":
                        redirect_method = "GET"
                        redirect_json = None
                        redirect_data = None

                    return await self._request(
                        redirect_method,
                        redirect_url,
                        headers=headers,
                        json=redirect_json,
                        data=redirect_data,
                        redirect_count=redirect_count + 1,
                    )

            return response

        except ServerTimeoutError:
            writer.close()
            raise

        except ClientConnectorError:
            writer.close()
            raise

        except asyncio.TimeoutError as e:
            writer.close()
            raise ServerTimeoutError(f"Read from {host}:{port} timed out") from e

        except (ConnectionError, OSError) as e:
            writer.close()
            raise ClientConnectorError(f"Connection error to {host}:{port}: {e}") from e

        except Exception:
            writer.close()
            raise

    async def _read_response(
        self,
        reader: asyncio.StreamReader,
        writer: asyncio.StreamWriter | None = None,
    ) -> HttpResponse:
        """Read the HTTP response status line and headers from the stream."""
        while True:
            # Read status line
            status_line = await asyncio.wait_for(
                reader.readline(),
                timeout=self._read_timeout,
            )

            if not status_line:
                raise ClientConnectorError("Connection closed before response")

            status_parts = status_line.decode("latin-1").strip().split(" ", 2)
            if len(status_parts) < 2:
                raise ClientConnectorError(f"Malformed status line: {status_line!r}")

            protocol = status_parts[0]
            try:
                status_code = int(status_parts[1])

            except ValueError as exc:
                raise ClientConnectorError(f"Invalid status code: {status_parts[1]}") from exc

            reason = status_parts[2] if len(status_parts) > 2 else ""

            # Read headers
            headers: dict[str, str] = {}
            if reason:
                headers["reason"] = reason

            while True:
                header_line = await asyncio.wait_for(
                    reader.readline(),
                    timeout=self._read_timeout,
                )

                if not header_line or header_line in (b"\r\n", b"\n"):
                    break

                decoded = header_line.decode("latin-1").rstrip("\r\n")
                if ":" not in decoded:
                    continue

                key, value = decoded.split(":", 1)
                key = key.strip().lower()
                value = value.strip()

                if key in headers:
                    headers[key] = f"{headers[key]}, {value}"

                else:
                    headers[key] = value

            # 1xx informational responses (e.g. 100 Continue) have no body.
            # Discard and read the next response.
            if 100 <= status_code < 200:
                continue

            # Determine if there's a body
            # HEAD requests have no body, and 204/304 responses have no body
            has_body = status_code not in (204, 304)

            return HttpResponse(
                status_code=status_code,
                headers=headers,
                reader=reader if has_body else None,
                protocol=protocol,
                writer=writer,
                read_timeout=self._read_timeout,
            )

    def _resolve_redirect(self, base_url: str, location: str) -> str:
        """Resolve a redirect Location header against the current URL."""
        if location.startswith(("http://", "https://")):
            return location

        parsed = urlsplit(base_url)
        if location.startswith("/"):
            # Absolute path redirect
            return urlunsplit((
                parsed.scheme,
                parsed.netloc,
                location,
                "",
                "",
            ))

        # Relative path redirect
        base_path = parsed.path.rsplit("/", 1)[0]
        return urlunsplit((
            parsed.scheme,
            parsed.netloc,
            f"{base_path}/{location}",
            "",
            "",
        ))
