"""Tests for HTTP tool allowlist matching and access enforcement."""

import asyncio
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from ai_tool import AIToolAuthorizationDenied, AIToolCall
from http_ai_tool.http_access_settings import HttpAccessSettings
from http_ai_tool.http_ai_tool import HttpAITool
from mindspace.mindspace import Mindspace


def _make_mindspace_mock(mindspace_path: str = "/tmp/mindspace") -> MagicMock:
    """Create a mock Mindspace with the given path."""
    mock = MagicMock(spec=Mindspace)
    mock.mindspace_path.return_value = mindspace_path
    mock.MINDSPACE_DIR = ".humbug"
    return mock


def _make_tool(
    allowlist: list[str] | None = None,
    denylist: list[str] | None = None,
    enabled: bool = True,
) -> HttpAITool:
    """Create a tool backed by a fixed set of access settings."""
    settings = HttpAccessSettings(
        allow_external_http_access=enabled,
        external_http_allowlist=allowlist or [],
        external_http_denylist=denylist or [],
    )
    return HttpAITool(_make_mindspace_mock(), lambda: settings)


def _make_tool_call(**kwargs: Any) -> AIToolCall:
    """Create an AIToolCall for the GET operation."""
    arguments: dict[str, Any] = {"operation": "get"}
    arguments.update(kwargs)
    return AIToolCall(id="test-id", name="http", arguments=arguments)


def _make_mock_response(status: int = 200, headers: dict[str, str] | None = None, text: str = "") -> MagicMock:
    """Create a mock HttpResponse."""
    response = MagicMock()
    response.status.return_value = status
    response.headers.return_value = headers or {}
    response.text = AsyncMock(return_value=text)
    return response


def _make_auth_callback(authorized: bool = True) -> MagicMock:
    """Create a mocked authorization callback that records its calls."""
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


def _matches(url: str, patterns: list[str]) -> bool:
    """Run the URL matcher directly."""
    return _make_tool(allowlist=patterns)._url_matches_patterns(url, patterns)


class TestUrlPatternParsing:
    """Tests for parsing allowlist and denylist entries."""

    def test_bare_host_parses_to_host_only(self) -> None:
        """A bare host should constrain the host and nothing else."""
        parsed = HttpAITool._parse_url_pattern("example.com")
        assert parsed == {"scheme": None, "host": "example.com", "port": None, "wildcard": False}

    def test_host_with_port_parses_port(self) -> None:
        """A host with a port should constrain that port."""
        parsed = HttpAITool._parse_url_pattern("example.com:8443")
        assert parsed == {"scheme": None, "host": "example.com", "port": 8443, "wildcard": False}

    def test_scheme_and_host_parses_scheme(self) -> None:
        """An explicit scheme should be captured."""
        parsed = HttpAITool._parse_url_pattern("https://example.com")
        assert parsed == {"scheme": "https", "host": "example.com", "port": None, "wildcard": False}

    def test_scheme_host_and_port_parses_all(self) -> None:
        """Scheme, host and port should all be captured."""
        parsed = HttpAITool._parse_url_pattern("https://example.com:8443")
        assert parsed == {"scheme": "https", "host": "example.com", "port": 8443, "wildcard": False}

    def test_wildcard_sets_flag_and_strips_prefix(self) -> None:
        """A leading wildcard should be recorded and removed from the host."""
        parsed = HttpAITool._parse_url_pattern("*.example.com")
        assert parsed == {"scheme": None, "host": "example.com", "port": None, "wildcard": True}

    def test_cidr_parses_to_network(self) -> None:
        """A CIDR entry should parse to an IP network."""
        parsed = HttpAITool._parse_url_pattern("10.0.0.0/8")
        assert parsed is not None
        assert str(parsed["network"]) == "10.0.0.0/8"

    def test_empty_pattern_returns_none(self) -> None:
        """An empty entry should be ignored."""
        assert HttpAITool._parse_url_pattern("") is None
        assert HttpAITool._parse_url_pattern("   ") is None

    def test_non_numeric_port_returns_none(self) -> None:
        """An entry with a non-numeric port should be ignored."""
        assert HttpAITool._parse_url_pattern("example.com:http") is None

    def test_out_of_range_port_returns_none(self) -> None:
        """An entry with an out-of-range port should be ignored."""
        assert HttpAITool._parse_url_pattern("example.com:70000") is None

    def test_bare_ipv6_address_parses_as_host(self) -> None:
        """An IPv6 literal in brackets should parse without being mistaken for a port."""
        parsed = HttpAITool._parse_url_pattern("[::1]")
        assert parsed == {"scheme": None, "host": "::1", "port": None, "wildcard": False}

    def test_bracketed_ipv6_with_port_parses_port(self) -> None:
        """A bracketed IPv6 literal may carry a port."""
        parsed = HttpAITool._parse_url_pattern("[::1]:8443")
        assert parsed == {"scheme": None, "host": "::1", "port": 8443, "wildcard": False}


class TestUrlMatching:
    """Tests for matching URLs against patterns."""

    def test_empty_pattern_list_matches_nothing(self) -> None:
        """An empty pattern list should match no URL."""
        assert _matches("https://example.com", []) is False

    def test_exact_host_matches(self) -> None:
        """An exact host entry should match that host."""
        assert _matches("https://example.com/path", ["example.com"]) is True

    def test_exact_host_is_case_insensitive(self) -> None:
        """Host matching should ignore case."""
        assert _matches("https://EXAMPLE.com", ["example.com"]) is True
        assert _matches("https://example.com", ["EXAMPLE.COM"]) is True

    def test_exact_host_does_not_match_subdomain(self) -> None:
        """An exact host entry should not match a subdomain."""
        assert _matches("https://api.example.com", ["example.com"]) is False

    def test_wildcard_matches_subdomain(self) -> None:
        """A wildcard entry should match a subdomain."""
        assert _matches("https://api.example.com", ["*.example.com"]) is True

    def test_wildcard_matches_deep_subdomain(self) -> None:
        """A wildcard entry should match any depth of subdomain."""
        assert _matches("https://a.b.example.com", ["*.example.com"]) is True

    def test_wildcard_does_not_match_bare_domain(self) -> None:
        """A wildcard entry should not match the bare domain."""
        assert _matches("https://example.com", ["*.example.com"]) is False

    def test_wildcard_does_not_match_suffix_lookalike(self) -> None:
        """A wildcard entry should not match a host that merely ends with the same text."""
        assert _matches("https://evilexample.com", ["*.example.com"]) is False

    def test_scheme_constraint_is_enforced(self) -> None:
        """An entry with a scheme should only match that scheme."""
        assert _matches("https://example.com", ["https://example.com"]) is True
        assert _matches("http://example.com", ["https://example.com"]) is False

    def test_port_constraint_is_enforced(self) -> None:
        """An entry with a port should only match that port."""
        assert _matches("https://example.com:8443", ["example.com:8443"]) is True
        assert _matches("https://example.com", ["example.com:8443"]) is False

    def test_default_https_port_is_used_when_absent(self) -> None:
        """A URL without an explicit port should be matched against the scheme default."""
        assert _matches("https://example.com", ["example.com:443"]) is True
        assert _matches("http://example.com", ["example.com:80"]) is True

    def test_scheme_and_port_together_are_enforced(self) -> None:
        """An entry constraining both scheme and port should require both."""
        assert _matches("https://example.com:8443", ["https://example.com:8443"]) is True
        assert _matches("http://example.com:8443", ["https://example.com:8443"]) is False
        assert _matches("https://example.com:9443", ["https://example.com:8443"]) is False

    def test_path_is_not_considered(self) -> None:
        """Matching should ignore the path and query string."""
        assert _matches("https://example.com/anything?x=1", ["example.com"]) is True

    def test_cidr_matches_ipv4_address_in_range(self) -> None:
        """A CIDR entry should match an IPv4 address inside the range."""
        assert _matches("http://10.1.2.3/", ["10.0.0.0/8"]) is True

    def test_cidr_does_not_match_ipv4_address_outside_range(self) -> None:
        """A CIDR entry should not match an IPv4 address outside the range."""
        assert _matches("http://11.1.2.3/", ["10.0.0.0/8"]) is False

    def test_cidr_matches_ipv6_address_in_range(self) -> None:
        """A CIDR entry should match an IPv6 address inside the range."""
        assert _matches("http://[fd00::1]/", ["fc00::/7"]) is True

    def test_cidr_does_not_match_hostname(self) -> None:
        """A CIDR entry should not match a hostname that is not an IP literal."""
        assert _matches("https://example.com", ["10.0.0.0/8"]) is False

    def test_malformed_entries_are_skipped(self) -> None:
        """A malformed entry should be ignored without preventing a later match."""
        assert _matches("https://example.com", ["not a url at all", "example.com"]) is True

    def test_any_one_pattern_matching_is_sufficient(self) -> None:
        """A URL matching any entry should match."""
        assert _matches("https://b.example.com", ["a.example.com", "*.example.com"]) is True


class TestAccessEnforcement:
    """Tests for how the access check gates requests."""

    def test_allowlisted_get_does_not_prompt(self) -> None:
        """A GET to an allowlisted host should run without asking the user."""
        tool = _make_tool(allowlist=["example.com"])
        tool_call = _make_tool_call(url="https://example.com/data")
        auth = _make_auth_callback(authorized=False)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=_make_mock_response(text="ok"))
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_not_called()

    def test_allowlisted_head_does_not_prompt(self) -> None:
        """A HEAD to an allowlisted host should run without asking the user."""
        tool = _make_tool(allowlist=["example.com"])
        tool_call = AIToolCall(
            id="test-id", name="http",
            arguments={"operation": "head", "url": "https://example.com"}
        )
        auth = _make_auth_callback(authorized=False)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.head = AsyncMock(return_value=_make_mock_response())
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_not_called()

    def test_unlisted_host_prompts(self) -> None:
        """A GET to a host that is not allowlisted should ask the user."""
        tool = _make_tool(allowlist=["example.com"])
        tool_call = _make_tool_call(url="https://other.com/data")
        auth = _make_auth_callback(authorized=True)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=_make_mock_response(text="ok"))
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_called_once()

    def test_denylisted_host_is_refused_without_prompting(self) -> None:
        """A GET to a denylisted host should be refused without asking the user."""
        tool = _make_tool(allowlist=["example.com"], denylist=["localhost"])
        tool_call = _make_tool_call(url="http://localhost/admin")
        auth = _make_auth_callback(authorized=True)

        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_not_called()

    def test_denylist_takes_precedence_over_allowlist(self) -> None:
        """A host on both lists should be refused."""
        tool = _make_tool(allowlist=["example.com"], denylist=["example.com"])
        tool_call = _make_tool_call(url="https://example.com")
        auth = _make_auth_callback(authorized=True)

        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_not_called()

    def test_switch_off_prompts_even_when_allowlisted(self) -> None:
        """With auto-approval disabled an allowlisted host should still prompt."""
        tool = _make_tool(allowlist=["example.com"], enabled=False)
        tool_call = _make_tool_call(url="https://example.com")
        auth = _make_auth_callback(authorized=True)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=_make_mock_response(text="ok"))
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_called_once()

    def test_switch_off_still_denies_denylisted_host(self) -> None:
        """With auto-approval disabled a denylisted host should still be refused."""
        tool = _make_tool(denylist=["localhost"], enabled=False)
        tool_call = _make_tool_call(url="http://localhost/")
        auth = _make_auth_callback(authorized=True)

        with pytest.raises(AIToolAuthorizationDenied):
            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_not_called()

    def test_post_is_never_auto_approved(self) -> None:
        """A POST to an allowlisted host should still ask the user."""
        tool = _make_tool(allowlist=["example.com"])
        tool_call = AIToolCall(
            id="test-id", name="http",
            arguments={"operation": "post", "url": "https://example.com/api", "json": {"x": 1}}
        )
        auth = _make_auth_callback(authorized=True)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.post = AsyncMock(return_value=_make_mock_response(text="ok"))
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_called_once()

    def test_delete_is_never_auto_approved(self) -> None:
        """A DELETE to an allowlisted host should still ask the user."""
        tool = _make_tool(allowlist=["example.com"])
        tool_call = AIToolCall(
            id="test-id", name="http",
            arguments={"operation": "delete", "url": "https://example.com/api/1"}
        )
        auth = _make_auth_callback(authorized=True)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.delete = AsyncMock(return_value=_make_mock_response(text="ok"))
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_called_once()

    def test_tool_without_settings_always_prompts(self) -> None:
        """A tool constructed without access settings should always ask the user."""
        tool = HttpAITool(_make_mindspace_mock())
        tool_call = _make_tool_call(url="https://example.com")
        auth = _make_auth_callback(authorized=True)

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=_make_mock_response(text="ok"))
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_called_once()


class TestRedirectHandling:
    """Tests for checking each redirect target before following it."""

    def test_allowlisted_redirect_is_followed_without_prompting(self) -> None:
        """A redirect to another allowlisted host should be followed silently."""
        tool = _make_tool(allowlist=["example.com", "cdn.example.com"])
        tool_call = _make_tool_call(url="https://example.com/start")
        auth = _make_auth_callback(authorized=False)

        redirect_response = _make_mock_response(
            status=302, headers={"location": "https://cdn.example.com/final"}
        )
        final_response = _make_mock_response(text="done")

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(side_effect=[redirect_response, final_response])
            mock_client_class.return_value = mock_client

            result = asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_not_called()
        assert "done" in result.content

    def test_redirect_to_denylisted_host_is_refused(self) -> None:
        """A redirect to a denylisted host should be refused even from an allowlisted host."""
        tool = _make_tool(allowlist=["example.com"], denylist=["169.254.169.254"])
        tool_call = _make_tool_call(url="https://example.com/start")
        auth = _make_auth_callback(authorized=True)

        redirect_response = _make_mock_response(
            status=302, headers={"location": "http://169.254.169.254/latest/meta-data/"}
        )

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(return_value=redirect_response)
            mock_client_class.return_value = mock_client

            with pytest.raises(AIToolAuthorizationDenied):
                asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_not_called()

    def test_redirect_to_unlisted_host_prompts(self) -> None:
        """A redirect to a host that is not allowlisted should ask the user."""
        tool = _make_tool(allowlist=["example.com"])
        tool_call = _make_tool_call(url="https://example.com/start")
        auth = _make_auth_callback(authorized=True)

        redirect_response = _make_mock_response(
            status=302, headers={"location": "https://other.com/final"}
        )
        final_response = _make_mock_response(text="done")

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(side_effect=[redirect_response, final_response])
            mock_client_class.return_value = mock_client

            asyncio.run(tool.execute(tool_call, None, auth))

        auth.assert_called_once()

    def test_redirect_loop_is_capped(self) -> None:
        """A chain of redirects longer than the limit should raise an execution error."""
        from ai_tool import AIToolExecutionError

        tool = _make_tool(allowlist=["example.com"])
        tool_call = _make_tool_call(url="https://example.com/0")
        auth = _make_auth_callback(authorized=True)

        def _redirect() -> MagicMock:
            return _make_mock_response(
                status=302, headers={"location": "https://example.com/next"}
            )

        with patch("http_ai_tool.http_ai_tool.HttpClient") as mock_client_class:
            mock_client = MagicMock()
            mock_client.__aenter__ = AsyncMock(return_value=mock_client)
            mock_client.__aexit__ = AsyncMock(return_value=None)
            mock_client.get = AsyncMock(side_effect=[_redirect() for _ in range(10)])
            mock_client_class.return_value = mock_client

            with pytest.raises(AIToolExecutionError, match="redirect"):
                asyncio.run(tool.execute(tool_call, None, auth))
