"""Tests for HTTP access settings."""

from http_ai_tool.http_access_settings import HttpAccessSettings


class TestHttpAccessSettingsDefaults:
    """Tests for the default HTTP access settings."""

    def test_default_allowlist_is_empty(self) -> None:
        """The default allowlist should be empty so nothing is auto-approved."""
        assert HttpAccessSettings.get_default_allowlist() == []

    def test_default_denylist_covers_loopback(self) -> None:
        """The default denylist should refuse loopback addresses."""
        denylist = HttpAccessSettings.get_default_denylist()
        assert "localhost" in denylist
        assert "127.0.0.1" in denylist
        assert "::1" in denylist

    def test_default_denylist_covers_cloud_metadata_endpoint(self) -> None:
        """The default denylist should refuse the cloud instance metadata address."""
        assert "169.254.169.254" in HttpAccessSettings.get_default_denylist()

    def test_default_denylist_covers_private_networks(self) -> None:
        """The default denylist should refuse the RFC1918 private ranges."""
        denylist = HttpAccessSettings.get_default_denylist()
        assert "10.0.0.0/8" in denylist
        assert "172.16.0.0/12" in denylist
        assert "192.168.0.0/16" in denylist

    def test_default_denylist_covers_ipv6_private_ranges(self) -> None:
        """The default denylist should refuse the IPv6 unique-local and link-local ranges."""
        denylist = HttpAccessSettings.get_default_denylist()
        assert "fc00::/7" in denylist
        assert "fe80::/10" in denylist

    def test_create_default_enables_auto_approval(self) -> None:
        """The default settings should have auto-approval enabled."""
        settings = HttpAccessSettings.create_default()
        assert settings.allow_external_http_access is True

    def test_create_default_uses_default_lists(self) -> None:
        """The default settings should use the platform default lists."""
        settings = HttpAccessSettings.create_default()
        assert settings.external_http_allowlist == HttpAccessSettings.get_default_allowlist()
        assert settings.external_http_denylist == HttpAccessSettings.get_default_denylist()
