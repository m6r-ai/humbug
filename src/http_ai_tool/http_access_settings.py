"""Settings for external HTTP access control."""

from dataclasses import dataclass, field


@dataclass
class HttpAccessSettings:
    """
    Settings for controlling which URLs the HTTP tool may reach without approval.

    The master switch gates automatic approval only.  When it is off every
    request prompts the user, exactly as if no allowlist were configured.  It
    does not block network access; disabling the tool does that.

    Attributes:
        allow_external_http_access: Master switch for automatic approval
        external_http_allowlist: URL patterns that are auto-approved
        external_http_denylist: URL patterns that are always refused
    """
    allow_external_http_access: bool = True
    external_http_allowlist: list[str] = field(default_factory=list)
    external_http_denylist: list[str] = field(default_factory=list)

    @staticmethod
    def get_default_allowlist() -> list[str]:
        """Get the default allowlist, which is empty."""
        return []

    @staticmethod
    def get_default_denylist() -> list[str]:
        """
        Get the default denylist of addresses that are never auto-approved.

        These cover loopback, link-local and private network ranges, which is
        where cloud instance metadata services and other internal endpoints
        live.  They are refused outright rather than offered for approval so
        that a redirect or a crafted URL cannot turn an allowlisted host into
        a route to internal infrastructure.
        """
        return [
            "localhost",
            "127.0.0.1",
            "::1",
            "169.254.169.254",
            "metadata.google.internal",
            "*.local",
            "10.0.0.0/8",
            "172.16.0.0/12",
            "192.168.0.0/16",
            "169.254.0.0/16",
            "fc00::/7",
            "fe80::/10",
        ]

    @classmethod
    def create_default(cls) -> "HttpAccessSettings":
        """Create settings with default values."""
        return cls(
            allow_external_http_access=True,
            external_http_allowlist=cls.get_default_allowlist(),
            external_http_denylist=cls.get_default_denylist()
        )
