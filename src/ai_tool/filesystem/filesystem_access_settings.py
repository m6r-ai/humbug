"""Settings for external file access control."""

from dataclasses import dataclass
import sys


@dataclass
class FilesystemAccessSettings:
    """
    Settings for controlling filesystem access outside the primary boundary.
    
    Attributes:
        allow_external_access: Master switch to enable/disable external file access
        external_allowlist: Newline-separated glob patterns for auto-approved paths
        external_denylist: Newline-separated glob patterns for blocked paths
    """
    allow_external_access: bool = True
    external_allowlist: str = ""
    external_denylist: str = ""

    @staticmethod
    def get_default_allowlist() -> str:
        """Get platform-specific default allowlist."""
        if sys.platform == "win32":
            return ""  # Windows doesn't have standard system header locations

        if sys.platform == "darwin":
            return "/usr/include/**\n/usr/share/doc/**\n/usr/share/man/**\n/Library/Developer/**"

        # Linux and other Unix-like systems
        return "/usr/include/**\n/usr/share/doc/**\n/usr/share/man/**"

    @staticmethod
    def get_default_denylist() -> str:
        """Get platform-specific default denylist."""
        if sys.platform == "win32":
            return ""  # TODO: Add Windows-specific sensitive paths

        # Unix-like systems (Linux, macOS, etc.)
        return "~/.humbug/**\n~/.ssh/**\n~/.aws/**\n~/.gnupg/**\n~/.password-store/**"

    @classmethod
    def create_default(cls) -> "FilesystemAccessSettings":
        """Create settings with platform-appropriate defaults."""
        return cls(
            allow_external_access=True,
            external_allowlist=cls.get_default_allowlist(),
            external_denylist=cls.get_default_denylist()
        )
