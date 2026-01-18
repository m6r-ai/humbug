"""Settings for external file access control."""

from dataclasses import dataclass, field
from typing import List
import sys


@dataclass
class FilesystemAccessSettings:
    """
    Settings for controlling filesystem access outside the primary boundary.

    Attributes:
        allow_external_access: Master switch to enable/disable external file access
        external_allowlist: List of glob patterns for auto-approved paths
        external_denylist: List of glob patterns for blocked paths
    """
    allow_external_access: bool = True
    external_allowlist: List[str] = field(default_factory=list)
    external_denylist: List[str] = field(default_factory=list)

    @staticmethod
    def get_default_allowlist() -> List[str]:
        """Get platform-specific default allowlist."""
        if sys.platform == "win32":
            return []  # Windows doesn't have standard system header locations

        if sys.platform == "darwin":
            return ["/usr/include/**", "/usr/share/doc/**", "/usr/share/man/**", "/Library/Developer/**"]

        # Linux and other Unix-like systems
        return ["/usr/include/**", "/usr/share/doc/**", "/usr/share/man/**"]

    @staticmethod
    def get_default_denylist() -> List[str]:
        """Get platform-specific default denylist."""
        if sys.platform == "win32":
            return []  # TODO: Add Windows-specific sensitive paths

        # Unix-like systems (Linux, macOS, etc.)
        return ["~/.humbug/**", "~/.ssh/**", "~/.aws/**", "~/.gnupg/**", "~/.password-store/**"]

    @classmethod
    def create_default(cls) -> "FilesystemAccessSettings":
        """Create settings with platform-appropriate defaults."""
        return cls(
            allow_external_access=True,
            external_allowlist=cls.get_default_allowlist(),
            external_denylist=cls.get_default_denylist()
        )
