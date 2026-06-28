"""Async GitHub release checker for update notifications."""

import logging

from PySide6.QtCore import QObject, Signal

from http_client import HttpClient
from desktop.version import CURRENT_VERSION


_GITHUB_API_URL = "https://api.github.com/repos/m6r-ai/humbug/releases/latest"
_REQUEST_TIMEOUT = 10.0


class UpdateChecker(QObject):
    """
    Checks GitHub for a newer Humbug release.

    Emits update_available when a newer version is found on startup check.
    Emits check_complete for the result of a manual check (always fired).
    All network failures are silent — no exceptions propagate to callers.
    Results are cached in memory for the lifetime of the instance.
    """

    update_available = Signal(str, str)
    check_complete = Signal(str, object, object)

    def __init__(self, parent: QObject | None = None) -> None:
        """Initialise the update checker."""
        super().__init__(parent)
        self._logger = logging.getLogger(__name__)
        self._cached_latest: str | None = None
        self._cached_url: str | None = None
        self._check_done = False

    def current_version_str(self) -> str:
        """Return the current version as a display string."""
        return f"v{CURRENT_VERSION}"

    def latest_version(self) -> str | None:
        """Return the cached latest version tag, or None if not yet checked."""
        return self._cached_latest

    def latest_release_url(self) -> str | None:
        """Return the cached latest release URL, or None if not yet checked."""
        return self._cached_url

    async def check(self, *, manual: bool = False) -> None:
        """
        Fetch the latest GitHub release and emit the appropriate signal.

        Args:
            manual: True when triggered by the user via the menu item.
                    Always emits check_complete.
                    False for the automatic startup check — only emits
                    update_available when a newer version is found.
        """
        if self._check_done and not manual:
            if self._cached_latest is not None:
                self.update_available.emit(self._cached_latest, self._cached_url or "")

            return

        latest_tag, release_url = await self._fetch_latest()

        if latest_tag is not None:
            self._cached_latest = latest_tag
            self._cached_url = release_url
            self._check_done = True

        if manual:
            self.check_complete.emit(
                self.current_version_str(),
                self._cached_latest,
                self._cached_url,
            )

        elif self._cached_latest is not None and self._is_newer(self._cached_latest):
            self.update_available.emit(self._cached_latest, self._cached_url or "")

    async def _fetch_latest(self) -> tuple[str | None, str | None]:
        """
        Fetch the latest release tag and URL from the GitHub API.

        Returns:
            A (tag_name, html_url) tuple, or (None, None) on any failure.
        """
        try:
            async with HttpClient(
                connect_timeout=_REQUEST_TIMEOUT,
                read_timeout=_REQUEST_TIMEOUT,
            ) as client:
                response = await client.get(
                    _GITHUB_API_URL,
                    headers={"Accept": "application/vnd.github+json"},
                )
                if response.status() != 200:
                    self._logger.debug(
                        "Update check returned HTTP %d", response.status()
                    )
                    return None, None

                data = await response.json()
                tag = data.get("tag_name")
                url = data.get("html_url")
                if not isinstance(tag, str) or not isinstance(url, str):
                    return None, None

                return tag, url

        except Exception as exc:  # pylint: disable=broad-exception-caught
            self._logger.debug("Update check failed: %s", exc)
            return None, None

    def _is_newer(self, tag: str) -> bool:
        """
        Return True if tag represents a version newer than CURRENT_VERSION.

        Args:
            tag: GitHub release tag, e.g. "v47" or "47".
        """
        try:
            version_str = tag.lstrip("vV")
            return int(version_str) > CURRENT_VERSION

        except ValueError:
            return False
