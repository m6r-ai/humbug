"""Cross-platform URL opener with WSL2 support."""

import subprocess
import sys
import webbrowser


def _is_wsl() -> bool:
    """Return True if running inside Windows Subsystem for Linux."""
    try:
        with open("/proc/version", encoding="utf-8") as f:
            return "microsoft" in f.read().lower()

    except OSError:
        return False


def open_url(url: str) -> None:
    """
    Open a URL in the system browser.

    On WSL2, delegates to the Windows host browser via cmd.exe since
    xdg-open/gio cannot handle https URLs without a desktop environment.

    Args:
        url: The URL to open.
    """
    if sys.platform == "linux" and _is_wsl():
        with subprocess.Popen(["cmd.exe", "/c", "start", url]):
            pass

    else:
        webbrowser.open(url)
