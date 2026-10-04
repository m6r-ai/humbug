"""Directories skipped when walking a mindspace's file tree."""

# Shared by every feature that enumerates mindspace content (global search, the
# Quick Switcher).  Hidden-directory policy is deliberately not part of this set:
# each caller decides whether to also skip dot-directories, because global search
# can be asked to include them while the Quick Switcher never shows them.
IGNORED_DIRS = frozenset({
    ".git",
    ".humbug",
    ".venv",
    "venv",
    "__pycache__",
    "node_modules",
    "dist",
    "build",
})
