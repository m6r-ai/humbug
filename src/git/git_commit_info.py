"""Git commit information dataclass."""

from dataclasses import dataclass


@dataclass(frozen=True)
class GitCommitInfo:
    """Information about a single git commit."""
    hash: str           # Full commit hash
    author_name: str    # Author name
    author_email: str   # Author email
    author_date: str    # Author date in ISO 8601 format
    subject: str        # First line of the commit message
