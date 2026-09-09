"""Persisted metadata for items sitting in a mindspace's trash directory."""

from dataclasses import dataclass
import json
import logging
import os
import re


_TRASH_NAME_PREFIX_RE = re.compile(r"^[0-9a-f]{8}_")


def strip_trash_prefix(trash_name: str) -> str:
    """Return trash_name with its uuid8_ uniqueness prefix removed, if present."""
    return _TRASH_NAME_PREFIX_RE.sub("", trash_name, count=1)


@dataclass
class TrashEntry:
    """Metadata for a single item currently in the trash."""
    trash_name: str
    original_path: str
    deleted_at: str
    is_dir: bool


class TrashManifest:
    """
    Tracks the original mindspace-relative location of trashed items.

    Stored as trash/manifest.json, keyed by trash_name (the basename each
    item was moved to within the trash directory). The trash path naming
    scheme alone does not preserve where an item came from, so this manifest
    is what makes restoring to the original location possible.
    """

    _logger = logging.getLogger("TrashManifest")

    def __init__(self) -> None:
        self._entries: dict[str, TrashEntry] = {}

    def get(self, trash_name: str) -> TrashEntry | None:
        """Return the entry for trash_name, or None if unknown."""
        return self._entries.get(trash_name)

    def record(self, trash_name: str, original_path: str, deleted_at: str, is_dir: bool) -> None:
        """Add or replace the entry for trash_name."""
        self._entries[trash_name] = TrashEntry(
            trash_name=trash_name,
            original_path=original_path,
            deleted_at=deleted_at,
            is_dir=is_dir,
        )

    def forget(self, trash_name: str) -> None:
        """Remove the entry for trash_name, if present."""
        self._entries.pop(trash_name, None)

    def to_dict(self) -> dict:
        """Serialise to a JSON-compatible dict."""
        return {
            "version": "1.0",
            "entries": {
                name: {
                    "original_path": entry.original_path,
                    "deleted_at": entry.deleted_at,
                    "is_dir": entry.is_dir,
                }
                for name, entry in self._entries.items()
            },
        }

    @classmethod
    def from_dict(cls, data: dict) -> "TrashManifest":
        """Deserialise from a previously saved dict, dropping malformed entries."""
        manifest = cls()
        entries = data.get("entries", {})
        if not isinstance(entries, dict):
            return manifest

        for name, item in entries.items():
            if not isinstance(item, dict):
                cls._logger.warning("Dropping malformed trash manifest entry for '%s'", name)
                continue

            try:
                manifest._entries[name] = TrashEntry(
                    trash_name=name,
                    original_path=str(item["original_path"]),
                    deleted_at=str(item["deleted_at"]),
                    is_dir=bool(item["is_dir"]),
                )

            except KeyError:
                cls._logger.warning("Dropping malformed trash manifest entry for '%s'", name)

        return manifest

    def save(self, manifest_path: str) -> None:
        """Persist this manifest to manifest_path."""
        try:
            with open(manifest_path, 'w', encoding='utf-8') as f:
                json.dump(self.to_dict(), f, indent=2)

        except OSError as e:
            self._logger.error("Failed to save trash manifest to '%s': %s", manifest_path, str(e))

    @classmethod
    def load(cls, manifest_path: str) -> "TrashManifest":
        """Load a manifest from manifest_path, returning an empty one if missing or invalid."""
        if not os.path.exists(manifest_path):
            return cls()

        try:
            with open(manifest_path, encoding='utf-8') as f:
                data = json.load(f)

            return cls.from_dict(data)

        except (OSError, json.JSONDecodeError) as e:
            cls._logger.warning("Failed to load trash manifest from '%s': %s", manifest_path, str(e))
            return cls()
