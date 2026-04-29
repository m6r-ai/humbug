from __future__ import annotations

import os
from dataclasses import dataclass

from humbug.mindspace.mindspace_view_type import MindspaceViewType


@dataclass(slots=True)
class MindspaceSearchMatch:
    """A single global-search match within the current mindspace."""

    view_type: MindspaceViewType
    path: str
    relative_path: str
    line_number: int | None = None
    line_text: str = ""
    is_path_match: bool = False


class MindspaceSearchEngine:
    """Search mindspace file paths and file contents."""

    _IGNORED_DIRS = {
        ".git",
        ".humbug",
        ".venv",
        "venv",
        "__pycache__",
        "node_modules",
        "dist",
        "build",
    }

    _MAX_MATCHES = 200
    _MAX_MATCHES_PER_FILE = 20
    _BINARY_SAMPLE_SIZE = 2048

    def search(self, mindspace_path: str, query: str) -> list[MindspaceSearchMatch]:
        """Search the current mindspace for the given query."""
        normalized_query = query.strip()
        if not mindspace_path or not normalized_query:
            return []

        lowered_query = normalized_query.casefold()
        matches: list[MindspaceSearchMatch] = []

        for root, dirs, files in os.walk(mindspace_path):
            dirs[:] = [
                directory
                for directory in dirs
                if directory not in self._IGNORED_DIRS and not directory.startswith(".pytest_cache")
            ]

            for filename in files:
                if len(matches) >= self._MAX_MATCHES:
                    return matches

                path = os.path.join(root, filename)
                relative_path = os.path.relpath(path, mindspace_path)
                view_type = self._classify_view_type(mindspace_path, path)
                file_matches = 0

                if lowered_query in relative_path.casefold():
                    matches.append(MindspaceSearchMatch(
                        view_type=view_type,
                        path=path,
                        relative_path=relative_path,
                        is_path_match=True,
                    ))
                    file_matches += 1

                if file_matches >= self._MAX_MATCHES_PER_FILE or self._is_binary_file(path):
                    continue

                for line_number, line in self._iter_matching_lines(path, lowered_query):
                    matches.append(MindspaceSearchMatch(
                        view_type=view_type,
                        path=path,
                        relative_path=relative_path,
                        line_number=line_number,
                        line_text=line,
                    ))
                    file_matches += 1
                    if file_matches >= self._MAX_MATCHES_PER_FILE or len(matches) >= self._MAX_MATCHES:
                        break

        return matches

    def _classify_view_type(self, mindspace_path: str, path: str) -> MindspaceViewType:
        conversations_path = os.path.join(mindspace_path, "conversations")
        normalized_path = os.path.normpath(path)
        normalized_conversations = os.path.normpath(conversations_path)
        if normalized_path.startswith(normalized_conversations + os.sep) or normalized_path == normalized_conversations:
            return MindspaceViewType.CONVERSATIONS

        return MindspaceViewType.FILES

    def _is_binary_file(self, path: str) -> bool:
        try:
            with open(path, "rb") as file:
                sample = file.read(self._BINARY_SAMPLE_SIZE)
        except OSError:
            return True

        return b"\x00" in sample

    def _iter_matching_lines(self, path: str, lowered_query: str) -> list[tuple[int, str]]:
        results: list[tuple[int, str]] = []
        try:
            with open(path, encoding="utf-8", errors="ignore") as file:
                for line_number, line in enumerate(file, start=1):
                    if lowered_query not in line.casefold():
                        continue

                    snippet = " ".join(line.strip().split())
                    results.append((line_number, snippet))
                    if len(results) >= self._MAX_MATCHES_PER_FILE:
                        break
        except OSError:
            return []

        return results
