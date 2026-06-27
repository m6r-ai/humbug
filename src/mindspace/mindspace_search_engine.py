import json
import os
import re
from dataclasses import dataclass

from mindspace.mindspace_content_type import MindspaceContentType


@dataclass(slots=True)
class MindspaceSearchMatch:
    """A single global-search match within the current mindspace."""

    content_type: MindspaceContentType
    path: str
    relative_path: str
    line_number: int | None = None
    line_text: str = ""
    is_path_match: bool = False
    message_id: str | None = None


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

    MAX_MATCHES = 500
    _MAX_MATCHES_PER_FILE = 20
    _MAX_CONVERSATION_MATCHES_PER_FILE = 50
    _BINARY_SAMPLE_SIZE = 2048

    def search(
        self,
        mindspace_path: str,
        query: str,
        case_sensitive: bool = False,
        whole_word: bool = False,
        regexp: bool = False,
        include_hidden: bool = False,
    ) -> list[MindspaceSearchMatch]:
        """Search the current mindspace for the given query."""
        normalized_query = query.strip()
        if not mindspace_path or not normalized_query:
            return []

        flags = 0 if case_sensitive else re.IGNORECASE
        if regexp:
            pattern_text = normalized_query

        elif whole_word:
            pattern_text = rf"\b{re.escape(normalized_query)}\b"

        else:
            pattern_text = ""

        pattern = re.compile(pattern_text, flags) if regexp or whole_word else None
        lowered_query = normalized_query if case_sensitive else normalized_query.casefold()
        matches: list[MindspaceSearchMatch] = []

        for root, dirs, files in os.walk(mindspace_path):
            dirs[:] = [
                directory
                for directory in dirs
                if directory not in self._IGNORED_DIRS
                and not directory.startswith(".pytest_cache")
                and (include_hidden or not directory.startswith("."))
            ]

            for filename in files:
                if len(matches) >= self.MAX_MATCHES:
                    return matches

                path = os.path.join(root, filename)
                relative_path = os.path.relpath(path, mindspace_path)
                content_type = self._classify_content_type(mindspace_path, path)
                file_matches = 0

                if self._matches_text(relative_path, lowered_query, pattern, case_sensitive):
                    matches.append(MindspaceSearchMatch(
                        content_type=content_type,
                        path=path,
                        relative_path=relative_path,
                        is_path_match=True,
                    ))
                    file_matches += 1

                if file_matches >= self._MAX_MATCHES_PER_FILE:
                    continue

                if content_type == MindspaceContentType.CONVERSATIONS:
                    content_matches = self._iter_matching_conversation_messages(
                        path, lowered_query, pattern, case_sensitive
                    )

                elif self._is_binary_file(path):
                    continue

                else:
                    content_matches = [
                        (line_number, line, None)
                        for line_number, line in self._iter_matching_lines(path, lowered_query, pattern, case_sensitive)
                    ]

                if content_type == MindspaceContentType.CONVERSATIONS:
                    per_file_cap = self._MAX_CONVERSATION_MATCHES_PER_FILE

                else:
                    per_file_cap = self._MAX_MATCHES_PER_FILE

                for line_number, line, message_id in content_matches:
                    matches.append(MindspaceSearchMatch(
                        content_type=content_type,
                        path=path,
                        relative_path=relative_path,
                        line_number=line_number,
                        line_text=line,
                        message_id=message_id,
                    ))
                    file_matches += 1
                    if file_matches >= per_file_cap or len(matches) >= self.MAX_MATCHES:
                        break

        return matches

    def _classify_content_type(self, mindspace_path: str, path: str) -> MindspaceContentType:
        """Classify a file as conversation content or general file content."""
        conversations_path = os.path.join(mindspace_path, "conversations")
        normalized_path = os.path.normpath(path)
        normalized_conversations = os.path.normpath(conversations_path)
        if normalized_path.startswith(normalized_conversations + os.sep) or normalized_path == normalized_conversations:
            return MindspaceContentType.CONVERSATIONS

        return MindspaceContentType.FILES

    def _is_binary_file(self, path: str) -> bool:
        """Return True if the file appears to be binary."""
        try:
            with open(path, "rb") as file:
                sample = file.read(self._BINARY_SAMPLE_SIZE)

        except OSError:
            return True

        return b"\x00" in sample

    def _iter_matching_lines(
        self,
        path: str,
        lowered_query: str,
        pattern: re.Pattern[str] | None,
        case_sensitive: bool,
    ) -> list[tuple[int, str]]:
        """Return (line_number, snippet) pairs for lines matching the query."""
        results: list[tuple[int, str]] = []
        try:
            with open(path, encoding="utf-8", errors="ignore") as file:
                for line_number, line in enumerate(file, start=1):
                    if not self._matches_text(line, lowered_query, pattern, case_sensitive):
                        continue

                    snippet = " ".join(line.strip().split())
                    results.append((line_number, snippet))
                    if len(results) >= self._MAX_MATCHES_PER_FILE:
                        break

        except OSError:
            return []

        return results

    def _iter_matching_conversation_messages(
        self,
        path: str,
        lowered_query: str,
        pattern: re.Pattern[str] | None,
        case_sensitive: bool,
    ) -> list[tuple[int | None, str, str | None]]:
        """
        Search a conversation JSON file and return matching message snippets.

        Returns:
            List of (line_number, snippet, message_id) tuples where line_number is
            always None (conversations navigate by message ID, not line number).
        """
        results: list[tuple[int | None, str, str | None]] = []
        try:
            with open(path, encoding="utf-8", errors="ignore") as f:
                data = json.load(f)

        except (OSError, json.JSONDecodeError):
            return results

        messages = data.get("conversation", [])
        if not isinstance(messages, list):
            return results

        for message in messages:
            if not isinstance(message, dict):
                continue

            message_id = message.get("id")
            content = message.get("content", "")
            if not isinstance(content, str) or not content:
                continue

            if not self._matches_text(content, lowered_query, pattern, case_sensitive):
                continue

            snippet = self._extract_match_snippet(content, lowered_query, pattern, case_sensitive)
            results.append((None, snippet, message_id))
            if len(results) >= self._MAX_CONVERSATION_MATCHES_PER_FILE:
                break

        return results

    def _extract_match_snippet(
        self,
        content: str,
        lowered_query: str,
        pattern: re.Pattern[str] | None,
        case_sensitive: bool,
        context_before: int = 30,
        context_after: int = 80,
    ) -> str:
        """Extract a short context snippet around the first match in content."""
        if pattern is not None:
            m = pattern.search(content)
            if m:
                start = self._word_boundary_start(content, max(0, m.start() - context_before))
                end = min(len(content), m.end() + context_after)
                snippet = " ".join(content[start:end].strip().split())
                return ("..." + snippet) if start > 0 else snippet

            return " ".join(content[:context_before + context_after].strip().split())

        haystack = content if case_sensitive else content.casefold()
        pos = haystack.find(lowered_query)
        if pos != -1:
            start = self._word_boundary_start(content, max(0, pos - context_before))
            end = min(len(content), pos + len(lowered_query) + context_after)
            snippet = " ".join(content[start:end].strip().split())
            return ("..." + snippet) if start > 0 else snippet

        return " ".join(content[:context_before + context_after].strip().split())

    def _word_boundary_start(self, content: str, pos: int) -> int:
        """Advance pos forward past any partial word so the snippet starts cleanly."""
        if pos == 0:
            return 0

        while pos < len(content) and content[pos] not in (' ', '\n', '\t'):
            pos += 1

        return pos

    def _matches_text(
        self,
        text: str,
        query: str,
        pattern: re.Pattern[str] | None,
        case_sensitive: bool,
    ) -> bool:
        """Return True if text matches the query."""
        if pattern is not None:
            return pattern.search(text) is not None

        haystack = text if case_sensitive else text.casefold()
        return query in haystack
