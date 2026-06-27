import os
import re
from typing import Any, Callable, Dict, List, Tuple



class PreviewContext:
    """
    Model-layer context for a preview tab.

    Owns the raw content strings from PreviewContent and implements search
    against them directly, without any Qt dependency.

    The visualisation side-effect (scrolling the viewport to a position) is
    emitted as a callback so the Qt PreviewWidget can react without the context
    needing to know about widgets.
    """

    context_type = "preview"

    def __init__(
        self,
        context_id: str,
        path: str,
        content_blocks: List[Tuple[Any, str]],
        on_scroll_to_position: Callable | None = None,
    ) -> None:
        """
        Initialise the preview context.

        Args:
            context_id: Stable identifier issued by the ContextRegistry.
            path: Absolute path to the file or directory being previewed.
            content_blocks: List of (PreviewContentType, str) tuples from
                PreviewContent.get_preview_content().
            on_scroll_to_position: Optional callable(block_index, section_index,
                text_position, viewport_position) invoked when the AI requests a
                scroll.  The Qt widget supplies this; a CLI would leave it None.
        """
        self._context_id = context_id
        self._path = path
        self._content_blocks = content_blocks
        self._on_scroll_to_position = on_scroll_to_position

    def context_id(self) -> str:
        """Return the stable context identifier."""
        return self._context_id

    def get_info(self) -> Dict[str, Any]:
        """
        Return high-level metadata about the preview content.

        Returns:
            Dictionary with path, content_type, block_count, and has_content.
        """
        if os.path.isdir(self._path):
            content_type = "directory"

        else:
            ext = os.path.splitext(self._path.lower())[1]
            image_extensions = {
                '.png', '.jpg', '.jpeg', '.gif', '.bmp',
                '.tiff', '.tif', '.webp', '.svg', '.ico',
            }
            markdown_extensions = {'.md', '.markdown'}
            if ext in image_extensions:
                content_type = "image"

            elif ext in markdown_extensions:
                content_type = "markdown"

            else:
                content_type = "file"

        return {
            "path": self._path,
            "content_type": content_type,
            "content_blocks": len(self._content_blocks),
            "has_content": bool(self._content_blocks),
        }

    def search(
        self,
        search_text: str,
        case_sensitive: bool = False,
        max_results: int = 50,
        regexp: bool = False,
    ) -> Dict[str, Any]:
        """
        Search for text across all raw content strings.

        Args:
            search_text:   Text or regular expression to search for.
            case_sensitive: Whether the search is case-sensitive.
            max_results:   Maximum number of matches to return.
            regexp:        If True, treat search_text as a regular expression.

        Returns:
            Dictionary with search_text, case_sensitive, total_matches,
            returned_matches, and a matches list.

        Raises:
            ValueError: If regexp is True and search_text is not a valid regex.
        """
        if not search_text:
            return {
                "search_text": search_text,
                "case_sensitive": case_sensitive,
                "total_matches": 0,
                "returned_matches": 0,
                "matches": [],
            }

        flags = 0 if case_sensitive else re.IGNORECASE

        if regexp:
            try:
                pattern = re.compile(search_text, flags)

            except re.error as e:
                raise ValueError(f"Invalid regular expression: {e}") from e

        else:
            pattern = None

        matches: List[Dict[str, Any]] = []

        for block_idx, (_content_type, raw_text) in enumerate(self._content_blocks):
            if pattern is not None:
                for m in pattern.finditer(raw_text):
                    pos = m.start()
                    context_start = max(0, pos - 50)
                    context_end = min(len(raw_text), m.end() + 50)
                    matches.append({
                        "block_index": block_idx,
                        "section_index": 0,
                        "start_position": pos,
                        "end_position": m.end(),
                        "context_before": raw_text[context_start:pos],
                        "match_text": m.group(),
                        "context_after": raw_text[m.end():context_end],
                    })
                    if len(matches) >= max_results:
                        break

            else:
                search_str = search_text if case_sensitive else search_text.lower()
                haystack = raw_text if case_sensitive else raw_text.lower()
                pos = 0
                while True:
                    pos = haystack.find(search_str, pos)
                    if pos == -1:
                        break

                    context_start = max(0, pos - 50)
                    context_end = min(len(raw_text), pos + len(search_text) + 50)
                    matches.append({
                        "block_index": block_idx,
                        "section_index": 0,
                        "start_position": pos,
                        "end_position": pos + len(search_text),
                        "context_before": raw_text[context_start:pos],
                        "match_text": raw_text[pos:pos + len(search_text)],
                        "context_after": raw_text[pos + len(search_text):context_end],
                    })
                    if len(matches) >= max_results:
                        break

                    pos += 1

            if len(matches) >= max_results:
                break

        return {
            "search_text": search_text,
            "case_sensitive": case_sensitive,
            "total_matches": len(matches),
            "returned_matches": len(matches),
            "matches": matches,
        }

    def scroll_to(
        self,
        block_index: int,
        section_index: int = 0,
        text_position: int = 0,
        viewport_position: str = "center",
    ) -> bool:
        """
        Request that the frontend scroll to a specific content position.

        Validates the block_index against the known content, then fires the
        on_scroll_to_position callback if one was supplied.  A CLI frontend
        would supply None and this becomes a no-op beyond the validation.

        Args:
            block_index:       0-based index of the content block.
            section_index:     0-based section within the block.
            text_position:     Text position within the section.
            viewport_position: Where in the viewport to place the target
                               ('top', 'center', or 'bottom').

        Returns:
            True if the scroll was requested successfully, False if the
            block_index is out of range.
        """
        if block_index < 0 or block_index >= len(self._content_blocks):
            return False

        if self._on_scroll_to_position is not None:
            return bool(
                self._on_scroll_to_position(
                    block_index, section_index, text_position, viewport_position
                )
            )

        return True
