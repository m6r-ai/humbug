"""Mindspace wiki functionality."""

from enum import Enum, auto
import logging
import os
from typing import List, Tuple, cast

from humbug.gui.tab.wiki.wiki_error import WikiIOError
from humbug.mindspace.mindspace_manager import MindspaceManager


class MindspaceWikiContentType(Enum):
    """Enum for wiki content types."""
    MARKDOWN = auto()           # Standard markdown text
    MARKDOWN_PREVIEW = auto()   # Markdown preview
    FILE = auto()               # File content (e.g., source code)


class MindspaceWiki:
    """
    Manager for virtual wiki pages in a mindspace.

    This class handles the creation of virtual wiki pages based on
    different file types or directories.
    """

    def __init__(self) -> None:
        """Initialize the mindspace wiki manager."""
        self._logger = logging.getLogger("MindspaceWiki")
        self._mindspace_manager = MindspaceManager()

    def get_wiki_content(self, path: str) -> List[Tuple[MindspaceWikiContentType, str]]:
        """
        Get wiki content for a path, generating it dynamically based on file type.

        Args:
            path: Path to the wiki file or virtual wiki path

        Returns:
            Tuple of (content, timestamp)

        Raises:
            WikiIOError: If the path cannot be read or does not exist
        """
        try:
            # Get file info
            if not os.path.exists(path):
                raise WikiIOError(f"Path does not exist: {path}")

            if os.path.isdir(path):
                # Generate directory listing
                content = self._generate_directory_content(path)

            else:
                # Source code or other file
                content = self._generate_file_content(path)

            return content

        except Exception as e:
            raise WikiIOError(f"Failed to read wiki content: {str(e)}") from e

    def _generate_directory_content(self, directory_path: str) -> List[Tuple[MindspaceWikiContentType, str]]:
        """
        Generate wiki content for a directory.

        Args:
            directory_path: Path to the directory

        Returns:
            Markdown content with directory listing
        """
        try:
            rel_path = self._mindspace_manager.get_relative_path(directory_path)
            dir_name = os.path.basename(cast(str, directory_path))
            entries = os.listdir(directory_path)
            print(f"Generating directory content for: {directory_path} (relative path: {rel_path})")

            if rel_path == "." or rel_path == "":
                dir_name = f"Project home: {os.path.basename(self._mindspace_manager.mindspace_path())}"

            contents: List[Tuple[MindspaceWikiContentType, str]] = []

            # Start with a heading
            lines = [
                f"# {dir_name}"
                "",
                f"Path: `{rel_path}`"
            ]
            contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(lines)))

            # Sort entries - directories first, then files
            dirs = []
            files = []

            for entry in entries:
                full_path = os.path.join(directory_path, entry)
                if os.path.isdir(full_path):
                    dirs.append(entry)

                else:
                    files.append(entry)

            dirs.sort()
            files.sort()

            # Add directories
            if dirs:
                md_lines = [
                    "  ",
                    "## Folders (directories)"
                ]
                contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(md_lines)))

                lines = []
                lines.append(f"`{os.path.basename(directory_path)}`  ")
                lines.append("`  ╷`  ")

                for i in range(len(dirs) - 1):
                    d = dirs[i]
                    full_path = os.path.join(directory_path, d)
                    lines.append(f"`  ├── ` [`{d}`]({full_path})  ")

                d = dirs[-1]
                full_path = os.path.join(directory_path, d)
                lines.append(f"`  └── ` [`{d}`]({full_path})")

                contents.append((MindspaceWikiContentType.MARKDOWN_PREVIEW, "\n".join(lines)))

            # Add files
            if files:
                md_lines = [
                    "  ",
                    "## Files"
                ]
                contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(md_lines)))

                lines = []
                lines.append(f"`{os.path.basename(directory_path)}`  ")
                lines.append("`  ╷`  ")

                for i in range(len(files) - 1):
                    f = files[i]
                    full_path = os.path.join(directory_path, f)
                    lines.append(f"`  ├── ` [`{f}`]({full_path})  ")

                f = files[-1]
                full_path = os.path.join(directory_path, f)
                lines.append(f"`  └── ` [`{f}`]({full_path})")

                contents.append((MindspaceWikiContentType.MARKDOWN_PREVIEW, "\n".join(lines)))

            return contents

        except Exception as e:
            self._logger.error("Error generating directory content: %s", str(e))
            return [(MindspaceWikiContentType.MARKDOWN, f"# Error\n\nFailed to generate directory content: {str(e)}")]

    def get_file_type(self, file_path: str) -> str:
        """
        Determine the type of a file based on its extension.

        Args:
            file_path: Path to the file

        Returns:
            Type of the file as a string ('image', 'markdown', or 'other')
        """
        ext = os.path.splitext(file_path.lower())[1]

        image_extensions = {'.png', '.jpg', '.jpeg', '.gif', '.bmp', '.tiff', '.tif', '.webp', '.svg', '.ico'}
        if ext in image_extensions:
            return 'image'

        markdown_extensions = {'.md', '.markdown'}
        if ext in markdown_extensions:
            return 'markdown'

        return 'other'

    def _generate_file_content(self, file_path: str) -> List[Tuple[MindspaceWikiContentType, str]]:
        """
        Generate wiki content for a file.

        Args:
            file_path: Path to the file

        Returns:
            Markdown content with file contents
        """
        try:
            rel_path = self._mindspace_manager.get_relative_path(file_path)
            file_name = os.path.basename(file_path)

            contents: List[Tuple[MindspaceWikiContentType, str]] = []

            # Generate markdown
            lines = [
                f"# {file_name}",
                "",
                f"Path: `{rel_path}`"
            ]
            contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(lines)))

            file_type = self.get_file_type(file_path)
            print(f"Generating file content for: {file_path} (type: {file_type})")
            if file_type == 'image':
                md_lines = [
                    "  ",
                    "## Preview"
                ]
                contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(md_lines)))
                md_lines2 = [
                    f"![{file_name}]({file_path})"
                ]
                contents.append((MindspaceWikiContentType.MARKDOWN_PREVIEW, "\n".join(md_lines2)))
                return contents

            with open(file_path, 'r', encoding='utf-8') as f:
                file_content = f.read()

            if file_type == 'markdown':
                md_lines = [
                    "  ",
                    "## Preview"
                ]
                contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(md_lines)))
                md_lines2 = [
                    file_content
                ]
                contents.append((MindspaceWikiContentType.MARKDOWN_PREVIEW, "\n".join(md_lines2)))

            source_lines = [
                "  ",
                "## Source"
            ]
            contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(source_lines)))
            source_lines2 = [
                file_content
            ]
            contents.append((MindspaceWikiContentType.FILE, "\n".join(source_lines2)))

            return contents

        except Exception as e:
            self._logger.error("Error generating file content: %s", str(e))
            return [(MindspaceWikiContentType.MARKDOWN, f"# Error\n\nFailed to generate content for {file_path}: {str(e)}")]

    def resolve_link(self, current_path: str, target_path: str) -> str | None:
        """
        Resolve a link path to an absolute path.

        Args:
            current_path: Path of the current wiki page
            target_path: Target path from the link

        Returns:
            Absolute path to the target or None if it's an external link

        Raises:
            WikiIOError: If the target path does not exist
        """
        # Handle anchors separately
        if target_path.startswith("#"):
            return None

        # Handle external links
        if target_path.startswith(("http://", "https://")):
            return None

        # Handle file:// links
        if target_path.startswith("file://"):
            target_path = target_path[7:]

        # Extract anchor if present
        base_path = target_path
        anchor = None
        if '#' in target_path:
            base_path, anchor = target_path.split('#', 1)

        # Resolve relative paths
        if not os.path.isabs(base_path) and current_path:
            current_dir = os.path.dirname(current_path)
            base_path = os.path.normpath(os.path.join(current_dir, base_path))

        # Check if path exists
        if not os.path.exists(base_path):
            raise WikiIOError(f"Path does not exist: {base_path}")

        # Return resolved path with anchor if present
        if anchor:
            return f"{base_path}#{anchor}"

        return base_path
