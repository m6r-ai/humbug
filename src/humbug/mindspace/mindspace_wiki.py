"""Mindspace wiki functionality."""

import logging
import os
from datetime import datetime
from typing import Tuple, Optional

from humbug.gui.tab.wiki.wiki_error import WikiIOError
from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.syntax.programming_language import ProgrammingLanguage
from humbug.syntax.programming_language_utils import ProgrammingLanguageUtils


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

    def get_wiki_content(self, path: str) -> Tuple[str, datetime]:
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

            timestamp = datetime.fromtimestamp(os.path.getmtime(path))

            if os.path.isdir(path):
                # Generate directory listing
                content = self._generate_directory_content(path)

            else:
                # Source code or other file
                content = self._generate_file_content(path)

            return content, timestamp

        except Exception as e:
            raise WikiIOError(f"Failed to read wiki content: {str(e)}") from e

    def _generate_directory_content(self, directory_path: str) -> str:
        """
        Generate wiki content for a directory.

        Args:
            directory_path: Path to the directory

        Returns:
            Markdown content with directory listing
        """
        try:
            rel_path = self._mindspace_manager.make_relative_path(directory_path)
            dir_name = os.path.basename(rel_path)
            entries = os.listdir(directory_path)

            # Start with a heading
            lines = [
                f"# {dir_name}"
                "",
                f"Path: `{rel_path}`"
            ]

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
                lines.append("## Folders (directories)")
                lines.append("")
                for d in dirs:
                    full_path = os.path.join(directory_path, d)
                    lines.append(f"- [{d}]({full_path})")
                lines.append("")

            # Add files
            if files:
                lines.append("## Files")
                lines.append("")
                for f in files:
                    full_path = os.path.join(directory_path, f)
                    lines.append(f"- [{f}]({full_path})")

            return "\n".join(lines)

        except Exception as e:
            self._logger.error("Error generating directory content: %s", str(e))
            return f"# Error\n\nFailed to generate directory content: {str(e)}"

    def _generate_file_content(self, file_path: str) -> str:
        """
        Generate wiki content for a file.

        Args:
            file_path: Path to the file

        Returns:
            Markdown content with file contents
        """
        try:
            rel_path = self._mindspace_manager.make_relative_path(file_path)
            file_name = os.path.basename(file_path)

            # Determine language for syntax highlighting
            language = ProgrammingLanguageUtils.from_file_extension(file_path)
            lang_name = ProgrammingLanguageUtils.get_display_name(language).lower()

            # If language is "None" (meaning TEXT), don't specify a language
            lang_spec = lang_name
            if language == ProgrammingLanguage.TEXT:
                lang_spec = ""

            # Read file contents
            with open(file_path, 'r', encoding='utf-8') as f:
                file_content = f.read()

            # Generate markdown
            lines = [
                f"# {file_name}",
                "",
                f"Path: `{rel_path}`",
                ""
            ]

            if file_path.lower().endswith(".md"):
                # Regular markdown file
                with open(file_path, 'r', encoding='utf-8') as f:
                    md_content = f.read()

                md_lines = [
                    "## Preview",
                    "",
                    "---",
                    md_content,
                    "---"
                ]

                lines += md_lines


            source_lines = [
                "## Source",
                "```" + lang_spec,
                file_content,
                "```"
            ]

            lines += source_lines

            return "\n".join(lines)

        except Exception as e:
            self._logger.error("Error generating file content: %s", str(e))
            return f"# Error\n\nFailed to generate content for {file_path}: {str(e)}"

    def resolve_link(self, current_path: str, target_path: str) -> Optional[str]:
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

        # Handle file:// links
        if target_path.startswith("file://"):
            target_path = target_path[7:]

        # Handle external links
        if target_path.startswith(("http://", "https://")):
            return None

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
