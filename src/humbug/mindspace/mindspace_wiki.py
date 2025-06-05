"""Mindspace wiki functionality."""

from enum import Enum, auto
import logging
import os
import stat
from datetime import datetime
from typing import List, Tuple, cast

from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.mindspace.mindspace_wiki_error import MindspaceWikiIOError


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
            MindspaceWikiIOError: If the path cannot be read or does not exist
        """
        try:
            # Get file info
            if not os.path.exists(path):
                raise MindspaceWikiIOError(f"Path does not exist: {path}")

            if os.path.isdir(path):
                # Generate directory listing
                content = self._generate_directory_content(path)

            else:
                # Source code or other file
                content = self._generate_file_content(path)

            return content

        except Exception as e:
            raise MindspaceWikiIOError(f"Failed to read wiki content: {str(e)}") from e

    def _get_file_size_bytes(self, file_path: str) -> int:
        """
        Get file size in bytes.

        Args:
            file_path: Path to the file or directory

        Returns:
            File size in bytes

        Raises:
            MindspaceWikiIOError: If file size cannot be accessed
        """
        try:
            file_stat = os.stat(file_path)
            return file_stat.st_size

        except OSError as e:
            self._logger.warning("Failed to get size for %s: %s", file_path, str(e))
            raise MindspaceWikiIOError(f"Failed to get size for {file_path}: {str(e)}") from e

    def _get_file_permissions(self, file_path: str) -> str:
        """
        Get file permissions in Unix-style format.

        Args:
            file_path: Path to the file or directory

        Returns:
            Formatted permissions string (e.g., 'rwxr-xr-x')

        Raises:
            MindspaceWikiIOError: If file permissions cannot be accessed
        """
        try:
            file_stat = os.stat(file_path)
            return self._format_permissions(file_stat.st_mode)

        except OSError as e:
            self._logger.warning("Failed to get permissions for %s: %s", file_path, str(e))
            raise MindspaceWikiIOError(f"Failed to get permissions for {file_path}: {str(e)}") from e

    def _get_file_modification_time(self, file_path: str) -> str:
        """
        Get formatted file modification time.

        Args:
            file_path: Path to the file or directory

        Returns:
            Formatted modification time string

        Raises:
            MindspaceWikiIOError: If file modification time cannot be accessed
        """
        try:
            file_stat = os.stat(file_path)
            mod_time = datetime.fromtimestamp(file_stat.st_mtime)
            return mod_time.strftime("%Y-%m-%d %H:%M:%S")

        except OSError as e:
            self._logger.warning("Failed to get modification time for %s: %s", file_path, str(e))
            raise MindspaceWikiIOError(f"Failed to get modification time for {file_path}: {str(e)}") from e

    def _format_permissions(self, mode: int) -> str:
        """
        Format file permissions in Unix-style format.

        Args:
            mode: File mode from os.stat()

        Returns:
            Formatted permissions string (e.g., 'rwxr-xr-x')
        """
        permissions = ""

        # Owner permissions
        permissions += 'r' if mode & stat.S_IRUSR else '-'
        permissions += 'w' if mode & stat.S_IWUSR else '-'
        permissions += 'x' if mode & stat.S_IXUSR else '-'

        # Group permissions
        permissions += 'r' if mode & stat.S_IRGRP else '-'
        permissions += 'w' if mode & stat.S_IWGRP else '-'
        permissions += 'x' if mode & stat.S_IXGRP else '-'

        # Other permissions
        permissions += 'r' if mode & stat.S_IROTH else '-'
        permissions += 'w' if mode & stat.S_IWOTH else '-'
        permissions += 'x' if mode & stat.S_IXOTH else '-'

        return permissions

    def _calculate_max_size_width(self, directory_path: str, entries: List[str]) -> int:
        """
        Calculate the maximum width needed for displaying file sizes in bytes.

        Args:
            directory_path: Path to the directory
            entries: List of entries in the directory

        Returns:
            Maximum width needed for size formatting
        """
        max_width = 0

        # Include ".." entry
        try:
            parent_path = os.path.dirname(directory_path)
            size = self._get_file_size_bytes(parent_path)
            max_width = max(max_width, len(str(size)))
        except MindspaceWikiIOError:
            pass

        # Check all entries in the directory
        for entry in entries:
            full_path = os.path.join(directory_path, entry)
            try:
                size = self._get_file_size_bytes(full_path)
                max_width = max(max_width, len(str(size)))
            except MindspaceWikiIOError:
                pass

        return max_width

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

            if rel_path in (".", ""):
                dir_name = f"Mindspace home: {os.path.basename(self._mindspace_manager.mindspace_path())}"

            contents: List[Tuple[MindspaceWikiContentType, str]] = []

            # Start with a heading
            lines = [
                f"# {dir_name}",
                ""
            ]

            # Add file metadata on separate lines
            try:
                size = self._get_file_size_bytes(directory_path)
                permissions = self._get_file_permissions(directory_path)
                mod_time = self._get_file_modification_time(directory_path)

                lines.extend([
                    f"**Permissions**: {permissions}  ",
                    f"**Size**: {size}  ",
                    f"**Modified**: {mod_time}  ",
                    f"**Path**: {rel_path}"
                ])

            except MindspaceWikiIOError as e:
                self._logger.warning("Could not retrieve metadata for %s: %s", directory_path, str(e))

            contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(lines)))

            # Sort entries - directories first, then files
            files = [".."]

            for entry in entries:
                files.append(entry)

            files.sort()

            if files:
                md_lines = [
                    "  ",
                    "## Files and folders (directories)"
                ]
                contents.append((MindspaceWikiContentType.MARKDOWN, "\n".join(md_lines)))

                # Calculate maximum size width for proper alignment
                max_size_width = self._calculate_max_size_width(directory_path, entries)

                lines = []

                for i in range(len(files) - 1):
                    f = files[i]
                    full_path = os.path.abspath(os.path.join(directory_path, f))
                    suffix = os.path.sep if os.path.isdir(full_path) else ""

                    # Get metadata for the file/directory
                    try:
                        size = self._get_file_size_bytes(full_path)
                        permissions = self._get_file_permissions(full_path)
                        mod_time = self._get_file_modification_time(full_path)

                        # Format with proper spacing alignment
                        size_str = str(size).rjust(max_size_width)
                        lines.append(f"`{permissions}  {size_str}  {mod_time}  `[`{f}{suffix}`]({full_path})  ")

                    except MindspaceWikiIOError:
                        # Fallback without metadata if we can't get it
                        lines.append(f"[`{f}{suffix}`]({full_path})  ")

                f = files[-1]
                full_path = os.path.abspath(os.path.join(directory_path, f))
                suffix = os.path.sep if os.path.isdir(full_path) else ""

                # Get metadata for the last file/directory
                try:
                    size = self._get_file_size_bytes(full_path)
                    permissions = self._get_file_permissions(full_path)
                    mod_time = self._get_file_modification_time(full_path)

                    # Format with proper spacing alignment
                    size_str = str(size).rjust(max_size_width)
                    lines.append(f"`{permissions}  {size_str}  {mod_time}  `[`{f}{suffix}`]({full_path})")

                except MindspaceWikiIOError:
                    # Fallback without metadata if we can't get it
                    lines.append(f"[`{f}{suffix}`]({full_path})")

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

            # Generate markdown with metadata
            lines = [
                f"# {file_name}",
                ""
            ]

            # Add file metadata on separate lines
            try:
                size = self._get_file_size_bytes(file_path)
                permissions = self._get_file_permissions(file_path)
                mod_time = self._get_file_modification_time(file_path)

                lines.extend([
                    f"**Permissions**: {permissions}  ",
                    f"**Size**: {size}  ",
                    f"**Modified**: {mod_time}  ",
                    f"**Path**: {rel_path}"
                ])

            except MindspaceWikiIOError as e:
                self._logger.warning("Could not retrieve metadata for %s: %s", file_path, str(e))

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
            MindspaceWikiIOError: If the target path does not exist
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
            raise MindspaceWikiIOError(f"Path does not exist: {base_path}")

        # Return resolved path with anchor if present
        if anchor:
            return f"{base_path}#{anchor}"

        return base_path
