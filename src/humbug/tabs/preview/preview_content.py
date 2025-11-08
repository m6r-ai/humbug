"""Preview functionality with dependency tracking."""

from enum import Enum, auto
import logging
import os
import stat
from datetime import datetime
from typing import List, Tuple, Set, cast

from humbug.mindspace.mindspace_manager import MindspaceManager
from humbug.tabs.preview.preview_error import PreviewIOError


class PreviewContentType(Enum):
    """Enum for preview content types."""
    MARKDOWN = auto()           # Standard markdown text
    MARKDOWN_PREVIEW = auto()   # Markdown preview
    FILE = auto()               # File content (e.g., source code)


class PreviewContent:
    """
    Manager for virtual preview pages in a mindspace.

    This class handles the creation of virtual preview pages based on
    different file types or directories.
    """

    def __init__(self) -> None:
        """Initialize the preview manager."""
        self._logger = logging.getLogger("PreviewContent")
        self._mindspace_manager = MindspaceManager()

    def get_preview_content(self, path: str) -> Tuple[List[Tuple[PreviewContentType, str]], Set[str]]:
        """
        Get preview content for a path, generating it dynamically based on file type.

        Args:
            path: Path to the preview file or virtual preview path

        Returns:
            Tuple of (content_list, dependency_paths)

        Raises:
            PreviewIOError: If the path cannot be read or does not exist
        """
        # Track dependencies - always include the main path
        dependencies: Set[str] = {os.path.abspath(path)}

        # Get file info
        if not os.path.exists(path):
            self._logger.error("Path does not exist: %s", path)
            raise PreviewIOError(f"Path does not exist: {path}")

        if os.path.isdir(path):
            # Generate directory listing
            content, dir_dependencies = self._generate_directory_content(path)
            dependencies.update(dir_dependencies)

        else:
            # Source code or other file
            content, file_dependencies = self._generate_file_content(path)
            dependencies.update(file_dependencies)

        return content, dependencies

    def _check_for_readme(self, directory_path: str) -> str | None:
        """
        Check for README.md in directory, return path if found.

        Args:
            directory_path: Path to the directory to check

        Returns:
            Path to README.md if found, None otherwise
        """
        readme_path = os.path.join(directory_path, "README.md")
        return readme_path if os.path.exists(readme_path) else None

    def _check_for_companion_md(self, file_path: str) -> str | None:
        """
        Check for companion .md file (e.g., foo.txt.md for foo.txt).

        Args:
            file_path: Path to the original file

        Returns:
            Path to companion .md file if found, None otherwise
        """
        companion_path = file_path + ".md"
        return companion_path if os.path.exists(companion_path) else None

    def _read_markdown_file(self, md_path: str) -> List[Tuple[PreviewContentType, str]]:
        """
        Read and prepare markdown file content for rendering.

        Args:
            md_path: Path to the markdown file to read

        Returns:
            List of content tuples ready for rendering
        """
        try:
            with open(md_path, 'r', encoding='utf-8') as f:
                content = f.read()

            if content.strip():  # Only add content if file is not empty
                return [(PreviewContentType.MARKDOWN_PREVIEW, content)]

            return []

        except OSError as e:
            self._logger.warning("Failed to read markdown file %s: %s", md_path, str(e))
            return []

    def _get_file_size_bytes(self, file_path: str) -> int:
        """
        Get file size in bytes.

        Args:
            file_path: Path to the file or directory

        Returns:
            File size in bytes

        Raises:
            PreviewIOError: If file size cannot be accessed
        """
        try:
            file_stat = os.stat(file_path)
            return file_stat.st_size

        except OSError as e:
            self._logger.warning("Failed to get size for %s: %s", file_path, str(e))
            raise PreviewIOError(f"Failed to get size for {file_path}: {str(e)}") from e

    def _get_file_permissions(self, file_path: str) -> str:
        """
        Get file permissions in Unix-style format.

        Args:
            file_path: Path to the file or directory

        Returns:
            Formatted permissions string (e.g., 'rwxr-xr-x')

        Raises:
            PreviewIOError: If file permissions cannot be accessed
        """
        try:
            file_stat = os.stat(file_path)
            return self._format_permissions(file_stat.st_mode)

        except OSError as e:
            self._logger.warning("Failed to get permissions for %s: %s", file_path, str(e))
            raise PreviewIOError(f"Failed to get permissions for {file_path}: {str(e)}") from e

    def _get_file_modification_time(self, file_path: str) -> str:
        """
        Get formatted file modification time.

        Args:
            file_path: Path to the file or directory

        Returns:
            Formatted modification time string

        Raises:
            PreviewIOError: If file modification time cannot be accessed
        """
        try:
            file_stat = os.stat(file_path)
            mod_time = datetime.fromtimestamp(file_stat.st_mtime)
            return mod_time.strftime("%Y-%m-%d %H:%M:%S")

        except OSError as e:
            self._logger.warning("Failed to get modification time for %s: %s", file_path, str(e))
            raise PreviewIOError(f"Failed to get modification time for {file_path}: {str(e)}") from e

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

        except PreviewIOError:
            pass

        # Check all entries in the directory
        for entry in entries:
            full_path = os.path.join(directory_path, entry)
            try:
                size = self._get_file_size_bytes(full_path)
                max_width = max(max_width, len(str(size)))

            except PreviewIOError:
                pass

        return max_width

    def _generate_directory_content(self, directory_path: str) -> Tuple[List[Tuple[PreviewContentType, str]], Set[str]]:
        """
        Generate preview content for a directory.

        Args:
            directory_path: Path to the directory

        Returns:
            Tuple of (markdown content with directory listing, dependency paths)
        """
        try:
            # Track dependencies - include the directory itself and all its entries
            dependencies: Set[str] = {os.path.abspath(directory_path)}

            entries = os.listdir(directory_path)

            # Add all directory entries as dependencies (for change detection)
            for entry in entries:
                entry_path = os.path.join(directory_path, entry)
                dependencies.add(os.path.abspath(entry_path))

            contents: List[Tuple[PreviewContentType, str]] = []

            files = ["."]

            # If we're not in the root of the mindspace, add ".." entry
            rel_path = self._mindspace_manager.get_relative_path(directory_path)
            if rel_path not in (".", ""):
                files.append("..")

            # Sort entries - directories first, then files
            for entry in entries:
                files.append(entry)

            files.sort()

            if files:
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

                    except PreviewIOError:
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

                except PreviewIOError:
                    # Fallback without metadata if we can't get it
                    lines.append(f"[`{f}{suffix}`]({full_path})")

                contents.append((PreviewContentType.MARKDOWN_PREVIEW, "\n".join(lines)))

            # Check for README.md and render it if found
            readme_path = self._check_for_readme(directory_path)
            if readme_path:
                dependencies.add(os.path.abspath(readme_path))
                readme_content = self._read_markdown_file(readme_path)
                contents.extend(readme_content)

            return contents, dependencies

        except Exception as e:
            self._logger.error("Error generating directory content: %s", str(e))
            dependencies = {os.path.abspath(directory_path)}
            return [(PreviewContentType.MARKDOWN, f"# Error\n\nFailed to generate directory content: {str(e)}")], dependencies

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

    def _generate_file_content(self, file_path: str) -> Tuple[List[Tuple[PreviewContentType, str]], Set[str]]:
        """
        Generate preview content for a file.

        Args:
            file_path: Path to the file

        Returns:
            Tuple of (markdown content with file contents, dependency paths)
        """
        try:
            # Track dependencies - just the file itself for regular files
            dependencies: Set[str] = {os.path.abspath(file_path)}

            file_name = os.path.basename(file_path)

            contents: List[Tuple[PreviewContentType, str]] = []

            # Check for companion .md file and render it if found
            companion_md_path = self._check_for_companion_md(file_path)
            if companion_md_path:
                dependencies.add(os.path.abspath(companion_md_path))
                contents.append((PreviewContentType.MARKDOWN, f"  \n## Notes from {os.path.basename(companion_md_path)}"))
                companion_content = self._read_markdown_file(companion_md_path)
                contents.extend(companion_content)

            file_type = self.get_file_type(file_path)
            if file_type == 'image':
                md_lines2 = [
                    f"![{file_name}]({file_path})"
                ]
                contents.append((PreviewContentType.MARKDOWN_PREVIEW, "\n".join(md_lines2)))
                return contents, dependencies

            with open(file_path, 'r', encoding='utf-8') as f:
                file_content = f.read()

            if file_type == 'markdown':
                md_lines2 = [
                    file_content
                ]
                contents.append((PreviewContentType.MARKDOWN_PREVIEW, "\n".join(md_lines2)))
                return contents, dependencies

            source_lines2 = [
                file_content
            ]
            contents.append((PreviewContentType.FILE, "\n".join(source_lines2)))
            return contents, dependencies

        except Exception as e:
            self._logger.error("Error generating file content: %s", str(e))
            dependencies = {os.path.abspath(file_path)}
            return [(PreviewContentType.MARKDOWN,
                        f"# Error\n\nFailed to generate content for {file_path}: {str(e)}")], dependencies

    def resolve_link(self, current_path: str, target_path: str) -> str | None:
        """
        Resolve a link path to an absolute path.

        Args:
            current_path: Path of the current preview page
            target_path: Target path from the link

        Returns:
            Absolute path to the target or None if it's an external link

        Raises:
            PreviewIOError: If the target path does not exist
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
            raise PreviewIOError(f"Path does not exist: {base_path}")

        # Return resolved path with anchor if present
        if anchor:
            return f"{base_path}#{anchor}"

        return base_path
