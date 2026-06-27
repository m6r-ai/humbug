"""Command for displaying file contents from the system shell."""

import os

from docx import DocxError, DocxUnsupportedError, extract_text as extract_docx_text
from pdf import PDFError, PDFUnsupportedError, extract_text as extract_pdf_text, parse as parse_pdf
from syntax import Token, TokenType

from desktop.shell_tab.shell_command import ShellCommand
from desktop.shell_tab.shell_event_source import ShellEventSource

_MAX_DISPLAY_BYTES = 10 * 1024 * 1024  # 10 MB


class ShellCommandCat(ShellCommand):
    """Command to display the text content of a file."""

    def name(self) -> str:
        """Get the name of the command."""
        return "cat"

    def help_text(self) -> str:
        """Get the help text for the command."""
        return "Displays the text content of a file (PDF and DOCX supported)"

    def _execute_command(self, tokens: list[Token]) -> bool:
        """
        Execute the command with parsed tokens.

        Args:
            tokens: List of tokens from command lexer

        Returns:
            True if command executed successfully, False otherwise
        """
        args = self._get_positional_arguments(tokens)
        if not args:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                "No filename specified. Usage: cat <filename>"
            )
            return False

        rel_path = args[0]
        full_path = self._mindspace.get_absolute_path(rel_path)

        if not os.path.exists(full_path):
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"File not found: {rel_path}"
            )
            return False

        if not os.path.isfile(full_path):
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Not a file: {rel_path}"
            )
            return False

        file_size = os.path.getsize(full_path)
        if file_size > _MAX_DISPLAY_BYTES:
            size_mb = file_size / (1024 * 1024)
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"File too large to display: {size_mb:.1f} MB (max 10 MB)"
            )
            return False

        ext = os.path.splitext(full_path)[1].lower()

        if ext == ".pdf":
            return self._read_pdf(full_path)

        if ext == ".docx":
            return self._read_docx(full_path)

        return self._read_text(full_path)

    def _read_pdf(self, full_path: str) -> bool:
        """
        Extract and display text from a PDF file.

        Args:
            full_path: Absolute path to the PDF file

        Returns:
            True if extraction succeeded, False otherwise
        """
        try:
            with open(full_path, "rb") as f:
                data = f.read()

            doc = parse_pdf(data)
            content = extract_pdf_text(doc)

        except PDFUnsupportedError as e:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"PDF not supported: {e}"
            )
            return False

        except PDFError as e:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Failed to extract PDF text: {e}"
            )
            return False

        except OSError as e:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Failed to read file: {e}"
            )
            return False

        if not content.strip():
            self._history_manager.add_message(
                ShellEventSource.SUCCESS,
                "(PDF — no extractable text found)"
            )
            return True

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            content
        )
        return True

    def _read_docx(self, full_path: str) -> bool:
        """
        Extract and display text from a DOCX file.

        Args:
            full_path: Absolute path to the DOCX file

        Returns:
            True if extraction succeeded, False otherwise
        """
        try:
            with open(full_path, "rb") as f:
                data = f.read()

            content = extract_docx_text(data)

        except DocxUnsupportedError as e:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"DOCX not supported: {e}"
            )
            return False

        except DocxError as e:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Failed to extract DOCX text: {e}"
            )
            return False

        except OSError as e:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Failed to read file: {e}"
            )
            return False

        if not content.strip():
            self._history_manager.add_message(
                ShellEventSource.SUCCESS,
                "(DOCX — no extractable text found)"
            )
            return True

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            content
        )
        return True

    def _read_text(self, full_path: str) -> bool:
        """
        Read and display a plain text file.

        Args:
            full_path: Absolute path to the file

        Returns:
            True if read succeeded, False otherwise
        """
        try:
            with open(full_path, "r", encoding="utf-8") as f:
                content = f.read()

        except UnicodeDecodeError:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Cannot display file: {full_path} (not valid UTF-8 text)"
            )
            return False

        except OSError as e:
            self._history_manager.add_message(
                ShellEventSource.ERROR,
                f"Failed to read file: {e}"
            )
            return False

        self._history_manager.add_message(
            ShellEventSource.SUCCESS,
            content
        )
        return True

    def get_token_completions(
        self,
        current_token: Token,
        _tokens: list[Token],
        _cursor_token_index: int
    ) -> list[str]:
        """
        Get completions for the current token based on token information.

        Args:
            current_token: The token at cursor position
            _tokens: All tokens in the command line
            _cursor_token_index: Index of current_token in tokens list

        Returns:
            List of possible completions
        """
        if current_token.type == TokenType.OPTION:
            return self._get_option_completions(current_token.value)

        return self._get_mindspace_path_completions(current_token.value)
