"""Choosing and reading files to attach to a message, shared by every input that takes attachments."""

import logging
import os

from PySide6.QtWidgets import QFileDialog, QWidget

from docx import DocxError, DocxUnsupportedError, extract_text as extract_docx_text
from pdf import PDFUnsupportedError, PDFError, extract_text, parse as parse_pdf
from syntax.programming_language_utils import ProgrammingLanguageUtils

from desktop.language.language_manager import LanguageManager
from desktop.message_box import MessageBox, MessageBoxButton, MessageBoxType


_MAX_ATTACHMENT_SIZE = 100 * 1024

_logger = logging.getLogger("AttachmentReader")


def choose_attachment(parent: QWidget) -> str:
    """
    Ask the user for a file to attach.

    Args:
        parent: Widget the file dialog belongs to

    Returns:
        The chosen file's path, or "" if the user chose none
    """
    strings = LanguageManager().strings()
    extensions = ProgrammingLanguageUtils.get_supported_file_extensions()
    ext_pattern = " ".join(f"*{ext}" for ext in sorted(extensions))
    file_filter = f"Text and code files ({ext_pattern} *.docx *.pdf);;All files (*)"
    path, _ = QFileDialog.getOpenFileName(parent, strings.file_dialog_attach_file, "", file_filter)
    return path


def read_attachment(parent: QWidget, path: str) -> str | None:
    """
    Read a file's text to attach it, asking before attaching a large one.

    PDF and DOCX files are attached as the text extracted from them.  Problems are
    shown to the user.

    Args:
        parent: Widget any message belongs to
        path: The file to read

    Returns:
        The text to attach, or None if there is nothing to attach
    """
    if path.lower().endswith(".pdf"):
        content = _extract_pdf_text(parent, path)
        if content is None or not _accept_size(parent, path, len(content)):
            return None

        return content

    if path.lower().endswith(".docx"):
        content = _extract_docx_text(parent, path)
        if content is None or not _accept_size(parent, path, len(content)):
            return None

        return content

    if not _accept_size(parent, path, os.path.getsize(path)):
        return None

    try:
        with open(path, encoding='utf-8', errors='replace') as f:
            return f.read()

    except Exception as e:
        _logger.warning("Failed to read attachment %s: %s", path, str(e))
        return None


def _accept_size(parent: QWidget, path: str, size: int) -> bool:
    """Return whether to attach a file of the given size, asking the user if it is large."""
    if size <= _MAX_ATTACHMENT_SIZE:
        return True

    strings = LanguageManager().strings()
    result = MessageBox.show_message(
        parent,
        MessageBoxType.WARNING,
        strings.file_error_title,
        strings.warning_file_too_large.format(filename=os.path.basename(path), size_kb=size // 1024),
        [MessageBoxButton.YES, MessageBoxButton.NO]
    )
    return result == MessageBoxButton.YES


def _extract_pdf_text(parent: QWidget, path: str) -> str | None:
    """
    Extract text from a PDF file, showing an error dialog on failure.

    Returns the extracted text, or None if extraction failed or was unsupported.
    """
    strings = LanguageManager().strings()
    filename = os.path.basename(path)
    try:
        with open(path, "rb") as f:
            data = f.read()

        doc = parse_pdf(data)
        return extract_text(doc)

    except PDFUnsupportedError as e:
        MessageBox.show_message(
            parent, MessageBoxType.WARNING, strings.file_error_title,
            strings.error_pdf_unsupported.format(filename=filename, reason=str(e))
        )
        _logger.warning("PDF not supported %s: %s", path, str(e))
        return None

    except PDFError as e:
        MessageBox.show_message(
            parent, MessageBoxType.WARNING, strings.file_error_title,
            strings.error_pdf_extraction_failed.format(filename=filename, reason=str(e))
        )
        _logger.warning("PDF extraction failed %s: %s", path, str(e))
        return None

    except Exception as e:
        MessageBox.show_message(
            parent, MessageBoxType.WARNING, strings.file_error_title,
            strings.error_pdf_extraction_failed.format(filename=filename, reason=str(e))
        )
        _logger.warning("Failed to read PDF %s: %s", path, str(e))
        return None


def _extract_docx_text(parent: QWidget, path: str) -> str | None:
    """
    Extract text from a DOCX file, showing an error dialog on failure.

    Returns the extracted text, or None if extraction failed or was unsupported.
    """
    strings = LanguageManager().strings()
    filename = os.path.basename(path)
    try:
        with open(path, "rb") as f:
            data = f.read()

        return extract_docx_text(data)

    except DocxUnsupportedError as e:
        MessageBox.show_message(
            parent, MessageBoxType.WARNING, strings.file_error_title,
            strings.error_docx_unsupported.format(filename=filename, reason=str(e))
        )
        _logger.warning("DOCX not supported %s: %s", path, str(e))
        return None

    except DocxError as e:
        MessageBox.show_message(
            parent, MessageBoxType.WARNING, strings.file_error_title,
            strings.error_docx_extraction_failed.format(filename=filename, reason=str(e))
        )
        _logger.warning("DOCX extraction failed %s: %s", path, str(e))
        return None

    except Exception as e:
        MessageBox.show_message(
            parent, MessageBoxType.WARNING, strings.file_error_title,
            strings.error_docx_extraction_failed.format(filename=filename, reason=str(e))
        )
        _logger.warning("Failed to read DOCX %s: %s", path, str(e))
        return None
