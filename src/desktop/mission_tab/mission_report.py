"""What can be done with a mission's final report: the files it names, and keeping it as a Word document."""

import os
import re

from docx import document_ir_to_docx_ast, serialise_docx
from markdown_.markdown_ast_builder import MarkdownASTBuilder
from markdown_.markdown_to_document_ir import markdown_ast_to_document_ir
from mindspace.journal_entry import JournalActor
from mindspace.mindspace import Mindspace


# How many of the files a report names are offered to open.
_MAX_REPORT_FILES = 6

# Something that reads as a path to a file: one or more names joined by slashes, ending in an extension.
_PATH = re.compile(r"(?<![\w/.-])((?:[\w.-]+/)*[\w.-]+\.[A-Za-z0-9]{1,8})(?![\w/-])")


def report_files(report: str, mindspace: Mindspace) -> list[str]:
    """
    Return the files in the mindspace a report names, in the order it names them, without repeats.

    Only names of files that exist are returned, so a name the lead got wrong, or a file it
    has since removed, is not offered.

    Args:
        report: The report, in Markdown
        mindspace: The open mindspace the paths are relative to
    """
    root = mindspace.mindspace_path()
    found: list[str] = []
    for match in _PATH.finditer(report):
        relative = os.path.normpath(match.group(1).lstrip("./"))
        absolute = os.path.join(root, relative)
        if (
            relative in found or relative.startswith("..") or mindspace.is_humbug_path(absolute)
            or not os.path.isfile(absolute)
        ):
            continue

        found.append(relative)
        if len(found) == _MAX_REPORT_FILES:
            break

    return found


def report_as_word(report: str) -> bytes:
    """
    Return a report as a Word document, with its headings, lists, tables and emphasis.

    Args:
        report: The report, in Markdown
    """
    builder = MarkdownASTBuilder(True)
    builder.update_ast(report, "", "report.md")
    return serialise_docx(document_ir_to_docx_ast(markdown_ast_to_document_ir(builder.document())))


def save_report(mindspace: Mindspace, path: str, content: bytes) -> None:
    """
    Write a saved report, recording the write in the change journal when the file is in the mindspace.

    The journal records the write before it happens, like any other change made through
    Humbug, and the record is cancelled if the write then fails.

    Args:
        mindspace: The open mindspace
        path: Absolute path to write
        content: The document

    Raises:
        OSError: If the file cannot be written
    """
    target = mindspace.journal_target(path)
    entry = None
    if target is not None:
        journal, relative = target
        entry = journal.record_write(path, relative, content, JournalActor.HUMAN, "")

    try:
        with open(path, "wb") as handle:
            handle.write(content)

    except OSError:
        if target is not None and entry is not None:
            target[0].void_entries([entry])

        raise
