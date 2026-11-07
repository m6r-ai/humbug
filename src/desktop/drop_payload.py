"""What a sidebar drag carries, and where it came from."""

from dataclasses import dataclass

from PySide6.QtCore import QMimeData

PATH_FORMAT = "application/x-humbug-path"
SOURCE_FORMAT = "application/x-humbug-source"
JOURNAL_SEQUENCE_FORMAT = "application/x-humbug-journal-sequence"


@dataclass(frozen=True)
class DropPayload:
    """
    What a dropped drag names.

    A drag names either a path within the mindspace or a recorded change, and the
    two are deliberately told apart.  A drag that names a change carries nothing
    else: a row in the change journal stands for a version of a file rather than for
    the file, so a drop of one must never be read as a drop of a path.  Anything that
    moves or opens the file itself accepts only the path form.

    Attributes:
        source_type: The panel the drag started in, or empty when it named none.
        path: The absolute path dragged, or empty when the drag named a change.
        journal_sequence: The recorded change dragged, or None when it named a path.
    """

    source_type: str
    path: str
    journal_sequence: int | None = None


def read_drop_payload(mime_data: QMimeData) -> DropPayload | None:
    """
    Read the payload a drag carries.

    Args:
        mime_data: The mime data of a drag or drop event.

    Returns:
        The payload, or None when the drag names neither a path nor a change.
    """
    sequence = _journal_sequence(mime_data)
    if sequence is not None:
        return DropPayload(source_type="", path="", journal_sequence=sequence)

    if not mime_data.hasFormat(PATH_FORMAT):
        return None

    return DropPayload(
        source_type=_text(mime_data, SOURCE_FORMAT),
        path=_text(mime_data, PATH_FORMAT),
    )


def journal_mime_data(sequence: int) -> QMimeData:
    """
    Return mime data that carries a recorded change and nothing else.

    Args:
        sequence: The sequence number of the change being dragged.
    """
    mime_data = QMimeData()
    mime_data.setData(JOURNAL_SEQUENCE_FORMAT, str(sequence).encode())
    return mime_data


def _text(mime_data: QMimeData, format_name: str) -> str:
    """
    Return one of a drag's values as text.

    Args:
        mime_data: The mime data of a drag or drop event.
        format_name: The format to read.
    """
    if not mime_data.hasFormat(format_name):
        return ""

    raw = mime_data.data(format_name).data()
    if not isinstance(raw, bytes):
        raw = bytes(raw)

    return raw.decode()


def _journal_sequence(mime_data: QMimeData) -> int | None:
    """
    Return the recorded change a drag names, or None when it names none.

    A value that is not a number is treated as absent, so a malformed drag is
    refused rather than resolved to some other change.

    Args:
        mime_data: The mime data of a drag or drop event.
    """
    raw = _text(mime_data, JOURNAL_SEQUENCE_FORMAT)
    if not raw:
        return None

    try:
        return int(raw)

    except ValueError:
        return None
