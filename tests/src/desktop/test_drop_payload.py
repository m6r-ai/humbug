"""Tests for reading and writing the payload of a sidebar drag."""

from PySide6.QtCore import QMimeData

from desktop.drop_payload import PATH_FORMAT, SOURCE_FORMAT, journal_mime_data, read_drop_payload


def _path_drag(path: str, source_type: str = "") -> QMimeData:
    """Build the mime data a panel that drags a file produces."""
    mime_data = QMimeData()
    mime_data.setData(PATH_FORMAT, path.encode())
    if source_type:
        mime_data.setData(SOURCE_FORMAT, source_type.encode())

    return mime_data


class TestReadingAPathDrag:
    """A drag from a panel that lists files names a path and the panel it came from."""

    def test_the_path_and_source_are_read(self):
        payload = read_drop_payload(_path_drag("/tmp/notes.txt", "files"))

        assert payload is not None
        assert payload.path == "/tmp/notes.txt"
        assert payload.source_type == "files"
        assert payload.journal_sequence is None

    def test_a_drag_from_nowhere_reads_as_an_empty_source(self):
        payload = read_drop_payload(_path_drag("/tmp/notes.txt"))

        assert payload is not None
        assert payload.source_type == ""


class TestReadingAChangeDrag:
    """A drag from the change journal names a recorded change and nothing else."""

    def test_the_sequence_is_read(self):
        payload = read_drop_payload(journal_mime_data(42))

        assert payload is not None
        assert payload.journal_sequence == 42

    def test_a_change_drag_names_no_path(self):
        payload = read_drop_payload(journal_mime_data(42))

        assert payload is not None
        assert payload.path == ""

    def test_a_sequence_that_is_not_a_number_names_no_change(self):
        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-journal-sequence", b"not a number")

        assert read_drop_payload(mime_data) is None


class TestReadingADragThatNamesNothing:
    """A drag carrying neither a path nor a change is refused rather than guessed at."""

    def test_an_empty_drag_reads_as_nothing(self):
        assert read_drop_payload(QMimeData()) is None

    def test_a_drag_carrying_only_a_source_names_nothing(self):
        mime_data = QMimeData()
        mime_data.setData("application/x-humbug-source", b"files")

        assert read_drop_payload(mime_data) is None
