"""Tests for writing a document's embedded images into a sidecar folder."""

from pathlib import Path

from document_ir.document_ir_node import DocumentIRImageNode, DocumentIRNode
from document_ir.image_sidecar import extract_images_to_sidecar


def _document_with_images(*images: tuple[str, bytes]) -> DocumentIRNode:
    """Build a document holding the given (mime type, bytes) images."""
    document = DocumentIRNode()
    for mime_type, data in images:
        node = DocumentIRImageNode("")
        node.mime_type = mime_type
        node.data = data
        document.add_child(node)

    return document


class TestSidecarRecording:
    """Tests for the seam a caller uses to record the writes before they happen."""

    def test_each_file_is_reported_before_it_exists(self, tmp_path: Path) -> None:
        """A report names the file and its bytes while the file is not yet there."""
        document = _document_with_images(("image/png", b"png"), ("image/jpeg", b"jpg"))
        sidecar = tmp_path / "notes_files"
        reported: list[tuple[Path, bytes | None]] = []

        def before_write(path: Path, data: bytes | None) -> None:
            reported.append((path, data))

        extract_images_to_sidecar(document, sidecar, before_write)

        assert reported == [
            (sidecar, None),
            (sidecar / "image1.png", b"png"),
            (sidecar / "image2.jpg", b"jpg"),
        ]

    def test_the_folder_is_reported_only_when_it_is_created(self, tmp_path: Path) -> None:
        """A folder that is already there is not reported, because nothing is created."""
        sidecar = tmp_path / "notes_files"
        sidecar.mkdir()
        document = _document_with_images(("image/png", b"png"))
        reported: list[Path] = []

        extract_images_to_sidecar(document, sidecar, lambda path, _data: reported.append(path))

        assert reported == [sidecar / "image1.png"]

    def test_a_document_without_images_writes_and_reports_nothing(self, tmp_path: Path) -> None:
        """Nothing is created and nothing is reported for a document with no images."""
        sidecar = tmp_path / "notes_files"
        reported: list[Path] = []

        written = extract_images_to_sidecar(DocumentIRNode(), sidecar, lambda path, _data: reported.append(path))

        assert written == 0
        assert reported == []
        assert not sidecar.exists()

    def test_a_caller_that_records_nothing_still_gets_its_images(self, tmp_path: Path) -> None:
        """The seam is optional; a conversion still writes the images without it."""
        document = _document_with_images(("image/png", b"png"))
        sidecar = tmp_path / "notes_files"

        written = extract_images_to_sidecar(document, sidecar)

        assert written == 1
        assert (sidecar / "image1.png").read_bytes() == b"png"
