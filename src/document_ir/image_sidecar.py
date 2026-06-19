from pathlib import Path

from document_ir.document_ir_node import DocumentIRImageNode, DocumentIRNode


# Maps a lowercase file extension (without dot) to a MIME type.
_MIME_BY_EXT: dict[str, str] = {
    "png": "image/png",
    "jpg": "image/jpeg",
    "jpeg": "image/jpeg",
    "gif": "image/gif",
    "webp": "image/webp",
    "svg": "image/svg+xml",
    "bmp": "image/bmp",
    "tiff": "image/tiff",
    "tif": "image/tiff",
}

# Maps a MIME type to the preferred file extension (without dot).
_EXT_BY_MIME: dict[str, str] = {
    "image/png": "png",
    "image/jpeg": "jpg",
    "image/gif": "gif",
    "image/webp": "webp",
    "image/svg+xml": "svg",
    "image/bmp": "bmp",
    "image/tiff": "tiff",
    "image/emf": "emf",
    "image/wmf": "wmf",
}


def mime_type_for_extension(ext: str) -> str | None:
    """Return the MIME type for a file extension, or None if unrecognised.

    Args:
        ext: File extension with or without a leading dot (case-insensitive).

    Returns:
        A MIME type string, or None.
    """
    return _MIME_BY_EXT.get(ext.lstrip(".").lower())


def extension_for_mime_type(mime_type: str) -> str:
    """Return the preferred file extension for a MIME type.

    Args:
        mime_type: A MIME type string (e.g. 'image/png').

    Returns:
        A file extension without a leading dot (e.g. 'png').
        Falls back to 'bin' for unrecognised types.
    """
    return _EXT_BY_MIME.get(mime_type, "bin")


def extract_images_to_sidecar(
    document: DocumentIRNode,
    sidecar_dir: Path,
    stem: str,
) -> int:
    """Walk a document IR tree and write embedded image data to a sidecar directory.

    For every DocumentIRImageNode that carries inline ``data``, the bytes are
    written to a file inside ``sidecar_dir`` and the node's ``url`` is updated
    to a relative path of the form ``{stem}_files/imageN.ext``.  The ``data``
    and ``mime_type`` fields are then cleared so subsequent serialisers treat
    the image as an ordinary file reference.

    Nodes whose ``data`` is None are left untouched.

    Args:
        document: The root IR node to walk (modified in place).
        sidecar_dir: Directory to write image files into (created if absent).
        stem: Base name used to build relative URL paths, typically the output
            file stem (e.g. ``'report'`` → ``'report_files/image1.png'``).

    Returns:
        The number of image files written.
    """
    nodes_with_data: list[DocumentIRImageNode] = []
    _collect_image_nodes(document, nodes_with_data)

    if not nodes_with_data:
        return 0

    sidecar_dir.mkdir(parents=True, exist_ok=True)
    dir_name = sidecar_dir.name

    for index, node in enumerate(nodes_with_data, start=1):
        assert node.data is not None
        ext = extension_for_mime_type(node.mime_type or "")
        filename = f"image{index}.{ext}"
        (sidecar_dir / filename).write_bytes(node.data)
        node.url = f"{dir_name}/{filename}"
        node.data = None
        node.mime_type = None

    return len(nodes_with_data)


def _collect_image_nodes(
    node: DocumentIRNode,
    result: list[DocumentIRImageNode],
) -> None:
    """Recursively collect all DocumentIRImageNodes that carry embedded data."""
    if isinstance(node, DocumentIRImageNode) and node.data is not None:
        result.append(node)

    for child in node.children:
        _collect_image_nodes(child, result)
