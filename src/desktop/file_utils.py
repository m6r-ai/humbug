"""Shared file-type utilities for the desktop front-end."""

import os

BINARY_IMAGE_EXTENSIONS: frozenset[str] = frozenset({
    ".png", ".jpg", ".jpeg", ".gif", ".bmp",
    ".tiff", ".tif", ".webp", ".svg", ".ico",
})

CONVERSATION_FILE_EXTENSIONS: frozenset[str] = frozenset({".conv", ".json"})


def is_binary_image_file(path: str) -> bool:
    """Return True if the file at *path* is a binary image that cannot be edited as text."""
    ext = os.path.splitext(path)[1].lower()
    return ext in BINARY_IMAGE_EXTENSIONS


def is_conversation_file(path: str) -> bool:
    """Return True if the file at *path* is a conversation file (.conv or .json)."""
    ext = os.path.splitext(path)[1].lower()
    return ext in CONVERSATION_FILE_EXTENSIONS
