"""Showing the version of a file that a change journal entry produced."""

import os

from context.context_registry import ContextRegistry
from mindspace.journal_blob_store import JournalBlobStore
from mindspace.journal_entry import JournalEntry

from desktop.mindspace.mindspace_manager import MindspaceManager


def open_recorded_version(sequence: int, ephemeral: bool) -> str | None:
    """
    Show the version a recorded change produced, in the tab that suits its content.

    Text is shown from the journal rather than from the file: what is on disk now is
    some later state, and a view of the change must not quietly answer with a
    different one.  Content that is not text can only be previewed from a file, so it
    is shown only while the file still holds exactly that version.

    A change that produced nothing - a removal, or a directory - has no version to
    show, and nothing is opened for it.

    Args:
        sequence: The sequence number of the entry whose produced version is wanted.
        ephemeral: True when the tab should close as soon as another opens beside it.

    Returns:
        The id of the tab showing the version, or None when there is no version the
        journal can show.
    """
    mindspace = MindspaceManager().mindspace()
    if not mindspace.has_mindspace():
        return None

    entry = _entry(sequence)
    if entry is None or entry.pre_image_is_directory or entry.post_image_hash is None:
        return None

    try:
        content = mindspace.file_journal().post_image(entry)

    except OSError:
        return None

    if content is None:
        return None

    path = os.path.join(mindspace.mindspace_path(), entry.path)
    title = os.path.basename(entry.path) or entry.path
    try:
        text = content.decode("utf-8")

    except UnicodeDecodeError:
        if _current_digest(path) != entry.post_image_hash:
            return None

        return _show_preview(path, title, ephemeral)

    return _show_text(path, title, text, ephemeral)


def _entry(sequence: int) -> JournalEntry | None:
    """
    Return the entry a sequence number names, or None when it names none.

    The sequence carried by a drag comes from outside this process's control, so a
    number that no longer names a change is refused rather than rounded to the
    nearest one.

    Args:
        sequence: The sequence number to resolve.
    """
    mindspace = MindspaceManager().mindspace()
    if not mindspace.has_mindspace():
        return None

    entries = mindspace.file_journal().entries_from(sequence, 1)
    if not entries or entries[0].sequence != sequence:
        return None

    return entries[0]


def _current_digest(absolute_path: str) -> str | None:
    """
    Return the digest of a path's current content, or None if it cannot be read.

    Args:
        absolute_path: The path to inspect.
    """
    try:
        return JournalBlobStore.digest_of_file(absolute_path)

    except OSError:
        return None


def _show_text(path: str, title: str, text: str, ephemeral: bool) -> str | None:
    """
    Show recorded text in a read-only editor tab.

    A tab already showing this text for this path is brought forward instead of a
    second one being opened: clicking a change and then double-clicking it must
    leave one tab behind, not two.

    Args:
        path: Absolute path of the file the change affected.
        title: The tab's title.
        text: The recorded content.
        ephemeral: True when the tab should close as soon as another opens beside it.
    """
    contexts = MindspaceManager().mindspace().contexts()
    existing = _editor_tab_showing(contexts, path, text)
    if existing is not None:
        contexts.focus(existing)
        if not ephemeral:
            contexts.update(existing, is_ephemeral=False)

        return existing

    return contexts.open(
        context_type="editor",
        path=path,
        title=title,
        is_ephemeral=ephemeral,
        read_only=True,
        initial_model=text,
    )


def _show_preview(path: str, title: str, ephemeral: bool) -> str | None:
    """
    Show a recorded version that is not text, from the file that still holds it.

    Args:
        path: Absolute path of the file the change affected.
        title: The tab's title.
        ephemeral: True when the tab should close as soon as another opens beside it.
    """
    contexts = MindspaceManager().mindspace().contexts()
    existing = contexts.get_by_path_and_type(path, "preview")
    if existing is not None:
        contexts.focus(existing.context_id)
        if not ephemeral:
            contexts.update(existing.context_id, is_ephemeral=False)

        return existing.context_id

    return contexts.open(
        context_type="preview",
        path=path,
        title=title,
        is_ephemeral=ephemeral,
    )


def _editor_tab_showing(contexts: ContextRegistry, path: str, text: str) -> str | None:
    """
    Return the read-only editor tab showing this text for this path, if there is one.

    The text is compared as well as the path, because one file has as many recorded
    versions as it has changes and two of them may be open side by side.

    Args:
        contexts: The registry to search.
        path: Absolute path of the file the change affected.
        text: The recorded content.
    """
    normalized = os.path.normpath(path)
    for info in contexts.list_all():
        if info.context_type != "editor" or not info.read_only or info.path != normalized:
            continue

        if contexts.get_model(info.context_id, str) == text:
            return info.context_id

    return None
