"""Records of the file mutations held in the mindspace change journal."""

from dataclasses import dataclass
from enum import Enum
from typing import Any


class JournalOperation(str, Enum):
    """
    The kind of mutation a journal entry describes.

    Values are written to disk, so they are explicit strings rather than auto().
    """

    WRITE = "write"
    DELETE = "delete"


class JournalActor(str, Enum):
    """Who performed a mutation."""

    AI = "ai"
    HUMAN = "human"


@dataclass
class JournalEntry:
    """
    One recorded file mutation.

    Attributes:
        sequence: Position of this entry in the journal, starting at 1.
        timestamp: ISO-8601 UTC time of the mutation.
        path: The mutated path, relative to the mindspace root.
        operation: The kind of mutation.
        actor: Who performed it.
        conversation: Identifier of the conversation whose activity caused it,
            empty when no conversation was involved.
        group: Identifier shared by the entries of one operation that must be
            reversed together, such as the two halves of a move.  Empty when the
            mutation stands alone.
        existed_before: True if the path existed before the mutation.
        pre_image_hash: Digest of the stored pre-image, or None when the path did
            not exist, or when it existed but its content was not captured.
        pre_image_trash: Name of the trash entry holding the removed content, when
            the removal parked it there, or None otherwise.  Reversal uses the
            stored pre-image either way; this records where the content also
            remains, so the interface can point the user at it.
        post_image_hash: Digest of the content after the mutation, or None when
            the mutation removed the path.
    """

    sequence: int
    timestamp: str
    path: str
    operation: JournalOperation
    actor: JournalActor
    conversation: str
    group: str
    existed_before: bool
    pre_image_hash: str | None
    post_image_hash: str | None
    pre_image_trash: str | None = None

    def to_dict(self) -> dict[str, Any]:
        """Serialise the entry to a JSON-compatible dict."""
        return {
            "sequence": self.sequence,
            "timestamp": self.timestamp,
            "path": self.path,
            "operation": self.operation.value,
            "actor": self.actor.value,
            "conversation": self.conversation,
            "group": self.group,
            "existed_before": self.existed_before,
            "pre_image_hash": self.pre_image_hash,
            "post_image_hash": self.post_image_hash,
            "pre_image_trash": self.pre_image_trash,
        }

    @staticmethod
    def from_dict(data: dict[str, Any]) -> "JournalEntry":
        """
        Deserialise an entry from a previously saved dict.

        Args:
            data: Dict produced by to_dict().

        Returns:
            The deserialised entry.

        Raises:
            ValueError: A required field is missing or has an unexpected type.
        """
        try:
            pre_image_hash = data["pre_image_hash"]
            if pre_image_hash is not None and not isinstance(pre_image_hash, str):
                raise ValueError("pre_image_hash must be a string or null")

            post_image_hash = data["post_image_hash"]
            if post_image_hash is not None and not isinstance(post_image_hash, str):
                raise ValueError("post_image_hash must be a string or null")

            pre_image_trash = data.get("pre_image_trash")
            if pre_image_trash is not None and not isinstance(pre_image_trash, str):
                raise ValueError("pre_image_trash must be a string or null")

            return JournalEntry(
                sequence=int(data["sequence"]),
                timestamp=str(data["timestamp"]),
                path=str(data["path"]),
                operation=JournalOperation(data["operation"]),
                actor=JournalActor(data["actor"]),
                conversation=str(data["conversation"]),
                group=str(data.get("group", "")),
                existed_before=bool(data["existed_before"]),
                pre_image_hash=pre_image_hash,
                post_image_hash=post_image_hash,
                pre_image_trash=pre_image_trash,
            )

        except KeyError as e:
            raise ValueError(f"Journal entry is missing field {str(e)}") from e

    def is_revertable(self) -> bool:
        """
        Return True if this mutation's effect can be undone.

        A creation is revertable by removing the file, and an overwrite is
        revertable when its pre-image was captured.  A mutation whose pre-image
        was too large to store is recorded but cannot be reversed.
        """
        return (not self.existed_before) or self.pre_image_hash is not None
