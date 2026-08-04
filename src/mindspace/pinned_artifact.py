from dataclasses import asdict, dataclass
from datetime import datetime, timezone
import uuid


@dataclass(frozen=True)
class PinnedArtifact:
    """A mindspace item preserved for quick project-level reference."""

    artifact_id: str
    title: str
    kind: str
    content: str
    path: str
    created_at: str

    @classmethod
    def create(cls, title: str, kind: str, content: str = "", path: str = "") -> "PinnedArtifact":
        """Create an artifact with a stable identifier and timestamp."""
        return cls(str(uuid.uuid4()), title, kind, content, path, datetime.now(timezone.utc).isoformat())

    def to_dict(self) -> dict:
        """Return a JSON-serialisable representation."""
        return asdict(self)

    def updated(self, title: str, kind: str, content: str) -> "PinnedArtifact":
        """Return this artifact with editable fields replaced."""
        return PinnedArtifact(
            self.artifact_id, title, kind, content, self.path, self.created_at
        )

    @classmethod
    def from_dict(cls, data: dict) -> "PinnedArtifact":
        """Restore an artifact from persisted data."""
        return cls(**data)
