import json
import os

from mindspace.pinned_artifact import PinnedArtifact


class PinnedArtifacts:
    """Manages the pinned artifacts saved for a mindspace."""

    def __init__(self) -> None:
        """Initialise an empty artifact collection."""
        self._artifacts: list[PinnedArtifact] = []

    def artifacts(self) -> list[PinnedArtifact]:
        """Return artifacts in pin order."""
        return self._artifacts.copy()

    def add(self, artifact: PinnedArtifact) -> None:
        """Add an artifact."""
        self._artifacts.append(artifact)

    def remove(self, artifact_id: str) -> bool:
        """Remove an artifact by ID."""
        for index, artifact in enumerate(self._artifacts):
            if artifact.artifact_id == artifact_id:
                del self._artifacts[index]
                return True

        return False

    def update(self, artifact: PinnedArtifact) -> bool:
        """Replace an existing artifact with the same ID."""
        for index, existing in enumerate(self._artifacts):
            if existing.artifact_id == artifact.artifact_id:
                self._artifacts[index] = artifact
                return True

        return False

    def save(self, file_path: str) -> None:
        """Save artifacts to a JSON file."""
        with open(file_path, "w", encoding="utf-8") as file:
            json.dump({"artifacts": [item.to_dict() for item in self._artifacts]}, file, indent=2)

    def load(self, file_path: str) -> None:
        """Load artifacts, treating a missing file as empty."""
        if not os.path.exists(file_path):
            self._artifacts = []
            return

        with open(file_path, encoding="utf-8") as file:
            data = json.load(file)

        self._artifacts = [PinnedArtifact.from_dict(item) for item in data.get("artifacts", [])]

    def clear(self) -> None:
        """Clear all artifacts."""
        self._artifacts = []
