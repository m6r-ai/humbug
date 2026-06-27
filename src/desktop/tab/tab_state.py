from dataclasses import dataclass, asdict
from typing import Any


@dataclass
class TabState:
    """Container for serializable tab state."""
    type: str
    tab_id: str
    path: str
    metadata: dict[str, Any] | None = None
    is_ephemeral: bool = False

    def to_dict(self) -> dict[str, Any]:
        """Convert the TabState to a JSON-serializable dictionary."""
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> 'TabState':
        """Create a TabState instance from a dictionary."""
        if 'type' in data and isinstance(data['type'], str):
            data['type'] = data['type'].lower()

        return cls(**data)
