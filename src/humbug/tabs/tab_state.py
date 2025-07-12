from dataclasses import dataclass, asdict
from typing import Any, Dict

from humbug.tabs.tab_type import TabType


@dataclass
class TabState:
    """Container for serializable tab state."""
    type: TabType
    tab_id: str
    path: str
    cursor_position: Dict[str, int] | None = None
    horizontal_scroll: int | None = None
    vertical_scroll: int | None = None
    metadata: Dict[str, Any] | None = None
    is_ephemeral: bool = False

    def to_dict(self) -> Dict[str, Any]:
        """Convert the TabState to a JSON-serializable dictionary.

        Returns:
            Dictionary with all state fields properly serialized
        """
        state_dict = asdict(self)

        # Convert TabType enum to string
        if state_dict['type']:
            state_dict['type'] = state_dict['type'].name

        return state_dict

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'TabState':
        """Create a TabState instance from a dictionary.

        Args:
            data: Dictionary containing the serialized state

        Returns:
            New TabState instance
        """
        # Convert string back to TabType enum
        if 'type' in data and isinstance(data['type'], str):
            data['type'] = TabType[data['type']]

        return cls(**data)
