from dataclasses import dataclass, asdict
from datetime import datetime
from typing import Any, Dict, Optional

from humbug.gui.tab.tab_type import TabType


@dataclass
class TabState:
    """Container for serializable tab state."""
    type: TabType
    tab_id: str
    path: str
    cursor_position: Optional[Dict[str, int]] = None
    horizontal_scroll: Optional[int] = None
    vertical_scroll: Optional[int] = None
    timestamp: Optional[datetime] = None
    metadata: Optional[Dict[str, Any]] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert the TabState to a JSON-serializable dictionary.

        Returns:
            Dictionary with all state fields properly serialized
        """
        state_dict = asdict(self)

        # Convert TabType enum to string
        if state_dict['type']:
            state_dict['type'] = state_dict['type'].name

        # Convert datetime to ISO format string
        if state_dict['timestamp']:
            state_dict['timestamp'] = state_dict['timestamp'].isoformat()

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

        # Convert timestamp string back to datetime if present
        if data.get('timestamp'):
            data['timestamp'] = datetime.fromisoformat(data['timestamp'])

        return cls(**data)
