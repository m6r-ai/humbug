"""Handle application styling"""

from enum import Enum, auto


class ColorRole(Enum):
    """Enumeration of color roles in the application."""
    # Background colors
    BACKGROUND_PRIMARY = auto()          # Main window background
    BACKGROUND_SECONDARY = auto()        # Secondary elements background
    BACKGROUND_INPUT = auto()            # Input box background

    # Message backgrounds
    MESSAGE_USER = auto()                # User message background
    MESSAGE_AI = auto()                  # AI message background
    MESSAGE_SYSTEM = auto()              # System message background
    MESSAGE_ERROR = auto()               # Error message background
    MESSAGE_HEADER = auto()              # Message header background

    # Code blocks
    CODE_BLOCK_BACKGROUND = auto()       # Code block background

    # UI elements
    TAB_ACTIVE = auto()                  # Active tab background
    TAB_INACTIVE = auto()                # Inactive tab background
    TAB_HOVER = auto()                   # Tab hover background
    MENU_BACKGROUND = auto()             # Menu background
    MENU_HOVER = auto()                  # Menu item hover
    SCROLLBAR_BACKGROUND = auto()        # Scrollbar background
    SCROLLBAR_HANDLE = auto()            # Scrollbar handle
    STATUS_BAR = auto()                  # Status bar background

    # Text colors
    TEXT_PRIMARY = auto()                # Primary text color
    SELECTED_TEXT = auto()               # Selected text background

    # Close button states
    CLOSE_BUTTON_HOVER = auto()          # Hover state of close button

    # Syntax highlighting
    SYNTAX_CODE = auto()                 # Code block
