"""Handle application styling"""

from enum import Enum, auto


class ColorRole(Enum):
    """Enumeration of color roles in the application."""
    # Background colours
    BACKGROUND_PRIMARY = auto()         # Main window background
    BACKGROUND_SECONDARY = auto()       # Secondary elements background
    BACKGROUND_DIALOG = auto()          # Dialog box background

    # Text colours
    TEXT_PRIMARY = auto()               # Primary text color
    TEXT_DISABLED = auto()              # Disabled text color
    TEXT_SELECTED = auto()              # Selected text background
    TEXT_FOUND = auto()                 # Found text background
    TEXT_FOUND_DIM = auto()             # Dimmed found text background

    # Tab colours
    TAB_BACKGROUND_ACTIVE = auto()      # Active tab background
    TAB_INACTIVE = auto()               # Inactive tab text colour
    TAB_BACKGROUND_INACTIVE = auto()    # Inactive tab background
    TAB_BACKGROUND_HOVER = auto()       # Tab hover background
    TAB_BORDER_ACTIVE = auto()          # Active tab border

    # Button colours
    BUTTON_BACKGROUND = auto()          # Push button background
    BUTTON_BACKGROUND_DISABLED = auto()
                                        # Disabled push button background
    BUTTON_BACKGROUND_PRESSED = auto()  # Pressed push button background
    BUTTON_BACKGROUND_HOVER = auto()    # Hover state push button background

    # Menu elements
    MENU_BACKGROUND = auto()            # Menu background
    MENU_HOVER = auto()                 # Menu item hover

    # Splitter bars
    SPLITTER = auto()                   # Splitter bar

    # Scroll bar elements
    SCROLLBAR_BACKGROUND = auto()       # Scrollbar background
    SCROLLBAR_HANDLE = auto()           # Scrollbar handle

    # Message colours
    MESSAGE_BACKGROUND = auto()         # Message background
    MESSAGE_USER = auto()               # User message background
    MESSAGE_AI = auto()                 # AI message background
    MESSAGE_SYSTEM = auto()             # System message background
    MESSAGE_BOOKMARK = auto()           # Bookmark highlight

    # Status bar elements
    STATUS_BAR_BACKGROUND = auto()      # Status bar background

    # Close button states
    CLOSE_BUTTON_BACKGROUND_HOVER = auto()
                                        # Hover state close button background

    # Editor line numbers
    LINE_NUMBER = auto()                # Line number

    # Syntax highlighting
    SYNTAX_ADDRESS = auto()
    SYNTAX_BACKTICK_CODE = auto()
    SYNTAX_CODE = auto()
    SYNTAX_COMMENT = auto()
    SYNTAX_CSS_AT_RULE = auto()
    SYNTAX_DOCTYPE = auto()
    SYNTAX_ELEMENT = auto()
    SYNTAX_ERROR = auto()
    SYNTAX_FUNCTION_OR_METHOD = auto()
    SYNTAX_HEADING = auto()
    SYNTAX_HTML_ATTRIBUTE = auto()
    SYNTAX_HTML_TAG = auto()
    SYNTAX_IDENTIFIER = auto()
    SYNTAX_KEYWORD = auto()
    SYNTAX_LANGUAGE = auto()
    SYNTAX_NUMBER = auto()
    SYNTAX_OPERATOR = auto()
    SYNTAX_PREPROCESSOR = auto()
    SYNTAX_REGEXP = auto()
    SYNTAX_STRING = auto()
    SYNTAX_TEXT = auto()
    SYNTAX_TYPE = auto()

    # Terminal basic colors
    TERM_BLACK = auto()
    TERM_RED = auto()
    TERM_GREEN = auto()
    TERM_YELLOW = auto()
    TERM_BLUE = auto()
    TERM_MAGENTA = auto()
    TERM_CYAN = auto()
    TERM_WHITE = auto()

    # Terminal bright colors
    TERM_BRIGHT_BLACK = auto()
    TERM_BRIGHT_RED = auto()
    TERM_BRIGHT_GREEN = auto()
    TERM_BRIGHT_YELLOW = auto()
    TERM_BRIGHT_BLUE = auto()
    TERM_BRIGHT_MAGENTA = auto()
    TERM_BRIGHT_CYAN = auto()
    TERM_BRIGHT_WHITE = auto()
