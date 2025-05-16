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
    TEXT_BRIGHT = auto()                # Bright text color
    TEXT_HEADING = auto()               # Heading text color
    TEXT_HEADING_BRIGHT = auto()        # Bright heading text color
    TEXT_DISABLED = auto()              # Disabled text color
    TEXT_SELECTED = auto()              # Selected text background
    TEXT_FOUND = auto()                 # Found text background
    TEXT_FOUND_DIM = auto()             # Dimmed found text background
    TEXT_RECOMMENDED = auto()           # Recommended text color
    TEXT_LINK = auto()                  # Link text color

    # Tab colours
    TAB_BACKGROUND_ACTIVE = auto()      # Active tab background
    TAB_INACTIVE = auto()               # Inactive tab text colour
    TAB_BACKGROUND_INACTIVE = auto()    # Inactive tab background
    TAB_BACKGROUND_HOVER = auto()       # Tab hover background
    TAB_BORDER_ACTIVE = auto()          # Active tab border

    # Button colours
    BUTTON_BACKGROUND = auto()          # Push button background
    BUTTON_BACKGROUND_PRESSED = auto()  # Pressed push button background
    BUTTON_BACKGROUND_HOVER = auto()    # Hover state push button background
    BUTTON_BACKGROUND_RECOMMENDED = auto()
                                        # Recommended primary action button
    BUTTON_BACKGROUND_RECOMMENDED_PRESSED = auto()
                                        # Recommended primary action button pressed
    BUTTON_BACKGROUND_RECOMMENDED_HOVER = auto()
                                        # Recommended primary action button hover
    BUTTON_BACKGROUND_DISABLED = auto()
                                        # Disabled push button background

    # Menu elements
    MENU_BACKGROUND = auto()            # Menu background
    MENU_HOVER = auto()                 # Menu item hover

    # Splitter bars
    SPLITTER = auto()                   # Splitter bar

    # Scroll bar elements
    SCROLLBAR_BACKGROUND = auto()       # Scrollbar background
    SCROLLBAR_HANDLE = auto()           # Scrollbar handle

    # Table elements
    TABLE_BORDER = auto()               # Table border
    TABLE_HEADER_BACKGROUND = auto()    # Table header background

    # Message colours
    MESSAGE_BACKGROUND = auto()         # Message background
    MESSAGE_USER_BACKGROUND = auto()    # Message background for user messages
    MESSAGE_FOCUSED = auto()            # For highlighting the focused message
    MESSAGE_USER = auto()               # User message
    MESSAGE_AI = auto()                 # AI response
    MESSAGE_REASONING = auto()          # AI reasoning
    MESSAGE_SYSTEM_ERROR = auto()       # System error message
    MESSAGE_SYSTEM_SUCCESS = auto()     # System success message
    MESSAGE_LANGUAGE = auto()           # Language heading
    MESSAGE_BOOKMARK = auto()           # Bookmark highlight
    MESSAGE_STREAMING = auto()          # Message streaming

    # Status bar elements
    STATUS_BAR_BACKGROUND = auto()      # Status bar background

    # Close button states
    CLOSE_BUTTON_BACKGROUND_HOVER = auto()
                                        # Hover state close button background

    # Editor line numbers
    LINE_NUMBER = auto()                # Line number

    # Syntax highlighting
    SYNTAX_ADDRESS = auto()
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
    SYNTAX_INLINE_CODE = auto()
    SYNTAX_KEYWORD = auto()
    SYNTAX_LANGUAGE = auto()
    SYNTAX_LIST_MARKER = auto()
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
