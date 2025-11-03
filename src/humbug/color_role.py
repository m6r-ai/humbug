"""Handle application styling"""

from enum import Enum, auto


class ColorRole(Enum):
    """Enumeration of color roles in the application."""
    # Background colours
    BACKGROUND_PRIMARY = auto()         # Main window background
    BACKGROUND_SECONDARY = auto()       # Secondary elements background
    BACKGROUND_TERTIARY = auto()        # Tertiary elements background
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
    TEXT_INACTIVE = auto()              # Inactive text colour
    TEXT_EPHEMERAL = auto()             # Ephemeral text color
    TEXT_EPHEMERAL_INACTIVE = auto()    # Inactive ephemeral text color
    TEXT_ERROR = auto()                 # Error text color
    TEXT_ERROR_INACTIVE = auto()        # Inactive error text color

    # Edit box colours
    EDIT_BOX_BORDER = auto()            # Edit box border
    EDIT_BOX_BACKGROUND = auto()        # Edit box background
    EDIT_BOX_ERROR = auto()             # Edit box error

    # Tab colours
    TAB_BAR_BACKGROUND = auto()         # Tab bar background
    TAB_BACKGROUND_ACTIVE = auto()      # Active tab background
    TAB_BACKGROUND_INACTIVE = auto()    # Inactive tab background
    TAB_BACKGROUND_HOVER = auto()       # Tab hover background
    TAB_BACKGROUND_UPDATED = auto()     # Tab background when content updated
    TAB_BORDER_ACTIVE = auto()          # Active tab border

    # Button colours
    BUTTON_BACKGROUND = auto()          # Push button background
    BUTTON_BACKGROUND_PRESSED = auto()  # Pressed push button background
    BUTTON_BACKGROUND_HOVER = auto()    # Hover state push button background
    BUTTON_SECONDARY_BACKGROUND = auto()
                                        # Push button background
    BUTTON_SECONDARY_BACKGROUND_PRESSED = auto()
                                        # Pressed push button background
    BUTTON_SECONDARY_BACKGROUND_HOVER = auto()
                                        # Hover state push button background
    BUTTON_BACKGROUND_RECOMMENDED = auto()
                                        # Recommended primary action button
    BUTTON_BACKGROUND_RECOMMENDED_PRESSED = auto()
                                        # Recommended primary action button pressed
    BUTTON_BACKGROUND_RECOMMENDED_HOVER = auto()
                                        # Recommended primary action button hover
    BUTTON_BACKGROUND_DESTRUCTIVE = auto()
                                        # Destructive primary action button
    BUTTON_BACKGROUND_DESTRUCTIVE_PRESSED = auto()
                                        # Destructive primary action button pressed
    BUTTON_BACKGROUND_DESTRUCTIVE_HOVER = auto()
                                        # Destructive primary action button hover
    BUTTON_BACKGROUND_DISABLED = auto()
                                        # Disabled push button background

    # Menu elements
    MENU_BACKGROUND = auto()            # Menu background
    MENU_HOVER = auto()                 # Menu item hover
    MENU_BORDER = auto()                # Menu border

    # Splitter bars
    SPLITTER = auto()                   # Splitter bar
    TAB_SPLITTER = auto()               # Tab bar splitter

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
    MESSAGE_TOOL_CALL = auto()          # AI tool call
    MESSAGE_TOOL_RESULT = auto()        # AI tool result
    MESSAGE_SYSTEM_ERROR = auto()       # System error message
    MESSAGE_SYSTEM_SUCCESS = auto()     # System success message
    MESSAGE_LANGUAGE = auto()           # Language heading
    MESSAGE_BOOKMARK = auto()           # Bookmark highlight
    MESSAGE_STREAMING = auto()          # Message streaming
    MESSAGE_TRACE = auto()              # Trace message
    MESSAGE_INFORMATION = auto()        # Information message
    MESSAGE_WARNING = auto()            # Warning message
    MESSAGE_ERROR = auto()              # Error message

    # Status bar elements
    STATUS_BAR_BACKGROUND = auto()      # Status bar background
    CANARY_BACKGROUND = auto()          # Canary indicator background for uncaught exceptions

    # Close button states
    CLOSE_BUTTON_BACKGROUND_HOVER = auto()
                                        # Hover state close button background

    # Editor line numbers
    LINE_NUMBER = auto()                # Line number

    # Syntax highlighting
    SYNTAX_ERROR = auto()               # Red
    SYNTAX_01 = auto()                  # Cyan
    SYNTAX_02 = auto()                  # White
    SYNTAX_03 = auto()                  # Light green
    SYNTAX_04 = auto()                  # Light pink
    SYNTAX_05 = auto()                  # Mid grey
    SYNTAX_06 = auto()                  # Light blue
    SYNTAX_07 = auto()                  # Light yellow
    SYNTAX_08 = auto()                  # Lilac
    SYNTAX_09 = auto()                  # Light cyan
    SYNTAX_10 = auto()                  # Mid purple
    SYNTAX_11 = auto()                  # Mid blue
    SYNTAX_12 = auto()                  # Light red
    SYNTAX_13 = auto()                  # Light purple
    SYNTAX_14 = auto()                  # Gold
    SYNTAX_15 = auto()                  # Teal
    SYNTAX_16 = auto()                  # Lime green
    SYNTAX_17 = auto()                  # Light grey
    SYNTAX_18 = auto()                  # Mid grey/green
    SYNTAX_19 = auto()                  # Mid orange
    SYNTAX_20 = auto()                  # Dark orange
    SYNTAX_21 = auto()                  # Green

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
