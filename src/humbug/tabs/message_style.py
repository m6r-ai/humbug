"""Shared stylesheet builders for message-oriented tab content."""

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


def build_message_tab_stylesheet(style_manager: StyleManager, include_border: bool = False) -> str:
    """Build the base stylesheet for message-oriented tab widgets."""
    border_rule = "border: none;" if include_border else ""
    return f"""
        QWidget {{
            background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
            {border_rule}
        }}

        {style_manager.get_menu_stylesheet()}
        {style_manager.get_scrollbar_stylesheet()}
    """


def build_shell_message_stylesheet(style_manager: StyleManager) -> str:
    """Build styles for shell message cards."""
    border_radius = style_manager.message_radius()

    return f"""
        #ShellMessage {{
            margin: 0;
            border-radius: {border_radius}px;
            background-color: {style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)};
            border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_BORDER)};
        }}
        #ShellMessage[message_source="user"] {{
            background-color: {style_manager.get_color_str(ColorRole.MESSAGE_USER_BACKGROUND)};
            border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_USER_BORDER)};
        }}
        #ShellMessage[border="spotlighted"] {{
            border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_SPOTLIGHTED)};
        }}

        #ShellMessage #_header {{
            background-color: transparent;
            border: none;
            border-radius: 0;
            padding: 0;
            margin: 0;
        }}

        #ShellMessage #_role_label {{
            color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            margin: 0;
            padding: 0;
            border: none;
            background-color: transparent;
        }}
        #ShellMessage #_role_label[message_source="user"] {{
            color: {style_manager.get_color_str(ColorRole.MESSAGE_USER)};
        }}
        #ShellMessage #_role_label[message_source="success"] {{
            color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_SUCCESS)};
        }}
        #ShellMessage #_role_label[message_source="error"] {{
            color: {style_manager.get_color_str(ColorRole.MESSAGE_SYSTEM_ERROR)};
        }}

        #ShellMessage #_text_area {{
            color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            border: none;
            border-radius: 0;
            padding: 0;
            margin: 0;
            background-color: transparent;
        }}

        {style_manager.get_scrollbar_stylesheet("#ShellMessage #_text_area QScrollBar")}
    """


def build_log_message_stylesheet(style_manager: StyleManager) -> str:
    """Build styles for log message cards."""
    border_radius = style_manager.message_radius()
    message_background = style_manager.get_color_str(ColorRole.MESSAGE_BACKGROUND)

    return f"""
        #LogMessage {{
            background-color: {message_background};
            margin: 0;
            border-radius: {border_radius}px;
            border: 1px solid {style_manager.get_color_str(ColorRole.MESSAGE_BORDER)};
        }}
        #LogMessage[border="spotlighted"] {{
            border: 2px solid {style_manager.get_color_str(ColorRole.MESSAGE_SPOTLIGHTED)};
        }}

        #LogMessage #_header {{
            background-color: {message_background};
            border: none;
            border-radius: 0;
            padding: 0;
            margin: 0;
        }}

        #LogMessage #_level_label {{
            color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            margin: 0;
            padding: 0;
            border: none;
            background-color: {message_background};
        }}

        #LogMessage #_level_label[log_level="trace"] {{
            color: {style_manager.get_color_str(ColorRole.MESSAGE_TRACE)};
        }}
        #LogMessage #_level_label[log_level="info"] {{
            color: {style_manager.get_color_str(ColorRole.MESSAGE_INFORMATION)};
        }}
        #LogMessage #_level_label[log_level="warn"] {{
            color: {style_manager.get_color_str(ColorRole.MESSAGE_WARNING)};
        }}
        #LogMessage #_level_label[log_level="error"] {{
            color: {style_manager.get_color_str(ColorRole.MESSAGE_ERROR)};
        }}

        #LogMessage #_text_area {{
            color: {style_manager.get_color_str(ColorRole.TEXT_PRIMARY)};
            selection-background-color: {style_manager.get_color_str(ColorRole.TEXT_SELECTED)};
            border: none;
            border-radius: 0;
            padding: 0;
            margin: 0;
            background-color: {message_background};
        }}

        {style_manager.get_scrollbar_stylesheet("#LogMessage #_text_area QScrollBar")}
    """
