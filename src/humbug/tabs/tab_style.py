"""Shared stylesheet builders for tab chrome."""

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


def build_column_manager_stylesheet(style_manager: StyleManager) -> str:
    """Build the base stylesheet for the column manager."""
    return f"""
        #ColumnManager QWidget {{
            background-color: {style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
        }}
    """


def build_tab_bar_stylesheet(style_manager: StyleManager) -> str:
    """Build the stylesheet for tab bars."""
    return f"""
        QTabBar {{
            border: none;
            margin: 0px;
            padding: 0px;
            background-color: {style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
        }}
        QTabBar::tab {{
            border: none;
            margin: 0px;
            padding: 7px 0px 7px 0px;
        }}
        QTabBar::scroller {{
            width: 28px;
        }}
        QTabBar QToolButton {{
            background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_INACTIVE)};
            border: 1px solid {style_manager.get_color_str(ColorRole.SPLITTER)};
        }}
        QTabBar QToolButton:hover {{
            background-color: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_HOVER)};
        }}
        QTabBar QToolButton::right-arrow {{
            image: url({style_manager.get_icon_path('arrow-right')});
            width: 12px;
            height: 12px;
        }}
        QTabBar QToolButton::left-arrow {{
            image: url({style_manager.get_icon_path('arrow-left')});
            width: 12px;
            height: 12px;
        }}
    """


def build_tab_label_text_stylesheet(style_manager: StyleManager, colour: ColorRole) -> str:
    """Build stylesheet for the text portion of a tab label."""
    return f"""
        background-color: transparent;
        color: {style_manager.get_color_str(colour)};
    """


def build_tab_drag_stylesheet(style_manager: StyleManager) -> str:
    """Build temporary stylesheet used while dragging a tab."""
    return f"""
        QWidget {{
            background: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
        }}
    """


def build_tab_type_icon_stylesheet(style_manager: StyleManager, base_color: ColorRole) -> str:
    """Build stylesheet for the tab type icon container."""
    return f"""
        QLabel {{
            border: none;
            outline: none;
            padding: 0px;
            margin: 0px;
            background: {style_manager.get_color_str(base_color)};
        }}
    """


def build_tab_close_button_stylesheet(style_manager: StyleManager, base_color: ColorRole, visible: bool) -> str:
    """Build stylesheet for the tab close button."""
    if not visible:
        return f"""
            QToolButton {{
                border: none;
                outline: none;
                padding: 0px;
                margin: 0px;
                background: {style_manager.get_color_str(base_color)};
            }}
        """

    return f"""
        QToolButton {{
            border: none;
            outline: none;
            padding: 0px;
            margin: 0px;
            background-color: {style_manager.get_color_str(base_color)};
        }}
        QToolButton:hover {{
            background-color: {style_manager.get_color_str(ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER)};
        }}
    """
