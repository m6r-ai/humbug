"""Shared stylesheet builders for tab chrome."""

from desktop.color_role import ColorRole
from desktop.style_manager import StyleManager


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
