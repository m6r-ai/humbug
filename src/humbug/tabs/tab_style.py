"""Shared stylesheet builders for tab chrome."""

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


def build_column_manager_stylesheet(style_manager: StyleManager) -> str:
    """Build the stylesheet for the tab column manager host."""
    return f"""
        #ColumnManager QWidget {{
            background-color: {style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
        }}
    """


def build_tab_bar_stylesheet(style_manager: StyleManager) -> str:
    """Build the stylesheet for tab bars and their scroll controls."""
    arrow_size = style_manager.scale(10)
    scroller_button_size = style_manager.tab_scroller_button_size()
    scroller_width = style_manager.tab_scroller_width()

    return f"""
        QTabBar {{
            border: none;
            margin: 0px;
            padding: 0px {scroller_width}px 0px 0px;
            background-color: {style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
        }}
        QTabBar::tab {{
            border: none;
            margin: 0px;
            padding: {style_manager.spacing(2)}px 0px;
        }}
        QTabBar::scroller {{
            width: {scroller_width}px;
        }}
        QTabBar QToolButton {{
            background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND)};
            border: 1px solid {style_manager.get_color_str(ColorRole.MENU_BORDER)};
            padding: {style_manager.spacing(2)}px 0px;
        }}
        QTabBar QToolButton:hover {{
            background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_HOVER)};
        }}
        QTabBar QToolButton:pressed {{
            background-color: {style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED)};
        }}
        QTabBar QToolButton:disabled {{
            background-color: {style_manager.get_color_str(ColorRole.TAB_BAR_BACKGROUND)};
            border: 1px solid transparent;
        }}
        QTabBar QToolButton::right-arrow {{
            image: url("{style_manager.get_icon_path('arrow-right')}");
            width: {arrow_size}px;
            height: {arrow_size}px;
        }}
        QTabBar QToolButton::left-arrow {{
            image: url("{style_manager.get_icon_path('arrow-left')}");
            width: {arrow_size}px;
            height: {arrow_size}px;
        }}
    """


def build_tab_label_text_stylesheet(style_manager: StyleManager, color_role: ColorRole) -> str:
    """Build the stylesheet for the text label inside a tab."""
    return f"""
        background-color: transparent;
        color: {style_manager.get_color_str(color_role)};
    """


def build_tab_drag_stylesheet(style_manager: StyleManager) -> str:
    """Build the temporary stylesheet used while rendering a dragged tab pixmap."""
    return f"""
        QWidget {{
            background: {style_manager.get_color_str(ColorRole.TAB_BACKGROUND_ACTIVE)};
        }}
    """


def build_tab_type_icon_stylesheet(style_manager: StyleManager, background_role: ColorRole) -> str:
    """Build the stylesheet for a tab's type icon label."""
    return f"""
        QLabel {{
            border: none;
            outline: none;
            padding: 0px;
            margin: 0px;
            background: {style_manager.get_color_str(background_role)};
        }}
    """


def build_tab_close_button_stylesheet(style_manager: StyleManager, visible: bool) -> str:
    """Build the stylesheet for a tab close button."""
    hover = style_manager.get_color_str(ColorRole.CLOSE_BUTTON_BACKGROUND_HOVER) if visible else "transparent"
    pressed = style_manager.get_color_str(ColorRole.BUTTON_BACKGROUND_PRESSED) if visible else "transparent"

    return f"""
        QToolButton {{
            border: none;
            outline: none;
            padding: 0px;
            margin: 0px;
            background-color: transparent;
            border-radius: {style_manager.radius()}px;
        }}
        QToolButton:hover {{
            background-color: {hover};
        }}
        QToolButton:pressed {{
            background-color: {pressed};
        }}
    """
