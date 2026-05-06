"""Shared style helpers for mindspace sidebar panes."""

from PySide6.QtCore import Qt

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


def build_mindspace_shell_stylesheet(
    style_manager: StyleManager,
    layout_direction: Qt.LayoutDirection,
) -> str:
    """Build the stylesheet for the mindspace shell, rail, and pane host."""
    rail_padding = style_manager.scale(6)
    rail_indicator = style_manager.active_indicator_width()
    checked_rail_padding = max(0, rail_padding - rail_indicator)
    content_radius = style_manager.radius("panel")
    panel_background = style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
    rail_background = style_manager.get_color_str(ColorRole.MINDSPACE_TOOL_RAIL_BACKGROUND)
    rail_hover = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
    text_color = style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
    disabled_color = style_manager.get_color_str(ColorRole.TEXT_DISABLED)
    subtle_text = style_manager.get_color_str(ColorRole.TEXT_INACTIVE)
    border_color = style_manager.get_color_str(ColorRole.MENU_BORDER)
    accent_color = style_manager.get_color_str(ColorRole.TAB_BORDER_ACTIVE)
    content_surface = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)

    indicator_side = "border-left" if layout_direction == Qt.LayoutDirection.LeftToRight else "border-right"
    indicator_padding_side = "padding-left" if layout_direction == Qt.LayoutDirection.LeftToRight else "padding-right"

    return f"""
        {style_manager.get_menu_stylesheet()}
        {style_manager.get_scrollbar_stylesheet()}

        MindspaceView {{
            background-color: {panel_background};
        }}

        QWidget#_rail_widget {{
            background-color: {rail_background};
        }}

        QWidget#_content_widget {{
            background-color: {panel_background};
        }}

        QPushButton#_header_widget {{
            background-color: {panel_background};
            border: none;
            color: {subtle_text};
            padding: 10px 8px 6px 8px;
            text-align: left;
            text-transform: uppercase;
        }}

        QPushButton#_header_widget:hover {{
            color: {text_color};
        }}

        QWidget#_pane_stack {{
            background-color: {panel_background};
            border: none;
        }}

        QToolButton#_sidebar_toggle_button {{
            color: {subtle_text};
            background-color: transparent;
            border: none;
            padding: {rail_padding}px;
            margin: 2px 0px;
        }}

        QToolButton#_settings_button,
        QToolButton#_update_button,
        QToolButton[view_type] {{
            color: {text_color};
            background-color: transparent;
            border: none;
            padding: {rail_padding}px;
            margin: 2px 0px;
        }}

        QToolButton[view_type]:checked {{
            {indicator_side}: {rail_indicator}px solid {accent_color};
            {indicator_padding_side}: {checked_rail_padding}px;
        }}

        QToolButton[view_type]:disabled,
        QToolButton#_settings_button:disabled {{
            color: {disabled_color};
        }}

        QWidget#_content_widget MindspaceSearchView,
        QWidget#_content_widget MindspaceConversationsView,
        QWidget#_content_widget MindspaceFilesView,
        QWidget#_content_widget MindspacePreviewView,
        QWidget#_content_widget MindspaceVCSView {{
            background-color: {content_surface};
            border: none;
            border-radius: {content_radius}px;
            margin: {style_manager.panel_margin()}px;
        }}

        QToolTip {{
            background-color: {rail_hover};
            color: {text_color};
            border: 1px solid {border_color};
            padding: 2px 4px;
        }}
    """


def build_tree_pane_stylesheet(
    style_manager: StyleManager,
    container_selector: str,
    tree_selector: str,
    layout_direction: Qt.LayoutDirection,
    zoom_factor: float,
) -> str:
    """Build a stylesheet for a tree-based mindspace pane."""
    panel_bg = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)
    tree_bg = style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
    tree_hover = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
    tree_selected = style_manager.get_color_str(ColorRole.TEXT_SELECTED)
    text = style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
    branch_icon_size = round(12 * zoom_factor)
    tree_margin = style_manager.spacing(2)
    row_height = style_manager.row_height()
    collapsed_icon = "arrow-right" if layout_direction == Qt.LayoutDirection.LeftToRight else "arrow-left"
    expanded_icon = "arrow-down"

    return f"""
        {container_selector} {{
            background-color: {panel_bg};
        }}
        {tree_selector} {{
            background-color: {tree_bg};
            color: {text};
            outline: none;
            margin-left: {tree_margin}px;
            border: none;
        }}
        {tree_selector}::item {{
            color: {text};
            min-height: {row_height}px;
            padding: 0px {style_manager.spacing(1)}px;
            margin: 1px {style_manager.spacing(1)}px;
            border-radius: {style_manager.radius()}px;
        }}
        {tree_selector}::item:hover {{
            background-color: {tree_hover};
        }}
        {tree_selector}::item:selected {{
            background-color: {tree_selected};
            color: {text};
        }}
        {tree_selector}::branch {{
            background-color: {tree_bg};
        }}
        {tree_selector}::branch:has-children:!has-siblings:closed,
        {tree_selector}::branch:closed:has-children:has-siblings {{
            image: url("{style_manager.get_icon_path(collapsed_icon)}");
            width: {branch_icon_size}px;
            height: {branch_icon_size}px;
        }}
        {tree_selector}::branch:open:has-children:!has-siblings,
        {tree_selector}::branch:open:has-children:has-siblings {{
            image: url("{style_manager.get_icon_path(expanded_icon)}");
            width: {branch_icon_size}px;
            height: {branch_icon_size}px;
        }}
    """


def build_list_pane_stylesheet(
    style_manager: StyleManager,
    container_selector: str,
    list_selector: str,
    layout_direction: Qt.LayoutDirection,
) -> str:
    """Build a stylesheet for a list-based mindspace pane."""
    panel_bg = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY)
    list_bg = style_manager.get_color_str(ColorRole.MINDSPACE_BACKGROUND)
    hover = style_manager.get_color_str(ColorRole.BACKGROUND_TERTIARY_HOVER)
    selected = style_manager.get_color_str(ColorRole.TEXT_SELECTED)
    text = style_manager.get_color_str(ColorRole.TEXT_PRIMARY)
    padding = f"{style_manager.spacing(1)}px"
    row_height = style_manager.row_height()

    return f"""
        {container_selector} {{
            background-color: {panel_bg};
        }}
        {list_selector} {{
            background-color: {list_bg};
            color: {text};
            outline: none;
            padding: {padding};
            border: none;
        }}
        {list_selector}::item {{
            color: {text};
            min-height: {row_height}px;
            padding: 0px {style_manager.spacing(2)}px;
            margin: 1px {style_manager.spacing(1)}px;
            border-radius: {style_manager.radius()}px;
        }}
        {list_selector}::item:selected {{
            background-color: {selected};
        }}
        {list_selector}::item:hover {{
            background-color: {hover};
        }}
    """
