"""Shared style helpers for mindspace sidebar panes."""

from PySide6.QtCore import Qt

from humbug.color_role import ColorRole
from humbug.style_manager import StyleManager


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
        }}
        {tree_selector}::item {{
            color: {text};
            padding: 3px 0px;
            margin: 0px;
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
    padding = "2px 0 0 5px" if layout_direction == Qt.LayoutDirection.LeftToRight else "2px 5px 0 0"

    return f"""
        {container_selector} {{
            background-color: {panel_bg};
        }}
        {list_selector} {{
            background-color: {list_bg};
            color: {text};
            outline: none;
            padding: {padding};
        }}
        {list_selector}::item {{
            color: {text};
            padding: 2px 0 2px 0;
            margin: 0px;
        }}
        {list_selector}::item:selected {{
            background-color: {selected};
        }}
        {list_selector}::item:hover {{
            background-color: {hover};
        }}
    """
