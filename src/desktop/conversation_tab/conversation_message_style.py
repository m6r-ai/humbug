"""Style data for conversation message widgets."""

from dataclasses import dataclass

from PySide6.QtGui import QFont, QIcon
from PySide6.QtCore import QSize


@dataclass
class ConversationMessageStyle:
    """
    Pre-computed style values shared across all ConversationMessage instances.

    Built once per style epoch by ConversationWidget and passed to each message's
    apply_style() call, avoiding redundant icon and font construction per message.
    """

    font: QFont
    chip_font: QFont
    spacing: int
    icon_size: QSize
    copy_icon: QIcon
    save_icon: QIcon
    fork_icon: QIcon
    edit_icon: QIcon
    delete_icon: QIcon
    paperclip_icon: QIcon
    expand_down_icon: QIcon
    expand_right_icon: QIcon
    expand_left_icon: QIcon
