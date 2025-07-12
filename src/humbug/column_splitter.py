from typing import Any

from PySide6.QtWidgets import QSplitter


class ColumnSplitter(QSplitter):
    """
    A custom splitter that allows for a minimum width for each column
    when moving the splitter handle.
    """
    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.setChildrenCollapsible(True)
        self.setHandleWidth(1)

    def moveSplitter(self, pos: int, index: int) -> None:
        # Get the widget on either side of the splitter handle
        widget1 = self.widget(index)
        widget2 = self.widget(index + 1)

        if widget1 and widget2:
            # Calculate the minimum width needed for tabs
            min_width = 100

            # If moving would make either widget too small, adjust the position
            if pos < min_width or (self.width() - pos) < min_width:
                # Trigger merge behavior through splitterMoved signal
                self.splitterMoved.emit(pos, index)

        return super().moveSplitter(pos, index)
