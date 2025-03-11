from PySide6.QtWidgets import QSplitter

class TabColumnSplitter(QSplitter):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.setChildrenCollapsible(True)
        self.setHandleWidth(1)
        
    def moveSplitter(self, pos: int, index: int):
        # Get the widget on either side of the splitter handle
        widget1 = self.widget(index)
        widget2 = self.widget(index + 1)
        
        if widget1 and widget2:
            # Calculate the minimum width needed for tabs
            min_width = 100  # Minimum width for a column
            
            # If moving would make either widget too small, adjust the position
            if pos < min_width or (self.width() - pos) < min_width:
                # Trigger merge behavior through splitterMoved signal
                self.splitterMoved.emit(pos, index)
        
        return super().moveSplitter(pos, index)