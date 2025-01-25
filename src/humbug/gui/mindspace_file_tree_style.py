from PySide6.QtWidgets import QProxyStyle


class MindspaceFileTreeStyle(QProxyStyle):
    def __init__(self, style_manager):
        super().__init__()
        self._style_manager = style_manager

    def pixelMetric(self, metric, option=None, widget=None):
        # Scale all tree view related metrics
        base_size = super().pixelMetric(metric, option, widget)
        return int(base_size * self._style_manager.zoom_factor)
