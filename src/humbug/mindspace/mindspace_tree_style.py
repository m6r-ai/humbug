from PySide6.QtWidgets import QProxyStyle, QStyle, QStyleOption, QWidget

from humbug.style_manager import StyleManager

class MindspaceTreeStyle(QProxyStyle):
    """
    A custom style for the Mindspace file tree that scales the pixel metrics
    according to the current zoom factor.
    """
    def __init__(self) -> None:
        super().__init__()
        self._style_manager = StyleManager()

    def pixelMetric(
        self,
        metric: QStyle.PixelMetric,
        option: QStyleOption | None = None,
        widget: QWidget | None =None
    ) -> int:
        # Scale all tree view related metrics
        base_size = super().pixelMetric(metric, option, widget)
        return int(base_size * self._style_manager.zoom_factor())
