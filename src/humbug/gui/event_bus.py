from PySide6.QtCore import QObject, Signal, Slot


# Global event bus
class EventBus(QObject):
    _instance = None

    @classmethod
    def instance(cls):
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    menuNeedsUpdate = Signal()
