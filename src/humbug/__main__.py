"""Main entry point for the Humbug application."""

import asyncio
import logging
import sys
import time
from types import TracebackType
from typing import List

from PySide6.QtCore import QObject, QEvent
from qasync import QEventLoop, QApplication  # type: ignore[import-untyped]

from humbug.gui.main_window import MainWindow
# pylint: disable=unused-import
import humbug.syntax.parser_imports
# pylint: enable=unused-import


def install_global_exception_handler() -> None:
    """Install a global exception handler for uncaught exceptions."""
    logger = logging.getLogger('GlobalExceptionHandler')

    def handle_exception(exc_type: type[BaseException], exc_value: BaseException, exc_traceback: TracebackType | None) -> None:
        """Handle uncaught exceptions and log them."""
        if issubclass(exc_type, KeyboardInterrupt):
            # Don't log keyboard interrupt
            sys.__excepthook__(exc_type, exc_value, exc_traceback)
            return

        logger.critical(
            "Uncaught exception",
            exc_info=(exc_type, exc_value, exc_traceback),
            stack_info=True
        )

    sys.excepthook = handle_exception


class HumbugApplication(QApplication):
    """Class for the application. Specialized to do event time reporting."""
    def __init__(self, argv: List[str]) -> None:
        super().__init__(argv)
        self._start_time = time.monotonic()

    def notify(self, arg__1: QObject, arg__2: QEvent) -> bool:
        event_type = arg__2.type()
        receiver_name = arg__1.objectName()
        start = time.monotonic()
        ret = QApplication.notify(self, arg__1, arg__2)
        end = time.monotonic()
        elapsed_time = (end - start) * 1000
        if elapsed_time > 20:
            rel_end = end - self._start_time
            print(f"{rel_end:.3f}: event {arg__2}, type {event_type}, object {receiver_name}, took {elapsed_time:.3f} msec")

        return ret


def main() -> int:
    """Main function to run the application."""
    install_global_exception_handler()

    # Create application
    app = HumbugApplication(sys.argv)

    # Create and set event loop
    loop = QEventLoop(app)
    asyncio.set_event_loop(loop)

    window = MainWindow()
    window.show()

    # Run the main function
    try:
        with loop:
            loop.run_forever()

    except KeyboardInterrupt:
        return 0

    return 0

if __name__ == "__main__":
    sys.exit(main())
