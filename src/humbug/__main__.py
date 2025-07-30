"""Main entry point for the Humbug application."""

import asyncio
from datetime import datetime, timezone
import glob
import logging
from logging.handlers import RotatingFileHandler
import os
import sys
import time
from types import TracebackType
from typing import List

from PySide6.QtCore import QObject, QEvent
from qasync import QEventLoop, QApplication  # type: ignore[import-untyped]

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from humbug.main_window import MainWindow


def setup_logging() -> None:
    """Configure application logging with timestamped files and rotation."""
    # Create logs directory in user's home .humbug directory
    log_dir = os.path.expanduser("~/.humbug/logs")
    os.makedirs(log_dir, exist_ok=True)

    # Generate timestamp for log filename
    timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
    log_file = os.path.join(log_dir, f"{timestamp}.log")

    # Configure rotating file handler
    # Keep up to 50 log files, max 1MB each
    handler = RotatingFileHandler(
        log_file,
        maxBytes=1024*1024,  # 1MB
        backupCount=49,  # Keep 50 files total (current + 49 backups)
        encoding='utf-8'
    )

    # Configure logging
    logging.basicConfig(
        level=logging.DEBUG,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[handler]
    )

    # Clean up old logs if we have too many
    cleanup_old_logs(log_dir, max_logs=50)


def cleanup_old_logs(log_dir: str, max_logs: int) -> None:
    """Remove oldest log files if we exceed maximum count."""
    log_files = glob.glob(os.path.join(log_dir, "*.log*"))
    log_files.sort(key=os.path.getctime)  # Sort by creation time

    # Remove oldest files if we have too many
    while len(log_files) > max_logs:
        try:
            os.remove(log_files.pop(0))  # Remove oldest file

        except OSError:
            pass  # Ignore errors removing old logs


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
    setup_logging()
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
