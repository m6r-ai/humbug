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
from PySide6.QtWidgets import QWidget
from qasync import QEventLoop, QApplication  # type: ignore[import-untyped]

# pylint: disable=unused-import
import syntax.parser_imports
# pylint: enable=unused-import

from ai.ai_backend import AIBackend

from humbug.exception_notifier import get_exception_notifier
from humbug.main_window import MainWindow
from humbug.tabs.tab_base import TabBase


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

        # Notify the UI about the uncaught exception
        try:
            exception_notifier = get_exception_notifier()
            exception_notifier.notify_exception()

        except Exception:
            # If notifying the UI fails, don't let that crash the exception handler
            logger.error("Failed to notify UI of uncaught exception", exc_info=True)

    sys.excepthook = handle_exception


class HumbugApplication(QApplication):
    """Class for the application. Specialized to do event time reporting and focus tracking."""
    def __init__(self, argv: List[str]) -> None:
        super().__init__(argv)
        self._start_time = time.monotonic()

    def notify(self, receiver: QObject, event: QEvent) -> bool:
        """Handle all event notifications."""
        event_type = event.type()

        # Look for focus changes, and update the focus when they occur
        if isinstance(receiver, QWidget):
            if event_type in (QEvent.Type.MouseButtonPress, QEvent.Type.FocusIn):
                parent = receiver.parent()
                while parent:
                    if isinstance(parent, TabBase):
                        parent.set_active(receiver, True)
                        break

                    parent = parent.parent()

            elif event_type == QEvent.Type.FocusOut:
                parent = receiver.parent()
                while parent:
                    if isinstance(parent, TabBase):
                        parent.set_active(receiver, False)
                        break

                    parent = parent.parent()

        # Monitor the performance of the event loop.  We don't need this in production code, but it's very useful in development.
        receiver_name = receiver.objectName()
        start = time.monotonic()
        ret = super().notify(receiver, event)
        end = time.monotonic()
        elapsed_time = (end - start) * 1000

        if elapsed_time > 20:
            rel_end = end - self._start_time
            print(f"{rel_end:.3f}: event {event}, type {event_type}, object {receiver_name}, took {elapsed_time:.3f} msec")

        return ret

def setup_ai_system_prompt() -> None:
    """Configure the global AI system prompt."""
    system_prompt = (
        "Note the following but do not volunteer information them unless asked:\n"
        "- You are an assistant inside a system called Humbug, but you should identify yourself as you would if you were not\n"
        "- The Humbug system supports multiple assistants at the same time so you are potentially one of many active assistants\n"
        "- You are operating inside v40 of the Humbug system\n"
        "- Humbug uses the concept of a mindspace to organize related activities (similar to a workspace). "
        "To ensure data privacy, mindspaces are isolated from each other\n"
        "- You must always use tools when appropriate rather than describing what to do\n"
        "- You must be precise in your responses\n"
        "- You must consider security implications of any tool operations you perform\n"
        "- You must explain your reasoning when making significant changes\n"
        "- You must ask for clarification when something is unclear or ambiguous\n"
        "- You must always use the language annotation when writing code blocks. "
        "If there is no specific language then use 'text'.  For Menai you must use 'menai'\n"
        "- You must follow best practices and include appropriate error handling when writing code\n"
        "- You MUST use the help tool to understand the Menai language before using the menai tool\n"
        "- Always check for a file AGENTS.md in the current mindspace. If it exists, read it it contains information "
        "that will help you understand the system and how to operate within it.  AGENTS.md files may also exist in "
        "subdirectories, and if so they contain information about specific tools or resources relevant to that subdirectory. "
    )

    AIBackend.set_system_prompt(system_prompt)

def main() -> int:
    """Main function to run the application."""
    setup_logging()
    install_global_exception_handler()
    setup_ai_system_prompt()

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
