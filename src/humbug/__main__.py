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

# Import parsers to ensure we register them
# pylint: disable=unused-import
from humbug.syntax.c.c_parser import CParser
from humbug.syntax.cpp.cpp_parser import CppParser
from humbug.syntax.csharp.csharp_parser import CSharpParser
from humbug.syntax.css.css_parser import CSSParser
from humbug.syntax.go.go_parser import GoParser
from humbug.syntax.html.html_parser import HTMLParser
from humbug.syntax.java.java_parser import JavaParser
from humbug.syntax.javascript.javascript_parser import JavaScriptParser
from humbug.syntax.json.json_parser import JSONParser
from humbug.syntax.kotlin.kotlin_parser import KotlinParser
from humbug.syntax.markdown.markdown_parser import MarkdownParser
from humbug.syntax.metaphor.metaphor_parser import MetaphorParser
from humbug.syntax.move.move_parser import MoveParser
from humbug.syntax.python.python_parser import PythonParser
from humbug.syntax.rust.rust_parser import RustParser
from humbug.syntax.scheme.scheme_parser import SchemeParser
from humbug.syntax.swift.swift_parser import SwiftParser
from humbug.syntax.text.text_parser import TextParser
from humbug.syntax.typescript.typescript_parser import TypeScriptParser
from humbug.syntax.parser_registry import ParserRegistry
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

    def notify(self, receiver: QObject, event: QEvent) -> bool:
        event_type = event.type()
        receiver_name = receiver.objectName()
        start = time.monotonic()
        ret = QApplication.notify(self, receiver, event)
        end = time.monotonic()
        elapsed_time = (end - start) * 1000
        if elapsed_time > 20:
            rel_end = end - self._start_time
            print(f"{rel_end:.3f}: processing event {event} type {event_type} for object {receiver_name} took {elapsed_time:.3f} msec")

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
