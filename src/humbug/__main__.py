"""Main entry point for the Humbug application."""

import asyncio
import logging
import sys
import time

from qasync import QEventLoop, QApplication

from humbug.ai.ai_provider import AIProvider
from humbug.gui.main_window import MainWindow

# Import parsers to ensure we register them
from humbug.syntax.c.c_parser import CParser  # noqa: F401
from humbug.syntax.cpp.cpp_parser import CppParser  # noqa: F401
from humbug.syntax.csharp.csharp_parser import CSharpParser  # noqa: F401
from humbug.syntax.css.css_parser import CSSParser  # noqa: F401
from humbug.syntax.go.go_parser import GoParser  # noqa: F401
from humbug.syntax.html.html_parser import HTMLParser  # noqa: F401
from humbug.syntax.java.java_parser import JavaParser  # noqa: F401
from humbug.syntax.javascript.javascript_parser import JavaScriptParser  # noqa: F401
from humbug.syntax.json.json_parser import JSONParser  # noqa: F401
from humbug.syntax.kotlin.kotlin_parser import KotlinParser  # noqa: F401
from humbug.syntax.metaphor.metaphor_parser import MetaphorParser  # noqa: F401
from humbug.syntax.move.move_parser import MoveParser
from humbug.syntax.python.python_parser import PythonParser  # noqa: F401
from humbug.syntax.rust.rust_parser import RustParser  # noqa: F401
from humbug.syntax.scheme.scheme_parser import SchemeParser  # noqa: F401
from humbug.syntax.swift.swift_parser import SwiftParser  # noqa: F401
from humbug.syntax.text.text_parser import TextParser  # noqa: F401
from humbug.syntax.typescript.typescript_parser import TypeScriptParser  # noqa: F401
from humbug.syntax.parser_registry import ParserRegistry


def install_global_exception_handler():
    """Install a global exception handler for uncaught exceptions."""
    logger = logging.getLogger('GlobalExceptionHandler')

    def handle_exception(exc_type, exc_value, exc_traceback):
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
    def __init__(self, argv):
        super().__init__(argv)
        self._start_time = time.monotonic()

    def notify(self, receiver, event):
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


def main():
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


if __name__ == "__main__":
    sys.exit(main())
