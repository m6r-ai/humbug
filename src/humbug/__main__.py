"""Main entry point for the Humbug application."""

import asyncio
import logging
import os
import sys
import time

from qasync import QEventLoop, QApplication

from humbug.ai.provider import AIProvider
from humbug.transcript.transcript_writer import TranscriptWriter
from humbug.gui.main_window import MainWindow

# Import parsers to ensure we register them
from humbug.syntax.c_parser import CParser  # noqa: F401
from humbug.syntax.cpp_parser import CppParser  # noqa: F401
from humbug.syntax.css_parser import CSSParser  # noqa: F401
from humbug.syntax.html_parser import HTMLParser  # noqa: F401
from humbug.syntax.javascript_parser import JavaScriptParser  # noqa: F401
from humbug.syntax.metaphor_parser import MetaphorParser  # noqa: F401
from humbug.syntax.python_parser import PythonParser  # noqa: F401
from humbug.syntax.text_parser import TextParser  # noqa: F401
from humbug.syntax.typescript_parser import TypeScriptParser  # noqa: F401
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
            rel_start = start - self._start_time
            print(f"{rel_start:.3f}: processing event type {event_type} for object {receiver_name} took {elapsed_time:.3f} msec")

        return ret


async def main():
    install_global_exception_handler()

    # Check for API keys
    openai_key = os.environ.get("OPENAI_API_KEY")
    google_key = os.environ.get("GOOGLE_API_KEY")
    anthropic_key = os.environ.get("ANTHROPIC_API_KEY")

    if not any([openai_key, google_key, anthropic_key]):
        print("Error: No API keys found. At least one of OPENAI_API_KEY, GOOGLE_API_KEY, or ANTHROPIC_API_KEY must be set")
        return 1

    # Initialize components
    ai_backends = AIProvider.create_backends(openai_key, google_key, anthropic_key)
    if not ai_backends:
        print("Error: No AI backends could be initialized")
        return 1

    transcript = TranscriptWriter()

    # Create and show main window
    window = MainWindow(ai_backends, transcript)
    window.show()

    return 0


def run_app():
    # Create application first
    app = HumbugApplication(sys.argv)

    # Create and set event loop
    loop = QEventLoop(app)
    asyncio.set_event_loop(loop)

    # Run the main function
    try:
        with loop:
            exit_code = loop.run_until_complete(main())
            # Only run forever if main succeeded
            if exit_code == 0:
                loop.run_forever()
            return exit_code
    except KeyboardInterrupt:
        return 0


if __name__ == "__main__":
    sys.exit(run_app())
