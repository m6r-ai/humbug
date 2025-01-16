"""Main entry point for the Humbug application."""

import asyncio
import json
import logging
import os
import sys
import time

from qasync import QEventLoop, QApplication

from humbug.ai.provider import AIProvider
from humbug.gui.main_window import MainWindow

# Import parsers to ensure we register them
from humbug.syntax.c_parser import CParser  # noqa: F401
from humbug.syntax.cpp_parser import CppParser  # noqa: F401
from humbug.syntax.css_parser import CSSParser  # noqa: F401
from humbug.syntax.html_parser import HTMLParser  # noqa: F401
from humbug.syntax.javascript_parser import JavaScriptParser  # noqa: F401
from humbug.syntax.kotlin_parser import KotlinParser  # noqa: F401
from humbug.syntax.metaphor_parser import MetaphorParser  # noqa: F401
from humbug.syntax.move_parser import MoveParser
from humbug.syntax.python_parser import PythonParser  # noqa: F401
from humbug.syntax.text_parser import TextParser  # noqa: F401
from humbug.syntax.typescript_parser import TypeScriptParser  # noqa: F401
from humbug.syntax.parser_registry import ParserRegistry


def get_api_keys() -> dict[str, str | None]:
    """
    Get API keys from configuration file and environment variables.

    The function will first check for a ~/.humbug/api-keys.json file and create it
    if it doesn't exist. Keys from this file will override environment variables.

    Returns:
        dict: Dictionary containing API keys for OpenAI, Google, and Anthropic

    Raises:
        json.JSONDecodeError: If the API keys file exists but contains invalid JSON
    """
    logger = logging.getLogger(__name__)

    # Initialize with environment variables
    api_keys = {
        "OPENAI_API_KEY": os.environ.get("OPENAI_API_KEY"),
        "GOOGLE_API_KEY": os.environ.get("GOOGLE_API_KEY"),
        "ANTHROPIC_API_KEY": os.environ.get("ANTHROPIC_API_KEY")
    }

    # Define the config directory and file path
    config_dir = os.path.expanduser("~/.humbug")
    config_file = os.path.join(config_dir, "api-keys.json")

    # Create directory if it doesn't exist
    try:
        os.makedirs(config_dir, mode=0o700, exist_ok=True)
    except PermissionError:
        logger.error("Failed to create config directory", exc_info=True)
        return api_keys

    # Create or read the config file
    try:
        if not os.path.exists(config_file):
            with open(config_file, 'w', encoding='utf-8') as f:
                json.dump({
                    "OPENAI_API_KEY": "",
                    "GOOGLE_API_KEY": "",
                    "ANTHROPIC_API_KEY": ""
                }, f, indent=4)
            os.chmod(config_file, 0o600)
        else:
            with open(config_file, 'r', encoding='utf-8') as f:
                file_content = f.read()
                if file_content.strip():  # Only parse if file is not empty
                    file_keys = json.loads(file_content)
                    # Override environment variables with non-empty file values
                    for key in api_keys:
                        if key in file_keys and file_keys[key]:
                            api_keys[key] = file_keys[key]

    except json.JSONDecodeError:
        logger.error("Failed to parse API keys file", exc_info=True)
        raise
    except PermissionError:
        logger.error("Failed to access API keys file", exc_info=True)
    except OSError:
        logger.error("Failed to read/write API keys file", exc_info=True)

    return api_keys


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

    # Get API keys from config and environment
    api_keys = get_api_keys()
    openai_key = api_keys["OPENAI_API_KEY"]
    google_key = api_keys["GOOGLE_API_KEY"]
    anthropic_key = api_keys["ANTHROPIC_API_KEY"]

    if not any([openai_key, google_key, anthropic_key]):
        print("Error: No API keys found. Please set keys in ~/.humbug/api-keys.json or environment variables")
        return 1

    # Initialize components
    ai_backends = AIProvider.create_backends(openai_key, google_key, anthropic_key)
    if not ai_backends:
        print("Error: No AI backends could be initialized")
        return 1

    # Create and show main window
    window = MainWindow(ai_backends)
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
