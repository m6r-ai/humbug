"""Humbug - A terminal-based AI chat application."""
from typing import Optional

__version__ = "0.1"

import logging
import sys


# Configure logging
logging.basicConfig(
    level=logging.DEBUG,  # Set the logging level to DEBUG to capture all events
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler("logs/humbug_debug.log"),  # Log messages to a file
    ]
)


def format_version() -> str:
    """Format version number, showing patch number only if non-zero."""
    parts = __version__.split('.')
    if len(parts) == 3 and parts[2] == '0':
        return '.'.join(parts[:2])
    return __version__
