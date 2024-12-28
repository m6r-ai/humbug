"""Humbug - A terminal-based AI chat application."""
from datetime import datetime
import os
from typing import Optional

__version__ = "0.1"

import logging
import sys


def setup_logging():
    """Configure application logging with timestamped files."""
    # Create logs directory if it doesn't exist
    os.makedirs("logs", exist_ok=True)

    # Generate timestamp for log filename
    timestamp = datetime.utcnow().strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
    log_file = f"logs/{timestamp}.log"

    # Configure logging
    logging.basicConfig(
        level=logging.DEBUG,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(log_file)
        ]
    )


def format_version() -> str:
    """Format version number, showing patch number only if non-zero."""
    parts = __version__.split('.')
    if len(parts) == 3 and parts[2] == '0':
        return '.'.join(parts[:2])
    return __version__


# Setup logging when module is imported
setup_logging()
