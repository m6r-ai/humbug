"""Humbug - An AI interaction application."""
from datetime import datetime
import glob
import logging
from logging.handlers import RotatingFileHandler
import os


__version__ = "0.10"


def setup_logging() -> None:
    """Configure application logging with timestamped files and rotation."""
    # Create logs directory in user's home .humbug directory
    log_dir = os.path.expanduser("~/.humbug/logs")
    os.makedirs(log_dir, exist_ok=True)

    # Generate timestamp for log filename
    timestamp = datetime.utcnow().strftime("%Y-%m-%d-%H-%M-%S-%f")[:23]
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


def format_version() -> str:
    """Format version number, showing patch number only if non-zero."""
    parts = __version__.split('.')
    if len(parts) == 3 and parts[2] == '0':
        return '.'.join(parts[:2])
    return __version__


# Setup when module is imported
setup_logging()
