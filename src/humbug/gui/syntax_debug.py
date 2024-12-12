"""Debug logging system for syntax highlighting."""

import logging
import os
import sys
from datetime import datetime
from enum import Enum, auto
from pathlib import Path
from typing import Optional, Dict, Any, NamedTuple

from PySide6.QtGui import QTextBlock


class BlockPosition(NamedTuple):
    """Position information for a text block."""
    number: int           # Block number in document
    position: int         # Position in document
    length: int          # Length of block
    line_count: int      # Number of lines in block
    column: int          # Starting column (for first line)
    text: str            # Actual text content


class SyntaxLogEvent(Enum):
    """Types of syntax highlighting events to log."""
    BLOCK_START = auto()          # Starting to process a block
    BLOCK_END = auto()            # Finished processing a block
    STATE_CHANGE = auto()         # Block state changed
    FORMAT_APPLIED = auto()       # Format was applied to text
    REHIGHLIGHT = auto()          # Block being rehighlighted
    STREAM_UPDATE = auto()        # Text updated during streaming
    ERROR = auto()                # Error occurred


class SyntaxLogger:
    """Logger for syntax highlighting events with enhanced position tracking."""

    def __init__(self, name: str, log_dir: Optional[str] = None) -> None:
        """Initialize syntax logger.
        
        Args:
            name: Logger name (typically class name)
            log_dir: Optional directory for log files, defaults to './logs/syntax'
        """
        self.name = name
        self.logger = logging.getLogger(f"syntax.{name}")
        self.logger.setLevel(logging.DEBUG)

        # Create logs directory if it doesn't exist
        if log_dir is None:
            log_dir = os.path.join("logs", "syntax")
        Path(log_dir).mkdir(parents=True, exist_ok=True)

        # Create timestamped log file
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        log_file = os.path.join(log_dir, f"syntax_{name}_{timestamp}.log")

        # File handler with detailed formatting
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(logging.DEBUG)
        file_formatter = logging.Formatter(
            '%(asctime)s.%(msecs)03d [%(name)s] %(levelname)s: %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        file_handler.setFormatter(file_formatter)
        self.logger.addHandler(file_handler)

        # Add stderr handler for errors
        stderr_handler = logging.StreamHandler(sys.stderr)
        stderr_handler.setLevel(logging.ERROR)
        stderr_formatter = logging.Formatter('%(levelname)s: %(message)s')
        stderr_handler.setFormatter(stderr_formatter)
        self.logger.addHandler(stderr_handler)

    def _format_block_info(self, pos: BlockPosition) -> str:
        """Format block position information for logging.
        
        Args:
            pos: BlockPosition tuple with position information
            
        Returns:
            Formatted string with block details
        """
        return (
            f"Block #{pos.number} "
            f"[pos={pos.position}, len={pos.length}, "
            f"lines={pos.line_count}, col={pos.column}]"
        )

    def _format_text(self, text: str) -> str:
        """Format text content for logging, showing whitespace characters.
        
        Args:
            text: Text to format
            
        Returns:
            Text with whitespace characters made visible
        """
        return text.replace(' ', '·').replace('\t', '→').replace('\n', '↵')

    def log_event(self, event: SyntaxLogEvent, block_pos: BlockPosition,
                  state: int, details: Optional[Dict[str, Any]] = None) -> None:
        """Log a syntax highlighting event.
        
        Args:
            event: Type of event being logged
            block_pos: Position information for the block
            state: Current state value
            details: Optional additional details to log
        """
        block_info = self._format_block_info(block_pos)
        text_info = f"Text: '{self._format_text(block_pos.text)}'"
        msg = f"{block_info}: {event.name}"
        if details:
            msg += f" {details}"
        msg += f"\n  {text_info}\n  State: {state}"
        
        self.logger.debug(msg)

    def log_error(self, message: str, block_pos: Optional[BlockPosition] = None,
                  exception: Optional[Exception] = None) -> None:
        """Log an error with optional block position and exception details."""
        if block_pos:
            block_info = self._format_block_info(block_pos)
            message = f"{block_info}: {message}"
        if exception:
            self.logger.error(f"{message}: {str(exception)}", exc_info=True)
        else:
            self.logger.error(message)


class HighlighterBase:
    """Base class adding debug logging to syntax highlighters."""

    def __init__(self, name: str) -> None:
        """Initialize with debug logging support.
        
        Args:
            name: Name for the logger
        """
        self.debug = SyntaxLogger(name)

    def _get_block_position(self, block: QTextBlock) -> BlockPosition:
        """Get detailed position information for a block.
        
        Args:
            block: QTextBlock to analyze
            
        Returns:
            BlockPosition tuple with block details
        """
        return BlockPosition(
            number=block.blockNumber(),
            position=block.position(),
            length=block.length(),
            line_count=block.lineCount(),
            column=block.position() - block.document().findBlock(block.position()).position(),
            text=block.text()
        )

    def _log_block_event(self, event: SyntaxLogEvent, block: QTextBlock,
                        state: int, details: Optional[Dict[str, Any]] = None) -> None:
        """Log a block-related highlighting event.
        
        Args:
            event: Type of event being logged
            block: QTextBlock being processed
            state: Current state value
            details: Optional additional details to log
        """
        block_pos = self._get_block_position(block)
        self.debug.log_event(event, block_pos, state, details)
