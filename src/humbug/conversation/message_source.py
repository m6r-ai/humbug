"""Conversation state management for the Humbug application."""

from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional
import uuid


class MessageSource(Enum):
    """Enumeration of possible message sources."""
    USER = "user"
    AI = "ai"
    SYSTEM = "system"
