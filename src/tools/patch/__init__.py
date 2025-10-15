"""
AIFPL Patch Tool - Intelligent patch application using AIFPL.

This package provides tools for applying unified diffs with intelligent
fuzzy matching, making it suitable for LLM-generated patches.
"""

from .aifpl_bridge import AIFPLPatchBridge
from .patcher import AIFPLPatcher

__version__ = "1.0.0"

__all__ = [
    "AIFPLPatchBridge",
    "AIFPLPatcher",
]
