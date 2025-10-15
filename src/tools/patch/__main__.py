"""
CLI entry point for AIFPL Patch Tool.

This allows the tool to be run as:
    python -m tools.patch --file myfile.py --patch changes.diff
"""

import sys
from .patcher import main

if __name__ == "__main__":
    sys.exit(main())
