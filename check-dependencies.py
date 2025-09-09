#!/usr/bin/env python3
"""
Convenience script to run the dependency checker.
"""

import sys
import os

# Add the tools directory to the Python path
tools_dir = os.path.join(os.path.dirname(__file__), 'tools')
sys.path.insert(0, tools_dir)

from dependency_checker.cli import main

if __name__ == '__main__':
    sys.exit(main())