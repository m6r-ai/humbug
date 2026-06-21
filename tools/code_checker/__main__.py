"""
Entry point for the code checker when run as a module.
"""

import sys
from .checker import run_all_checks, CHECKS

if __name__ == "__main__":
    sys.exit(run_all_checks(CHECKS))
