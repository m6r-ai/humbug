"""
Entry point for running the Humbug style checks standalone.

The checks normally run as part of the full code checker:

    source venv/bin/activate && python -m tools.code_checker

This module runs just the style-check portion (the Humbug pylint plugin)
against ``src`` for quick iteration during development:

    source venv/bin/activate && python -m tools.style_checker
"""

import subprocess
import sys


def main() -> int:
    """Run pylint with only the Humbug style plugin enabled."""
    return subprocess.run(
        [
            sys.executable,
            "-m",
            "pylint",
            "--load-plugins=tools.style_checker.humbug_style_checker",
            "--disable=all",
            "--enable=humbug-no-property,humbug-no-optional,humbug-no-aligned-assigns,"
            "humbug-no-union,humbug-blank-before-dedent,humbug-multiline-docstring",
            "src",
        ],
        check=False,
    ).returncode


if __name__ == "__main__":
    sys.exit(main())
