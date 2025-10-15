#!/usr/bin/env python3
"""
AIFPL Patcher - Command-line tool for applying unified diffs using AIFPL.

This tool uses AIFPL for intelligent patch application with fuzzy matching,
making it suitable for LLM-generated patches that may have imprecise line numbers.

Usage:
    python -m tools.patch --file <source_file> --patch <diff_file> [options]

Options:
    --file PATH       Source file to patch (required)
    --patch PATH      Unified diff file (required)
    --apply           Actually apply the patch (default is dry-run)
    --backup          Create backup before applying (file.bak)
    --fuzz N          Fuzzy matching range in lines (default: 50)
    --verbose         Show detailed output
    --help            Show this help message
"""

import argparse
from pathlib import Path
import shutil
import sys
import traceback
from typing import List

from .aifpl_bridge import AIFPLPatchBridge


class Colors:
    """ANSI color codes for terminal output."""
    RESET = '\033[0m'
    BOLD = '\033[1m'
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'

    @classmethod
    def disable(cls) -> None:
        """Disable colors (for non-terminal output)."""
        cls.RESET = ''
        cls.BOLD = ''
        cls.RED = ''
        cls.GREEN = ''
        cls.YELLOW = ''
        cls.BLUE = ''
        cls.CYAN = ''


class AIFPLPatcher:
    """
    Main patcher application.

    Coordinates:
    - Parsing command-line arguments
    - Reading files
    - Applying patches via AIFPL (parse + apply in one call)
    - Writing results
    """

    def __init__(self, args: argparse.Namespace):
        """
        Initialize patcher with command-line arguments.

        Args:
            args: Parsed command-line arguments
        """
        self.args = args
        self.source_file = Path(args.file)
        self.patch_file = Path(args.patch)
        self.verbose = args.verbose

        # Disable colors if not in terminal or if explicitly disabled
        if not sys.stdout.isatty() or args.no_color:
            Colors.disable()

        # Initialize bridge
        self.bridge = AIFPLPatchBridge(fuzz_range=args.fuzz)

    def run(self) -> int:
        """
        Run the patcher.

        Returns:
            Exit code (0 for success, non-zero for failure)
        """
        try:
            # Validate inputs
            if not self._validate_inputs():
                return 1

            # Read source file
            source_lines = self._read_source_file()
            if source_lines is None:
                return 1

            # Read patch file
            diff_text = self._read_patch_file()
            if diff_text is None:
                return 1

            # Show patch info (parse separately for display)
            if not self._show_patch_info(diff_text):
                return 1

            # Validate patch (dry run using step-by-step API)
            if not self._validate_patch(diff_text, source_lines):
                return 1

            # Apply patch if requested
            if self.args.apply:
                return self._apply_patch(diff_text, source_lines)

            self._show_dry_run_message()
            return 0

        except KeyboardInterrupt:
            self._print_error("\nInterrupted by user")
            return 130

        except Exception as e:
            self._print_error(f"Unexpected error: {e}")
            if self.verbose:
                traceback.print_exc()

            return 1

    def _validate_inputs(self) -> bool:
        """Validate that input files exist."""
        if not self.source_file.exists():
            self._print_error(f"Source file not found: {self.source_file}")
            return False

        if not self.source_file.is_file():
            self._print_error(f"Source path is not a file: {self.source_file}")
            return False

        if not self.patch_file.exists():
            self._print_error(f"Patch file not found: {self.patch_file}")
            return False

        if not self.patch_file.is_file():
            self._print_error(f"Patch path is not a file: {self.patch_file}")
            return False

        return True

    def _read_source_file(self) -> List[str] | None:
        """Read source file into list of lines."""
        try:
            with open(self.source_file, 'r', encoding='utf-8') as f:
                content = f.read()

            # Split into lines, preserving empty lines
            lines = content.split('\n')

            # Remove trailing empty line if file ended with newline
            if lines and lines[-1] == '':
                lines = lines[:-1]

            self._print_verbose(f"Read {len(lines)} lines from {self.source_file}")
            return lines

        except Exception as e:
            self._print_error(f"Failed to read source file: {e}")
            return None

    def _read_patch_file(self) -> str | None:
        """Read patch file into string."""
        try:
            with open(self.patch_file, 'r', encoding='utf-8') as f:
                diff_text = f.read()

            self._print_verbose(f"Read {len(diff_text)} bytes from {self.patch_file}")
            return diff_text

        except Exception as e:
            self._print_error(f"Failed to read patch file: {e}")
            return None

    def _show_patch_info(self, diff_text: str) -> bool:
        """Display information about the patch."""
        try:
            # Parse diff to get info (using step-by-step API for display)
            filename, hunks = self.bridge.parse_diff(diff_text)

            if hunks is None:
                self._print_error("Failed to parse patch file")
                return False

            print(f"\n{Colors.BOLD}Patch Information:{Colors.RESET}")
            print(f"  Source file: {Colors.CYAN}{self.source_file}{Colors.RESET}")
            print(f"  Patch file:  {Colors.CYAN}{self.patch_file}{Colors.RESET}")
            print(f"  Target file: {Colors.CYAN}{filename}{Colors.RESET}")
            print(f"  Hunks:       {Colors.CYAN}{len(hunks)}{Colors.RESET}")
            print(f"  Fuzz range:  {Colors.CYAN}±{self.args.fuzz} lines{Colors.RESET}")

            # Verify target file matches source file
            if filename:
                source_name = self.source_file.name
                target_name = Path(filename).name

                if source_name != target_name:
                    self._print_warning(
                        f"Patch target '{target_name}' doesn't match source '{source_name}'"
                    )

            if self.verbose:
                print(f"\n{Colors.BOLD}Hunk Details:{Colors.RESET}")
                for i, hunk in enumerate(hunks, 1):
                    changes = hunk['changes']
                    context_count = sum(1 for t, _ in changes if t == 'context')
                    delete_count = sum(1 for t, _ in changes if t == 'delete')
                    insert_count = sum(1 for t, _ in changes if t == 'insert')

                    print(f"  Hunk {i}: line {hunk['start_line'] + 1}")
                    print(f"    Context: {context_count}, Deletions: {delete_count}, Insertions: {insert_count}")

            return True

        except Exception as e:
            self._print_error(f"Failed to parse patch: {e}")
            if self.verbose:
                traceback.print_exc()
            return False

    def _validate_patch(self, diff_text: str, source_lines: List[str]) -> bool:
        """Validate that patch can be applied."""
        print(f"\n{Colors.BOLD}Validating patch...{Colors.RESET}")

        try:
            # Parse diff first (step-by-step API)
            filename, hunks = self.bridge.parse_diff(diff_text)

            if hunks is None:
                self._print_error("Failed to parse patch for validation")
                return False

            # Validate using step-by-step API
            valid, result = self.bridge.validate_patch(source_lines, hunks)

            if valid:
                print(f"{Colors.GREEN}✓ All hunks can be applied{Colors.RESET}")
                return True

            print(f"{Colors.RED}✗ Patch validation failed:{Colors.RESET}")
            print(f"  {result}")
            return False

        except Exception as e:
            self._print_error(f"Validation failed: {e}")
            if self.verbose:
                traceback.print_exc()
            return False

    def _apply_patch(self, diff_text: str, source_lines: List[str]) -> int:
        """Apply the patch to the source file."""
        print(f"\n{Colors.BOLD}Applying patch...{Colors.RESET}")

        # Create backup if requested
        if self.args.backup:
            if not self._create_backup():
                return 1

        # Apply patch using optimized one-shot API
        success, result = self.bridge.parse_and_apply_patch(diff_text, source_lines)

        if not success:
            self._print_error(f"Patch application failed: {result}")
            return 1

        # Write patched content
        if not self._write_patched_file(result):
            return 1

        print(f"{Colors.GREEN}✓ Patch applied successfully{Colors.RESET}")
        print(f"  Modified: {Colors.CYAN}{self.source_file}{Colors.RESET}")

        if self.args.backup:
            backup_file = self.source_file.with_suffix(self.source_file.suffix + '.bak')
            print(f"  Backup:   {Colors.CYAN}{backup_file}{Colors.RESET}")

        return 0

    def _create_backup(self) -> bool:
        """Create backup of source file."""
        backup_file = self.source_file.with_suffix(self.source_file.suffix + '.bak')

        try:
            shutil.copy2(self.source_file, backup_file)
            self._print_verbose(f"Created backup: {backup_file}")
            return True

        except Exception as e:
            self._print_error(f"Failed to create backup: {e}")
            return False

    def _write_patched_file(self, patched_lines: List[str]) -> bool:
        """Write patched content to source file."""
        try:
            # Join lines with newlines
            content = '\n'.join(patched_lines)

            # Add final newline if original file had one
            with open(self.source_file, 'r', encoding='utf-8') as f:
                original_content = f.read()

            if original_content and original_content[-1] == '\n':
                content += '\n'

            # Write patched content
            with open(self.source_file, 'w', encoding='utf-8') as f:
                f.write(content)

            self._print_verbose(f"Wrote {len(patched_lines)} lines to {self.source_file}")
            return True

        except Exception as e:
            self._print_error(f"Failed to write patched file: {e}")
            return False

    def _show_dry_run_message(self) -> None:
        """Show message about dry-run mode."""
        print(f"\n{Colors.YELLOW}Dry-run mode: No changes were made{Colors.RESET}")
        print(f"  Use {Colors.BOLD}--apply{Colors.RESET} to actually apply the patch")
        print(f"  Use {Colors.BOLD}--backup{Colors.RESET} to create a backup before applying")

    def _print_error(self, message: str) -> None:
        """Print error message."""
        print(f"{Colors.RED}Error:{Colors.RESET} {message}", file=sys.stderr)

    def _print_warning(self, message: str) -> None:
        """Print warning message."""
        print(f"{Colors.YELLOW}Warning:{Colors.RESET} {message}")

    def _print_verbose(self, message: str) -> None:
        """Print verbose message."""
        if self.verbose:
            print(f"{Colors.BLUE}[verbose]{Colors.RESET} {message}")


def parse_arguments() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Apply unified diffs using AIFPL with fuzzy matching",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Dry run (default) - show what would happen
  python -m tools.patch --file src/example.py --patch changes.diff

  # Apply the patch
  python -m tools.patch --file src/example.py --patch changes.diff --apply

  # Apply with backup
  python -m tools.patch --file src/example.py --patch changes.diff --apply --backup

  # Adjust fuzzy matching tolerance
  python -m tools.patch --file src/example.py --patch changes.diff --fuzz 100

  # Verbose output
  python -m tools.patch --file src/example.py --patch changes.diff --verbose
        """
    )

    parser.add_argument(
        '--file',
        required=True,
        help='Source file to patch'
    )

    parser.add_argument(
        '--patch',
        required=True,
        help='Unified diff file'
    )

    parser.add_argument(
        '--apply',
        action='store_true',
        help='Actually apply the patch (default is dry-run)'
    )

    parser.add_argument(
        '--backup',
        action='store_true',
        help='Create backup before applying (file.bak)'
    )

    parser.add_argument(
        '--fuzz',
        type=int,
        default=50,
        help='Fuzzy matching range in lines (default: 50)'
    )

    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Show detailed output'
    )

    parser.add_argument(
        '--no-color',
        action='store_true',
        help='Disable colored output'
    )

    return parser.parse_args()


def main() -> int:
    """Main entry point."""
    args = parse_arguments()
    patcher = AIFPLPatcher(args)
    return patcher.run()


if __name__ == "__main__":
    sys.exit(main())
