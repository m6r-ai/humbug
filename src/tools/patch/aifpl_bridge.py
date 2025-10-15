"""
AIFPL Patch Bridge - Data structure conversion and AIFPL integration.

This module provides the bridge between Python data structures and AIFPL,
handling conversion of file contents, patch hunks, and results.
"""

from pathlib import Path
from typing import List, Dict, Tuple, Any

from aifpl import AIFPL, AIFPLError


class AIFPLPatchBridge:
    """
    Bridge between Python and AIFPL for patch operations.

    Handles:
    - Converting Python data to AIFPL expressions
    - Loading AIFPL patch library
    - Executing AIFPL patch functions
    - Converting AIFPL results back to Python
    """

    def __init__(self, fuzz_range: int = 50):
        """
        Initialize the AIFPL bridge.

        Args:
            fuzz_range: Number of lines to search around expected position
        """
        self.aifpl = AIFPL(max_depth=500)  # Increased depth for complex patches
        self.fuzz_range = fuzz_range
        self._patch_functions_loaded = False
        self._library_code: str | None = None
        self._load_patch_library()

    def _load_patch_library(self) -> None:
        """Load the AIFPL patch library code."""
        try:
            library_path = Path(__file__).parent / "patch_library.aifpl"

            if not library_path.exists():
                raise FileNotFoundError(f"AIFPL patch library not found: {library_path}")

            with open(library_path, 'r', encoding='utf-8') as f:
                self._library_code = f.read()

            # Test that the library loads correctly
            result = self.aifpl.evaluate(self._library_code)

            # Result should be a list of [name, function] pairs
            if not isinstance(result, list):
                raise ValueError(f"Expected list from patch library, got {type(result)}")

            self._patch_functions_loaded = True

        except Exception as e:
            raise RuntimeError(f"Failed to load AIFPL patch library: {e}") from e

    def python_to_aifpl_string(self, text: str) -> str:
        """
        Convert a Python string to AIFPL string literal.

        Args:
            text: Python string

        Returns:
            AIFPL string literal (with proper escaping)
        """
        # Escape special characters for AIFPL
        escaped = text.replace('\\', '\\\\')  # Backslash first
        escaped = escaped.replace('"', '\\"')   # Quote
        escaped = escaped.replace('\n', '\\n')  # Newline
        escaped = escaped.replace('\t', '\\t')  # Tab
        escaped = escaped.replace('\r', '\\r')  # Carriage return

        return f'"{escaped}"'

    def python_list_to_aifpl_list(self, items: List[str]) -> str:
        """
        Convert a Python list of strings to AIFPL list expression.

        Args:
            items: List of Python strings

        Returns:
            AIFPL list expression
        """
        if not items:
            return "(list)"

        aifpl_items = [self.python_to_aifpl_string(item) for item in items]
        return f"(list {' '.join(aifpl_items)})"

    def hunk_to_aifpl(self, hunk: Dict[str, Any]) -> str:
        """
        Convert a parsed hunk dictionary to AIFPL hunk structure.

        Args:
            hunk: Dictionary with keys: start_line, old_count, new_count, changes
                  changes is list of tuples: (type, line) where type is 'context', 'delete', or 'insert'

        Returns:
            AIFPL hunk expression
        """
        start_line = hunk['start_line']
        old_count = hunk['old_count']
        new_count = hunk['new_count']
        changes = hunk['changes']

        # Convert changes to AIFPL format
        aifpl_changes = []
        for change_type, line in changes:
            line_str = self.python_to_aifpl_string(line)
            aifpl_changes.append(f'(list "{change_type}" {line_str})')

        changes_list = f"(list {' '.join(aifpl_changes)})" if aifpl_changes else "(list)"

        return f"""(list
  (list "start-line" {start_line})
  (list "old-count" {old_count})
  (list "new-count" {new_count})
  (list "changes" {changes_list}))"""

    def hunks_to_aifpl(self, hunks: List[Dict[str, Any]]) -> str:
        """
        Convert list of hunks to AIFPL list expression.

        Args:
            hunks: List of hunk dictionaries

        Returns:
            AIFPL list of hunks expression
        """
        if not hunks:
            return "(list)"

        aifpl_hunks = [self.hunk_to_aifpl(hunk) for hunk in hunks]
        return f"(list\n{chr(10).join(aifpl_hunks)})"

    def apply_patch(self, file_lines: List[str], hunks: List[Dict[str, Any]]) -> Tuple[bool, Any]:
        """
        Apply patch using AIFPL patch engine.

        Args:
            file_lines: List of file lines (strings)
            hunks: List of hunk dictionaries

        Returns:
            Tuple of (success: bool, result: List[str] or error message)
        """
        try:
            # Convert to AIFPL expressions
            aifpl_lines = self.python_list_to_aifpl_list(file_lines)
            aifpl_hunks = self.hunks_to_aifpl(hunks)

            # Create expression that loads library and applies patch
            # The library returns a list of (name, function) pairs
            # We extract the patch-apply function and call it
            expression = f"""(let ((functions {self._library_code}))
  (let ((patch-apply-fn (first (rest (first functions)))))
    (patch-apply-fn {aifpl_lines} {aifpl_hunks} {self.fuzz_range})))"""

            # Execute AIFPL
            result = self.aifpl.evaluate(expression)

            # Parse result
            return self._parse_patch_result(result)

        except AIFPLError as e:
            return False, f"AIFPL error: {e}"

        except Exception as e:
            return False, f"Unexpected error: {e}"

    def validate_patch(self, file_lines: List[str], hunks: List[Dict[str, Any]]) -> Tuple[bool, Any]:
        """
        Validate patch without applying (dry run).

        Args:
            file_lines: List of file lines (strings)
            hunks: List of hunk dictionaries

        Returns:
            Tuple of (valid: bool, validation_info)
        """
        try:
            # Convert to AIFPL expressions
            aifpl_lines = self.python_list_to_aifpl_list(file_lines)
            aifpl_hunks = self.hunks_to_aifpl(hunks)

            # Create expression that loads library and validates patch
            expression = f"""(let ((functions {self._library_code}))
  (let ((patch-validate-fn (first (rest (first (rest functions))))))
    (patch-validate-fn {aifpl_lines} {aifpl_hunks} {self.fuzz_range})))"""

            # Execute AIFPL
            result = self.aifpl.evaluate(expression)

            # Parse validation result
            return self._parse_validation_result(result)

        except AIFPLError as e:
            return False, f"AIFPL error: {e}"

        except Exception as e:
            return False, f"Unexpected error: {e}"

    def _parse_patch_result(self, result: Any) -> Tuple[bool, Any]:
        """
        Parse AIFPL patch application result.

        Args:
            result: AIFPL result (Python list from evaluate())

        Returns:
            Tuple of (success, result_data)
        """
        if not isinstance(result, list) or len(result) < 2:
            return False, f"Invalid result format: {result}"

        status = result[0]

        if status == "success":
            # Result is ["success", [line1, line2, ...]]
            patched_lines = result[1]
            if not isinstance(patched_lines, list):
                return False, f"Expected list of lines, got {type(patched_lines)}"

            return True, patched_lines

        if status == "error":
            # Result is ["error", message, position]
            if len(result) >= 2:
                error_msg = result[1]
                position = result[2] if len(result) > 2 else "unknown"
                return False, f"{error_msg} (at line {position})"

            return False, "Unknown error"

        return False, f"Unknown status: {status}"

    def _parse_validation_result(self, result: Any) -> Tuple[bool, Any]:
        """
        Parse AIFPL validation result.

        Args:
            result: AIFPL result (Python list from evaluate())

        Returns:
            Tuple of (all_valid, validation_details)
        """
        if not isinstance(result, list) or len(result) < 2:
            return False, f"Invalid validation format: {result}"

        status = result[0]
        details = result[1]

        if status == "all-valid":
            return True, "All hunks can be applied"

        if status == "some-invalid":
            # Parse details to show which hunks failed
            invalid_hunks = []
            for item in details:
                if isinstance(item, list) and len(item) >= 2:
                    hunk_num = item[0]
                    validation = item[1]
                    if isinstance(validation, list) and len(validation) >= 2:
                        val_status = validation[0]
                        if val_status == "invalid":
                            msg = validation[1] if len(validation) > 1 else "unknown error"
                            invalid_hunks.append(f"Hunk {hunk_num}: {msg}")

            if invalid_hunks:
                return False, "\n".join(invalid_hunks)

            return False, "Some hunks invalid (details unavailable)"

        return False, f"Unknown validation status: {status}"
