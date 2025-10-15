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
    - Loading AIFPL patch and diff parser libraries
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
        self._libraries_loaded = False
        self._patch_library_code: str | None = None
        self._diff_parser_library_code: str | None = None
        self._load_libraries()

    def _load_libraries(self) -> None:
        """Load the AIFPL patch and diff parser library code."""
        try:
            # Load patch library (reformatted version)
            patch_library_path = Path(__file__).parent / "patch_library.aifpl"
            if not patch_library_path.exists():
                raise FileNotFoundError(f"AIFPL patch library not found: {patch_library_path}")

            with open(patch_library_path, 'r', encoding='utf-8') as f:
                self._patch_library_code = f.read()

            # Load diff parser library (reformatted version)
            diff_parser_library_path = Path(__file__).parent / "diff_parser_library.aifpl"
            if not diff_parser_library_path.exists():
                raise FileNotFoundError(f"AIFPL diff parser library not found: {diff_parser_library_path}")

            with open(diff_parser_library_path, 'r', encoding='utf-8') as f:
                self._diff_parser_library_code = f.read()

            # Test that the libraries load correctly by evaluating them separately
            patch_result = self.aifpl.evaluate(self._patch_library_code)
            if not isinstance(patch_result, list):
                raise ValueError(f"Expected list from patch library, got {type(patch_result)}")

            diff_result = self.aifpl.evaluate(self._diff_parser_library_code)
            if not isinstance(diff_result, list):
                raise ValueError(f"Expected list from diff parser library, got {type(diff_result)}")

            self._libraries_loaded = True

        except Exception as e:
            raise RuntimeError(f"Failed to load AIFPL libraries: {e}") from e

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

    def aifpl_to_python_hunks(self, aifpl_hunks: List) -> List[Dict[str, Any]]:
        """
        Convert AIFPL hunks structure to Python dictionaries.

        Args:
            aifpl_hunks: AIFPL hunks (Python list from evaluate())

        Returns:
            List of hunk dictionaries
        """
        if not isinstance(aifpl_hunks, list):
            raise ValueError(f"Expected list of hunks, got {type(aifpl_hunks)}")

        python_hunks = []
        for hunk in aifpl_hunks:
            if not isinstance(hunk, list):
                raise ValueError(f"Expected hunk to be a list, got {type(hunk)}")

            # Parse hunk structure: [[key1, val1], [key2, val2], ...]
            hunk_dict = {}
            for item in hunk:
                if isinstance(item, list) and len(item) >= 2:
                    key = item[0]
                    value = item[1]
                    hunk_dict[key] = value

            # Convert to expected format
            start_line = hunk_dict.get("start-line", 0)
            old_count = hunk_dict.get("old-count", 0)
            new_count = hunk_dict.get("new-count", 0)
            changes_aifpl = hunk_dict.get("changes", [])

            # Convert changes
            changes = []
            if isinstance(changes_aifpl, list):
                for change in changes_aifpl:
                    if isinstance(change, list) and len(change) >= 2:
                        change_type = change[0]
                        line_text = change[1]
                        changes.append((change_type, line_text))

            python_hunks.append({
                'start_line': start_line,
                'old_count': old_count,
                'new_count': new_count,
                'changes': changes
            })

        return python_hunks

    def parse_diff(self, diff_text: str) -> Tuple[str | None, List[Dict[str, Any]] | None]:
        """
        Parse diff text using AIFPL parser.

        This is the step-by-step API for compatibility with existing code.

        Args:
            diff_text: Unified diff text

        Returns:
            Tuple of (filename, hunks) or (None, None) on error
        """
        try:
            # Convert diff text to AIFPL string
            aifpl_diff = self.python_to_aifpl_string(diff_text)

            # Create expression that loads library and parses
            expression = f"""(let ((diff-lib {self._diff_parser_library_code}))
  (let ((parse-fn (first (rest (first diff-lib)))))
    (parse-fn {aifpl_diff})))"""

            # Execute AIFPL
            result = self.aifpl.evaluate(expression)

            # Parse result: ["ok", filename, hunks] or ["error", message]
            if not isinstance(result, list) or len(result) < 2:
                return None, None

            status = result[0]
            if status == "error":
                error_msg = result[1] if len(result) > 1 else "Unknown error"
                raise AIFPLError(f"Diff parsing failed: {error_msg}")

            if status != "ok":
                raise AIFPLError(f"Unexpected parse status: {status}")

            filename = result[1]
            aifpl_hunks = result[2]

            # Convert AIFPL hunks to Python dicts
            python_hunks = self.aifpl_to_python_hunks(aifpl_hunks)

            return filename, python_hunks

        except AIFPLError:
            raise

        except Exception as e:
            raise RuntimeError(f"Failed to parse diff: {e}") from e

    def parse_and_apply_patch(
        self,
        diff_text: str,
        file_lines: List[str]
    ) -> Tuple[bool, Any]:
        """
        Parse diff and apply patch in one optimized call (Mode 1: High-level API).

        This avoids the round-trip conversion by keeping data in AIFPL format.

        Args:
            diff_text: Unified diff text
            file_lines: List of file lines (strings)

        Returns:
            Tuple of (success: bool, result: List[str] or error message)
        """
        try:
            # Convert to AIFPL expressions
            aifpl_diff = self.python_to_aifpl_string(diff_text)
            aifpl_lines = self.python_list_to_aifpl_list(file_lines)

            # Create expression that loads both libraries and does parse + apply
            expression = f"""(let ((patch-lib {self._patch_library_code})
                                  (diff-lib {self._diff_parser_library_code}))
  (let ((patch-apply-fn (first (rest (first patch-lib))))
        (parse-and-apply-fn (first (rest (first (rest diff-lib))))))
    (parse-and-apply-fn {aifpl_diff} {aifpl_lines} {self.fuzz_range} patch-apply-fn)))"""

            # Execute AIFPL
            result = self.aifpl.evaluate(expression)

            # Parse result
            return self._parse_patch_result(result)

        except AIFPLError as e:
            return False, f"AIFPL error: {e}"

        except Exception as e:
            return False, f"Unexpected error: {e}"

    def apply_patch(self, file_lines: List[str], hunks: List[Dict[str, Any]]) -> Tuple[bool, Any]:
        """
        Apply patch using AIFPL patch engine (Mode 2: Step-by-step API).

        This is for compatibility with existing code that parses separately.

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
            expression = f"""(let ((functions {self._patch_library_code}))
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
            expression = f"""(let ((functions {self._patch_library_code}))
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
