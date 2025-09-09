"""
Reporting and output formatting for dependency validation results.
"""

from typing import List, Dict
from pathlib import Path

from .validator import ValidationResult, Violation


class DependencyReporter:
    """Formats and outputs dependency validation results."""

    def __init__(self) -> None:
        pass

    def format_text(self, result: ValidationResult) -> str:
        """Format results as human-readable text."""
        lines = []

        # Header
        lines.append("Dependency Check Results")
        lines.append("=" * 24)
        lines.append("")

        # Summary by module
        modules_with_violations = set()
        modules_clean = set()

        for violation in result.violations:
            modules_with_violations.add(violation.importing_module)

        for module in result.modules_checked:
            if module not in modules_with_violations:
                modules_clean.add(module)

        # Show clean modules
        for module in sorted(modules_clean):
            lines.append(f"✓ {module}/ - checked, no violations")

        # Show modules with violations
        for module in sorted(modules_with_violations):
            module_violations = [v for v in result.violations if v.importing_module == module]
            internal_count = len([v for v in module_violations if v.violation_type == "internal"])
            external_count = len([v for v in module_violations if v.violation_type == "external"])

            violation_desc = []
            if internal_count > 0:
                violation_desc.append(f"{internal_count} internal")

            if external_count > 0:
                violation_desc.append(f"{external_count} external")

            lines.append(f"✗ {module}/ - {len(module_violations)} violation(s) found ({', '.join(violation_desc)})")

        if result.violations:
            lines.append("")

            # Group violations by type
            internal_violations = result.internal_violations
            external_violations = result.external_violations

            if internal_violations:
                lines.append("Internal Module Violations:")
                lines.append("-" * 28)
                self._add_violation_details(lines, internal_violations)

            if external_violations:
                if internal_violations:
                    lines.append("")

                lines.append("External Dependency Violations:")
                lines.append("-" * 33)
                self._add_violation_details(lines, external_violations)

        # Overall summary
        lines.append("")
        lines.append("Summary:")
        lines.append(f"  Files checked: {result.files_checked}")
        lines.append(f"  Modules checked: {len(result.modules_checked)}")
        lines.append(f"  Total violations: {result.violation_count}")

        if result.internal_violations:
            lines.append(f"  Internal violations: {len(result.internal_violations)}")

        if result.external_violations:
            lines.append(f"  External violations: {len(result.external_violations)}")

        if result.has_violations:
            lines.append(f"  Status: ✗ FAILED - {result.violation_count} violation(s)")

        else:
            lines.append("  Status: ✓ PASSED - No violations found")

        return "\n".join(lines)

    def _add_violation_details(self, lines: List[str], violations: List[Violation]) -> None:
        """Add violation details to the output lines."""
        # Group violations by file
        violations_by_file: Dict[str, List[Violation]] = {}
        for violation in violations:
            if violation.file_path not in violations_by_file:
                violations_by_file[violation.file_path] = []
            violations_by_file[violation.file_path].append(violation)

        for file_path in sorted(violations_by_file.keys()):
            file_violations = violations_by_file[file_path]

            # Show relative path if possible
            try:
                display_path = str(Path(file_path).relative_to(Path.cwd()))

            except ValueError:
                display_path = file_path

            for violation in sorted(file_violations, key=lambda v: v.line_number):
                violation_icon = "🔒" if violation.violation_type == "external" else "🔗"
                lines.append(f"{display_path}:{violation.line_number}")
                lines.append(f"  └─ {violation_icon} Illegal import: {violation.import_statement}")
                lines.append(f"     Rule: {violation.rule_description}")
                lines.append("")

    def print_results(self, result: ValidationResult) -> None:
        """Print results to stdout as text."""
        print(self.format_text(result))

    def save_results(self, result: ValidationResult, output_path: str) -> None:
        """Save results to a file as text."""
        content = self.format_text(result)

        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(content)

    def get_exit_code(self, result: ValidationResult) -> int:
        """Get appropriate exit code for CI/CD integration."""
        return 1 if result.has_violations else 0
