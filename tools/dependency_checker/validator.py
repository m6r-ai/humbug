"""
Dependency validation logic.
"""

import os
from pathlib import Path
from dataclasses import dataclass

from .config import DependencyConfig
from .parser import ImportParser


@dataclass
class Violation:
    """Represents a dependency rule violation."""

    file_path: str
    line_number: int
    importing_module: str
    imported_module: str
    import_statement: str
    rule_description: str
    violation_type: str = "internal"  # "internal", "external", or "unused"


@dataclass
class ValidationResult:
    """Results of dependency validation."""

    violations: list[Violation]
    files_checked: int
    modules_checked: set[str]

    def has_violations(self) -> bool:
        """Check if there are any violations in the result."""
        return len(self.violations) > 0

    def violation_count(self) -> int:
        """Get the total number of violations."""
        return len(self.violations)

    def internal_violations(self) -> list[Violation]:
        """Get only the internal dependency violations."""
        return [v for v in self.violations if v.violation_type == "internal"]

    def external_violations(self) -> list[Violation]:
        """Get only the external dependency violations."""
        return [v for v in self.violations if v.violation_type == "external"]

    def unused_violations(self) -> list[Violation]:
        """Get only the unused dependency declaration violations."""
        return [v for v in self.violations if v.violation_type == "unused"]


class DependencyValidator:
    """Validates module dependencies against configuration rules."""

    def __init__(self, config: DependencyConfig):
        self.config = config
        self.parser = ImportParser(config.src_root)

    def validate_all(self) -> ValidationResult:
        """Validate all modules in the project."""
        violations = []
        files_checked = 0
        modules_checked = set()

        for module in self.config.get_all_modules():
            module_result = self.validate_module(module)
            violations.extend(module_result.violations)
            files_checked += module_result.files_checked
            modules_checked.update(module_result.modules_checked)

        return ValidationResult(
            violations=violations,
            files_checked=files_checked,
            modules_checked=modules_checked
        )

    def validate_module(self, module_name: str) -> ValidationResult:
        """Validate a specific module."""
        violations = []
        files_checked = 0

        module_path = os.path.join(self.config.src_root, module_name)
        if not os.path.exists(module_path):
            return ValidationResult(
                violations=[],
                files_checked=0,
                modules_checked=set()
            )

        python_files = self.parser.get_python_files(module_path, self.config.ignore_patterns)
        allowed_deps = self.config.get_allowed_dependencies(module_name)
        known_modules = self.config.get_all_modules()

        for file_path in python_files:
            file_violations = self._validate_file(
                file_path, module_name, allowed_deps, known_modules
            )
            violations.extend(file_violations)
            files_checked += 1

        actual_deps = self._collect_actual_deps(
            module_name, python_files, known_modules
        )
        unused_violations = self._check_unused_dependencies(module_name, actual_deps)
        violations.extend(unused_violations)

        return ValidationResult(
            violations=violations,
            files_checked=files_checked,
            modules_checked={module_name}
        )

    def _validate_file(
        self,
        file_path: str,
        current_module: str,
        allowed_deps: set[str],
        known_modules: set[str]
    ) -> list[Violation]:
        """Validate dependencies in a single file."""
        violations = []
        imports = self.parser.parse_file(file_path)

        for import_info in imports:
            # Check if it's an internal module dependency
            target_module = self.parser.resolve_module_name(
                import_info, file_path, known_modules
            )

            if target_module is not None:
                # This is an internal module dependency
                # Skip self-imports (imports within the same module)
                if target_module == current_module:
                    continue

                # Check if this dependency is allowed
                if target_module not in allowed_deps:
                    violations.append(Violation(
                        file_path=file_path,
                        line_number=import_info.line_number,
                        importing_module=current_module,
                        imported_module=target_module,
                        import_statement=import_info.raw_statement,
                        rule_description=f"{current_module} cannot depend on {target_module}",
                        violation_type="internal"
                    ))

            else:
                # This might be an external dependency
                if self.parser.is_third_party_import(import_info.module, known_modules):
                    external_module = self.parser.get_external_module_name(import_info)

                    # Check if this external dependency is allowed for this module
                    if not self.config.is_external_dependency_allowed(current_module, external_module):
                        violations.append(Violation(
                            file_path=file_path,
                            line_number=import_info.line_number,
                            importing_module=current_module,
                            imported_module=external_module,
                            import_statement=import_info.raw_statement,
                            rule_description=f"{current_module} cannot depend on external module {external_module}",
                            violation_type="external"
                        ))

        return violations

    def _collect_actual_deps(
        self,
        module_name: str,
        python_files: list[str],
        known_modules: set[str]
    ) -> tuple[set[str], set[str]]:
        """Collect the set of internal and external deps actually used by a module."""
        actual_internal: set[str] = set()
        actual_external: set[str] = set()

        for file_path in python_files:
            imports = self.parser.parse_file(file_path)

            for import_info in imports:
                target_module = self.parser.resolve_module_name(
                    import_info, file_path, known_modules
                )

                if target_module is not None:
                    if target_module != module_name:
                        actual_internal.add(target_module)

                elif self.parser.is_third_party_import(import_info.module, known_modules):
                    external_module = self.parser.get_external_module_name(import_info)
                    actual_external.add(external_module)

        return actual_internal, actual_external

    def _check_unused_dependencies(
        self,
        module_name: str,
        actual_deps: tuple[set[str], set[str]]
    ) -> list[Violation]:
        """Check for declared dependencies that are not actually used."""
        violations: list[Violation] = []
        actual_internal, actual_external = actual_deps

        if module_name not in self.config.modules:
            return violations

        module_config = self.config.modules[module_name]

        for dep in module_config.internal_dependencies:
            if dep not in actual_internal:
                violations.append(Violation(
                    file_path="",
                    line_number=0,
                    importing_module=module_name,
                    imported_module=dep,
                    import_statement="",
                    rule_description=f"{module_name} declares unused internal dependency '{dep}'",
                    violation_type="unused"
                ))

        special_markers = {"standard_library", "third_party"}

        for dep in module_config.external_dependencies:
            if dep in special_markers:
                continue

            if dep in actual_external:
                continue

            if dep.endswith("*"):
                prefix = dep[:-1]
                if any(name.startswith(prefix) for name in actual_external):
                    continue

            violations.append(Violation(
                file_path="",
                line_number=0,
                importing_module=module_name,
                imported_module=dep,
                import_statement="",
                rule_description=f"{module_name} declares unused external dependency '{dep}'",
                violation_type="unused"
            ))

        return violations

    def validate_file(self, file_path: str) -> ValidationResult:
        """Validate a specific file."""
        # Determine which module this file belongs to
        file_path_obj = Path(file_path)

        try:
            rel_path = file_path_obj.relative_to(self.config.src_root)
            module_name = rel_path.parts[0]  # First directory is the module

        except (ValueError, IndexError):
            # File is not in src directory or path structure is unexpected
            return ValidationResult(violations=[], files_checked=0, modules_checked=set())

        if module_name not in self.config.get_all_modules():
            return ValidationResult(violations=[], files_checked=0, modules_checked=set())

        allowed_deps = self.config.get_allowed_dependencies(module_name)
        known_modules = self.config.get_all_modules()

        violations = self._validate_file(file_path, module_name, allowed_deps, known_modules)

        return ValidationResult(
            violations=violations,
            files_checked=1,
            modules_checked={module_name}
        )
