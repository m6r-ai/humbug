"""
Configuration management for dependency checker.
"""

import os
import yaml
from pathlib import Path
from typing import Dict, List, Set
from dataclasses import dataclass, field


@dataclass
class ModuleConfig:
    """Configuration for a single module."""
    internal_dependencies: List[str] = field(default_factory=list)
    external_dependencies: List[str] = field(default_factory=list)


@dataclass
class DependencyConfig:
    """Configuration for module dependency rules."""

    modules: Dict[str, ModuleConfig] = field(default_factory=dict)
    ignore_patterns: List[str] = field(default_factory=list)
    src_root: str = "src"

    @classmethod
    def load_from_file(cls, config_path: str) -> 'DependencyConfig':
        """Load configuration from YAML file."""
        if not os.path.exists(config_path):
            raise FileNotFoundError(f"Configuration file not found: {config_path}")

        with open(config_path, 'r') as f:
            data = yaml.safe_load(f)

        # Parse modules
        modules = {}
        for module_name, module_data in data.get('modules', {}).items():
            modules[module_name] = ModuleConfig(
                internal_dependencies=module_data.get('internal_dependencies', []),
                external_dependencies=module_data.get('external_dependencies', [])
            )

        return cls(
            modules=modules,
            ignore_patterns=data.get('ignore_patterns', []),
            src_root=data.get('src_root', 'src')
        )

    @classmethod
    def create_default(cls, src_root: str = "src") -> 'DependencyConfig':
        """Create a default configuration by discovering modules."""
        modules = {}

        if os.path.exists(src_root):
            for item in os.listdir(src_root):
                item_path = os.path.join(src_root, item)
                if os.path.isdir(item_path) and not item.startswith('.'):
                    # Create empty module config - user will need to define dependencies
                    modules[item] = ModuleConfig(
                        internal_dependencies=[],
                        external_dependencies=["standard_library"]  # Safe default
                    )

        return cls(
            modules=modules,
            ignore_patterns=["*/test_*.py", "*/tests/*", "*/__pycache__/*"],
            src_root=src_root
        )

    def save_to_file(self, config_path: str) -> None:
        """Save configuration to YAML file."""
        modules_data = {}
        for module_name, module_config in self.modules.items():
            modules_data[module_name] = {
                'internal_dependencies': module_config.internal_dependencies,
                'external_dependencies': module_config.external_dependencies
            }

        data = {
            'modules': modules_data,
            'ignore_patterns': self.ignore_patterns,
            'src_root': self.src_root
        }

        with open(config_path, 'w') as f:
            yaml.dump(data, f, default_flow_style=False, sort_keys=True)

    def get_allowed_dependencies(self, module: str) -> Set[str]:
        """Get the set of internal modules that the given module is allowed to depend on."""
        if module in self.modules:
            return set(self.modules[module].internal_dependencies)
        return set()

    def get_allowed_external_dependencies(self, module: str) -> Set[str]:
        """Get the set of external dependencies allowed for a specific module."""
        if module in self.modules:
            return set(self.modules[module].external_dependencies)
        return set()

    def is_external_dependency_allowed(self, module: str, external_module: str) -> bool:
        """Check if an external dependency is allowed for a specific module."""
        allowed = self.get_allowed_external_dependencies(module)

        # If no external dependencies defined, deny everything (secure by default)
        if not allowed:
            return False

        # Check exact match
        if external_module in allowed:
            return True

        # Check pattern matches
        for allowed_pattern in allowed:
            if self._matches_pattern(external_module, allowed_pattern):
                return True

        return False

    def _matches_pattern(self, module_name: str, pattern: str) -> bool:
        """Check if a module name matches a pattern."""
        if pattern == "standard_library":
            # This would need to be enhanced with actual stdlib detection
            return False
        elif pattern == "third_party":
            # This is a catch-all for non-stdlib modules
            return True
        elif pattern.endswith("*"):
            # Wildcard pattern matching
            prefix = pattern[:-1]
            return module_name.startswith(prefix)
        else:
            # Exact match
            return module_name == pattern

    def get_all_modules(self) -> Set[str]:
        """Get all defined modules."""
        return set(self.modules.keys())

    def validate(self) -> List[str]:
        """Validate the configuration and return any errors."""
        errors = []

        all_modules = self.get_all_modules()

        # Validate internal module dependencies
        for module_name, module_config in self.modules.items():
            for dep in module_config.internal_dependencies:
                if dep not in all_modules:
                    errors.append(f"Module '{module_name}' depends on undefined module '{dep}'")

        # Check for circular dependencies
        visited = set()
        rec_stack = set()

        def has_cycle(node: str) -> bool:
            if node in rec_stack:
                return True
            if node in visited:
                return False

            visited.add(node)
            rec_stack.add(node)

            if node in self.modules:
                for neighbor in self.modules[node].internal_dependencies:
                    if has_cycle(neighbor):
                        return True

            rec_stack.remove(node)
            return False

        for module in all_modules:
            if module not in visited:
                if has_cycle(module):
                    errors.append(f"Circular dependency detected involving module '{module}'")

        return errors
