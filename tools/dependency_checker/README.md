# Python Module Dependency Checker

A comprehensive tool for enforcing inter-module dependency rules in Python projects. This tool helps maintain clean architecture by ensuring modules only depend on explicitly allowed other modules and external packages.

## Features

- **Unified Module Configuration**: Define both internal and external dependencies for each module in one place
- **Internal Module Control**: Manage dependencies between your project's modules
- **External Dependency Control**: Fine-grained control over third-party package usage
- **Secure by Default**: External dependencies must be explicitly allowed
- **Comprehensive Analysis**: Parses all Python files to extract import statements
- **Multiple Output Formats**: Text, JSON, and CSV output formats
- **CI/CD Integration**: Returns appropriate exit codes for automated pipelines
- **Detailed Reporting**: Clear violation reports with file locations and line numbers
- **Pattern Matching**: Support for wildcard patterns and ignore rules

## Installation

The tool requires Python 3.7+ and PyYAML:

```bash
pip install PyYAML>=6.0
```

## Quick Start

1. **Initialize configuration**:
   ```bash
   python check-dependencies.py init
   ```

2. **Edit the generated `dependency-rules.yaml`** to define your dependency rules

3. **Run dependency check**:
   ```bash
   python check-dependencies.py check
   ```

## Configuration

The tool uses a YAML configuration file (`dependency-rules.yaml` by default) where each module defines both its internal and external dependencies in one place:

### Complete Configuration Example

```yaml
modules:
  # Top-level module can depend on all others
  humbug:
    internal_dependencies:
      - ai
      - syntax
      - dmarkdown
    external_dependencies:
      - "standard_library"
      - "PySide6"
      - "PyQt*"             # Wildcard patterns supported
      - "requests"
      - "third_party"       # Allow any other packages (use sparingly)
  
  # Mid-level modules with restricted dependencies
  ai:
    internal_dependencies:
      - syntax
      - dmarkdown
    external_dependencies:
      - "standard_library"
      - "openai"
      - "anthropic"
      - "aiohttp"
      - "requests"
  
  # Low-level modules with minimal dependencies
  syntax:
    internal_dependencies: []  # No internal dependencies
    external_dependencies:
      - "standard_library"
      - "pygments"          # Specific packages allowed
  
  dmarkdown:
    internal_dependencies: []
    external_dependencies:
      - "standard_library"
      - "markdown"
      - "mistune"
  
  # Modules with no external dependencies defined cannot use any external packages

# File patterns to ignore
ignore_patterns:
  - "*/test_*.py"
  - "*/tests/*"
  - "*/__pycache__/*"

# Source root directory
src_root: "src"
```

### Configuration Benefits

This unified structure provides several advantages:

- **Everything in one place**: All dependencies for a module are defined together
- **Clear overview**: Easy to see what each module can and cannot depend on
- **Maintainable**: When adding a new module, define everything in one section
- **Less error-prone**: No risk of forgetting to define external rules for a module

### External Dependency Control

#### Secure by Default Approach

- **Not defined** = No external dependencies allowed
- **Empty list** = No external dependencies allowed  
- **Listed packages** = Only those packages allowed

#### Pattern Types

**Exact Match:**
```yaml
external_dependencies:
  - "requests"          # Exactly "requests" package
```

**Wildcard Patterns:**
```yaml
external_dependencies:
  - "PyQt*"            # PyQt5, PyQt6, etc.
  - "django*"          # django, django-rest-framework, etc.
```

**Special Patterns:**
```yaml
external_dependencies:
  - "standard_library"  # All Python standard library modules
  - "third_party"       # Any third-party package (use sparingly)
```

#### Architecture Examples

**Layer Separation:**
```yaml
modules:
  # UI layer can use GUI frameworks
  ui:
    internal_dependencies: [business, data]
    external_dependencies:
      - "standard_library"
      - "PySide6"
      - "PyQt*"
  
  # Business logic - framework agnostic
  business:
    internal_dependencies: [data]
    external_dependencies:
      - "standard_library"
  
  # Data layer - database only
  data:
    internal_dependencies: []
    external_dependencies:
      - "standard_library"
      - "sqlalchemy"
      - "psycopg2"
```

**Microservice Architecture:**
```yaml
modules:
  api:
    internal_dependencies: [auth, database]
    external_dependencies:
      - "standard_library"
      - "fastapi"
      - "uvicorn"
      - "pydantic"
  
  database:
    internal_dependencies: []
    external_dependencies:
      - "standard_library"
      - "sqlalchemy"
      - "psycopg2"
  
  auth:
    internal_dependencies: []
    external_dependencies:
      - "standard_library"
      - "jwt"
      - "bcrypt"
```

## Usage

### Command Line Interface

```bash
# Check all modules
python check-dependencies.py check

# Check specific module
python check-dependencies.py check --module humbug

# Generate JSON report
python check-dependencies.py check --format json --output report.json

# Use custom configuration file
python check-dependencies.py check --config my-rules.yaml

# Validate configuration
python check-dependencies.py validate-config

# Generate dependency graph
python check-dependencies.py graph --output deps.dot

# Show project statistics
python check-dependencies.py stats
```

### Python API

```python
from dependency_checker import DependencyConfig, DependencyValidator, DependencyReporter

# Load configuration
config = DependencyConfig.load_from_file('dependency-rules.yaml')

# Run validation
validator = DependencyValidator(config)
result = validator.validate_all()

# Generate report
reporter = DependencyReporter()
reporter.print_results(result)

# Check for violations
if result.has_violations:
    print(f"Found {result.violation_count} violations")
    print(f"Internal: {len(result.internal_violations)}")
    print(f"External: {len(result.external_violations)}")

# Test specific external dependency
allowed = config.is_external_dependency_allowed("ai", "requests")
print(f"AI can use requests: {allowed}")

# Save reports in different formats
reporter.save_results(result, "violations.json", "json")
reporter.save_results(result, "violations.csv", "csv")
```

## Output Formats

### Text Output (Default)
```
Dependency Check Results
========================

✓ syntax/ - checked, no violations
✓ dmarkdown/ - checked, no violations
✗ ai/ - 2 violation(s) found (1 internal, 1 external)

Internal Module Violations:
---------------------------
src/ai/models.py:5
  └─ 🔗 Illegal import: from humbug.logger import Logger
     Rule: ai cannot depend on humbug

External Dependency Violations:
-------------------------------
src/ai/client.py:3
  └─ 🔒 Illegal import: import PySide6
     Rule: ai cannot depend on external module PySide6

Summary:
  Files checked: 45
  Modules checked: 4
  Total violations: 2
  Internal violations: 1
  External violations: 1
  Status: ✗ FAILED - 2 violation(s)
```

### JSON Output
```json
{
  "summary": {
    "files_checked": 45,
    "modules_checked": ["ai", "syntax", "dmarkdown", "humbug"],
    "violation_count": 2,
    "internal_violation_count": 1,
    "external_violation_count": 1,
    "has_violations": true
  },
  "violations": [
    {
      "file_path": "src/ai/models.py",
      "line_number": 5,
      "importing_module": "ai",
      "imported_module": "humbug",
      "import_statement": "from humbug.logger import Logger",
      "rule_description": "ai cannot depend on humbug",
      "violation_type": "internal"
    }
  ]
}
```

## Testing and Exploration

### Test Your Configuration

```bash
# Validate your rules
python check-dependencies.py validate-config

# See what modules were discovered
python check-dependencies.py stats

# Test specific module
python check-dependencies.py check --module core --verbose
```

### Debug Import Detection

```python
from dependency_checker import ImportParser, DependencyConfig

# Test import detection
parser = ImportParser("src")
config = DependencyConfig.load_from_file("dependency-rules.yaml")

imports = parser.parse_file("src/ai/models.py")
for imp in imports:
    is_stdlib = parser.is_standard_library_import(imp.module)
    is_third_party = parser.is_third_party_import(imp.module, config.get_all_modules())
    is_allowed = config.is_external_dependency_allowed("ai", imp.module)
    
    print(f"Line {imp.line_number}: {imp.raw_statement}")
    print(f"  Module: {imp.module}")
    print(f"  Standard library: {is_stdlib}")
    print(f"  Third-party: {is_third_party}")
    print(f"  Allowed for 'ai': {is_allowed}")
```

## Integration

### Pre-commit Hook

Add to `.pre-commit-config.yaml`:

```yaml
repos:
  - repo: local
    hooks:
      - id: dependency-check
        name: Check module dependencies
        entry: python check-dependencies.py check
        language: system
        pass_filenames: false
```

### GitHub Actions

```yaml
name: Dependency Check
on: [push, pull_request]

jobs:
  dependency-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Set up Python
        uses: actions/setup-python@v2
        with:
          python-version: '3.8'
      - name: Install dependencies
        run: pip install PyYAML
      - name: Check dependencies
        run: python check-dependencies.py check
```

### Make Target

```makefile
.PHONY: check-deps
check-deps:
	python check-dependencies.py check

.PHONY: check-deps-json
check-deps-json:
	python check-dependencies.py check --format json --output dependency-report.json
```

## Advanced Usage

### Custom Module Discovery

The tool automatically discovers modules by scanning the `src_root` directory. Each subdirectory is treated as a module.

### Relative Imports

The tool correctly handles relative imports and maps them back to your defined modules:

```python
# In src/ai/models.py
from ..syntax import Parser  # Maps to 'syntax' module
from .utils import helper    # Internal to 'ai' module (allowed)
```

### Import Detection

The tool automatically detects and categorizes imports:

- **Standard library**: Built-in Python modules (comprehensive list included)
- **Third-party**: Packages installed via pip that aren't part of your project  
- **Internal**: Your project's modules defined in the configuration

### Ignore Patterns

Use glob-style patterns to ignore files:

```yaml
ignore_patterns:
  - "*/test_*.py"      # All test files
  - "src/*/legacy/*"   # Legacy code directories
  - "*.generated.py"   # Generated files
  - "*/build/*"        # Build directories
```

## Best Practices

### 1. Start Simple, Iterate

```yaml
# Phase 1: Basic structure
modules:
  ui:
    internal_dependencies: [core]
    external_dependencies: ["standard_library"]
  core:
    internal_dependencies: []
    external_dependencies: ["standard_library"]

# Phase 2: Add specific external dependencies
modules:
  ui:
    internal_dependencies: [core]
    external_dependencies: ["standard_library", "PySide6"]
  core:
    internal_dependencies: []
    external_dependencies: ["standard_library"]

# Phase 3: Fine-tune as needed
```

### 2. Secure by Default

```yaml
# Only explicitly allow what's needed
modules:
  ui:
    internal_dependencies: [core]
    external_dependencies:
      - "standard_library"
      - "PySide6"          # Only specific GUI framework
  
  core:
    internal_dependencies: []
    external_dependencies:
      - "standard_library"  # Only stdlib - no external deps
```

### 3. Document Your Architecture

```yaml
# Use comments to explain design decisions
modules:
  # API layer needs web framework and serialization
  api:
    internal_dependencies: [business, data]
    external_dependencies:
      - "standard_library"
      - "fastapi"          # Web framework
      - "pydantic"         # Data validation
    
  # Core business logic should be framework-agnostic
  business:
    internal_dependencies: [data]
    external_dependencies:
      - "standard_library"  # No external frameworks
```

### 4. Layer-Based Organization

```yaml
modules:
  # Top layer can depend on all others
  ui:
    internal_dependencies: [business, data, utils]
    external_dependencies: ["standard_library", "PySide6"]
  
  # Business layer cannot depend on UI
  business:
    internal_dependencies: [data, utils]
    external_dependencies: ["standard_library"]
  
  # Data layer is independent
  data:
    internal_dependencies: [utils]
    external_dependencies: ["standard_library", "sqlalchemy"]
  
  # Utils are foundational
  utils:
    internal_dependencies: []
    external_dependencies: ["standard_library"]
```

### 5. CI/CD Integration

- Run checks on every commit
- Use JSON output for automated analysis
- Set up notifications for violations
- Block merges with dependency violations

## Troubleshooting

### Common Issues

1. **"Configuration file not found"**
   - Run `python check-dependencies.py init` to create the configuration file

2. **"Module not found in configuration"**
   - Add the module to your `dependency-rules.yaml` file
   - Ensure the module directory exists in your `src_root`

3. **"False positive violations"**
   - Add appropriate patterns to `ignore_patterns`
   - Check if the import is actually needed

4. **"Configuration validation failed"**
   - Run `python check-dependencies.py validate-config` for details
   - Ensure all referenced modules are defined
   - Check for circular dependencies

5. **"External dependency not detected correctly"**
   - Check if it's in the standard library list (parser.py)
   - Verify the module has `external_dependencies` defined

### Environment-Specific Rules

You can have different configuration files for different environments:

```bash
# Development (more permissive)
python check-dependencies.py check --config dependency-rules-dev.yaml

# Production (more restrictive)  
python check-dependencies.py check --config dependency-rules-prod.yaml
```

## Use Cases

### Architecture Enforcement

- **Layer separation**: Prevent GUI frameworks in business logic layers
- **Service boundaries**: Enforce microservice dependency rules
- **Plugin systems**: Control plugin dependencies

### Security & Compliance

- **Package approval**: Only allow pre-approved packages
- **Vulnerability management**: Block packages with known issues
- **Audit trails**: Track and report all external dependencies

### Performance & Maintenance

- **Lightweight modules**: Prevent heavy dependencies in critical paths
- **Dependency bloat**: Control the growth of external dependencies
- **Refactoring support**: Enforce new architecture during migrations

## Files Overview

- **Core modules**: `config.py`, `parser.py`, `validator.py`, `reporter.py`
- **CLI interface**: `cli.py`, `__main__.py`
- **Documentation**: `README.md` (this file)
- **Utilities**: `utils.py` - helper functions

## Contributing

The tool is designed to be extensible. Key areas for enhancement:

- Additional output formats
- More sophisticated import resolution
- Integration with other tools
- Performance optimizations
- Enhanced pattern matching
- IDE integrations

## License

This tool is part of your project and follows the same license terms.