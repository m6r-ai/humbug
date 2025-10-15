# AIFPL Patch Tool - Installation and Setup

## Project Structure

The patch tool is now integrated into the Humbug project structure:

```
humbug/
├── src/
│   ├── tools/
│   │   ├── README.md              # Tools overview
│   │   ├── __init__.py
│   │   └── patch/                 # Patch tool
│   │       ├── __init__.py        # Package exports
│   │       ├── __main__.py        # CLI entry point
│   │       ├── patcher.py         # Main patcher class
│   │       ├── diff_parser.py     # Diff parser
│   │       ├── aifpl_bridge.py    # AIFPL bridge
│   │       ├── patch_library.aifpl # AIFPL implementation
│   │       └── docs/              # Documentation
│   │           ├── README.md
│   │           ├── ARCHITECTURE.md
│   │           ├── COMPLETION_REPORT.md
│   │           └── INSTALLATION.md (this file)
│   │
│   └── aifpl/                     # AIFPL interpreter
│
└── tests/
    └── tools/
        └── patch/                 # Patch tool tests
            ├── __init__.py
            ├── test_diff_parser.py
            ├── test_patcher.py
            └── fixtures/
                ├── example.py
                └── example.diff
```

## Installation

No additional installation required! The patch tool is part of the Humbug project.

### Prerequisites

- Python 3.10 or higher
- Humbug project with AIFPL interpreter

### Verify Installation

```bash
# From project root
python -m tools.patch --help
```

You should see the help message with all available options.

## Usage

### Command Line

```bash
# Dry run (safe, no changes)
python -m tools.patch --file myfile.py --patch changes.diff

# Apply patch
python -m tools.patch --file myfile.py --patch changes.diff --apply

# Apply with backup
python -m tools.patch --file myfile.py --patch changes.diff --apply --backup

# Verbose output
python -m tools.patch --file myfile.py --patch changes.diff --verbose

# Custom fuzz range
python -m tools.patch --file myfile.py --patch changes.diff --fuzz 100
```

### As a Library

```python
from tools.patch import UnifiedDiffParser, AIFPLPatchBridge

# Parse diff
parser = UnifiedDiffParser()
filename, hunks = parser.parse_file('changes.diff')

# Read source file
with open('myfile.py', 'r') as f:
    lines = f.read().split('\n')
    if lines and lines[-1] == '':
        lines = lines[:-1]

# Apply patch
bridge = AIFPLPatchBridge(fuzz_range=50)
success, result = bridge.apply_patch(lines, hunks)

if success:
    # Write result
    with open('myfile.py', 'w') as f:
        f.write('\n'.join(result) + '\n')
else:
    print(f"Error: {result}")
```

## Running Tests

```bash
# From project root

# Run all patch tool tests
pytest tests/tools/patch/ -v

# Run specific test file
pytest tests/tools/patch/test_diff_parser.py -v

# Run with coverage
pytest tests/tools/patch/ --cov=tools.patch --cov-report=html
```

## Development

### Project Structure

The tool follows Python best practices:

- **Package structure**: Proper `__init__.py` files for imports
- **Module entry point**: `__main__.py` for CLI execution
- **Relative imports**: Uses relative imports within package
- **Test organization**: Tests mirror source structure
- **Documentation**: Comprehensive docs in `docs/` directory

### Making Changes

1. Edit files in `src/tools/patch/`
2. Run tests: `pytest tests/tools/patch/`
3. Update documentation if needed
4. Test CLI: `python -m tools.patch --help`

### Adding Features

1. Implement in appropriate module (`patcher.py`, `diff_parser.py`, etc.)
2. Add tests in `tests/tools/patch/`
3. Update `__init__.py` if adding new public API
4. Update documentation

## Troubleshooting

### Import Errors

If you get import errors, make sure you're running from the project root:

```bash
cd /path/to/humbug
python -m tools.patch --file myfile.py --patch changes.diff
```

### AIFPL Library Not Found

The tool looks for `patch_library.aifpl` in the same directory as `aifpl_bridge.py`. If you move files, update the path in `aifpl_bridge.py`:

```python
library_path = Path(__file__).parent / "patch_library.aifpl"
```

### Test Failures

Make sure fixture files exist:

```bash
ls tests/tools/patch/fixtures/
# Should show: example.py  example.diff
```

## Integration with Other Tools

The patch tool can be imported and used by other Humbug tools:

```python
from tools.patch import AIFPLPatchBridge

# Use in your tool
bridge = AIFPLPatchBridge()
# ... use bridge methods
```

## Next Steps

- Read [README.md](README.md) for quick start guide
- Read [ARCHITECTURE.md](ARCHITECTURE.md) for design details
- Read [COMPLETION_REPORT.md](COMPLETION_REPORT.md) for development history
- Check [../README.md](../README.md) for tools overview

## Support

For issues or questions:
1. Check the documentation
2. Run tests to verify installation
3. Check import paths and project structure
