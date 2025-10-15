# Humbug Tools

This directory contains command-line tools and utilities for the Humbug project.

## Available Tools

### Patch Tool (`tools.patch`)

Intelligent patch application using AIFPL for fuzzy matching. Perfect for applying LLM-generated patches where line numbers may be approximate.

**Quick Start:**
```bash
# Dry run (validate only)
python -m tools.patch --file myfile.py --patch changes.diff

# Apply patch
python -m tools.patch --file myfile.py --patch changes.diff --apply

# Apply with backup
python -m tools.patch --file myfile.py --patch changes.diff --apply --backup
```

**Features:**
- ✨ Intelligent fuzzy matching
- 🔒 Safe dry-run mode by default
- 💾 Backup support
- 🎨 Colored CLI output
- ⚡ Fast AIFPL-powered matching

**Documentation:**
- [README](patch/docs/README.md) - Quick start guide
- [ARCHITECTURE](patch/docs/ARCHITECTURE.md) - System architecture
- [COMPLETION_REPORT](patch/docs/COMPLETION_REPORT.md) - Development status

**Library Usage:**
```python
from tools.patch import UnifiedDiffParser, AIFPLPatchBridge

# Parse diff
parser = UnifiedDiffParser()
filename, hunks = parser.parse_file('changes.diff')

# Apply patch
bridge = AIFPLPatchBridge(fuzz_range=50)
with open('myfile.py', 'r') as f:
    lines = f.read().split('\n')

success, result = bridge.apply_patch(lines, hunks)
if success:
    with open('myfile.py', 'w') as f:
        f.write('\n'.join(result))
```

## Tool Structure

Each tool follows this structure:

```
tools/
└── <tool_name>/
    ├── __init__.py           # Package exports
    ├── __main__.py           # CLI entry point
    ├── <modules>.py          # Implementation
    └── docs/                 # Documentation
        └── README.md
```

## Testing

Tests are located in `tests/tools/<tool_name>/`:

```bash
# Run all tool tests
pytest tests/tools/

# Run specific tool tests
pytest tests/tools/patch/

# Run with coverage
pytest tests/tools/ --cov=tools
```

## Adding New Tools

To add a new tool:

1. Create directory: `src/tools/<tool_name>/`
2. Add `__init__.py` with exports
3. Add `__main__.py` for CLI entry point
4. Implement functionality
5. Add tests in `tests/tools/<tool_name>/`
6. Update this README

## Project Integration

Tools are part of the main Humbug project and can import from:
- `aifpl` - AIFPL interpreter
- `ai` - AI conversation tools
- `humbug` - Core Humbug functionality
- Other project modules

## License

Part of the Humbug project.
