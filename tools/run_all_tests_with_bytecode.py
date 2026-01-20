#!/usr/bin/env python3
"""
Run all AIFPL tests with bytecode enabled by temporarily modifying the fixture.

This script:
1. Backs up conftest.py
2. Modifies the aifpl fixture to use bytecode
3. Runs all tests
4. Restores conftest.py
"""

import sys
import subprocess
import shutil
from pathlib import Path

def main():
    """Run all tests with bytecode."""
    
    # Paths
    root = Path(__file__).parent.parent
    conftest_path = root / "tests" / "aifpl" / "conftest.py"
    backup_path = conftest_path.with_suffix(".py.backup")
    
    print("=" * 80)
    print("RUNNING ALL AIFPL TESTS WITH BYTECODE")
    print("=" * 80)
    print()
    
    try:
        # Backup conftest.py
        print("📦 Backing up conftest.py...")
        shutil.copy2(conftest_path, backup_path)
        
        # Read current conftest
        content = conftest_path.read_text()
        
        # Modify the aifpl fixture to use bytecode
        modified_content = content.replace(
            'def aifpl():\n    """Create a fresh AIFPL instance for each test."""\n    return AIFPL()',
            'def aifpl():\n    """Create a fresh AIFPL instance for each test."""\n    return AIFPL(use_bytecode=True)'
        )
        
        if content == modified_content:
            print("❌ Failed to modify conftest.py - pattern not found")
            return 1
        
        print("✏️  Modified aifpl fixture to use bytecode")
        conftest_path.write_text(modified_content)
        
        # Run tests
        print("\n" + "=" * 80)
        print("RUNNING TESTS...")
        print("=" * 80)
        print()
        
        cmd = [
            "pytest",
            "tests/aifpl/",
            "-v",
            "--tb=line",  # Shorter traceback
            # Don't stop on first failure - collect all failures
        ]
        
        result = subprocess.run(cmd, cwd=root)
        
        return result.returncode
        
    finally:
        # Restore conftest.py
        print("\n" + "=" * 80)
        print("📦 Restoring conftest.py...")
        if backup_path.exists():
            shutil.copy2(backup_path, conftest_path)
            backup_path.unlink()
            print("✅ Restored conftest.py")
        print("=" * 80)


if __name__ == "__main__":
    sys.exit(main())
