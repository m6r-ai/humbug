#!/usr/bin/env python3
"""
Run a specific AIFPL test file with bytecode enabled by temporarily modifying the fixture.

Usage:
    python tools/run_one_test_with_bytecode.py tests/aifpl/test_arithmetic.py
    python tools/run_one_test_with_bytecode.py tests/aifpl/test_arithmetic.py::TestArithmetic::test_bitwise_operations

This script:
1. Backs up conftest.py
2. Modifies the aifpl fixture to use bytecode
3. Runs the specified test(s)
4. Restores conftest.py
"""

import sys
import subprocess
import shutil
from pathlib import Path

def main():
    """Run specific test(s) with bytecode."""
    
    if len(sys.argv) < 2:
        print("Usage: python tools/run_one_test_with_bytecode.py <test_path>")
        print("Example: python tools/run_one_test_with_bytecode.py tests/aifpl/test_arithmetic.py")
        return 1
    
    test_path = sys.argv[1]
    extra_args = sys.argv[2:]  # Pass through any additional pytest args
    
    # Paths
    root = Path(__file__).parent.parent
    conftest_path = root / "tests" / "aifpl" / "conftest.py"
    backup_path = conftest_path.with_suffix(".py.backup")
    
    print("=" * 80)
    print(f"RUNNING TEST WITH BYTECODE: {test_path}")
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
            test_path,
            "-v",
        ] + extra_args
        
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
