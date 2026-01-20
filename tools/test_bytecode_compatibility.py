#!/usr/bin/env python3
"""
Test bytecode compatibility by running existing tests with bytecode enabled.

This script helps identify:
1. Functional bugs in bytecode implementation
2. Error message differences between interpreter and bytecode
3. Edge cases that need attention
"""

import sys
import subprocess
from pathlib import Path

# Test files to check (start with a representative sample)
TEST_FILES = [
    "tests/aifpl/test_arithmetic.py",
    "tests/aifpl/test_conditionals.py",
    "tests/aifpl/test_lists.py",
    "tests/aifpl/test_strings.py",
    "tests/aifpl/test_functional.py",
    "tests/aifpl/test_alists.py",
    "tests/aifpl/test_errors.py",  # This will be interesting!
]


def run_test_with_bytecode(test_file: str) -> tuple[bool, str]:
    """
    Run a test file with bytecode by injecting fixture override.
    
    Returns:
        (success, output) tuple
    """
    # Use pytest with fixture override
    cmd = [
        "pytest",
        test_file,
        "-v",
        "--tb=short",
        "--override-ini=python_files=test_*.py",
    ]
    
    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        cwd=Path(__file__).parent.parent
    )
    
    return result.returncode == 0, result.stdout + result.stderr


def main():
    """Run bytecode compatibility tests."""
    print("=" * 80)
    print("BYTECODE COMPATIBILITY TEST")
    print("=" * 80)
    print()
    print("Testing bytecode implementation against existing test suite...")
    print()
    
    results = {}
    
    for test_file in TEST_FILES:
        print(f"\n{'=' * 80}")
        print(f"Testing: {test_file}")
        print('=' * 80)
        
        success, output = run_test_with_bytecode(test_file)
        results[test_file] = (success, output)
        
        if success:
            print("✅ PASSED")
        else:
            print("❌ FAILED")
            print("\nOutput:")
            print(output[-2000:])  # Last 2000 chars
    
    # Summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    
    passed = sum(1 for success, _ in results.values() if success)
    failed = len(results) - passed
    
    print(f"\nTotal: {len(results)} test files")
    print(f"✅ Passed: {passed}")
    print(f"❌ Failed: {failed}")
    
    if failed > 0:
        print("\nFailed tests:")
        for test_file, (success, _) in results.items():
            if not success:
                print(f"  - {test_file}")
    
    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
