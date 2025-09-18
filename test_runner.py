#!/usr/bin/env python3
"""Simple test runner to track progress on AIFPL test fixes."""

import subprocess
import sys

def run_aifpl_tests():
    """Run AIFPL tests and return summary."""
    try:
        result = subprocess.run([
            sys.executable, "-m", "pytest", "tests/aifpl/", "-v", "--tb=short"
        ], capture_output=True, text=True, cwd=".")
        
        output = result.stdout + result.stderr
        
        # Extract summary line
        lines = output.split('\n')
        summary_line = None
        for line in reversed(lines):
            if "failed" in line and "passed" in line:
                summary_line = line.strip()
                break
        
        print("=== AIFPL Test Summary ===")
        if summary_line:
            print(summary_line)
        else:
            print("Could not extract summary")
        
        print(f"\nReturn code: {result.returncode}")
        
        # Show failed test names
        failed_tests = []
        for line in lines:
            if line.startswith("FAILED tests/aifpl/"):
                test_name = line.split(" ")[1]
                failed_tests.append(test_name)
        
        if failed_tests:
            print(f"\nFailed tests ({len(failed_tests)}):")
            for test in failed_tests[:10]:  # Show first 10
                print(f"  {test}")
            if len(failed_tests) > 10:
                print(f"  ... and {len(failed_tests) - 10} more")
        
        return result.returncode == 0
        
    except Exception as e:
        print(f"Error running tests: {e}")
        return False

if __name__ == "__main__":
    run_aifpl_tests()