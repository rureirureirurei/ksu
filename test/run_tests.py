#!/usr/bin/env python3
"""
KSU Test Runner

This script runs all .ksu test files in test/callcc, test/generic, and test/lists directories
and compares the output with the expected result from the first line comment.
"""

import os
import subprocess
import sys
import re
from pathlib import Path

def extract_expected_result(file_path):
    """Extract expected result from the first line comment."""
    with open(file_path, 'r') as f:
        first_line = f.readline().strip()
    
    # Look for comment starting with ; followed by expected result
    match = re.match(r'^;(.+)$', first_line)
    if match:
        return match.group(1).strip()
    else:
        raise ValueError(f"No expected result found in first line: {first_line}")

def run_ksu_file(file_path):
    """Run a KSU file and return the output."""
    try:
        result = subprocess.run(
            ['dune', 'exec', 'ksu', '--', str(file_path)],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent  # Run from project root
        )
        
        if result.returncode != 0:
            return f"ERROR: {result.stderr.strip()}"
        
        # Get the last line of output (the final result)
        output_lines = result.stdout.strip().split('\n')
        if output_lines:
            return output_lines[-1]  # Return the last line
        else:
            return ""
            
    except subprocess.CalledProcessError as e:
        return f"ERROR: Command failed with return code {e.returncode}"
    except FileNotFoundError:
        return "ERROR: 'dune' command not found"

def run_tests():
    """Run all tests and report results."""
    test_dirs = ['test/callcc', 'test/generic', 'test/lists']
    total_tests = 0
    passed_tests = 0
    failed_tests = []
    
    print("Running KSU tests...")
    print("=" * 50)
    
    for test_dir in test_dirs:
        if not os.path.exists(test_dir):
            print(f"Warning: Test directory {test_dir} not found")
            continue
            
        print(f"\nTesting {test_dir}:")
        print("-" * 30)
        
        for file_path in Path(test_dir).glob("*.ksu"):
            if file_path.stat().st_size == 0:
                print(f"  SKIP {file_path.name} (empty file)")
                continue
                
            total_tests += 1
            
            try:
                expected = extract_expected_result(file_path)
                actual = run_ksu_file(file_path)
                
                if actual == expected:
                    print(f"  PASS {file_path.name}: {actual}")
                    passed_tests += 1
                else:
                    print(f"  FAIL {file_path.name}")
                    print(f"    Expected: {expected}")
                    print(f"    Got:      {actual}")
                    failed_tests.append((file_path.name, expected, actual))
                    
            except Exception as e:
                print(f"  ERROR {file_path.name}: {e}")
                failed_tests.append((file_path.name, "ERROR", str(e)))
    
    print("\n" + "=" * 50)
    print(f"Test Results: {passed_tests}/{total_tests} passed")
    
    if failed_tests:
        print(f"\nFailed tests ({len(failed_tests)}):")
        for test_name, expected, actual in failed_tests:
            print(f"  {test_name}: expected '{expected}', got '{actual}'")
        return 1
    else:
        print("All tests passed!")
        return 0

if __name__ == "__main__":
    sys.exit(run_tests()) 