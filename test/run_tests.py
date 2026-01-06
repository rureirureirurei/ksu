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
        expected = match.group(1).strip()
        # Convert \n escape sequences to actual newlines
        expected = expected.replace('\\n', '\n')
        return expected
    else:
        raise ValueError(f"No expected result found in first line: {first_line}")

def run_ksu_file(file_path):
    """Compile a KSU file to C, build it with gcc, run it, and return stdout."""
    try:
        # 1) Generate C from ksu
        gen = subprocess.run(
            ['dune', 'exec', 'ksu', '--', str(file_path)],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent
        )
        if gen.returncode != 0:
            return f"ERROR: {gen.stderr.strip()}"

        c_code = gen.stdout

        # 2) Compile and run in an isolated temp dir
        import tempfile
        with tempfile.TemporaryDirectory() as td:
            c_path = Path(td) / 'test.c'
            exe_path = Path(td) / 'a.out'
            c_path.write_text(c_code)

            # Get project root directory
            project_root = Path(__file__).parent.parent
            runtime_c = project_root / 'src' / 'Runtime' / 'ksu_runtime.c'
            runtime_include = project_root / 'src' / 'Runtime'

            comp = subprocess.run(
                ['gcc', str(c_path), str(runtime_c), '-I', str(runtime_include), '-o', str(exe_path)],
                capture_output=True,
                text=True,
            )
            if comp.returncode != 0:
                return f"ERROR: C compile failed: {comp.stderr.strip()}"

            run = subprocess.run(
                [str(exe_path)],
                capture_output=True,
                text=True,
            )
            if run.returncode != 0:
                return f"ERROR: Program exited with {run.returncode}: {run.stderr.strip()}"

            return run.stdout.strip()

    except subprocess.CalledProcessError as e:
        return f"ERROR: Command failed with return code {e.returncode}"
    except FileNotFoundError as e:
        return f"ERROR: command not found: {e}"

def run_tests():
    """Run all tests and report results."""
    test_dirs = [
            'test/callcc',
            'test/generic',
            'test/lists',
            'test/closures',
            'test/state',
            ]
    total_tests = 0
    passed_tests = 0
    failed_tests = []

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
                    print(f"  PASS {file_path.name}")
                    passed_tests += 1
                else:
                    print(f"  FAIL {file_path.name}")
                    failed_tests.append(file_path.name)                    
            except Exception as e:
                print(f"  ERROR {file_path.name}: {e}")
                failed_tests.append(file_path.name)
    
    color = '\033[32m' if passed_tests == total_tests else '\033[31m'
    print(f"\n{color}Test Results: {passed_tests}/{total_tests} passed\033[0m")
    if passed_tests != total_tests:
        print("\033[31mYou bring great dishonor to your ancestors! Commit seppuku to restore your family's honor!\033[0m")
    
    if failed_tests:
        # print(f"\nFailed tests ({len(failed_tests)}):")
        # for test_name in failed_tests:
        #     print(f"  {test_name}")
        return 1
    else:
        print("All tests passed!")
        return 0

if __name__ == "__main__":
    sys.exit(run_tests()) 