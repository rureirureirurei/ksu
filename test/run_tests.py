#!/usr/bin/env python3
"""
KSU Test Runner - Parallel execution via ThreadPoolExecutor
"""

import os
import subprocess
import sys
import re
import random
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed

def extract_expected_result(file_path):
    with open(file_path, 'r') as f:
        first_line = f.readline().strip()
    match = re.match(r'^;(.+)$', first_line)
    if match:
        expected = match.group(1).strip()
        return expected.replace('\\n', '\n')
    raise ValueError(f"No expected result found in first line: {first_line}")

def red(s):
    return "\033[31m" + s + "\033[0m"

def green(s):
    return "\033[32m" + s + "\033[0m"

PASSED_MESSAGES = [
    "Tests passed! But this is only beginning of your journey!",
    "Honorable success! However, true warrior does not celebrate!",
    "Most satisfactory! But warrior who rests after one victory dies in second battle!",
]

FAILED_MESSAGES = [
    "Pathetic disgrace! Exile yourself to mountains immediately!",
    "Disgraceful! Your keyboard must be melted down for this dishonor!",
    "Unforgivable! Ancestors demand you delete all code and start life as rice farmer!",
]

PROJECT_ROOT = Path(__file__).parent.parent
KSU_BIN = PROJECT_ROOT / '_build' / 'default' / 'src' / 'ksu.exe'

def run_ksu_file(file_path):
    """Compile and run a single .ksu file, return stdout or error string."""
    try:
        gen = subprocess.run(
            [str(KSU_BIN), str(file_path)],
            capture_output=True, text=True, cwd=PROJECT_ROOT
        )
        if gen.returncode != 0:
            return f"ERROR: {gen.stderr.strip()}"

        import tempfile
        with tempfile.TemporaryDirectory() as td:
            c_path = Path(td) / 'test.c'
            exe_path = Path(td) / 'a.out'
            c_path.write_text(gen.stdout)

            runtime_c = PROJECT_ROOT / 'src' / 'Runtime' / 'ksu_runtime.c'
            runtime_include = PROJECT_ROOT / 'src' / 'Runtime'

            comp = subprocess.run(
                ['gcc', str(c_path), str(runtime_c), '-I', str(runtime_include), '-o', str(exe_path)],
                capture_output=True, text=True,
            )
            if comp.returncode != 0:
                return f"ERROR: C compile failed: {comp.stderr.strip()}"

            run = subprocess.run([str(exe_path)], capture_output=True, text=True)
            if run.returncode != 0:
                return f"ERROR: Program exited with {run.returncode}: {run.stderr.strip()}"
            return run.stdout.strip()

    except subprocess.CalledProcessError as e:
        return f"ERROR: Command failed with return code {e.returncode}"
    except FileNotFoundError as e:
        return f"ERROR: command not found: {e}"

def run_single_test(file_path):
    """Run one test, return (file_path, passed, error_msg)."""
    try:
        expected = extract_expected_result(file_path)
        actual = run_ksu_file(file_path)
        if actual == expected:
            return (file_path, True, None)
        return (file_path, False, f"expected: {expected!r}, got: {actual!r}")
    except Exception as e:
        return (file_path, False, str(e))

def run_tests():
    # Build ksu first
    print("Building ksu...")
    build = subprocess.run(['dune', 'build'], cwd=PROJECT_ROOT, capture_output=True, text=True)
    if build.returncode != 0:
        print(red(f"Build failed: {build.stderr}"))
        return 1

    test_dirs = [
        'test/callcc', 'test/generic', 'test/lists',
        'test/closures', 'test/state', 'test/quote', 'test/errors',
    ]

    # Collect all test files
    all_tests = []
    for test_dir in test_dirs:
        if not os.path.exists(test_dir):
            print(f"Warning: {test_dir} not found")
            continue
        for fp in Path(test_dir).glob("*.ksu"):
            if fp.stat().st_size > 0:
                all_tests.append(fp)

    # Run in parallel
    results = {}
    workers = min(8, os.cpu_count() or 4)
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {executor.submit(run_single_test, fp): fp for fp in all_tests}
        for future in as_completed(futures):
            fp, passed, err = future.result()
            results[fp] = (passed, err)

    # Print grouped by directory
    passed_tests = 0
    failed_tests = []
    for test_dir in test_dirs:
        dir_tests = [fp for fp in all_tests if str(fp).startswith(test_dir)]
        if not dir_tests:
            continue
        print(f"\nTesting {test_dir}:")
        print("-" * 30)
        for fp in sorted(dir_tests):
            passed, err = results[fp]
            if passed:
                print(f"  PASS {fp.name}")
                passed_tests += 1
            else:
                print(red(f"  FAIL {fp.name}"))
                failed_tests.append((fp.name, err))

    total = len(all_tests)
    color = green if passed_tests == total else red
    print(f"\n{color(f'Test Results: {passed_tests}/{total} passed')}")

    if failed_tests:
        print(red(random.choice(FAILED_MESSAGES)))
        for name, err in failed_tests:
            print(f"  {name}: {err}")
        return 1
    print(random.choice(PASSED_MESSAGES))
    return 0

if __name__ == "__main__":
    sys.exit(run_tests())
