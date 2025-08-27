#!/usr/bin/env python3
"""
KSU Benchmark Runner

This script compiles and runs all .ksu files under benchmark/examples.
It measures timings for:
- ksu code generation
- C compilation
- program execution

Outputs a simple table per file.
"""

import subprocess
import time
from pathlib import Path
import tempfile
import sys

ROOT = Path(__file__).resolve().parent.parent
EXAMPLES_DIR = ROOT / 'benchmark' / 'examples'


def run_command(cmd, cwd=None, input_text=None):
    start = time.perf_counter()
    proc = subprocess.run(
        cmd,
        cwd=cwd,
        input=input_text,
        text=True,
        capture_output=True,
    )
    end = time.perf_counter()
    return proc, end - start


def benchmark_file(file_path: Path):
    # 1) Generate C from ksu
    gen_proc, gen_time = run_command(['dune', 'exec', 'ksu', '--', str(file_path)], cwd=ROOT)
    if gen_proc.returncode != 0:
        return {
            'name': file_path.name,
            'status': 'GEN_ERROR',
            'gen_time_s': gen_time,
            'compile_time_s': None,
            'run_time_s': None,
            'stdout': '',
            'stderr': gen_proc.stderr.strip(),
        }
    c_code = gen_proc.stdout

    # 2) Compile and 3) Run
    with tempfile.TemporaryDirectory() as td:
        c_path = Path(td) / 'bench.c'
        exe_path = Path(td) / 'a.out'
        c_path.write_text(c_code)

        comp_proc, comp_time = run_command(['gcc', str(c_path), '-O2', '-o', str(exe_path)])
        if comp_proc.returncode != 0:
            return {
                'name': file_path.name,
                'status': 'COMPILE_ERROR',
                'gen_time_s': gen_time,
                'compile_time_s': comp_time,
                'run_time_s': None,
                'stdout': '',
                'stderr': comp_proc.stderr.strip(),
            }

        run_proc, run_time = run_command([str(exe_path)])
        status = 'OK' if run_proc.returncode == 0 else f'RUN_ERROR({run_proc.returncode})'
        return {
            'name': file_path.name,
            'status': status,
            'gen_time_s': gen_time,
            'compile_time_s': comp_time,
            'run_time_s': run_time,
            'stdout': run_proc.stdout.strip(),
            'stderr': run_proc.stderr.strip(),
        }


def main():
    files = sorted(EXAMPLES_DIR.glob('*.ksu'))
    if not files:
        print(f"No .ksu files found in {EXAMPLES_DIR}")
        return 1

    print(f"Benchmarking {len(files)} file(s) in {EXAMPLES_DIR} (times in seconds):")
    print("-" * 72)
    print(f"{'file':30} {'gen':>10} {'compile':>10} {'run':>10} {'status':>10}")
    print("-" * 72)

    exit_code = 0
    for f in files:
        res = benchmark_file(f)
        if res['status'] != 'OK':
            exit_code = 1
        gt = f"{res['gen_time_s']:.4f}" if res['gen_time_s'] is not None else '-'
        ct = f"{res['compile_time_s']:.4f}" if res['compile_time_s'] is not None else '-'
        rt = f"{res['run_time_s']:.4f}" if res['run_time_s'] is not None else '-'
        print(f"{res['name']:30} {gt:>10} {ct:>10} {rt:>10} {res['status']:>10}")

    print("-" * 72)
    return exit_code


if __name__ == '__main__':
    sys.exit(main())
