#!/usr/bin/env python3
"""
Racket Benchmark Runner

Compiles .rkt files with raco exe and measures:
- compilation time
- program execution time
- memory usage (RSS)
- binary size
"""

import subprocess
import time
from pathlib import Path
import sys
from datetime import datetime
import tempfile

ROOT = Path(__file__).resolve().parent.parent
RACKET_DIR = ROOT / 'benchmark' / 'racket'


def run_command(cmd, cwd=None):
    start = time.perf_counter()
    proc = subprocess.run(
        cmd,
        cwd=cwd,
        text=True,
        capture_output=True,
    )
    end = time.perf_counter()
    return proc, end - start


def benchmark_file(file_path: Path):
    with tempfile.TemporaryDirectory() as td:
        exe_path = Path(td) / file_path.stem

        # Compile with raco exe
        comp_proc, comp_time = run_command(['raco', 'exe', '-o', str(exe_path), str(file_path)])
        if comp_proc.returncode != 0:
            return {
                'name': file_path.name,
                'status': 'COMPILE_ERROR',
                'compile_time_s': comp_time,
                'run_time_s': None,
                'resindent_set_kb': None,
                'exe_size_bytes': None,
                'stderr': comp_proc.stderr.strip(),
            }

        exe_size = exe_path.stat().st_size

        # Run the compiled executable
        run_proc, run_time = run_command(['/usr/bin/time', '-f', '%M', str(exe_path)])
        status = 'OK' if run_proc.returncode == 0 else f'RUN_ERROR({run_proc.returncode})'

        # /usr/bin/time outputs to stderr
        rss_kb = int(run_proc.stderr.strip()) if run_proc.stderr.strip().isdigit() else 0

        return {
            'name': file_path.name,
            'status': status,
            'compile_time_s': comp_time,
            'run_time_s': run_time,
            'resindent_set_kb': rss_kb,
            'exe_size_bytes': exe_size,
            'stderr': run_proc.stderr.strip(),
        }


def main():
    files = sorted(RACKET_DIR.glob('*.rkt'))
    if not files:
        print(f"No .rkt files found in {RACKET_DIR}")
        return 1

    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(f"Benchmarking {len(files)} Racket file(s) in {RACKET_DIR}")
    print(f"Date: {timestamp}")
    print("-" * 90)
    print(f"{'file':20} {'compile(s)':>10} {'run(s)':>10} {'rss kb':>10} {'binary':>12} {'status':>12}")
    print("-" * 90)

    exit_code = 0
    for f in files:
        res = benchmark_file(f)
        if res['status'] != 'OK':
            exit_code = 1
        ct = f"{res['compile_time_s']:.4f}" if res['compile_time_s'] is not None else '-'
        rt = f"{res['run_time_s']:.4f}" if res['run_time_s'] is not None else '-'
        rs = f"{res['resindent_set_kb']}" if res['resindent_set_kb'] is not None else '-'
        es = f"{res['exe_size_bytes']}" if res['exe_size_bytes'] is not None else '-'
        print(f"{res['name']:20} {ct:>10} {rt:>10} {rs:>10} {es:>12} {res['status']:>12}")

    print("-" * 90)
    return exit_code


if __name__ == '__main__':
    sys.exit(main())
