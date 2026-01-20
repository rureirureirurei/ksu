#!/usr/bin/env python3
"""Racket Interpreter Benchmark Runner"""

import subprocess
import time
from pathlib import Path
import sys
from datetime import datetime

ROOT = Path(__file__).resolve().parent.parent
RACKET_DIR = ROOT / 'benchmark' / 'racket'


def run_command(cmd, cwd=None):
    start = time.perf_counter()
    proc = subprocess.run(cmd, cwd=cwd, text=True, capture_output=True)
    end = time.perf_counter()
    return proc, end - start


def benchmark_file(file_path: Path):
    run_proc, run_time = run_command(['/usr/bin/time', '-f', '%M', 'racket', str(file_path)])
    status = 'OK' if run_proc.returncode == 0 else f'ERROR({run_proc.returncode})'
    rss_kb = int(run_proc.stderr.strip()) if run_proc.stderr.strip().isdigit() else 0

    return {
        'name': file_path.name,
        'status': status,
        'run_time_s': run_time,
        'resindent_set_kb': rss_kb,
    }


def main():
    files = sorted(RACKET_DIR.glob('*.rkt'))
    if not files:
        print(f"No .rkt files found in {RACKET_DIR}")
        return 1

    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    print(f"Benchmarking {len(files)} Racket file(s) (interpreter mode)")
    print(f"Date: {timestamp}")
    print("-" * 70)
    print(f"{'file':20} {'run(s)':>10} {'rss kb':>10} {'status':>12}")
    print("-" * 70)

    exit_code = 0
    for f in files:
        res = benchmark_file(f)
        if res['status'] != 'OK':
            exit_code = 1
        rt = f"{res['run_time_s']:.4f}"
        rs = f"{res['resindent_set_kb']}"
        print(f"{res['name']:20} {rt:>10} {rs:>10} {res['status']:>12}")

    print("-" * 70)
    return exit_code


if __name__ == '__main__':
    sys.exit(main())
