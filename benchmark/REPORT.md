# Benchmark Report
Date: 2026-01-20

## Benchmarks

| # | Description |
|---|-------------|
| 00 | Sum 1..1000000 |
| 01 | Y-combinator factorial |
| 02 | List build and search |
| 03 | Backtracking (Pythagorean triples) |
| 04 | Coroutine ping-pong (100k iterations) |
| 05 | Merge sort (10k elements) |
| 06 | N-Queens (N=8) |

## KSU (Compiled to C)
```
file              gen(s)  compile(s)  run(s)  rss(MB)  .c(KB)  binary(KB)
benchmark-00.ksu   0.043       0.413   2.136     2168      26          75
benchmark-01.ksu   0.027       0.476   0.287      292      31          76
benchmark-02.ksu   0.027       0.549   0.140      144      34          76
benchmark-03.ksu   0.030       1.139   1.058     1134      89         144
benchmark-04.ksu   0.030       0.942   2.678     2861      75         144
benchmark-05.ksu   0.029       1.001   1.003     1025      77         144
benchmark-06.ksu   0.028       1.262   0.791      834     107         210
```

## Racket (Interpreter)
```
file              run(s)  rss(MB)
benchmark-00.rkt   0.209      139
benchmark-01.rkt   0.222      138
benchmark-02.rkt   0.213      138
benchmark-03.rkt   0.213      138
benchmark-04.rkt   0.228      138
benchmark-05.rkt   0.209      137
benchmark-06.rkt   0.211      138
```

## Racket (raco exe)
```
file              compile(s)  run(s)  rss(MB)  binary(MB)
benchmark-00.rkt       15.95   0.178      139        12.1
benchmark-01.rkt       16.00   0.177      139        12.1
benchmark-02.rkt       16.15   0.177      139        12.1
benchmark-03.rkt       16.39   0.181      139        12.1
benchmark-04.rkt       16.20   0.196      141        12.1
benchmark-05.rkt       16.65   0.179      139        12.1
benchmark-06.rkt       16.45   0.187      141        12.1
```

## Summary

- **Runtime:** Racket compiled runs 5-15x faster than Ksu on most benchmarks
- **Memory:** Ksu scales with workload (144MB-2.8GB), Racket constant ~139MB overhead
- **Compilation:** Ksu 0.5-1.3s total, Racket ~16s
- **Binary size:** Ksu 75-210KB, Racket 12MB (embedded runtime)
- **New benchmarks (04-06):** Coroutines, sorting, and backtracking stress-test continuation handling
