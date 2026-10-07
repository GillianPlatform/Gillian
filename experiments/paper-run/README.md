# The paper's run

`experiments/run.sh`, with its default 120-second timeout, on:

| | |
| --- | --- |
| Gillian | `cse4-popl27` at `5d9c531` |
| CSE | `cse4-popl27` at `b5b4b4e0` (vendored in `GillianCore/cse`) |
| Z3 | 4.15.4 |
| OCaml | 5.3.0 |
| Machine | Apple M5 (10 cores, 16 GB), macOS 27.0.1 |
| Date | 2026-10-07 |

All 32 cases exited as `run.sh` expects. Case 21 (C, Amazon `main`) reached the
timeout, so its 2,948 queries are those it produced in 120 seconds; every other
case ran to completion.
