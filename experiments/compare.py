#!/usr/bin/env python3
"""Compare a run of run.sh against the paper's run, query by query.

Usage: compare.py [RUN_DIR] [--against PAPER_RUN_DIR]

  RUN_DIR   default: results (where run.sh writes)
  --against default: paper-run

Queries are matched by command line and query number. A run's query sequence
is deterministic, but a case that hits the timeout (the Amazon `main` cases on
a slow machine) stops at a machine-dependent point, so the two runs need not
have the same number of queries. What must hold is that every query both runs
produced is the same: the same input, the same SMT-LIB from both encoders, and
the same answers. Solver times are not compared.

Only the cases RUN_DIR ran are compared, so a partial run (run.sh 00 23) works.
Exits 0 when every shared query matches, 1 otherwise.
"""

import argparse
import json
from collections import Counter, defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent

# What must match, field by field: (name, how to read it from a record).
FIELDS = [
    ("expressions", lambda r: r["expressions"]),
    ("typing context", lambda r: r["gamma"]),
    ("Gillian's SMT-LIB", lambda r: r["unverified"]["smt_query"]),
    ("Gillian's answer", lambda r: r["unverified"]["sat_result"]),
    ("translated", lambda r: r["verified"]["coerced"]),
    ("encoded", lambda r: r["verified"].get("encoded")),
    ("verified SMT-LIB", lambda r: r["verified"].get("smt_query")),
    ("verified answer", lambda r: r["verified"].get("sat_result")),
]


def load(run_dir):
    by_command = defaultdict(dict)
    for line in open(Path(run_dir) / "queries.jsonl"):
        r = json.loads(line)
        by_command[" ".join(r["argv"][1:])][r["query_id"]] = r
    return by_command


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    p.add_argument("run_dir", nargs="?", default=HERE / "results")
    p.add_argument("--against", default=HERE / "paper-run")
    args = p.parse_args()

    run, paper = load(args.run_dir), load(args.against)
    print(f"{args.run_dir}  vs  {args.against}\n")
    print(f"{'queries: this run / paper / both':>34}  identical  command")

    totals = Counter()
    mismatches = Counter()
    examples = {}
    for command in sorted(run):
        mine, theirs = run[command], paper.get(command, {})
        shared = sorted(set(mine) & set(theirs))
        same = 0
        for q in shared:
            bad = [name for name, get in FIELDS if get(mine[q]) != get(theirs[q])]
            if bad:
                for name in bad:
                    mismatches[name] += 1
                    examples.setdefault(name, (command, q))
            else:
                same += 1
        totals.update(run=len(mine), paper=len(theirs), shared=len(shared), same=same)
        flag = "" if same == len(shared) else "  <-- differs"
        print(f"{len(mine):>16} / {len(theirs):>5} / {len(shared):>5}  {same:>9}  {command[:70]}{flag}")

    print(f"\n{'total':>16} {totals['run']:>3} / {totals['paper']:>5} / {totals['shared']:>5}  {totals['same']:>9}")
    if not run:
        print("no queries in this run")
        return 1
    if mismatches:
        print("\nShared queries that differ, by field:")
        for name, n in mismatches.most_common():
            command, q = examples[name]
            print(f"  {n:5}  {name}  (first: query {q} of {command[:60]})")
        return 1
    print("\nOK: every query both runs produced is identical.")
    extra = totals["run"] - totals["shared"]
    missing = totals["paper"] - totals["shared"]
    if extra or missing:
        print(f"This run has {extra} queries the paper's does not, and lacks {missing} it has: "
              "a case that hit the timeout stopped at a different point.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
