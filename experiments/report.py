#!/usr/bin/env python3
"""The numbers of the paper's Sec. 7.2 (Fig. 9, and its appendix table), for one
run of run.sh.

Usage: report.py [RUN_DIR] [--tsv DIR]

  RUN_DIR    a run, laid out as run.sh leaves it (default: paper-run)
  --tsv DIR  also write the scatter plot's data, one file per language, in the
             format the paper's pgfplots figure reads

Also a module: analysis.ipynb and compare.py load runs with load_run.
"""

import argparse
import json
from pathlib import Path

import pandas as pd

HERE = Path(__file__).resolve().parent
LANGUAGES = ["JS", "C", "WISL"]
ANSWERS = ("sat", "unsat")

# The stages a query goes through on the verified side, in order. A query that
# stops at a stage has passed every stage before it.
STAGES = ["not translated", "not encoded", "not answered", "answered"]


def language_of_argv(argv):
    exe = Path(argv[0]).name
    return {"gillian-js": "JS", "gillian-c": "C", "wisl": "WISL"}.get(exe, exe)


def language_of_case(case_id):
    # Case ids are NN-<language>-<name>, as in run.sh.
    return {"js": "JS", "c": "C", "wisl": "WISL"}[case_id.split("-")[1]]


def stage(verified):
    if not verified["coerced"]:
        return "not translated"
    if not verified.get("encoded"):
        return "not encoded"
    if verified.get("sat_result") not in ANSWERS:
        return "not answered"
    return "answered"


def failure_reason(verified):
    """Why a translated query got no verified answer: the first diagnostic."""
    failures = verified.get("encoding_failures") or []
    if not failures:
        return None
    reason = failures[0].get("reason", "")
    # The solver's own complaint is the interesting part of a solving failure.
    if "SMT heartbeat gave unexpected result" in reason:
        return "solver rejected the query"
    return reason


def load_run(run_dir=HERE / "paper-run"):
    """One row per query, plus the run's cases (from status.tsv)."""
    run_dir = Path(run_dir)
    records = [json.loads(line) for line in open(run_dir / "queries.jsonl")]
    rows = []
    for i, r in enumerate(records):
        u, v = r["unverified"], r["verified"]
        rows.append({
            "idx": i,
            "language": language_of_argv(r["argv"]),
            "command": " ".join(r["argv"][1:]),
            "query_id": r["query_id"],
            "stage": stage(v),
            "reason": failure_reason(v),
            "u_sat": u["sat_result"],
            "v_sat": v.get("sat_result"),
            "u_ms": 1000 * u["time_seconds"] if u["time_seconds"] is not None else None,
            "v_ms": 1000 * v["time_seconds"] if v.get("time_seconds") is not None else None,
            "uses_int_to_num": '["IntToNum"]' in json.dumps(r["expressions"]),
        })
    df = pd.DataFrame(rows)
    df["answered"] = (df.stage == "answered") & df.u_sat.isin(ANSWERS)
    df["agree"] = df.answered & (df.u_sat == df.v_sat)

    cases = pd.read_csv(run_dir / "status.tsv", sep=r"\s+", header=None)
    cases = cases.rename(columns={0: "case", 1: "exit"})[["case", "exit"]]
    cases["language"] = cases.case.map(language_of_case)
    return df, cases, records


def coverage_table(df, cases):
    """Fig. 9's table: how far the verified side gets, per language."""
    g = df.groupby("language")
    t = pd.DataFrame({
        "files": cases.groupby("language").size(),
        "queries": g.size(),
        "translated": g.stage.apply(lambda s: (s != "not translated").sum()),
        "encoded": g.stage.apply(lambda s: s.isin(["not answered", "answered"]).sum()),
        "answered": g.answered.sum(),
        "agree": g.agree.sum(),
    }).reindex(LANGUAGES).fillna(0).astype(int)
    t.loc["Total"] = t.sum()
    return t


def timing_table(df):
    """The appendix table's timing columns: check-sat time in ms, median and
    95th percentile, over the queries both backends answered."""
    a = df[df.answered]
    def stats(col):
        g = a.groupby("language")[col]
        s = pd.DataFrame({"median": g.median(), "p95": g.quantile(0.95)}).reindex(LANGUAGES)
        s.loc["Total"] = [a[col].median(), a[col].quantile(0.95)]
        return s
    return pd.concat({"Gillian ms": stats("u_ms"), "verified ms": stats("v_ms")}, axis=1)


def write_scatter_tsvs(df, out_dir):
    """backend-solver-times-{js,c,wisl}.tsv, as the paper's figure reads them."""
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    a = df[df.answered]
    for lang in LANGUAGES:
        path = out_dir / f"backend-solver-times-{lang.lower()}.tsv"
        (a[a.language == lang][["u_ms", "v_ms"]]
         .rename(columns={"u_ms": "gillian_ms", "v_ms": "certified_ms"})
         .to_csv(path, sep="\t", index=False, float_format="%.9f"))
        print(f"wrote {path}")


def pct(n, d):
    return f"{100 * n / d:.1f}%" if d else "-"


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    p.add_argument("run_dir", nargs="?", default=HERE / "paper-run")
    p.add_argument("--tsv", metavar="DIR")
    args = p.parse_args()

    df, cases, _ = load_run(args.run_dir)
    print(f"Run {args.run_dir}: {len(df)} queries from {len(cases)} cases\n")

    cov = coverage_table(df, cases)
    show = cov.copy()
    show["translated %"] = [pct(r.translated, r.queries) for r in cov.itertuples()]
    show["agree %"] = [pct(r.agree, r.answered) for r in cov.itertuples()]
    print("Coverage and agreement (Fig. 9). Each column is a subset of the one before:")
    print("  translated: the bridge put the query in CSE's syntax")
    print("  encoded:    the verified encoder produced SMT-LIB for it")
    print("  answered:   both backends answered sat or unsat\n")
    print(show.to_string(), "\n")

    print("Check-sat time, ms, median / 95th percentile, over answered queries (appendix):")
    t = timing_table(df)
    for lang, r in t.iterrows():
        print(f"  {lang:6} Gillian {r[('Gillian ms', 'median')]:.2f} / {r[('Gillian ms', 'p95')]:.2f}"
              f"   verified {r[('verified ms', 'median')]:.2f} / {r[('verified ms', 'p95')]:.2f}")
    print()

    dis = df[df.answered & ~df.agree]
    print(f"Disagreements: {len(dis)} of {int(df.answered.sum())} answered queries")
    if len(dis):
        print(f"  using IntToNum: {int(dis.uses_int_to_num.sum())} of {len(dis)}"
              f" (CSE's numbers are positive rationals, so IntToNum(0) has no CSE meaning)")
        print("  " + ", ".join(f"{u}->{v}" for u, v in dis.groupby(["u_sat", "v_sat"]).size().index)
              + "  (Gillian -> verified)")
    print()

    lost = df[df.stage.isin(["not encoded", "not answered"])]
    if len(lost):
        print("Translated but not answered, by reason:")
        for (s, reason), n in lost.groupby(["stage", "reason"]).size().sort_values(ascending=False).items():
            print(f"  {n:5}  {s}: {reason[:90]}")
        print()

    top = df.sort_values("u_ms", ascending=False).head(25)
    print("The 25 slowest queries, by Gillian's check-sat time:")
    for s in STAGES:
        print(f"  {s:15} {int((top.stage == s).sum())}")

    if args.tsv:
        print()
        write_scatter_tsvs(df, args.tsv)


if __name__ == "__main__":
    main()
