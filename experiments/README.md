# The verified SMT backend on Gillian's queries

The experiment of Sec. 7.2 of *An End-to-end Theory for Compositional Symbolic
Execution*. Gillian, run with `--certified-smt`, sends every SMT query to two
backends and records both:

- its own SMT encoder, as usual;
- the encoder extracted from the paper's Rocq development, through a bridge
  that translates Gillian's expressions into CSE's.

Gillian's verification continues on its own encoder's answer, so the verified
backend only observes; it cannot change a proof's outcome.

| | |
| --- | --- |
| `paper-run/` | the run in the paper |
| `report.py` | a run's numbers: Fig. 9 and the appendix table |
| `analysis.ipynb` | the same, with the plot, the disagreements, and what is not covered |
| `run.sh` | re-run the 32 cases; writes `results/` |
| `compare.py` | check a re-run against `paper-run/`, query by query |

## Reading the paper's run

```sh
python3 -m venv .venv && .venv/bin/pip install -r requirements.txt
.venv/bin/python report.py              # Fig. 9's table, the timings, the disagreements
.venv/bin/python report.py --tsv DIR    # also the scatter plot's data, as the paper reads it
.venv/bin/jupyter lab analysis.ipynb
```

## Re-running

```sh
experiments/run.sh                      # all 32 cases, a few minutes; run.sh 25 runs one
.venv/bin/python compare.py             # results/ against paper-run/
.venv/bin/python report.py results
```

`run.sh` builds Gillian, then runs each case under a 120-second timeout
(`-t` changes it) and checks its exit code against the expected one. Expect:

- **`compare.py` reports every query both runs produced as identical**: the
  same input, the same SMT-LIB from both encoders, the same answers. Gillian's
  query sequence is deterministic.
- **Possibly a different number of queries.** The two Amazon `main` cases may not
  finish within the timeout. Every query they produce before it counts, so on a
  slower or faster machine they stop at a different point.
- **Different solver times.** Times are the solver's `check-sat` call only, and
  depend on the machine and the Z3 version.

Some cases are expected to fail, and fail the same way without
`--certified-smt`: `run.sh` lists them with their exit codes, and why.

## What the numbers mean

A query passes through three stages on the verified side:

1. **translated**: the bridge (`GillianCore/smt/CertifiedSMT.ml`) expressed it in
   CSE's syntax. Gillian's sets and quantifiers have no counterpart in CSE.
2. **encoded**: the extracted encoder (`GillianCore/cse/`) produced SMT-LIB for it.
3. **answered**: both backends' solver calls returned `sat` or `unsat`.

The differences between Gillian's language and CSE's show up as follows:

- **Disagreements.** All are `sat` from Gillian and `unsat` from the verified
  encoder, on a query that uses Gillian's integer-to-number cast `IntToNum`.
  CSE's numbers are the strictly positive rationals, so its cast `AsNum` is
  undefined at 0, and its encoding asserts that the argument is positive.
  `analysis.ipynb` shows this, and checks it: without those side conditions,
  every disagreeing query is `sat`.
- **Representability guards.** The paper's satisfiability check (Sec. 6) also
  asserts, for each variable, that its value is one CSE has. The bridge leaves
  them out: Gillian's numbers include 0 and the negatives, and with the guards
  the comparison would measure that difference rather than the encoders.
  Leaving out assertions weakens a query, so an `unsat` without them is still
  `unsat` with them.
- **Numbers that are not positive.** For the same reason, the bridge translates
  Gillian's number literals that are not positive (almost always `0.`), which
  CSE does not have: the encoder handles them as any rational, with Gillian's
  meaning. Negative *integer* literals are not translated: CSE's integers are
  naturals, and the extracted encoder cannot represent a negative one.
- **Ill-typed expressions.** Gillian compares integers with numbers
  (`l-len #view < 22.`), which Z3 accepts; in CSE that is ill-typed, and the
  verified encoder declines it: such a query is translated but not encoded.

`GillianCore/cse/` is CSE's extracted library plus a small, additions-only
patch for Gillian's values; `scripts/vendor-cse/vendor.sh --check` confirms it.

### Solver time

The two encodings take comparable time on most queries: in the paper's run,
the median check-sat is 0.09 ms with Gillian's encoding and 0.16 ms with the
verified one. The exception is a tail of 67 of the 4,341 answered queries that
take over 100 ms with the verified encoding, up to 7.3 s, where Gillian's
encoding takes at most 1.3 ms. All 67 come from the two Amazon case studies,
all are `sat`, and all combine a list's length with a cast or a
multiplication. CSE gained those operators after the submitted paper; before,
the bridge could not translate these queries at all.

The cause is how the encoders encode a list's length:

- **The verified encoder** encodes it exactly, as `seq.len` of an SMT-LIB
  sequence. To answer `sat`, Z3 must build the lists themselves: in the
  slowest query, `96 * len(c) = len(l)` with `len(c) >= 1` makes it construct
  a list of at least 96 values.
- **Gillian's encoder** abstracts it (`GillianCore/smt/smt.ml`, `encode_unop`):
  a list variable that appears only under length gets an uninterpreted length,
  `l-len(x)`, and no list is built.

The abstraction never turns a satisfiable query unsatisfiable, so an `unsat`
from it is always right. A `sat` is right when the query also says
`0 <= l-len(x)` and no such list is bound by a quantifier: then any list of
that length is a witness. Both conditions hold for every query in the paper's
run (3,170 use the abstraction), but Gillian's symbolic engine maintains them,
not its encoder; an under-approximate (UX) analysis relies on them.

`analysis.ipynb` checks the explanation: with Gillian's abstraction applied to
the 67 verified queries, each gives the same answer, and they take as long as
the rest. Solving them again, each in a fresh `z3`, is also faster than the
times the run recorded: Gillian keeps one solver process for a whole case,
which slows these queries further.

## `queries.jsonl`

One JSON record per SMT query:

| field | |
| --- | --- |
| `argv`, `query_id` | the Gillian command, and the query's number within it |
| `expressions`, `gamma` | the query: Gillian expressions, and their typing context |
| `unverified` | Gillian's encoder: `sat_result`, `time_seconds`, `smt_query` (the SMT-LIB sent) |
| `verified` | the verified encoder: the same, plus `coerced` (translated), `encoded`, and `coercion_failures` / `encoding_failures` saying why a query went no further |

`status.tsv` gives each case's exit code, and `logs/` its output.
