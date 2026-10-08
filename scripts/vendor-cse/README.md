# Re-vendoring CSE

CSE (`~/dev/CSE`) is the source of truth for the verified SMT encoder.
`GillianCore/cse/` is a vendored copy of its `lib/`, plus a small set of
Gillian-specific additions that deliberately do **not** live in CSE.

```sh
scripts/vendor-cse/vendor.sh [path-to-CSE]   # default ~/dev/CSE
dune build @check
git diff --stat GillianCore/cse
```

`vendor.sh` builds CSE's extraction, copies CSE's hand-written libraries and the
*generated* extraction, applies [`gillian.patch`](gillian.patch), and installs
the result over the `.ml`/`.mli` sources in `GillianCore/cse`.

To check, without changing anything, that `GillianCore/cse` is exactly CSE plus
the patch:

```sh
scripts/vendor-cse/vendor.sh --check [path-to-CSE]
```

## What is local to Gillian, and why

GIL has four values with no counterpart in CSE's verified value language:
`none`, `empty`, and object locations (`loc`). Gillian adds them as constructors
of the SMT `Val` datatype, with the matching type tests and coercions:

| Where | Addition |
| --- | --- |
| `extracted/extracted.{ml,mli}` | `PVGillian*` / `TGillian*` constructors, the `c_/g_/p_/is_/to_gillian_*` helpers, and the corresponding arms of `encode_type`, `is_type`, `to_type_curried` and `encode_preval` |
| `syntax/type.{ml,mli}`, `syntax/val.{ml,mli}` | the `None` / `Empty` / `Loc` constructors and their `to_extracted` arms |
| `smt/smt.ml` | the three `declare-datatype` entries that announce them to the solver |

These are additions to *extracted* code, so they cannot be source edits in CSE
and extraction will never regenerate them. They live in `gillian.patch`, which
only adds lines: it removes or changes nothing of CSE's.

Two further differences are pure build wiring, and are **not** additions:

- CSE builds its utility library as `utils` (module `Utils`). Gillian must
  rename it to `extraction_utils`, because `GillianCore/utils` already claims
  `utils`; dune then wraps it as `Extraction_utils.Utils`. The patch adds
  `open Extraction_utils` to every copied `.ml` that refers to `Utils.`, which
  restores the prefix the copied sources use.
- The `dune` files here differ from CSE's (different library and public names,
  and the extraction is checked in rather than produced by a rocq rule).
  `vendor.sh` never copies a `dune` file.

## Changing the additions

`gillian.patch` is generated; do not edit it by hand. Edit the additions in
`GillianCore/cse` itself, then regenerate the patch, which also checks that it
reproduces the tree:

```sh
scripts/vendor-cse/vendor.sh --update-patch [path-to-CSE]
```

The patch is applied with no fuzz (`patch -F0`), in a scratch copy. If CSE has
changed under a hunk, the re-vendor fails and leaves `GillianCore/cse`
untouched. Then re-vendor by hand (copy CSE over the sources, re-apply the
additions, build), and run `--update-patch`.

### The limit of that guarantee

A patch that applies is not a patch that is still right. Two ways it can apply
and be wrong:

- the additions name CSE's generated constructors (`IdSimple`, `PVNull`, ...).
  If CSE renames one outside a hunk's context, the patch applies and the build
  fails;
- `to_gillian_value` is a hand-written variant of the generated `to_null`, which
  maps into `Val` rather than unwrapping to `Null`. If CSE changes the shape of
  its coercions, that addition has to be re-derived by hand, and it will still
  compile.

So after a re-vendor, build *and* re-run the experiments.

## Formatting

Vendored sources keep CSE's formatting verbatim, so that re-vendor diffs show
only real changes. The tree is therefore not ocamlformat-clean (it already
wasn't). If CI ever enforces `dune fmt` here, add `GillianCore/cse` to
`.ocamlformat-ignore` rather than reformatting a vendored copy.

## Numeric literals

The encoder names integer and rational literals with prefixed function symbols
(`int_literal_5`, `decimal_literal_3/2`) so that the metatheory can tell a
literal apart from other function symbols by name. SMT-LIB has no such symbols,
so CSE's printer (`lib/utils/utils.ml`, `sexp_of_identifier`) renders them as
the literals themselves -- `5`, `(/ 3.0 2.0)` -- in the same layer that already
prints a string literal's symbol as the quoted SMT text.

That mapping is keyed on the two prefixes. If they ever change in
`smt_theories/Theory/Reals_Ints.v`, the printer stops matching and the symbols
reach the solver undeclared, which shows up as
`(error "unknown constant int_literal_0")` rather than as a build failure.
Re-run a benchmark after a re-vendor, not just `dune build`.
