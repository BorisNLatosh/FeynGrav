# Rewritten initialisation and removal of FeynArts context prefixes

Applied to `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb` on 12 October 2026. Wolfram 15.0.1, FeynCalc 10.2.1, patched FeynArts 3.12. No package, model or library file was changed; no commit or merge was made.

## What changed

The initialisation Input cell was replaced. It previously replayed the whole setup — `$LoadFeynArts`, an explicit `Get` of `FeynGrav.wl`, `importGravitons[2]`, `$ModelPath` `DeleteCases`/`Prepend`, `Reinitialize -> False` and a `Print`/`Abort` guard — and now contains only what the user's `Code` cell does not already do:

```wl
grModel = FileNameJoin[{NotebookDirectory[], "..", "Benchmark", "FeynArtsEvidence",
   "model-first-2026-10-12", "model", "FGPureGR5a149c98161e2b2e"}];
InitializeModel[grModel, GenericModel -> grModel];
```

`GenericModel` is retained deliberately: the `.mod` file does not name its generic model and the FeynArts default is the stock `"Lorentz"` file. Everything else is redundant: the `Code` cell already performs `$LoadAddOns = {"FeynArts"}; << FeynGrav``, package initialisation already imports graviton orders `{1,2}`, `$ModelPath` is unnecessary when the model file is named directly, `Reinitialize` defaults to re-reading, and `InitializeModel` itself messages `InitializeModel::noopen` and aborts for a missing file. See `../notebook-loading-2026-10-12/README.md` for the per-statement evidence.

All 28 `FeynArts`` prefixes were removed from input cells, including `CreateTopologies`, `Adjacencies`, `InsertFields`, `T`, `Model`, `GenericModel`, `InsertionLevel`, `Classes`, `Paint`, `ColumnsXRows`, `Numbering`, `Simple`, `SheetHeader`, `AutoEdit`, `CreateFeynAmp`, `PreFactor`, `GaugeRules`, `MomentumConservation` and `Truncated`. The single remaining qualified name in the whole notebook is `FeynGrav`` inside `<< FeynGrav``, which must stay. One sentence of prose was updated because it referred to `FeynArts`Paint` and to `$LoadFeynArts`.

The rewrite touched only the initialisation cell and two prose sentences. `build-notebook.wls` copied the notebook to `before.nb` first and left every other cell untouched, including all 44 saved `Output`/`Print`/`Message` cells, so the previously rendered diagram sheet and preview survive.

## Validation

`check-notebook.wls` evaluates the candidate's nine input-like cells in document order in a fresh kernel and leaves the file alone. `check-notebook.log` records **16 checks passed**, shell exit 0.

- Every unqualified name resolves in the FeynArts context: `Context/@{InitializeModel, GenericModel, CreateTopologies, Adjacencies, InsertFields, T, Model, InsertionLevel, Classes, Paint, ColumnsXRows, Numbering, Simple, SheetHeader, AutoEdit, CreateFeynAmp, PreFactor, GaugeRules, MomentumConservation, Truncated}` returned `FeynArts`` twenty times. This matters because an unqualified name that silently resolved to `Global`` would be an unset option rather than an error.
- Physics is unchanged: four diagrams, denominator counts `{0,1,1,1}`, routes `{-p2+p3, -p2+p4, p3+p4}`, four scalar FORM results, `\[ScriptCapitalM] === Total[diagramAmplitudes]`, diagram graphics produced before the amplitudes, and the contact exactly equal to `GravitonVertex[mu1,nu1,p1,mu2,nu2,p2,mu3,nu3,-p3,mu4,nu4,-p4]`. The contact's indices are exactly the declared `mu1…nu4`, which is why the source is rebuilt with those names.
- Off-shell input is retained: virtualities, all four longitudinal contractions and all four traces appear in `\[ScriptCapitalM]`.
- No `FeynArts`` prefix remains in any input cell; the only qualified name left is `FeynGrav`` in the load line.

Timings: package/model cells 3.6 s + 2.3 s; amplitude generation and conversion 34.7 s; the four FORM contractions 5.6 s. Total `\[ScriptCapitalM]` is 52,101,824 bytes. Peak cgroup memory 318,636,032 bytes under a verified 1-GiB cap with zero swap, group-OOM policy and zero cap/OOM events.

## Superseded runs

- `build-notebook.log` (first attempt) is retained but **invalid**: the builder inserted an unevaluated helper expression instead of a `Cell`, producing a malformed candidate. Only the box-building statement was at fault; the second build is correct.
- The first two `check-notebook.log` versions are retained as **harness errors, not notebook defects**: the first iterated only `nb[[1]]` and so found no cells (the user's notebook nests everything in `CellGroupData`), and the second used `FreeQ` with substring patterns and rebuilt the contact source with `Lor1…Lor8` rather than the notebook's declared `mu1…nu4`.

## Reproduce

From `Benchmark/`, with host headroom:

```bash
unit="fgfa-nocontext-$(date +%s)-$$"
systemd-run --user --scope --same-dir --unit="$unit" \
  -p MemoryAccounting=yes -p MemoryMax=1G -p MemorySwapMax=0 \
  -p OOMPolicy=kill -p KillMode=control-group -p TasksMax=48 \
  -p RuntimeMaxSec=230s -p TimeoutStopSec=7s \
  /usr/bin/timeout --kill-after=5s 210s /usr/local/bin/WolframKernel -noinit -script \
  FeynArtsEvidence/notebook-no-contexts-2026-10-12/check-notebook.wls
```

Installed notebook SHA-256 `a21916411679513c3079793ed87c3b8892db6b35c62fa9449858f970dd8723d7`; pre-change snapshot `before.nb` retains the earlier SHA-256 `79d9cc7049e62ebdabd442b3ca0bbed731ff8ed34af0ef9e299530a17b36af55`.
