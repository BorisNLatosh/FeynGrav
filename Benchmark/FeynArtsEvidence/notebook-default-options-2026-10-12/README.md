# Contraction cell: all CalcFormCalculate options removed

Applied 12 October 2026 to `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb`. Only the contraction cell and one prose sentence changed; every other cell, including the 44 saved outputs, was preserved. No model, rule, library or package file changed. No commit or merge was made.

## Before and after

```wl
Clear[diagramAmplitudes, \[ScriptCapitalM]];
treeWorkDirectory = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "fgfa-four-tree-offshell-" <> CreateUUID[]}]];
contractDiagram[tensor_] := Module[{result},
  result = CalcFormCalculate[FeynAmpDenominatorExplicit[tensor] externalTensors,
    Dimension -> D, FORMThreads -> 1, TimeConstraint -> 60,
    WorkingDirectory -> treeWorkDirectory, KeepFiles -> True];
  If[FailureQ[result] || MemberQ[{$Failed, $Aborted}, result] || !FreeQ[result, _LorentzIndex | _CalcFormCalculate],
    Print["A diagram contraction did not complete. Inspect ", treeWorkDirectory]; Abort[]];
  result];
diagramAmplitudes = contractDiagram /@ diagramTensors;
```

became

```wl
diagramAmplitudes = CalcFormCalculate[FeynAmpDenominatorExplicit[#] externalTensors] & /@ diagramTensors;
If[!FreeQ[diagramAmplitudes, $Failed | $Aborted | _LorentzIndex | _CalcFormCalculate],
  Print["A diagram contraction did not complete."]; Abort[]];
```

The `Module`, the named helper, the working directory and the `Clear` are gone. The per-diagram failure check became one post-hoc check on the completed list, which covers the same cases in one statement.

## Why the options could go

Defaults are in `CalcFormConverter/FORMRuntime.wl:16-20`:

| Option | Removed value | Default | Effect of removal |
| --- | --- | --- | --- |
| `Dimension` | `D` | `Automatic` | inference from the expression, "defaulting to `D` when there are no dimension-tagged objects" (`CalcFormConverter.wl:504`). The expression carries symbolic `D`, so the same space is selected |
| `FORMThreads` | `1` | `Automatic` | up to eight TFORM workers are allowed instead of one |
| `TimeConstraint` | `60` | `Infinity` | the per-stage time bound is gone; the outer test timeout and cgroup still bound every run |
| `WorkingDirectory` | a private directory | `Automatic` | the job uses the default location |
| `KeepFiles` | `True` | `False` | successful jobs are cleaned up; failed jobs are still retained |

`Dimension` was the one worth checking rather than assuming: had `Automatic` resolved to four dimensions, the contact would no longer have matched the D-dimensional source. It did match, and the off-shell virtuality, longitudinal and trace terms all survive unchanged.

Removing `KeepFiles -> True` also removes the only reason the cell needed a named working directory, which is why `treeWorkDirectory` and its `CreateDirectory`/`CreateUUID` scaffolding disappeared with it. The prose sentence that described the helper and the retained job files was rewritten accordingly.

## Validation

`check-notebook.wls/.log`: all nine input-like cells of the candidate evaluated in a fresh bounded kernel, **19 checks passed**, exit 0. Identical physics to the previous version: four diagrams, denominators `{0,1,1,1}`, routes `{-p2+p3, -p2+p4, p3+p4}`, four scalar FORM results, `\[ScriptCapitalM] === Total[diagramAmplitudes]`, the contact equal to the exact source quartic, diagram graphics before the amplitudes, retained off-shell virtuality/longitudinal/trace terms, no `Transversality` option, no context prefixes, no benchmark path.

The contractions took 5.32 s against 5.57 s with a single worker and a 60-second limit, so the multi-worker default brought no measurable gain here. Peak memory 319,057,920 bytes against 318,828,544 bytes previously — a 0.2 MB difference, and both far below the verified 1-GiB cgroup cap, with zero swap and zero cap/OOM events.

Installed notebook SHA-256 `ef96c23b8dbf0cbc1cbfd6dd59e117770aaf4f6e58f1a3a66d056dd67a0a12e3`; pre-change snapshot `before.nb`.

The earlier [readability pass](../notebook-readability-2026-10-12/README.md) introduced `contractDiagram` and the explicit options described there; those statements are superseded by this change.
