# Readability pass over the example notebook

Applied 12 October 2026 to `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb`. Only that notebook changed; four input cells were rewritten, the rest of the cells (including all 44 saved `Output`/`Print`/`Message` cells) were kept byte-for-byte. No model, rule, library or package file changed. No commit or merge was made.

> Superseded in part: the `contractDiagram` helper and the explicit `CalcFormCalculate` options introduced below were removed in a later pass, which dropped every option in favour of the defaults — see [../notebook-default-options-2026-10-12/README.md](../notebook-default-options-2026-10-12/README.md).

## What changed

**Diagram display (cell 4).** The nested call `Show[First[Show[diagramGraphics]], ImageSize -> 640]` became two named steps:

```wl
diagramSheets = Show[diagramGraphics];
diagramSheet = Show[First[diagramSheets], ImageSize -> 640];
```

FeynArts defines `Show[graphics_FeynArtsGraphics]` as `Show /@ Render[...]`, so the first call renders the sheets and the second only applies the size. The nesting was doing real work but read badly.

**Conversion (cell 5).** Three options were removed because they are FeynCalc defaults, verified in `FeynCalc/Feynman/FCFAConvert.m:84-98`:

| Removed | Default |
| --- | --- |
| `Contract -> False` | `Contract -> False` |
| `List -> True` | `List -> True` |
| `Prefactor -> 1` | `Prefactor -> 1` |

`ChangeDimension -> D` stays, because that option defaults to `False`, and `LorentzIndexNames` stays so the external indices are the readable `mu1…nu4`. The guard `If[!ListQ[x] || Length[x] =!= 4, …]` became `If[Length[x] =!= 4, …]`: `Length` already returns 0 for a non-list.

**Contraction (cells 7 and 8).** The inline `Function[tensor, Module[…]] /@ diagramTensors` became a named helper plus a one-line application:

```wl
contractDiagram[tensor_] := Module[{result},
  result = CalcFormCalculate[FeynAmpDenominatorExplicit[tensor] externalTensors,
    Dimension -> D, FORMThreads -> 1, TimeConstraint -> 60,
    WorkingDirectory -> treeWorkDirectory, KeepFiles -> True];
  If[FailureQ[result] || MemberQ[{$Failed, $Aborted}, result] ||
    !FreeQ[result, _LorentzIndex | _CalcFormCalculate],
    Print["A diagram contraction did not complete. Inspect ", treeWorkDirectory];
    Abort[]];
  result];

diagramAmplitudes = contractDiagram /@ diagramTensors;
```

`ShowProgress -> False` was dropped because `ShowProgress` already defaults to `False` (`CalcFormConverter.wl:60`). `StringJoin[…, …]` was written as `… <> …`. The second guard, `If[Length[diagramAmplitudes] =!= 4, …]`, was removed: `contractDiagram` already aborts unless every result is a scalar, so the total is now simply `\[ScriptCapitalM] = Total[diagramAmplitudes];`. The count check that matters, on the converted diagram list, remains in cell 5.

**Prose.** Two sentences were extended: why `GenericModel` is explicit, and what `contractDiagram` sets and requires.

## What was deliberately left alone

- `Model -> grModel, GenericModel -> grModel` in `InsertFields`. These are **not** redundant: `Options[InsertFields]` defaults them to `"SM"` and `"Lorentz"` (`FeynArts/Insert.m:21-29`), so omitting them would select the stock models. Checking this first avoided a silent physics change.
- `Transversality -> False` on all four external tensors. This pass left it in place, which was wrong: `False` is the default, so the option is noise. The user removed it from the notebook, and the current state is re-validated in [../notebook-polarisation-default-2026-10-12/README.md](../notebook-polarisation-default-2026-10-12/README.md).
- `AutoEdit -> False` and `DisplayFunction -> Identity` in `Paint`. The first avoids launching the topology editor; the second stops `Paint` rendering before `Show` does, since `Paint` calls its display function and still returns the graphics.
- `FORMThreads -> 1`, `TimeConstraint -> 60`, `KeepFiles -> True`, `Dimension -> D`: all carry meaning that is not the default.

## Validation

`check-notebook.wls/.log` evaluates all nine input-like cells of the candidate in a fresh bounded kernel and leaves the file alone: **18 checks passed**, exit 0, including four diagrams with denominator counts `{0,1,1,1}`, routes `{-p2+p3, -p2+p4, p3+p4}`, four scalar FORM results, `\[ScriptCapitalM] === Total[diagramAmplitudes]`, the contact equal to `GravitonVertex[mu1,nu1,p1,mu2,nu2,p2,mu3,nu3,-p3,mu4,nu4,-p4]`, diagram graphics produced before the amplitudes, retained off-shell virtualities/longitudinal/trace terms, and no stray context prefixes. Peak memory 318,668,800 bytes under a verified 1-GiB cgroup, zero swap, zero cap/OOM events. Amplitude generation and conversion took 34.76 s and the four contractions 5.57 s, essentially unchanged.

The first build attempt is retained as `build-notebook.log` **but is invalid**: `replace[test_, expr_]` had no hold attribute, so the replacement code was evaluated as an argument while `treeInsertions` was undefined, producing `Show::gtype` messages and zero replacements. The builder now builds boxes with `ToBoxes[Defer[...]]` before the call and passes the boxes.

Installed notebook SHA-256 `eab01f47394035a0202907ffee8940af68db758332158f0125e894cd35fd2e66`; pre-pass snapshot `before.nb`.

## Note

The saved outputs in the installed notebook are from earlier runs and still show the older code paths and the older model path. Re-evaluating the notebook from the top refreshes them.
