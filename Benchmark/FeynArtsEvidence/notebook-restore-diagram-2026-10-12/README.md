# Restoring the diagram display

Applied 12 October 2026 to `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb`, on top of the user's own edits. Only the `Paint` cell was rewritten; every other cell, including the user's newer cells and their saved outputs, was preserved. No model, rule, library or package file changed. No commit or merge was made.

## What was missing

The sheet was being computed but never shown. The cell ended with a semicolon:

```wl
diagramGraphics = Paint[treeInsertions, ...];
diagramSheets = Show[diagramGraphics];
diagramSheet = Show[First[diagramSheets], ImageSize -> 640];   (* output suppressed *)
```

A trailing semicolon makes the cell return `Null`, so the front end has nothing to display. The semicolon came from the earlier [readability pass](../notebook-readability-2026-10-12/README.md), which rewrote the nested `Show[First[Show[...]]]` into two named steps and added the terminator by mistake. The fix drops it, so the last assignment returns the rendered sheet:

```wl
diagramGraphics = Paint[treeInsertions,
  ColumnsXRows -> {2, 2}, Numbering -> Simple,
  SheetHeader -> "Four-graviton tree diagrams", AutoEdit -> False,
  DisplayFunction -> Identity];
diagramSheets = Show[diagramGraphics];
diagramSheet = Show[First[diagramSheets], ImageSize -> 640]
```

The rendered sheet is also cached as that cell's output, so the four diagrams are visible when the notebook is opened, not only after re-evaluation.

## Validation

`check-notebook.wls/.log`: all ten input-like cells evaluated in a fresh bounded kernel, **14 checks passed**, exit 0. The `Paint` cell returns `Graphics` (0.013 s). Physics unchanged: four converted diagrams, denominators `{0,1,1,1}`, routes `{-p2+p3, -p2+p4, p3+p4}`, the contact equal to the exact source quartic, `theAmplitude[p1,p2,p3,p4]` a nonzero scalar retaining off-shell virtuality, longitudinal and trace terms, no `Transversality` option, no `FeynArts`` prefixes, and no integration wrapper loaded.

The harness prints only the rendered sheet into the notebook; the user's other saved outputs are untouched.

## Two things worth the user's attention

**Memory.** `CGROUP_MEMORY_PEAK=1073741824` — the run reached the 1-GiB cap exactly, with `max 61` reclaim events (no OOM kill). The earlier version of this notebook peaked at 319 MB with the same first eight cells, so the growth comes from the two cells the user added: `Length[theAmplitude[p1,p2,p3,p4]]` and, above all, `CalcFormCalculate[theAmplitude[p1,p2,p3,p4] == theAmplitude[p2,p1,p3,p4]]`, which took 11.1 s and built a 9.97 MB residual expression. It completed here, but with no headroom left.

**The permutation check did not return `True`.** That last cell returned an unevaluated `Equal` of the form `huge expression == 0`, not `True`. This is not necessarily a defect: substituting `p1 -> p2, p2 -> p1` exchanges the two incoming momenta but leaves the Lorentz index labels `mu1,nu1` and `mu2,nu2` attached to their original leg slots, so the result is not the full `1 <-> 2` leg exchange and need not equal the original. As written, the cell does not establish a symmetry.

Both observations are reported, not acted on: the user's cells were left exactly as written.

## Also noted, not changed

The prose cell before the amplitude now says the result "is stored in `\[ScriptCapitalM]`", which no longer exists — the user's cells use `theAmplitude`. The stale sentence was left alone because the surrounding edit is the user's. The harness log also contains one ~10 MB line from printing that `Equal` result; the log is otherwise readable and the file was retained as it was produced.

Installed notebook SHA-256 `7271d71a53756b903d19a287fba839124492433caa991b9504c1456305d9f8ee`; pre-change snapshot `before.nb`.
