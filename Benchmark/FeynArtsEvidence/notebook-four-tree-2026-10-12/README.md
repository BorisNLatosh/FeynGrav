# Complete off-shell four-graviton tree notebook

The notebook now displays the four contributing native FeynArts diagrams **before** calling `CreateFeynAmp` or FORM, contracts their external polarisation tensors diagram by diagram, and assigns the complete sum to `\[ScriptCapitalM]` (ℳ):

```wl
\[ScriptCapitalM] = Total[diagramAmplitudes];
```

## What changed

- `Adjacencies -> {3,4}` includes the quartic contact as well as the three exchange channels; the previous s-channel selection is removed.
- `Paint` constructs a numbered 2-by-2 sheet from the actual inserted topologies. `Show` converts it to native Mathematica graphics, and the evaluated graphics output is saved in the notebook before the amplitude-generation section. No external image file is needed to display it.
- Incoming momenta remain `p1,p2`, outgoing momenta `p3,p4`; symbolic D, arbitrary virtualities and scalar products, and explicit `Transversality -> False` are retained.
- `diagramAmplitudes` retains four separate contracted scalar expressions. The total ℳ is assigned only after all four calculations succeed; it is cleared before recalculation. No extra graph factors, spin sums or integration measures are added.
- The large total is not embedded as a saved output. Evaluating the notebook stores it in ℳ, and `Short[ℳ,4]` previews it without changing the expression.

## Actual validation

`check-notebook-final.log` records **19 passing checks**, explicit completion and shell exit 0. The actual notebook cells were evaluated sequentially in a fresh kernel, with only `NotebookDirectory[]` replaced by the Examples directory for headless evaluation.

Checks establish: four native painted diagrams, a native Graphics sheet produced before amplitude generation, four converted tensors, denominator counts `{0,1,1,1}`, routes `{p3+p4,-p2+p4,-p2+p3}`, four successful FORM contractions, exact structural equality of ℳ with their sum, and retention of external virtualities, all longitudinal contractions and polarisation traces. No specialised kinematics or integration companion was used.

Amplitude generation/conversion took 23.08 seconds; the four FORM contractions together took 4.82 seconds. The total occupies 52,101,824 bytes in kernel memory. A verified aggregate 1-GiB cgroup with zero swap and group-OOM policy recorded peak memory 330,883,072 bytes and zero cap/OOM events; host headroom before the run was about 8.2 GiB.

## Evidence and rendering boundary

- `before.nb`: the actual user-saved notebook before this update, including its front-end formatting and old outputs.
- `build-notebook.wls`: builds `candidate.nb`; loading commands are preserved semantically and reboxed as one compound input expression.
- `check-notebook.wls`: evaluates the candidate, records checks and creates `validated.nb` with its genuine native diagram graphics output.
- `validated.nb`: copied to the example notebook after checking that the user's file has not changed since `before.nb` was captured.
- `diagrams.eps`: ancillary native FeynArts PostScript export. `diagrams-preview.eps/.png` supply missing PostScript canvas bounds and page finalisation for standalone Ghostscript inspection; no diagram geometry or installed dependency was changed. This auxiliary preview is not the notebook's graphics output.

The first `check-notebook.log` and `check-notebook-svg-attempt.wls` are retained as **incomplete**, despite a shell exit of zero: an unnecessary headless SVG export attempted to start a front end and hit the scope's process limit. The remaining scope processes were explicitly stopped. That attempt also exposed the user's front-end-saved multi-expression initialisation boxes returning a Sequence in headless parsing; the same commands were reboxed into one compound expression. The final run avoids SVG export entirely and reaches all 19 assertions. Native PostScript export emits the installed Wolfram version's obsolete-Display warning but does not prevent the calculation. Native notebook front-end rendering was not itself interactively tested; the actual graphics structure and four native diagrams were verified and cached.

The original neighbouring scattering notebook, model bundle and package code are unchanged. No commit or merge was made.
