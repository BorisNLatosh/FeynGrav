# Off-shell revision of the FeynArts s-channel notebook

This revision follows the user's correction: demonstrate ordinary FeynArts usage, not reproduce a fixed helicity amplitude from the neighbouring notebook.

- Incoming momenta are `p1,p2`; outgoing momenta are `p3,p4`, with no prescribed scalar products or virtualities.
- The same connectivity-selected s-channel is converted with `FCFAConvert`, retaining symbolic D and the massless internal propagator.
- Every external `PolarizationTensorD` explicitly uses `Transversality -> False`. Its factorised vector polarisations have neither a null-vector nor a normalisation condition imposed. Incoming/outgoing conjugation labels do not select helicities.
- No `SetMandelstam`, helicity tables, scalar-product assignments, assumptions, reference formula or on-shell output remains in the notebook.
- Short package command names are used after loading. The one explicit `FeynGrav` qualification on `importGravitons` in the bootstrap cell is retained because that whole cell is parsed before the package loads; the FeynArts qualifications remain explicit.
- `AmplitudeSChannel` contains the FORM-contracted off-shell contribution. Its preview cell uses `Short`; the large amplitude output is deliberately not embedded in the notebook file. It is recomputed on evaluation.

## Validation

`check-notebook.wls` executes the actual candidate notebook cells sequentially in a fresh kernel, replacing only the front-end `NotebookDirectory[]` call with the Examples directory. `check-notebook-final.log` records **14 checks passed**: one s-channel, outgoing-sum routing, the unconstrained propagator pole, successful scalar FORM output, symbolic external virtualities, all four longitudinal and trace contractions, actual longitudinal and trace terms in the amplitude, and absence of specialised kinematic code or repeated FeynCalc prefixes. No integration wrapper is loaded. The subsequent `check-notebook-installed.log` repeats the checks while omitting the large cached preview output.

The first successful run took 0.770 seconds for the contraction cell, under a verified aggregate 1-GiB cgroup with zero swap and group-OOM policy. Peak memory was 307,261,440 bytes, all cap/OOM event counters zero, and pre-run host headroom approximately 8.3 GiB. The full expression is about 16.2 MiB in kernel memory; retaining its factored input avoids a large intermediate open-index expansion.

The initial `check-notebook.log`/`check-notebook-structural-pole.wls` are retained: its only failing assertion expected an unexpanded composite-momentum Pair. `FeynAmpDenominatorExplicit` actually writes the identical pole as `1/(p3^2 + 2 p3.p4 + p4^2)`. The final test compares after typed scalar-product expansion; no physics or model change was needed. The preview was also corrected to explicit evaluable `Short[...]` boxes and its bulky headless rendering is not cached.

`build-notebook.wls` reconstructs the revision from the preserved previous `../notebook-s-channel-2026-10-12/validated.nb`, retaining that older on-shell evidence as historical. `candidate.nb` contains unevaluated cells; `validated.nb` adds the evaluated routing and off-shell-pole outputs and is copied to `Examples/Graviton_Scattering_Tree_Level_FeynArt.nb`. Graphical front-end rendering is not certified by these headless tests. The original neighbouring notebook, package code and model files are unchanged.

Use a fresh kernel when opening the revised notebook; a kernel in which the former helicity tables were evaluated retains their scalar-product assignments.
