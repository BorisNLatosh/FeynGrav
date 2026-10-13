# Relocation of the pure-GR FeynArts model into the package

Executed 12 October 2026. Model bytes were copied, never regenerated, and then the duplicate was removed. No rule, library or other package file was modified apart from the small `FeynGrav.wl` addition described below. No commit or merge was made.

> The model files listed below were later renamed from `FGPureGR<hash>.gen/.mod` to `PureGR.gen/.mod`, with no change of bytes; see [../model-rename-puregr-2026-10-12/README.md](../model-rename-puregr-2026-10-12/README.md). This page is the record of the relocation as it happened.

## New canonical location

```
FeynGrav/FeynArts/Models/
├── FGPureGR5a149c98161e2b2e.gen
├── FGPureGR5a149c98161e2b2e.mod
├── manifest.json
└── README.md
```

This mirrors the role of `FeynCalc/FeynArts/Models`, which holds the FeynArts models shipped with FeynCalc. The previous copy under `Benchmark/FeynArtsEvidence/model-first-2026-10-12/model/` was **deleted** by the user's decision, leaving a single source.

Integrity was checked before deletion: both files were byte-identical between the two locations and matched the `ModelFileSHA256` entries of `manifest.json` — generic `9fff5547…f67d9`, classes `536144da…5828a0`. The copied manifest was byte-identical too. Verification that the packaged copy initialises by name and reproduces four diagrams: `check-packaged-model.wls/.log`.

## Package change: automatic model path registration

`FeynGrav.wl` now ends its initialisation with:

```wl
FeynGravModelDirectory = FileNameJoin[{packageDirectory, "FeynArts", "Models"}];

If[MemberQ[$Packages, "FeynArts`"] &&
   DirectoryQ[FeynGravModelDirectory] &&
   !MemberQ[ExpandFileName /@ FeynArts`$ModelPath, ExpandFileName[FeynGravModelDirectory]],
    PrependTo[FeynArts`$ModelPath, FeynGravModelDirectory]
];
```

`FeynGravModelDirectory` lives in the `FeynGrav`Private`` context and is not public API. FeynGrav still never loads FeynArts itself: the block only extends a search path that already exists, and does nothing when FeynArts is absent. FeynArts must be requested while FeynGrav loads (`$LoadAddOns = {"FeynArts"}` or the legacy `$LoadFeynArts = True`) for the registration to happen.

Side effect, verified and benign: because the symbol name appears in the package text, reading `FeynGrav.wl` creates the (undefined) symbol `FeynArts`$ModelPath` even when FeynArts is not loaded. `Names["FeynArts`*"]` therefore grows from 20 to 21 entries in that situation; the other 20 come from FeynCalc. FeynArts loads and defines the symbol normally afterwards.

## Verification

- `check-package-registration.wls/.log`: `$ModelPath` head is the packaged directory after `$LoadAddOns = {"FeynArts"}; << FeynGrav``, registered exactly once, and `InitializeModel["FGPureGR5a149c98161e2b2e", GenericModel -> "FGPureGR5a149c98161e2b2e"]` succeeds **with no path handling at all**, giving four diagrams, denominator counts `{0,1,1,1}` and a contact equal to the source.
- `check-no-feynarts.wls/.log`: without any FeynArts request, FeynGrav still loads cleanly (`FeynArts_LOADED=False`, not on `$ContextPath`, no error messages), the quartic vertex and propagator evaluate, and package initialisation reports success. This is the regression check for the package change.
- `check-notebook.wls/.log`: all nine cells of the updated example notebook, **18 checks passed**, exit 0. The model directory on `$ModelPath` is the packaged one, the model loads by name, twenty unqualified names resolve to `FeynArts``, four diagrams with routes `{-p2+p3, -p2+p4, p3+p4}`, `\[ScriptCapitalM] === Total[diagramAmplitudes]`, the contact equals the source, off-shell virtualities/longitudinal/trace terms survive, no `FeynArts`` prefix and no benchmark path remain in any input cell, and the only qualified name left is `FeynGrav`` in the load line. Peak memory 319,291,392 bytes under a verified 1-GiB cgroup, zero swap, zero cap/OOM events.
- `direct-three-after-move.log`: the earlier evidence harness still runs after the deletion, falling back to `FeynArts/Models` (`MODEL_DIR=` reported, `MODEL_HASHES_VERIFIED=True`) and passing its 10 assertions.

## Notebook changes

`Examples/Graviton_Scattering_Tree_Level_FeynArt.nb`:

- The initialisation cell is now just
  ```wl
  grModel = "FGPureGR5a149c98161e2b2e";
  InitializeModel[grModel, GenericModel -> grModel];
  ```
  The `$ModelPath` line was removed because the package registers it.
- The `Code` cell no longer calls `importGravitons[2]`. Package initialisation already imports orders `{1,2}`, and the explicit call created a `Global`` symbol before FeynGrav loaded, producing an `importGravitons::shdw` warning.

## Superseded runs

Retained as harness errors, not model or notebook defects:

- `check-notebook-undefined-grmodel.log`: an earlier rewrite dropped the `grModel` assignment while later cells still pass `Model -> grModel`, so FeynArts tried to open `grModel.gen` and aborted.
- `check-notebook-relative-path.log`: the notebook stored an unexpanded `..` path and the canonical-directory assertion failed.
- `check-notebook-qualified-extraction.log` and `check-notebook-quoted-name.log`: the qualified-name assertion compared against `"FeynGrav`"` while notebook boxes store the name as text that already contains its quote characters. The extraction now strips them.
- `check-packaged-model-count-bug.log`: the probe counted the bare `PropagatorDenominator` head instead of `_PropagatorDenominator`, reporting `{0,0,0,0}`.

## References updated

- `Benchmark/FeynArtsEvidence/model-first-2026-10-12/README.md` reproduction command no longer sets `FGFA_MODEL_DIR`, and its `setup.wl` falls back to the packaged directory while still verifying the manifest hashes. `run-provenance.json` deliberately still lists the original paths as the frozen record of that run.
- `Benchmark/FeynArtsEvidence/model-relocation-2026-10-12/README.md` (this file), `FeynArts/Models/README.md` and the main `README.md`.
- Historical logs elsewhere still mention the deleted path; they are records of runs made before the move and were left untouched. The probe scripts in `notebook-loading-2026-10-12/` hard-code that old path and are now historical only.
