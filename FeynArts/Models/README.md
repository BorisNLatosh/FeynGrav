# FeynGrav FeynArts models

This directory mirrors the role of `FeynCalc/FeynArts/Models`: it holds the FeynArts model files that ship with FeynGrav, so they live inside the package tree instead of a benchmark directory.

## Contents

| File | Role |
| --- | --- |
| `PureGR.gen` | Generic model: kinematic structure and propagators for the pure-GR snapshot |
| `PureGR.mod` | Classes model: `T[1]` graviton, oriented `U[1]`/`-U[1]` ghost and antighost, cubic and quartic couplings |
| `manifest.json` | Provenance: gauge, dimension, field classes, interaction families, source-library hashes and model-file hashes |

The model is conventional pure GR in the metric parametrisation `g = eta + kappa h`, at fixed gauge parameter `GaugeFixingEpsilon = 2`, with symbolic dimension `D` and unassigned `kappa`. It contains the cubic and quartic graviton interactions and the oriented cubic ghost coupling, and it is the exact snapshot verified by the model-first checks.

The files were originally named `FGPureGR5a149c98161e2b2e`, spelling out the first 16 hex digits of the snapshot hash. They are now `PureGR`: a short name in FeynArts' own style (`SM`, `MSSM`, `Lorentz`), and a genuine symbol so that `PureGR` also parses as an identifier rather than a pattern. The snapshot is still pinned, by `SnapshotSHA256` and `ModelFileSHA256` in `manifest.json`.

## Loading

FeynArts finds model files on `$ModelPath`. FeynGrav adds this directory to that path automatically when patched FeynArts is already loaded, so no path handling is needed:

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;

InitializeModel["PureGR", GenericModel -> "PureGR"];
```

Request FeynArts while loading FeynGrav (`$LoadAddOns = {"FeynArts"}` before `<< FeynGrav``, or the legacy `$LoadFeynArts = True`); if FeynArts is loaded only afterwards, add this directory to `$ModelPath` yourself:

```wl
PrependTo[$ModelPath, FileNameJoin[{$UserBaseDirectory, "Applications", "FeynGrav", "FeynArts", "Models"}]];
```

`GenericModel` must be given explicitly. FeynArts' `InitializeModel[modname]` loads `modname.mod` **plus** the generic model named by the `GenericModel` option, whose default is the stock `"Lorentz"` file; this classes file does not name its own generic model.

## Provenance and integrity

The two model files match the SHA-256 values recorded in `manifest.json`:

- generic `.gen`: `9fff5547d9bb9cb9c548f5861699844006fe8dc7cdcc56620004c6deb48f67d9`
- classes `.mod`: `536144daf0106bf49c9d7c56e6858a447ba68c8287edb704272836606e5828a0`

They were originally built and verified under `Benchmark/FeynArtsEvidence/model-first-2026-10-12/`, whose duplicate copy has since been removed; this directory is now the single source. Renaming the files did not alter their bytes, so these hashes still apply; the rename and its checks are recorded in `Benchmark/FeynArtsEvidence/model-rename-puregr-2026-10-12/`, and the packaged copy's initialisation by name in `Benchmark/FeynArtsEvidence/model-relocation-2026-10-12/`.
