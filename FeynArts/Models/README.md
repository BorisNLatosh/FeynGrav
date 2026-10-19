# Supplied FeynArts models

Each model has the same self-contained bundle layout:

```text
FeynArts/Models/
├── README.md
├── GR/
├── Single-Scalar-Tensor-Minimal-Coupling/
├── John/
└── Einstein-scalar-Gauss-Bonnet/
```

Each model directory contains `<Model>.gen`, `<Model>.mod`, `manifest.json` and `README.md`.

| Model | Contents through $\kappa^2$ | Guide |
| --- | --- | --- |
| `GR` | Conventional GR and its gravitational ghosts | [GR](GR/README.md) |
| `Single-Scalar-Tensor-Minimal-Coupling` | Massive scalar with independent positive cubic and quartic potential couplings; includes five- and six-leg contacts | [Single-Scalar-Tensor-Minimal-Coupling](Single-Scalar-Tensor-Minimal-Coupling/README.md) |
| `John` | Massive scalar with the John derivative interaction | [John](John/README.md) |
| `Einstein-scalar-Gauss-Bonnet` | Massive scalar with independent linear and quadratic scalar–Gauss–Bonnet couplings | [Einstein-scalar-Gauss-Bonnet](Einstein-scalar-Gauss-Bonnet/README.md) |

For every supplied model, `<Model>.gen` contains its kinematic structures and propagators; `<Model>.mod` contains fields and coupling coefficients; `manifest.json` records provenance and hashes; `README.md` documents use and verification limits. Future models should follow this layout. The directory name and the `.gen`/`.mod` basenames must agree. Developer builders remain in `FeynArtsIntegration/` and write their files into the explicitly chosen model directory.

## Common loading workflow

Start a fresh kernel with FeynCalc-patched FeynArts available:

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;
FeynArts`InitializeModel["GR", FeynArts`GenericModel -> "GR"];
```

Use the chosen name from the table in both positions to load that model instead, starting a fresh kernel when switching models. Switching after a conversion in the same kernel produced FeynArts warnings in the checked environment; that workflow is not verified. FeynGrav registers the immediate model folders containing matching `.gen` and `.mod` files in ``FeynArts`$ModelPath`` when FeynArts is already loaded. Registration neither loads FeynArts nor initialises a model and does not add duplicate paths.

To use the files without loading FeynGrav, or when FeynArts was loaded later, prepend the selected model folder explicitly:

```wl
$LoadAddOns = {"FeynArts"};
Needs["FeynCalc`"];
root = DirectoryName[FindFile["FeynGrav`"]];
modelName = "Single-Scalar-Tensor-Minimal-Coupling"; (* or "GR" *)
modelDirectory = FileNameJoin[{root, "FeynArts", "Models", modelName}];
FeynArts`$ModelPath = Prepend[
  DeleteCases[FeynArts`$ModelPath, modelDirectory], modelDirectory];
FeynArts`InitializeModel[modelName, FeynArts`GenericModel -> modelName];
```

Both model files are self-contained; the manifest's historical absolute source paths are not loading dependencies. Supply `GenericModel` explicitly. Construct diagrams through native FeynArts as shown in each guide; the optional integration companion remains a pure-GR interface.

## Naming migration — 19 October 2026

| Former name | Current name |
| --- | --- |
| `PureGR` | `GR` |
| `GREsGB` | `Einstein-scalar-Gauss-Bonnet` |
| `GRJohn` | `John` |
| `GRScalar` and `GRScalarPotential` | `Single-Scalar-Tensor-Minimal-Coupling` |

Use the exact quoted string for both `Model` and `GenericModel`. Folder names and `.gen`/`.mod` basenames match these strings. Update manually configured paths and restart the kernel; no old-name aliases or duplicate bundles remain. The unified scalar bundle retains the potential model's arbitrary mass and independent cubic/quartic couplings. Setting both couplings to zero recovers the minimal-only theory.

The generated generic/classes expressions are unchanged from the previously verified bundles; the redundant minimal-only files have been removed. Historical verification reports and saved notebook output retain the old names and paths. The supplied notebook's model-selection input now uses `GR`.

Earlier checks are recorded in [GR](../../Documentation/Verification/PureGRTreeRelease.md), [minimal scalar](../../Documentation/Verification/GRScalarTreeRelease.md) and [scalar extensions](../../Documentation/Verification/ScalarModelExtensions.md). The earlier [layout report](../../Documentation/Verification/ModelLayout.md) predates this rename. Loop and general on-shell/Ward validation remain outside this tree delivery.

## Scalar extension verification

`Einstein-scalar-Gauss-Bonnet` is supplied after correction of its two-graviton source and stored library. Nine source/regeneration checks, six construction checks and twenty native checks passed. Higher-order EsGB source routines remain outside this delivery. See [scalar-model verification](../../Documentation/Verification/ScalarModelExtensions.md). John has exact independent conformal-projection checks, not a full independent component proof.

The [19 October naming checks](../../Documentation/Verification/ModelNames.md) verify all four exact names, including hyphenated names, in fresh kernels.
