# Supplied FeynArts models

Each model has the same self-contained bundle layout:

```text
FeynArts/Models/
├── README.md
├── PureGR/
├── GRScalar/
├── GRScalarPotential/
├── GRJohn/
└── GREsGB/
```

Each model directory contains `<Model>.gen`, `<Model>.mod`, `manifest.json` and `README.md`.

| Model | Contents through $\kappa^2$ | Guide |
| --- | --- | --- |
| `PureGR` | Conventional GR and its gravitational ghosts | [PureGR](PureGR/README.md) |
| `GRScalar` | The same GR sector plus one real minimally coupled scalar of symbolic mass | [GRScalar](GRScalar/README.md) |
| `GRScalarPotential` | Massive scalar with independent positive cubic and quartic potential couplings; includes five- and six-leg contacts | [GRScalarPotential](GRScalarPotential/README.md) |
| `GRJohn` | Massive scalar with the John derivative interaction | [GRJohn](GRJohn/README.md) |
| `GREsGB` | Massive scalar with independent linear and quadratic scalar–Gauss–Bonnet couplings | [GREsGB](GREsGB/README.md) |

For every supplied model, `<Model>.gen` contains its kinematic structures and propagators; `<Model>.mod` contains fields and coupling coefficients; `manifest.json` records provenance and hashes; `README.md` documents use and verification limits. Future models should follow this layout. The directory name and the `.gen`/`.mod` basenames must agree. Developer builders remain in `FeynArtsIntegration/` and write their files into the explicitly chosen model directory.

## Common loading workflow

Start a fresh kernel with FeynCalc-patched FeynArts available:

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;
FeynArts`InitializeModel["PureGR", FeynArts`GenericModel -> "PureGR"];
```

Use the chosen name from the table in both positions to load that model instead, starting a fresh kernel when switching models. Switching after a conversion in the same kernel produced FeynArts warnings in the checked environment; that workflow is not verified. FeynGrav registers the immediate model folders containing matching `.gen` and `.mod` files in ``FeynArts`$ModelPath`` when FeynArts is already loaded. Registration neither loads FeynArts nor initialises a model and does not add duplicate paths.

To use the files without loading FeynGrav, or when FeynArts was loaded later, prepend the selected model folder explicitly:

```wl
$LoadAddOns = {"FeynArts"};
Needs["FeynCalc`"];
root = DirectoryName[FindFile["FeynGrav`"]];
modelName = "GRScalar"; (* or "PureGR" *)
modelDirectory = FileNameJoin[{root, "FeynArts", "Models", modelName}];
FeynArts`$ModelPath = Prepend[
  DeleteCases[FeynArts`$ModelPath, modelDirectory], modelDirectory];
FeynArts`InitializeModel[modelName, FeynArts`GenericModel -> modelName];
```

Both model files are self-contained; the manifest's historical absolute source paths are not loading dependencies. Supply `GenericModel` explicitly. Construct diagrams through native FeynArts as shown in each guide; the optional integration companion remains a pure-GR interface.

## Migration from the flat PureGR directory

PureGR's `.gen`, `.mod` and manifest moved unchanged from `FeynArts/Models/` to `FeynArts/Models/PureGR/`. Update manually configured model/manifest paths and restart the kernel. No duplicate flat model files or forwarding stubs are kept. Existing notebook input that loads FeynGrav and selects PureGR by name continues to work; its saved output may still show the old historical path.

The [layout verification](../../Documentation/Verification/ModelLayout.md) records the focused registration/loading check. Existing algebra checks remain in the [PureGR](../../Documentation/Verification/PureGRTreeRelease.md) and [GRScalar](../../Documentation/Verification/GRScalarTreeRelease.md) reports. Loop and general on-shell/Ward validation remain outside this tree delivery.

## Scalar extension verification

`GREsGB` is supplied after correction of its two-graviton source and stored library. Nine source/regeneration checks, six construction checks and twenty native checks passed. Higher-order EsGB source routines remain outside this delivery. See [scalar-model verification](../../Documentation/Verification/ScalarModelExtensions.md). John has exact independent conformal-projection checks, not a full independent component proof.
