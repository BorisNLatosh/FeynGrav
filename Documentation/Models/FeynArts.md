# Supplied FeynArts models

[Documentation home](../README.md) · [All FeynGrav model sectors](../Models.md)

Each model has the same self-contained bundle layout:

```text
FeynArts/Models/
├── README.md
├── GR/
├── Single-Scalar-Tensor-Minimal-Coupling/
├── John/
├── Einstein-scalar-Gauss-Bonnet/
├── GR-SU-N-Yang-Mills/
└── QuadraticGravity/
```

Each model directory contains `<Model>.gen`, `<Model>.mod`, `manifest.json` and `README.md`.

| Model | Contents through $\kappa^2$ | Guide |
| --- | --- | --- |
| `GR` | Conventional GR and its gravitational ghosts | [GR](./GeneralRelativity/NativeModel.md) |
| `Single-Scalar-Tensor-Minimal-Coupling` | Massive scalar with independent positive cubic and quartic potential couplings; includes five- and six-leg contacts | [Single-Scalar-Tensor-Minimal-Coupling](./Scalar/NativeModel.md) |
| `John` | Massive scalar with the John derivative interaction | [John](./Horndeski/John/NativeModel.md) |
| `Einstein-scalar-Gauss-Bonnet` | Massive scalar with independent linear and quadratic scalar–Gauss–Bonnet couplings | [Einstein-scalar-Gauss-Bonnet](./ScalarGaussBonnet/NativeModel.md) |
| `GR-SU-N-Yang-Mills` | SU(N) gauge field, one massive Dirac quark flavour and both ghost species; includes five/six-leg mixed contacts | [GR-SU-N-Yang-Mills](./YangMills/NativeModel.md) |
| `QuadraticGravity` | Full quadratic-gravity tensor propagator, cubic and quartic graviton contacts, conventional FP ghosts | [QuadraticGravity](./QuadraticGravity/NativeModel.md) |

For every supplied model, `<Model>.gen` contains its kinematic structures and propagators; `<Model>.mod` contains fields and coupling coefficients; `manifest.json` records provenance and hashes; `README.md` documents use and verification limits. The directory name and the `.gen`/`.mod` basenames must agree.

<a name="Common loading workflow"></a>

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

This section records a historical naming change, not a verification date for the current checkout. Older saved output may still display former names and paths. The three scalar bundles also include the corrected minimal two-graviton scalar contact; replacing an older bundle requires recomputing affected results. For current scope, see [GR](../ScientificScope.md#General%20relativity), [scalar models](../ScientificScope.md#Scalar%20models) and [installation and compatibility](../ScientificScope.md#Installation%20and%20compatibility).

## Scalar extension verification

All three scalar bundles include the minimal TTSS contact independently checked against the canonical action. Both outgoing graviton Ward identities pass for their selected scalar–scalar to graviton–graviton trees with symbolic D, mass and couplings, on-shell states and momentum conservation. The scalar potential contacts have independent action comparisons; John has full independent off-shell cubic/quartic comparisons, and EsGB has an independent order-two curvature derivation. See [scalar-model scope](../ScientificScope.md#Scalar%20models) and [John verification](./Horndeski/John/NativeModel.md#Verification). These results do not establish higher-order EsGB sources, arbitrary amplitudes or loop validation.

## Yang–Mills model verification

The [GR + SU(N) guide](./YangMills/NativeModel.md) describes the 21 supplied interaction families through κ², including one massive fundamental quark flavour and two ghost species. Independent action and native-contact comparisons, selected massive-quark trees and direct bosonic Ward checks establish the stated [scientific scope](../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions), with symbolic D and N. Native loops remain unvalidated; the optional bridge remains restricted to GR.

**Upgrading an older model:** `F[1]` denotes the quark and `S[1]` the vector gravitational ghost. Update old explicit ghost selections and restart the kernel.