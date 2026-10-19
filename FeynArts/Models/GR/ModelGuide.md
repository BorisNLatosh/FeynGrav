# Pure GR model for FeynArts

[Directory overview](README.md)

`GR` is ready for native three- and four-graviton tree construction with FeynCalc-patched FeynArts. No model build or FORM installation is needed.

## Scope and conventions

The finite model uses `g = eta + kappa h`, `GaugeFixingEpsilon = 2`, symbolic `D` and unassigned κ. It contains a symmetric `T[1]` graviton, an oriented `U[1]` ghost and `-U[1]` antighost, the cubic and quartic graviton vertices, and the cubic ghost coupling. Source vertex momenta are all incoming. Dimensional continuation is used as a dimensional-regularisation setup.

The ghost propagator is **`-I MTD[mu,nu] FAD[p]`**. Its cubic vertex and FeynArts' native Grassmann signs are unchanged. The selected linear Faddeev–Popov operator needs no two-graviton ghost contact.

| File | Role |
| --- | --- |
| `GR.gen` | Kinematic structures, paired graviton indices and propagators |
| `GR.mod` | Particle classes and matching coupling-coefficient vectors |
| `manifest.json` | Gauge, dimension, included orders, source-library provenance and model hashes |

This release is validated for **tree construction and source-translation fidelity**. Loop compatibility checks are deferred. The optional bridge rejects this snapshot's one-loop requests with `GhostCompletenessGate`; its old loop pins have deliberately not been updated. Native FeynArts does not enforce that bridge gate, so native loop generation must not be mistaken for validated loop support. On-shell, helicity and Ward-identity tests remain separate.

## Load the supplied model

Start a fresh kernel after updating model files. With FeynGrav installed in Mathematica's application path:

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;
FeynArts`InitializeModel["GR", FeynArts`GenericModel -> "GR"];
```

FeynGrav adds this directory to ``FeynArts`$ModelPath`` when FeynArts is already loaded. If it was loaded later, prepend the absolute path to this `FeynArts/Models/GR` directory yourself. Supply `GenericModel` explicitly: its FeynArts default is the stock `"Lorentz"` model.

The optional integration companion is not needed for native calls. If you intend to use it too, load `FeynArtsIntegration/FeynArtsIntegration.wl` **after FeynGrav but before model initialisation**, in a fresh kernel. Loading it after native initialisation can cause a `mom::shdw` namespace warning; the verified recipe uses the earlier loading order.

## Construct and convert a four-graviton tree

The following native workflow corresponds to the tested tree recipe:

```wl
top = FeynArts`CreateTopologies[0, 2 -> 2,
  FeynArts`Adjacencies -> {3, 4}];
ins = FeynArts`InsertFields[top,
  {FeynArts`T[1], FeynArts`T[1]} -> {FeynArts`T[1], FeynArts`T[1]},
  FeynArts`Model -> "GR", FeynArts`GenericModel -> "GR",
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
raw = FeynArts`CreateFeynAmp[ins,
  FeynArts`PreFactor -> 1, FeynArts`GaugeRules -> {},
  FeynArts`MomentumConservation -> True, FeynArts`Truncated -> True];
diagrams = FeynCalc`FCFAConvert[raw,
  FeynCalc`IncomingMomenta -> {p1, p2},
  FeynCalc`OutgoingMomenta -> {q1, q2},
  FeynCalc`LorentzIndexNames ->
    {mu1, nu1, mu2, nu2, mu3, nu3, mu4, nu4, a, b, c, d},
  FeynCalc`ChangeDimension -> D, FeynCalc`Contract -> False,
  List -> True, FeynCalc`Prefactor -> 1];
(* Optional: FeynArts`Paint[ins] *)
```

This produces the contact and all three exchange diagrams, retaining two free indices per external graviton. For the three-point tree, use `1 -> 2`, `{T[1]} -> {T[1],T[1]}`, one incoming and two outgoing momenta, and six Lorentz-index names, with the same prefactor and truncation settings.

`PreFactor -> 1` preserves the source vertex normalisation; FeynArts supplies graph/statistics factors. `MomentumConservation -> True` resolves internal routing without eliminating an external momentum. `Truncated -> True` removes external polarisation markers. Supply physical external states and on-shell conditions explicitly afterwards. Avoid expanding all exchange products merely to inspect them.

FeynArts can omit a topology when its coupling is absent. The shipped finite model was checked for the contact and every exchange channel. For changed models/processes, use the [bridge's completeness check](../../../FeynArtsIntegration/BridgeGuide.md#small-explicit-tree-workflow); equal topology counts alone do not prove completeness. The quartic model is not a complete five-graviton model.

## Verification and provenance

The corrected candidate was installed byte-for-byte on 18 October 2026. Native ghost checks and tree source-factor comparisons passed; see the [verification summary](../../../Documentation/Verification/PureGRTreeRelease.md). The classes file is unchanged; the only generic-file change from the previous shipped snapshot is the ghost propagator sign.

- Generic SHA-256: `f5f86c7242ea1fcbcee7d8cb5a85a8cf7b386e24e7662783b684dcf11ceee76d`
- Classes SHA-256: `536144daf0106bf49c9d7c56e6858a447ba68c8287edb704272836606e5828a0`
- Snapshot: `77cbd95462c8827a0bf20d06d8f05d3f2a351c0a5234e367015a7a519d9ab3f7`

The manifest's absolute source-library paths record the build environment; model loading does not require that machine's paths. The two model files are self-contained. Rebuilding is optional; see the [builder guide](../../../FeynArtsIntegration/BridgeGuide.md).
