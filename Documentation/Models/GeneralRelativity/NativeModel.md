# Pure GR model for FeynArts

[Documentation home](../../README.md) · [GR action and model reference](../GeneralRelativity.md)

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

FeynGrav adds the package's `FeynArts/Models/GR` directory to ``FeynArts`$ModelPath`` when FeynArts is already loaded. If FeynArts was loaded later, or you want to load the model without loading FeynGrav, follow the [explicit model-path registration recipe](../FeynArts.md#Common%20loading%20workflow) with `modelName = "GR"`. The path is relative to the FeynGrav package root, not this documentation folder. Supply `GenericModel` explicitly: its FeynArts default is the stock `"Lorentz"` model.

The optional integration companion is not needed for native calls. If you intend to use it too, load `FeynArtsIntegration/FeynArtsIntegration.wl` **after FeynGrav but before model initialisation**, in a fresh kernel. Loading it after native initialisation can cause a `mom::shdw` namespace warning; the verified recipe uses the earlier loading order.

<a name="Construct and convert a four-graviton tree"></a>

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

FeynArts can omit a topology when its coupling is absent. The shipped finite model was checked for the contact and every exchange channel. For changed models/processes, use the [bridge's completeness check](../../Services/FeynArtsIntegration/Guide.md#Small%20explicit%20tree%20workflow); equal topology counts alone do not prove completeness. The quartic model is not a complete five-graviton model.

## Verification and provenance

Native ghost checks and the three-/four-graviton tree source-factor comparisons pass for the supplied model, including the quartic contact and all three exchange channels. These checks establish the tested native translation and signs; they do not certify arbitrary physical amplitudes or corrected-model loops. See [scientific scope](../../ScientificScope.md#General%20relativity).

**Upgrading an older model:** the ordinary gravitational ghost propagator was corrected to `-I MTD[mu,nu] FAD[p]`. Replace the matched model bundle and restart the kernel. Results obtained with the former sign are historical and need separate revalidation.

The manifest's absolute source-library paths record the build environment; model loading does not require that machine's paths. The two model files are self-contained. Rebuilding is optional; see the [builder guide](../../Services/FeynArtsIntegration/Guide.md).

[Native-model order limits](./HigherOrders.md).
