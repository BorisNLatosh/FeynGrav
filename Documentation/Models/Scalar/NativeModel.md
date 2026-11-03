# GR with a massive scalar and cubic/quartic potential

[Documentation home](../../README.md)

This native FeynArts model contains one real scalar of arbitrary symbolic mass and vertices through kappa squared per vertex. The action is

$$S=\int d^4x\sqrt{-g}\left[-2R/\kappa^2+\tfrac12(\partial\phi)^2-\tfrac12m^2\phi^2+\lambda_3\phi^3/3!+\lambda_4\phi^4/4!\right]+S_{gf}+S_{FP}.$$

Use ``FeynGrav`ScalarMass``, ``FeynGrav`ScalarLambda3`` and ``FeynGrav`ScalarLambda4`` as unassigned symbols. Specialise converted amplitudes by replacement. Both potential signs are positive in the action. The mass term is already included in the propagator and minimal gravitational couplings.

## Loading and calculation

In a fresh kernel:

```wl
$LoadAddOns = {"FeynArts"};
Needs["FeynGrav`"];
FeynArts`InitializeModel["Single-Scalar-Tensor-Minimal-Coupling",
  FeynArts`GenericModel -> "Single-Scalar-Tensor-Minimal-Coupling"];
top = FeynArts`CreateTopologies[0, 2 -> 2,
  FeynArts`Adjacencies -> {3,4,5,6}];
ins = FeynArts`InsertFields[top,
  {FeynArts`S[1],FeynArts`S[1]} -> {FeynArts`S[1],FeynArts`S[1]},
  FeynArts`Model -> "Single-Scalar-Tensor-Minimal-Coupling",
  FeynArts`GenericModel -> "Single-Scalar-Tensor-Minimal-Coupling",
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
raw = FeynArts`CreateFeynAmp[ins, FeynArts`PreFactor -> 1,
  FeynArts`GaugeRules -> {}, FeynArts`MomentumConservation -> True,
  FeynArts`Truncated -> True];
diagrams = FeynCalc`FCFAConvert[raw,
  FeynCalc`IncomingMomenta -> {p1,p2}, FeynCalc`OutgoingMomenta -> {q1,q2},
  FeynCalc`LorentzIndexNames -> {mu,nu,rho,sigma,a,b,c,d},
  FeynCalc`ChangeDimension -> D, FeynCalc`Contract -> False,
  List -> True, FeynCalc`Prefactor -> 1];
```

There are seven scalar 2-to-2 diagrams: the quartic contact, three scalar exchanges and three graviton exchanges. The model files are standalone: to use them without loading FeynGrav, load FeynCalc-patched FeynArts and register the package's `FeynArts/Models/Single-Scalar-Tensor-Minimal-Coupling` directory in ``FeynArts`$ModelPath``. Follow the [explicit model-path registration recipe](../FeynArts.md#Common%20loading%20workflow) with `modelName = "Single-Scalar-Tensor-Minimal-Coupling"`. This is a directory under the FeynGrav package root, not the documentation folder.

## Inventory and limits

The inherited families are TTT, TTTT, TUU, TSS and TTSS. Additional families are SSS, TSSS, TTSSS, SSSS, TSSSS and TTSSSS. Consequently five- and six-leg contacts are required despite the kappa-squared cutoff. Restricting topology adjacency to three and four omits these contacts in higher-multiplicity processes. The cutoff is per vertex, not total amplitude order.

GaugeFixingEpsilon is fixed at 2; the gravitational ghost propagator is negative imaginary with native Grassmann signs. The physical action is four-dimensional; symbolic D is its dimensional-regularisation continuation. External-state conditions and physical on-shell kinematics are not imposed by generation. No loops have been validated. Native scalar 1-to-3 construction is an algebraic crossing check, not a physical decay of one massive scalar to three identical massive scalars.

The six potential contacts agree exactly with action-derived expressions, and native contact and exchange factors have been compared with the source rules. The corrected minimal TTSS contact independently agrees with the canonical kinetic and mass terms off shell. Both outgoing graviton Ward identities pass for scalar–scalar to graviton–graviton scattering, with symbolic D, mass and couplings, on-shell external states and momentum conservation. These selected checks do not establish general crossed gravitational-amplitude equality or loop amplitudes. See [scientific scope](../../ScientificScope.md#Scalar%20models). Normal model use needs no build or FORM process.

## Minimal-scalar limit and naming

This single bundle replaces both former `GRScalar` and `GRScalarPotential` models. It contains an arbitrary scalar mass and independent cubic and quartic couplings. Set ``FeynGrav`ScalarLambda3`` and ``FeynGrav`ScalarLambda4`` to zero by replacement after conversion to recover the massive minimally coupled scalar theory without self-interactions. No separate minimal-only bundle is supplied. Its model name is a string, including the hyphens; always quote it.

## Minimal scalar normalisation update — 19 October 2026

This is historical upgrade guidance. Before this correction, the minimal two-scalar/two-graviton contact was smaller by a factor of two. The supplied bundle includes the corrected contact. When replacing an older model, restart the kernel and recompute affected amplitudes; existing saved notebook results do not update automatically. The verification scope above describes the corrected model. See [scientific scope](../../ScientificScope.md#Scalar%20models).

[Native-model order limits](./HigherOrders.md).
