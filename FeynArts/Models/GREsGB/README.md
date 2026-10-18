# Einstein–scalar–Gauss–Bonnet model

`GREsGB` contains GR and one real scalar of arbitrary symbolic mass, with action

$$S=\int d^4x\sqrt{-g}\left[-\frac{2R}{\kappa^2}+\frac12g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi-\frac12m^2\phi^2+\left(g_1\phi+\frac{g_2}{2!}\phi^2\right)\mathcal G\right]+S_{\mathrm{gf}}+S_{\mathrm{FP}},$$

$$\mathcal G=R_{abcd}R^{abcd}-4R_{ab}R^{ab}+R^2.$$

The parameter symbols are ``FeynGrav`ScalarMass``, ``FeynGrav`GaussBonnetG1`` and ``FeynGrav`GaussBonnetG2``. They remain unassigned; specialise them by replacement after conversion. Expansion is about flat space and zero scalar background, with g = eta + kappa h. GaugeFixingEpsilon = 2 and the accepted negative gravitational ghost propagator are inherited from PureGR. All vertex momenta are incoming. Symbolic D provides a dimensional-regularisation continuation of the four-dimensional action; no 1/(D-4) rescaling or four-dimensional curvature identity is used.

## Vertex inventory

| Family | Content |
| --- | --- |
| TTT, TTTT, TUU | Unchanged GR and gravitational ghost sector |
| TSS | Minimal massive scalar coupling |
| TTSS | Minimal massive scalar coupling plus kappa-squared g2 Gauss–Bonnet contribution |
| TTS | kappa-squared g1 Gauss–Bonnet contribution |

The two scalar factorial cancels under differentiation. There are no lower-order Gauss–Bonnet vertices and no additional propagator or mass insertion. Both zero couplings recover GRScalar.

The cutoff is **per vertex**, through kappa squared. A diagram may contain two TTS vertices and therefore be proportional to kappa to the fourth power and g1 squared. The native model does not remove those diagrams.

## Native workflow

Use a fresh kernel and FeynCalc-patched FeynArts:

```wl
$LoadAddOns = {"FeynArts"};
Needs["FeynGrav`"];
FeynArts`InitializeModel["GREsGB", FeynArts`GenericModel -> "GREsGB"];
top = FeynArts`CreateTopologies[0, 2 -> 2,
  FeynArts`Adjacencies -> {3, 4}];
ins = FeynArts`InsertFields[top,
  {FeynArts`S[1], FeynArts`S[1]} -> {FeynArts`T[1], FeynArts`T[1]},
  FeynArts`Model -> "GREsGB", FeynArts`GenericModel -> "GREsGB",
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
raw = FeynArts`CreateFeynAmp[ins,
  FeynArts`PreFactor -> 1, FeynArts`GaugeRules -> {},
  FeynArts`MomentumConservation -> True, FeynArts`Truncated -> True];
diagrams = FeynCalc`FCFAConvert[raw,
  FeynCalc`IncomingMomenta -> {p1, p2},
  FeynCalc`OutgoingMomenta -> {q1, q2},
  FeynCalc`LorentzIndexNames -> {mu, nu, rho, sigma, a, b, c, d},
  FeynCalc`ChangeDimension -> D, FeynCalc`Contract -> False,
  List -> True, FeynCalc`Prefactor -> 1];
specialised = diagrams /. {
  FeynGrav`ScalarMass -> mass,
  FeynGrav`GaussBonnetG1 -> coupling1,
  FeynGrav`GaussBonnetG2 -> coupling2};
```

The mixed process contains six diagrams: the combined TTSS contact; one minimal graviton exchange; two minimal scalar exchanges; and two g1-squared graviton exchanges. Four-graviton scattering additionally has three massive-scalar exchanges, one in each channel, beside the four pure-GR diagrams. Completeness must include the internal particle species, not merely the number of requested topologies.

To load without FeynGrav, load FeynCalc with its FeynArts add-on and add this directory to ``FeynArts`$ModelPath`` before initialisation. The `.gen` and `.mod` files are self-contained; their historical source paths are not runtime dependencies. The optional pure-GR bridge's process restrictions remain unchanged.

## Verification and limitations

The corrected order-two library agrees exactly with an independent linearised-curvature derivation, including both graviton Ward identities. Native contacts and selected exchange factors are checked against that source with their signed momentum routes. See [the verification report](../../../Documentation/Verification/ScalarModelExtensions.md) for checks, retained failures and resource limits.

This model has no loop validation, no general physical on-shell amplitude proof and no higher-order EsGB vertices. Higher-order source routines retain the legacy contraction and require a separate audit; this delivery makes no claim about them. Do not reuse already imported old EsGB rules: restart the kernel or explicitly reimport the corrected library.

The developer command `FeynArtsIntegration/BuildGREsGB.wl` takes a fresh absolute output directory. Normal model use requires neither that script, FORM nor interaction-library generation.
