# GR with a massive scalar and John interaction

[Documentation home](../../../README.md) · [Horndeski action and conventions](../../Horndeski.md)

The base action contains GR and the canonical scalar terms $\tfrac12 g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi-\tfrac12m^2\phi^2$, plus
$$\int d^4x\sqrt{-g}\,\beta G^{\mu\nu}\partial_\mu\phi\partial_\nu\phi.$$
The scalar has arbitrary mass ``FeynGrav`ScalarMass``; the independent coupling is ``FeynGrav`JohnBeta``. There is no extra half in the John term and no scalar self-potential. Expansion is about flat space and phi = 0. GaugeFixingEpsilon = 2 and the negative gravitational ghost propagator are inherited. Symbolic D is a dimensional-regularisation continuation of the four-dimensional action.

## Inventory

TTT, TTTT and TUU are unchanged. TSS and TTSS combine the minimal massive-scalar contribution with the John interaction through kappa squared. No duplicate coupling row and no free-propagator correction is introduced. John is constructed using G5 = -beta phi and the covariant contracted Bianchi identity, up to a boundary term.

## Use

For a complete scalar `2 -> 2` example, see example 10 (`Examples/10. Scalar_Scattering_John_Tree_Level_FeynArts.nb`). It displays all three graviton-exchange diagrams and calculates the on-shell amplitude directly, separating the terms of orders β⁰, β¹ and β².

Follow the [Single-Scalar-Tensor-Minimal-Coupling loading and native calculation example](../../Scalar/NativeModel.md), replacing both model names by `John`. Use a fresh kernel. The supplied files need no builder, source-library import or FORM execution at model load. Alternatively load FeynGrav after requesting FeynArts to register the folder automatically.

For use without loading FeynGrav, follow the [explicit model-path registration recipe](../../FeynArts.md#Common%20loading%20workflow) with `modelName = "John"`. Register `FeynArts/Models/John` under the FeynGrav package root, not this documentation folder.

For example, after loading FeynCalc-patched FeynArts and registering that model directory:
```wl
FeynArts`InitializeModel["John", FeynArts`GenericModel -> "John"];
top = FeynArts`CreateTopologies[0, 2 -> 2, FeynArts`Adjacencies -> {3,4}];
ins = FeynArts`InsertFields[top,
 {FeynArts`S[1],FeynArts`S[1]} -> {FeynArts`T[1],FeynArts`T[1]},
 FeynArts`Model -> "John", FeynArts`GenericModel -> "John",
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

Specialise mass and beta by replacement in converted expressions; beta = 0 recovers the minimal massive scalar sector (the unified scalar model with both potential couplings set to zero). The vertex cutoff does not discard products quadratic in beta or higher total powers of kappa. There are three graviton channels in scalar elastic scattering and four channels (contact, graviton exchange, two scalar exchanges) in scalar–scalar to graviton–graviton scattering.

<a name="Verification"></a>

## Verification

The full cubic and quartic John tensors and native contacts agree exactly with an independent expansion of the action through κ², with symbolic D and generic off-shell momenta subject to momentum conservation. Scalar and graviton permutation checks pass. Native cubic, mixed-contact and exchange factors in the two processes described above agree with the source rules; removing a required contact fails the comparison.

For scalar–scalar to graviton–graviton scattering, both outgoing graviton Ward identities pass with the corrected minimal contact, symbolic D, mass and coupling, on-shell external states and momentum conservation. These checks do not validate higher-order G5 libraries, arbitrary helicity observables or loops. See [scientific scope](../../../ScientificScope.md#Scalar%20models).



## Minimal scalar normalisation update — 19 October 2026

This is historical upgrade guidance. Before this correction, the minimal two-scalar/two-graviton contact was smaller by a factor of two. The supplied bundle includes the corrected contact. When replacing an older model, restart the kernel and recompute affected amplitudes; existing saved notebook results do not update automatically. The verification scope above describes the corrected model. See [scientific scope](../../../ScientificScope.md#Scalar%20models).

[Native-model order limits](./HigherOrders.md).
