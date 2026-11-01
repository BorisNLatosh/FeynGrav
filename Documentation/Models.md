# Models and interaction sectors

[Getting started](GettingStarted.md) · [Common conventions](Conventions.md) · [Documentation index](README.md)

**Inventory of the 4.2.0 development checkout, checked 1 November 2026.**
Use this page to find the physics description, commands and available FeynArts
bundle for a model. General relativity and Horndeski gravity have central model pages; other
entries link to their existing guides and reference sections as documentation
is consolidated.

## Reading the inventory

FeynGrav supplies both main-package rules and rule-generation routines. Most
main-package vertices read stored libraries; their availability is limited to
the files installed and the orders imported into the current kernel. The
generators have separate interfaces and restrictions. A supplied FeynArts
bundle is a finite collection of translated rules with its own field content
and cutoff.

These are distinct statements: a generator exists, a rule has been generated,
a rule is loaded, and a rule has passed an independent mathematical check.
The entries below identify implementation scope, not a blanket certification
of every interaction or amplitude. Read the [common conventions](Conventions.md)
before comparing actions or combining sectors.

## Gravity

| Model or formulation | Documentation and commands | Native FeynArts support and limits |
| --- | --- | --- |
| General relativity in conventional metric variables | [GR model page](Models/GeneralRelativity.md): action, gauge fixing, ghosts, propagators, vertices and examples | `GR`: cubic/quartic graviton vertices and the cubic ghost coupling, through κ² per vertex. The separate stored five-graviton rule is [independently verified off shell](Verification/GravitonQuintic-2026-11-01.md), but is not included in the shipped native model. |
| General relativity in Cheung–Remmen variables | [CR interface and scope](Models/GeneralRelativity.md#cheungremmen-interface): graviton and auxiliary-field propagators, three cubic bosonic vertex commands | No CR bundle. This is a different parametrisation of GR, not an additional physical model; conventional-model verification does not validate it. |
| Quadratic gravity | [Propagators](Reference.md#propagators), [vertices](Reference.md#graviton-self-interactions-and-gravitational-ghosts) and [gauge parameters](Reference.md#gauge-parameters-and-coupling); conventional and higher-derivative gauge-fixing interfaces | No supplied bundle; native integration remains planned. Stored rules and generators have their own scope. |

`GravitonPropagatorMassive` additionally supplies a massive spin-2 propagator.
It is a [propagator utility](Reference.md#propagators), not a complete
interacting massive-gravity model.

## Matter coupled to gravity

| Sector | Documentation and commands | Native FeynArts support and limits |
| --- | --- | --- |
| Minimally coupled scalar with mass and potential | [Scalar rules](Reference.md#scalars-fermions-and-vectors); [scalar model action and guide](../FeynArts/Models/Single-Scalar-Tensor-Minimal-Coupling/ModelGuide.md) | `Single-Scalar-Tensor-Minimal-Coupling`: one real scalar, arbitrary mass, independent cubic and quartic potential couplings, through κ² per vertex. The general potential-rule interface is not restricted to those two monomials. |
| Minimally coupled Dirac fermion | [Fermion rules](Reference.md#scalars-fermions-and-vectors) | No separate fermion bundle. A massive fundamental Dirac flavour is included in the Yang–Mills bundle below. |
| Minimally coupled massive vector (Proca) | [Vector rules](Reference.md#scalars-fermions-and-vectors) and [Proca propagator](Reference.md#propagators) | No separate bundle. The Proca propagator's massless limit requires separate treatment. |
| Massless Abelian vector, with its gauge-fixing and ghost sector | [Vector and vector-ghost rules](Reference.md#scalars-fermions-and-vectors) | No separate bundle. This entry does not assert a complete charged-matter QED model. |
| SU(N) Yang–Mills, quarks and ghosts coupled to gravity | [Yang–Mills rules](Reference.md#sun-yangmills-interactions) and [native model action and guide](../FeynArts/Models/GR-SU-N-Yang-Mills/ModelGuide.md) | `GR-SU-N-Yang-Mills`: symbolic N, one arbitrary-mass fundamental Dirac flavour and both ghost species, through κ² per vertex. |

## Non-minimal and scalar–tensor interactions

| Sector | Documentation and commands | Native FeynArts support and limits |
| --- | --- | --- |
| Horndeski G2, G3, G4 and G5 monomial interactions | [Horndeski model page](Models/Horndeski.md): corrected action conventions, monomial normalisations, commands, inventory and [implementation audits](Verification/HorndeskiImplementationPlan.md) | No general Horndeski bundle. Installed `(a,b,n)` combinations are finite. [G2/G3/G5 audits](Verification/HorndeskiAudit-2026-11-01.md) cover the stated finite selections; [G4 source accepted](Verification/HorndeskiG4-2026-11-01.md) and [all shipped libraries reconciled](Verification/HorndeskiLibraryMigration-2026-11-01.md); [G5 TII supports higher graviton orders](Verification/HorndeskiG5HigherOrders-2026-11-01.md) within the inverse-metric bounds b <= 4 at n = 1, b <= 3 at n = 2 and b <= 2 at n >= 3; b = 0 remains a separate route. Verification includes the complete n = 4, a = 0, b = 1 vertex and selected higher T8 sectors, without full n = 5, arbitrary-order runtime or loop certification. No additional stored library or native-model order is supplied. |
| John derivative coupling | [John action and guide](../FeynArts/Models/John/ModelGuide.md): massive scalar with $\beta G^{\mu\nu}\partial_\mu\phi\,\partial_\nu\phi$ | `John`, through κ² per vertex. This particular Horndeski interaction has its own verification and is useful as a separate model entry. |
| Einstein–scalar–Gauss–Bonnet | [EsGB action and guide](../FeynArts/Models/Einstein-scalar-Gauss-Bonnet/ModelGuide.md) and [scalar–Gauss–Bonnet interface](Reference.md#horndeski-axion-and-scalargaussbonnet-interactions) | `Einstein-scalar-Gauss-Bonnet`: massive scalar, independent linear and quadratic scalar–Gauss–Bonnet couplings, through κ² per vertex. Higher-order source validation remains open. |
| Axion-like scalar–Abelian-vector interactions coupled to gravity | [Axion interface](Reference.md#horndeski-axion-and-scalargaussbonnet-interactions) and [generator contracts](../Rules/Contracts.md) | No bundle. Rules are present, but the epsilon-density/action convention remains unresolved. |

## Using a model

For direct rules, start with the model's commands and required
[library imports](Reference.md#library-importers). Query
`FeynGravLibraryInformation[]` for actual imported orders and provenance.
An unmatched vertex call is not evidence that an interaction vanishes.

For native diagrams, use the exact name from the
[five supplied FeynArts bundles](../FeynArts/Models/ModelCatalogue.md), in both
`Model` and `GenericModel`, with a fresh kernel. The cutoff applies to each
vertex; products of allowed vertices can have higher total powers of κ.
Selected tree checks do not establish general native loop support.

Each complete model page should connect its action, fields and parameters to
its gauge fixing, available rules, library orders, commands, examples and
verification limits. Begin with [General relativity](Models/GeneralRelativity.md)
for this structure, or choose a [worked example](../Examples/README.md).
