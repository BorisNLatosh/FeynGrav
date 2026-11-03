# Models and interaction sectors

[Getting started](./GettingStarted.md) · [Common conventions](./Conventions.md) · [Documentation index](./README.md)

**Inventory of the 4.2.0 development checkout, checked 1 November 2026.**
Use this page to find the physics description, commands and available FeynArts
bundle for a model. Each sector below has a model overview and links to its individual command pages.
The detailed native-model guides retain their existing locations.

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
of every interaction or amplitude. Read the [common conventions](./Conventions.md)
before comparing actions or combining sectors.

## Gravity

| Model or formulation | Documentation and commands | Native FeynArts support and limits |
| --- | --- | --- |
| General relativity in conventional metric variables | [GR model page](./Models/GeneralRelativity/README.md): action, gauge fixing, ghosts, propagators, vertices and examples | `GR`: cubic/quartic graviton vertices and the cubic ghost coupling, through κ² per vertex. The separate stored five-graviton rule is [Scientific scope](./ScientificScope.md#General%20relativity), but is not included in the shipped native model. |
| General relativity in Cheung–Remmen variables | [CR interface and scope](./Models/GeneralRelativity/README.md#Cheung%E2%80%93Remmen%20interface): graviton and auxiliary-field propagators, three cubic bosonic vertex commands | No CR bundle. This is a different parametrisation of GR, not an additional physical model; conventional-model verification does not validate it. |
| Quadratic gravity | [Quadratic-gravity overview and commands](./Models/QuadraticGravity/README.md); conventional and higher-derivative gauge-fixing interfaces | No supplied bundle; native integration remains planned. Stored rules and generators have their own scope. |

`GravitonPropagatorMassive` additionally supplies a massive spin-2 propagator.
It is a [propagator utility](./Models/MassiveSpin2/README.md), not a complete
interacting massive-gravity model.

## Matter coupled to gravity

| Sector | Documentation and commands | Native FeynArts support and limits |
| --- | --- | --- |
| Minimally coupled scalar with mass and potential | [Scalar overview and commands](./Models/Scalar/README.md); [scalar model action and guide](./Models/Scalar/NativeModel.md) | `Single-Scalar-Tensor-Minimal-Coupling`: one real scalar, arbitrary mass, independent cubic and quartic potential couplings, through κ² per vertex. The general potential-rule interface is not restricted to those two monomials. |
| Minimally coupled Dirac fermion | [Fermion overview and command](./Models/Fermion/README.md) | No separate fermion bundle. A massive fundamental Dirac flavour is included in the Yang–Mills bundle below. |
| Minimally coupled massive vector (Proca) | [Vector overview and commands](./Models/Vector/README.md) and [Proca propagator](./Models/Vector/ProcaPropagator.md) | No separate bundle. The Proca propagator's massless limit requires separate treatment. |
| Massless Abelian vector, with its gauge-fixing and ghost sector | [Vector and vector-ghost rules](./Models/Vector/README.md) | No separate bundle. This entry does not assert a complete charged-matter QED model. |
| SU(N) Yang–Mills, quarks and ghosts coupled to gravity | [Yang–Mills overview and commands](./Models/YangMills/README.md) and [native model action and guide](./Models/YangMills/NativeModel.md) | `GR-SU-N-Yang-Mills`: symbolic N, one arbitrary-mass fundamental Dirac flavour and both ghost species, through κ² per vertex. |

## Non-minimal and scalar–tensor interactions

| Sector | Documentation and commands | Native FeynArts support and limits |
| --- | --- | --- |
| Horndeski G2, G3, G4 and G5 monomial interactions | [Horndeski model page](./Models/Horndeski/README.md): corrected action conventions, monomial normalisations, commands, inventory and [scientific scope](./ScientificScope.md#Horndeski) | No general Horndeski bundle. Installed `(a,b,n)` combinations are finite. [Scientific scope](./ScientificScope.md#Horndeski) cover the stated finite selections; [Scientific scope](./ScientificScope.md#Horndeski) and [Scientific scope](./ScientificScope.md#Horndeski); [Scientific scope](./ScientificScope.md#Horndeski) within the inverse-metric bounds b <= 4 at n = 1, b <= 3 at n = 2 and b <= 2 at n >= 3; b = 0 remains a separate route. Verification includes the complete n = 4, a = 0, b = 1 vertex and selected higher T8 sectors, without full n = 5, arbitrary-order runtime or loop certification. No additional stored library or native-model order is supplied. |
| John derivative coupling | [John overview](./Models/Horndeski/John/README.md): massive scalar with $\beta G^{\mu\nu}\partial_\mu\phi\,\partial_\nu\phi$ | `John`, through κ² per vertex. This particular Horndeski interaction has its own verification and is useful as a separate model entry. |
| Einstein–scalar–Gauss–Bonnet | [EsGB overview](./Models/ScalarGaussBonnet/README.md) and [scalar–Gauss–Bonnet interface](./Models/ScalarGaussBonnet/ScalarGaussBonnet.md) | `Einstein-scalar-Gauss-Bonnet`: massive scalar, independent linear and quadratic scalar–Gauss–Bonnet couplings, through κ² per vertex. Higher-order source validation remains open. |
| Axion-like scalar–Abelian-vector interactions coupled to gravity | [Axion overview and interface](./Models/AxionVector/README.md) and [generator contracts](./Services/RuleConstruction/Contracts.md) | No bundle. Rules are present, but the epsilon-density/action convention remains unresolved. |

## Using a model

For direct rules, start with the model's commands and required
[library imports](./Reference.md#Library%20importers). Query
`FeynGravLibraryInformation[]` for actual imported orders and provenance.
An unmatched vertex call is not evidence that an interaction vanishes.

For native diagrams, use the exact name from the
[five supplied FeynArts bundles](./Models/FeynArts.md), in both
`Model` and `GenericModel`, with a fresh kernel. The cutoff applies to each
vertex; products of allowed vertices can have higher total powers of κ.
Selected tree checks do not establish general native loop support.

Use the model overviews to find actions, fields, parameters, gauge choices,
available library orders and command pages. Read each model’s verification
limits before choosing a calculation. Start with
[General relativity](./Models/GeneralRelativity/README.md), or choose a
[worked example](./Examples/README.md).
