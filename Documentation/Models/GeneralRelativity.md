# General relativity

[Model index](../Models.md) · [Common conventions](../Conventions.md) · [Getting started](../GettingStarted.md)

**Applies to the 4.2.0 development checkout.** FeynGrav supplies conventional
pure-GR propagators and vertices about flat spacetime, together with the
gravitational Faddeev–Popov ghosts. The supplied native FeynArts model is named
`GR` and contains vertices through κ² per vertex.

This page describes the conventional metric formulation in full and separately
lists the available [Cheung–Remmen interface](#cheungremmen-interface).
The two interfaces do not share a field-variable selection option. GR coupled
to matter is covered by the other entries in the [model index](../Models.md).

## Contents

- [Action and field content](#action-and-field-content)
- [Gauge fixing and ghosts](#gauge-fixing-and-ghosts)
- [Propagators](#propagators)
- [Interactions and available orders](#interactions-and-available-orders)
- [Commands and first calculations](#commands-and-first-calculations)
- [Native FeynArts model](#native-feynarts-model)
- [Examples and physical amplitudes](#examples-and-physical-amplitudes)
- [Cheung–Remmen interface](#cheungremmen-interface)
- [Verification and limitations](#verification-and-limitations)
- [Sources and further development](#sources-and-further-development)

## Action and field content

With the signature and curvature definition in
[Common conventions](../Conventions.md#curvature), the Einstein–Hilbert action is

$$
S_{\mathrm{EH}}=-\frac{2}{\kappa^2}\int d^D x\,\sqrt{-g}\,R[g].
$$

There is no cosmological constant or matter term in this pure-GR sector. The
physical four-dimensional action is continued to symbolic D for tensor
algebra and dimensional regularisation. The
[coupling normalisation](../Conventions.md#metric-expansion-and-gravitational-coupling)
is retained; no renormalisation-scale factor is inserted automatically.

The expansion variable is the symmetric metric perturbation:

$$
g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu},
\qquad h_{\mu\nu}=h_{\nu\mu},
\qquad h=\eta^{\mu\nu}h_{\mu\nu}.
$$

Perturbative indices are raised with the background metric. Bulk vertices
are obtained after discarding total derivatives. Boundary observables and
boundary-action completion are outside this flat-space rule interface.

The gauge-fixed action used for these rules is

$$
S=S_{\mathrm{EH}}+S_{\mathrm{gf}}+S_{\mathrm{gh}},
$$

with the terms below. The action sign and background-gauge construction follow
[FeynGrav 4.0, Eqs. (3), (26) and (30)](https://arxiv.org/html/2510.17320v1#S2).
Here the gauge and ghost terms are written directly on the flat background.

| Field | Meaning | Native `GR` label |
| --- | --- | --- |
| $h_{\mu\nu}$ | Symmetric massless graviton | `T[1]`, self-conjugate, two Lorentz indices |
| $c^\mu$ | Grassmann-odd vector ghost | `U[1]`, one Lorentz index |
| $\bar c_\mu$ | Independent Grassmann-odd antighost | `-U[1]`, one Lorentz index |

The auxiliary gauge-fixing field has been integrated out and is not a field
in the native `GR` model. Ghosts are not physical external graviton states.

## Gauge fixing and ghosts

Define the linear background de Donder function

$$
F_\mu=\partial^\nu h_{\mu\nu}-\frac12\partial_\mu h.
$$

In the package's gauge-parameter convention,

$$
S_{\mathrm{gf}}=\frac{2}{\varepsilon}
\int d^D x\,F_\mu F^\mu,
\qquad \varepsilon=\texttt{GaugeFixingEpsilon}.
$$

The default is $\varepsilon=2$, so $S_{\mathrm{gf}}=\int d^D x\,F_\mu F^\mu$.
The coefficient follows from the two background C tensors in
[Eq. (26)](https://arxiv.org/html/2510.17320v1#S2.E26), each of which contracts
with a derivative of h to give $2F_\mu$. Thus the parameter is in the
denominator of the gauge-fixing action. This is a background quadratic term,
with no $\sqrt{-g}$ multiplying it.

For an explicit ghost action, define the background tensor

$$
C^{\mu\nu\alpha\beta}
=\eta^{\mu\alpha}\eta^{\nu\beta}
+\eta^{\mu\beta}\eta^{\nu\alpha}
-\eta^{\mu\nu}\eta^{\alpha\beta}.
$$

The ghost terms, with a derivative moved onto the antighost, are

$$
\begin{aligned}
S_{\mathrm{gh}}=\int d^D x\,\Big[&
\bar c_\mu\Box c^\mu
-\frac{\kappa}{2}(\partial_\nu\bar c_\mu)C^{\mu\nu\alpha\beta}\\
&\times\big(
h_{\alpha\rho}\partial_\beta c^\rho
+h_{\beta\rho}\partial_\alpha c^\rho
+(\partial_\rho h_{\alpha\beta})c^\rho
\big)\Big],
\end{aligned}
$$

where $\Box=\eta^{\mu\nu}\partial_\mu\partial_\nu$ in this section. This is the
flat-background form of [Eq. (30)](https://arxiv.org/html/2510.17320v1#S2.E30).

The gauge term affects only the graviton propagator. The ghost action contains
a free term and one cubic $h\bar c c$ coupling; there is no $hh\bar c c$
contact in this gauge. These statements apply to this choice of gauge fixing,
not to every gauge used in gravitational perturbation theory.

`GravitonGhostVertex` takes the ghost before the antighost. Preserve this
ordering and the derivative momentum when assembling Grassmann lines. The
source and model conventions are documented in the
[source-to-model contract](../../FeynArtsIntegration/BridgeGuide.md#finite-source-to-model-contract).

## Propagators

At `GaugeFixingEpsilon = 2`, the algebraic graviton propagator is

$$
\Delta_{\mu\nu,\alpha\beta}(p)
=\frac{i}{p^2}\left[
\frac12\left(\eta_{\mu\alpha}\eta_{\nu\beta}
+\eta_{\mu\beta}\eta_{\nu\alpha}\right)
-\frac{1}{D-2}\eta_{\mu\nu}\eta_{\alpha\beta}
\right].
$$

This is returned in FeynCalc notation with `FAD[p]`; the displayed fraction
suppresses prescription notation. See the
[denominator convention](../Conventions.md#diagrams-and-loop-integrals).
The $D=2$ singularity is not resolved by substituting into this expression.
For other gauge values the command returns the corresponding longitudinal
terms in addition to the physical tensor structures.

The ordered ghost–antighost propagator is

$$
\Delta^{\mathrm{gh}}_{\mu\nu}(p)
=-\frac{i\eta_{\mu\nu}}{p^2}.
$$

In particular, $+\bar c\Box c$ gives a momentum-space kinetic operator
$-p^2\eta_{\mu\nu}$. The **negative** ghost propagator is the current
implemented and tested convention, including in `GR.gen`; do not copy the
historical positive line. See the
[ghost-sign correction and tree verification](../Verification/PureGRTreeRelease.md).

`GaugeFixingEpsilon` is initialised to 2 when the package loads and is read by
`GravitonPropagator` when called. Changing it does not rebuild the stored
FeynArts model, whose gauge is fixed to 2. The conventional pure-graviton
vertices do not depend on this parameter.

## Interactions and available orders

A pure-graviton vertex with r legs carries $\kappa^{r-2}$, so the library order
is $n=r-2$. Every such Einstein–Hilbert interaction has two derivatives.
The following is the inventory of this checkout:

| Object | Coupling order | Main-package availability | Shipped native `GR` model |
| --- | --- | --- | --- |
| Graviton and ghost propagators | Quadratic action | Direct commands | Included |
| Three-graviton vertex | κ | `Libs/GravitonVertex_1`, loaded by default | `TTT` |
| Four-graviton vertex | κ² | `Libs/GravitonVertex_2`, loaded by default | `TTTT` |
| Graviton–ghost–antighost vertex | κ | Direct `GravitonGhostVertex` definition | `TUU` |
| Five-graviton vertex | κ³ | `Libs/GravitonVertex_3`, explicit import required | Not included; [source independently verified](../Verification/GravitonQuintic-2026-11-01.md) |

The default import is `FeynGravImportGravitons[2]`. To request the currently
stored order-three library, use `FeynGravImportGravitons[3]`; this loads an
existing expression and does not independently validate it. Inspect
`FeynGravLibraryInformation[FeynGravImportGravitons]` for the orders actually
imported. Import requests are capped at available orders; missing intermediate
files cause failure. The importer neither downloads nor generates new rules.

The source generator is a separate interface documented in
[rule contracts](../../Rules/Contracts.md). Its ability to construct an order
does not establish that the corresponding stored rule or amplitude is verified.

A κ² **vertex** cutoff still permits trees with higher total powers of κ.
For a complete five-graviton tree, the κ³ contact must be present alongside
the exchange diagrams. A four-graviton model cannot supply that complete
process merely because FeynArts can draw some of its topologies.

## Commands and first calculations

Start in a fresh kernel with D and κ unassigned:

```mathematica
<< FeynGrav`
```

The conventional public interface is:

| Command | Arguments and result |
| --- | --- |
| `GravitonPropagator[mu, nu, alpha, beta, p]` | Two Lorentz-index pairs and line momentum; includes `I` and the massless denominator |
| `GhostVectorPropagator[mu, nu, p]` | Ordered vector-ghost propagator, including `-I` |
| `GravitonVertex[mu1, nu1, p1, ..., mur, nur, pr]` | Separate index–index–momentum triples for every graviton; all momenta incoming; no enclosing list |
| `GravitonGhostVertex[rho, sigma, k, mu, pGhost, nu, pAntiGhost]` | Graviton triple, ghost index/momentum, antighost index/momentum; all incoming |
| `FeynGravImportGravitons[n, Verbose -> False]` | Import installed vertices through order n; n defaults to 2, diagnostics are opt-in |

For example, obtain a cubic interaction without imposing on-shell kinematics:

```mathematica
v3 = GravitonVertex[
  \[Mu]1, \[Nu]1, p1,
  \[Mu]2, \[Nu]2, p2,
  \[Mu]3, \[Nu]3, p3];
```

Retrieve a propagator in another cell:

```mathematica
propagator = GravitonPropagator[\[Mu], \[Nu], \[Alpha], \[Beta], p];
```

The semicolons suppress potentially long output. These commands do not require
FORM. The standard rules include their Feynman-rule factors of `I` and κ;
do not insert them a second time. An unmatched vertex call is not a zero
vertex. Arguments and restrictions are also in the
[command reference](../Reference.md#graviton-self-interactions-and-gravitational-ghosts).

To simplify a supported expression stored in `amplitude`, with FORM installed:

```mathematica
simplifiedAmplitude = FeynGravSimplify[amplitude];
```

For a tree expression leave `LoopMomenta` empty. The simplifier does not build
the diagrams, select external polarisations or impose kinematics for you. See
the [calculation guide](../../CalcFormConverter/UserGuide.md#simplified-front-end-development).

## Native FeynArts model

In a fresh kernel with FeynCalc-patched FeynArts installed, load:

```mathematica
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;
InitializeModel["GR", GenericModel -> "GR"];
```

The quoted model name must be `GR` in both positions. The bundle contains
`GR.gen`, `GR.mod` and a [provenance manifest](../../FeynArts/Models/GR/manifest.json).
It loads its stored rules without generating source libraries or running FORM.

For the four-graviton tree, generate and insert the external fields:

```mathematica
topologies = CreateTopologies[0, 2 -> 2, Adjacencies -> {3, 4}];
insertions = InsertFields[topologies,
  {T[1], T[1]} -> {T[1], T[1]},
  Model -> "GR", GenericModel -> "GR", InsertionLevel -> {Classes}];
Paint[insertions];
```

The complete tree consists of the four-graviton contact and three exchange
channels. Continue with the
[native amplitude-conversion recipe](../../FeynArts/Models/GR/ModelGuide.md#construct-and-convert-a-four-graviton-tree)
or example 04 below. That recipe fixes prefactors, truncation, routing and the
two Lorentz indices on each external graviton. FeynArts supplies diagram and
statistics factors; those factors must not be applied again after conversion.

The optional `FeynArtsIntegration` companion is a narrower pure-GR bridge,
not a prerequisite for these native calls. Its corrected-model loop guard
still rejects loop construction. Native FeynArts does not apply that guard;
native loop generation therefore is not evidence of validated loop support.
Use a fresh kernel when switching models.

## Examples and physical amplitudes

| Example | Purpose and scope |
| --- | --- |
| [01 — Nieuwenhuizen operators](../../Examples/01.%20Nieuwenhuizen_Operators.nb) | Projectors and the operator language used in gravitational propagators |
| [03 — Graviton scattering](../../Examples/03.%20Graviton_Scattering_Tree_Level.nb) | Direct rule assembly, contact/exchange contributions and external helicities |
| [04 — Graviton scattering with FeynArts](../../Examples/04.%20Graviton_Scattering_Tree_Level_FeynArts.nb) | Native 2 → 2 tree construction and amplitude workflow |
| [05 — Graviton self-energy](../../Examples/05.%20Graviton_Self_Energy.nb) | Manually assembled graviton/ghost loop workflow; not a validation of native-model loops |

The [example index](../../Examples/README.md) records requirements and saved-output
limitations. On-shell and helicity specialisations are separate from off-shell
vertex retrieval. Outgoing physical gravitons use conjugate polarisations;
keep the free polarisation dependence when testing a Ward identity before
specialising to a helicity basis. The
[common polarisation and loop conventions](../Conventions.md) explain these
distinctions and the factors required when manually assembling diagrams.

## Cheung–Remmen interface

FeynGrav also exposes a separate GR parametrisation using a densitised inverse
metric and an auxiliary field. The original action and field redefinitions
are given in [FeynGrav 4.0, Section 3](https://arxiv.org/html/2510.17320v1#S3).
The current public commands are:

| Command | Object |
| --- | --- |
| `GravitonPropagatorCR` | Propagating CR graviton |
| `GravitonPropagatorAuxiliaryCR` | Algebraic auxiliary-field propagator, six index arguments and no momentum |
| `GravitonVertexCRhhh` | Three CR gravitons |
| `GravitonVertexCRBhh` | One auxiliary field and two CR gravitons |
| `GravitonVertexCRBBh` | Two auxiliary fields and one CR graviton |

These are direct definitions, not imported conventional vertex libraries.
`GaugeFixingEpsilonCR` defaults to `-1/2`. Full argument orders are in the
[propagator reference](../Reference.md#propagators) and
[CR vertex reference](../Reference.md#cheungremmen-vertices).
Their κ powers and normalisations must be read as defined; the conventional
$\kappa^{r-2}$ library-order rule is not an instruction to rescale CR commands.

There is no supplied CR FeynArts model and no dedicated public CR ghost-vertex
command in the current interface. In particular, the conventional
`GravitonGhostVertex` is not documented as a substitute for the CR ghost
interaction. The cubic bosonic commands alone do not establish a complete
gauge-fixed CR calculation. The standard `GR` model's source comparisons and
ghost checks do not validate CR normalisations or a mixed-parametrisation
amplitude. Use the conventional formulation for the verified native workflow
described above.

## Verification and limitations

| Item | Established evidence | Boundary |
| --- | --- | --- |
| Cubic/quartic native contacts and trees | [Tree-release report](../Verification/PureGRTreeRelease.md): 32 checks, including 15 exact zero rational residuals, source factors, permutations, routing and contact plus s/t/u inventory | Source-to-model fidelity and the selected checks; not a new independent proof of every physical amplitude |
| Ghost convention | The same report records negative-propagator, arrow-orientation and wrong-sign controls | Correct ordering and native statistics must be retained; native loops were deferred |
| Missing contact | A deliberately excluded quartic coupling was rejected by the bridge completeness check | Native FeynArts can omit unsupported topologies without this extra guard |
| Five-graviton source | [Independent Einstein–Hilbert comparison](../Verification/GravitonQuintic-2026-11-01.md): full off-shell tensor, all pair/leg symmetries, symbolic-D traces and negative controls | Stored κ³ contact verified; no new five-point amplitude/Ward check, arbitrary-order claim or enlargement of the shipped model |
| Five-graviton native development path | A separate order-three model can be built | Expensive historical loading; full five-point amplitude construction did not finish within its recorded bound |
| CR commands | Direct definitions and documented signatures are present | No transfer of conventional-model verification or new CR mathematical validation here |

The three-/four-point tree report retains the historical name `PureGR`. The
later [naming record](../Verification/ModelNames.md) explains its change to `GR`.
The current manifest identifies the shipped snapshot; a dated report describes
what was checked on that snapshot, not every later source revision.

The independent quintic check adds exact action-level source verification.
It does not rerun the examples, regenerate rules, establish arbitrary-order
completeness or carry out new loop calculations.

## Sources and further development

- [Main-package definitions](../../FeynGrav.wl): public propagators, ghost vertex, importers and CR definitions.
- [Graviton rule generator](../../Rules/GravitonVertex.wl): conventional self-interactions and generator-specific interface.
- [Common conventions](../Conventions.md): curvature, Fourier phase, dimensions and overall normalisation.
- [Native operational guide](../../FeynArts/Models/GR/ModelGuide.md): complete loading/conversion workflow and model provenance.
- [Adding higher-order vertices](../../FeynArts/Models/GR/HigherOrderVertices.md): independent source checks, candidate construction, channel completeness and resource limitations.
- [Library-generation guide](../../Libs/Generator.md): separate generation environment and installation of validated libraries.

Increasing an import order does not modify the stored native model. To extend
that model, verify the source rule and rebuild the matching generic/classes
files and manifest together, following the higher-order guide.
