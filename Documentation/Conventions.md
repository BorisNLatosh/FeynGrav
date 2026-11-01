# Common conventions

[Getting started](GettingStarted.md) · [Documentation index](README.md) · [Command reference](Reference.md)

**Applies to the 4.2.0 development checkout.** This page states the shared
mathematical conventions and distinguishes them from adjustable session
settings. Model actions and their coupling definitions belong in the
corresponding model guides. The main-package rules, rule generators and
FeynArts models have different interfaces and available orders; their
agreement must be checked within the scope stated by each model.

To inspect the conventions and settings in your current session, load FeynGrav
and evaluate:

```mathematica
FeynGravConventions[]
```

The command prints a report and returns `Null`. It neither changes conventions
nor transforms expressions already calculated.

## Contents

- [Metric and Lorentz indices](#metric-and-lorentz-indices)
- [Curvature](#curvature)
- [Fourier transform and external momenta](#fourier-transform-and-external-momenta)
- [Metric expansion and gravitational coupling](#metric-expansion-and-gravitational-coupling)
- [Dimensions and FeynCalc notation](#dimensions-and-feyncalc-notation)
- [Gauge parameters and library imports](#gauge-parameters-and-library-imports)
- [Polarisation and helicity](#polarisation-and-helicity)
- [Dirac, colour and epsilon conventions](#dirac-colour-and-epsilon-conventions)
- [Diagrams and loop integrals](#diagrams-and-loop-integrals)
- [Model conventions and verification](#model-conventions-and-verification)

## Metric and Lorentz indices

The four-dimensional Minkowski signature is

$$
\eta_{\mu\nu}=\operatorname{diag}(+1,-1,-1,-1),
\qquad p^2=\eta_{\mu\nu}p^\mu p^\nu.
$$

Curved-space indices are raised and lowered with $g_{\mu\nu}$ and its inverse.
In the expansion about Minkowski space, graviton indices are raised and lowered
with $\eta_{\mu\nu}$. Repeated contracted Lorentz indices are summed.

The dimensional continuation uses $\eta^\mu{}_{\mu}=D$.
Changing FeynCalc's Cartesian metric setting does not convert the stored
FeynGrav interactions to a different spacetime signature.

## Curvature

The connection is the torsion-free, metric-compatible connection:

$$
\Gamma^\alpha{}_{\mu\nu}
=\frac12 g^{\alpha\beta}
\left(\partial_\mu g_{\beta\nu}
+\partial_\nu g_{\beta\mu}
-\partial_\beta g_{\mu\nu}\right).
$$

With the derivative indices written first, the curvature convention is

$$
\begin{aligned}
R_{\mu\nu}{}^\alpha{}_{\beta}
={}&\partial_\mu\Gamma^\alpha{}_{\nu\beta}
-\partial_\nu\Gamma^\alpha{}_{\mu\beta}\\
&+\Gamma^\alpha{}_{\mu\sigma}\Gamma^\sigma{}_{\nu\beta}
-\Gamma^\alpha{}_{\nu\sigma}\Gamma^\sigma{}_{\mu\beta}.
\end{aligned}
$$

The Ricci tensor and scalar are the contractions

$$
R_{\nu\beta}=R_{\mu\nu}{}^\mu{}_{\beta},
\qquad R=g^{\nu\beta}R_{\nu\beta},
$$

and the Einstein tensor is

$$
G_{\mu\nu}=R_{\mu\nu}-\frac12 g_{\mu\nu}R.
$$

For a scalar field, our notation for its covariant Hessian and d'Alembertian is

$$
\nabla_\mu\nabla_\nu\phi
=\partial_\mu\partial_\nu\phi
-\Gamma^\rho{}_{\mu\nu}\partial_\rho\phi,
\qquad
\Box\phi=g^{\mu\nu}\nabla_\mu\nabla_\nu\phi.
$$

When comparing an action with the literature, match the full curvature
definition as well as the metric signature. Also match definitions such as
$X$: a sign or a factor of $1/2$ in $X$ changes derivatives of functions of
$X$. The coefficient of a model's Hessian interaction must be determined
from that model's consistently translated action.

## Fourier transform and external momenta

The Fourier phase is positive:

$$
f(x)=\int\frac{d^D k}{(2\pi)^D}\,f(k)e^{i k\cdot x},
\qquad \partial_\mu\longleftrightarrow i k_\mu.
$$

Set $D=4$ in this formula for the four-dimensional transform. Each derivative
acts on the momentum of its own field.

**Momenta supplied to a FeynGrav vertex are incoming.** For a physical process
$p_1+p_2\to p_3+p_4$, the corresponding external vertex momenta are
$p_1,p_2,-p_3,-p_4$, with routing adapted to each diagram. Momentum conservation
for an all-incoming vertex is $\sum_i k_i=0$.

Vertex construction and `FeynGravSimplify` do not automatically impose your
process's momentum conservation, on-shell conditions or Mandelstam relations.
State these explicitly when needed. FeynArts conversion has its own momentum
assignments; follow the selected model's example rather than reversing
converted momenta a second time.

## Metric expansion and gravitational coupling

The conventional field parametrisation is

$$
g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu},
\qquad h_{\mu\nu}=h_{\nu\mu},
\qquad \kappa^2=32\pi G_{\mathrm N}.
$$

Here $G_{\mathrm N}$ is the four-dimensional Newton constant. The package keeps
`\[Kappa]` symbolic; it does not substitute this relation automatically.
Cheung–Remmen commands use a separate field
parametrisation and include an auxiliary field; their rules cannot be mixed
with the conventional rules without the corresponding field transformation.

Library order has a sector-dependent meaning. In pure GR, order $n$ supplies
the interaction of $n+2$ gravitons, proportional to $\kappa^n$. For matter
libraries, $n$ counts attached gravitons. Consult the
[importer reference](Reference.md#library-importers) for other sectors.
A cutoff in the order of each vertex is not a cutoff in the total order of
a diagram containing several vertices.

## Dimensions and FeynCalc notation

Main-package tensor rules generally retain symbolic `D`. Use a consistent
Lorentz space throughout an expression:

| Quantity | D-dimensional FeynCalc notation |
| --- | --- |
| $\eta^{\mu\nu}$ | `MTD[mu, nu]` |
| $p^\mu$ | `FVD[p, mu]` |
| $p\cdot q$ | `SPD[p, q]` |
| Internal scalar product | `Pair[Momentum[p, D], Momentum[q, D]]` |

`MT`, `FV` and `SP` denote the corresponding four-dimensional objects.
`FCI` converts shorthand to FeynCalc's internal representation; it does not
change the physical dimension or impose kinematics.

The full FORM pipeline requires one consistent Lorentz space in each expression.
It does not set $D=4-2\epsilon$, carry out a Laurent expansion or perform
renormalisation. A four-dimensional helicity calculation and a sum over
D-dimensional polarisations are different operations.

## Gauge parameters and library imports

The following are initial package settings, not universal choices for every
model or values guaranteed to remain unchanged in a session:

| Parameter | Initial value |
| --- | --- |
| `GaugeFixingEpsilon` | `2` |
| `GaugeFixingEpsilonCR` | `-1/2` |
| `GaugeFixingEpsilonVector` | `-1` |
| `GaugeFixingEpsilonSUNYM` | `-1` |
| `GaugeFixingEpsilonHD`, `GaugeFixingEpsilonHD0`, `GaugeFixingEpsilonHD1` | Unassigned |

The conventional gravitational propagator reads its gauge parameter when
called. Vector and Yang–Mills gauge values can instead be substituted when
their vertex libraries are imported. Set those parameters before importing;
changing a value afterwards does not restore dependence already substituted
away. Reimport the affected libraries when necessary.

```mathematica
FeynGravLibraryInformation[]
```

This reports the last successful imports, actual loaded orders and recorded
settings. It does not certify manually redefined vertices or old expressions.
Supplied FeynArts models contain their own stored rules; changing a main-package
gauge parameter does not rebuild those files. Their gauge-fixing actions and
ghost conventions belong to their model guides.

## Polarisation and helicity

The shortcuts take the momentum first. For a D-dimensional vector, evaluate:

```mathematica
PolarizationVectorD[p, \[Mu], I]
```

For a conjugate factorised tensor, evaluate in a separate cell:

```mathematica
PolarizationTensorD[p, \[Mu], \[Nu], -I]
```

`I` labels a polarisation and `-I` its complex conjugate. These labels are
neither multiplicative factors of $i$ nor helicity values. The examples use
the conjugate polarisation for an outgoing external state. The outgoing
state's momentum label and its sign in an all-incoming vertex serve different
purposes; the sign reversal in a vertex does not replace conjugation.

The graviton shortcut constructs the factorised tensor

$$
\varepsilon^{\mu\nu}(p)
=\varepsilon^\mu(p)\varepsilon^\nu(p).
$$

It does not construct a complete spin-2 polarisation sum or impose a helicity
basis, normalisation or tracelessness. `Transversality -> True` imposes only
$p\cdot\varepsilon(p)=0$. Without an explicit option, the wrappers inherit
the effective setting from `Options[Polarization]`; its initial transversality
default is `False`. Mass-shell and null-polarisation conditions remain separate.

`PolarizationTensor` uses four-dimensional indices; `PolarizationTensorD`
retains `D`. The FeynGrav shortcuts require symbolic momentum and index labels.
See [polarisation commands](Reference.md#polarisation-objects) for their full
interfaces.

## Dirac, colour and epsilon conventions

The ordinary Clifford algebra is

$$
\{\gamma^\mu,\gamma^\nu\}=2\eta^{\mu\nu}\mathbf 1.
$$

Matrix multiplication is ordered and is represented with `Dot` in FeynCalc.
The effective `TraceOfOne` setting determines the trace of the identity.
The full FORM pipeline supports ordinary chains and explicit traces within
its documented vocabulary; displaying a FeynCalc Dirac scheme does not
establish support for all operations in that scheme. See the
[Dirac and colour guide](../CalcFormConverter/UserGuide.md#dirac-and-colour-translation).

For fundamental SU(N) generators,

$$
\operatorname{tr}(T^aT^b)=\frac12\delta^{ab},
\qquad [T^a,T^b]=i f^{abc}T^c,
\qquad C_A=N,\qquad C_F=\frac{N^2-1}{2N}.
$$

Colour reduction does not insert a physical quark-flavour multiplicity.
Model field content and flavour sums must be specified separately.

The supported Levi-Civita tensors have four slots. `LCD` denotes four slots
in D-dimensional Lorentz space, not a tensor with $D$ slots. The relevant
epsilon convention is controlled by FeynCalc's `$LeviCivitaSign`, which is
recorded at export and must agree at import. Do not infer a convention for
an axion action merely from this algebra setting. See the
[epsilon guide](../CalcFormConverter/UserGuide.md#lorentz-levi-civita-tensors).

## Diagrams and loop integrals

Propagators and vertices include their prescribed factors of $i$ and their
couplings. For example, `ScalarPropagator[p, m]` returns `I FAD[{p, m}]`, whose
algebraic denominator is $p^2-m^2$. The ordinary `FAD` representation used by
the converter does not explicitly track the sign of an infinitesimal $i0$
prescription; it must not be treated as encoding arbitrary prescriptions.

The conventional gravitational ghost propagator is
`-I MTD[mu, nu] FAD[p]`, with the ordered ghost–antighost convention. Model
guides specify the corresponding vertices and Grassmann-sign handling.

When constructing diagrams manually, supply their symmetry factors,
closed-fermion/ghost-loop signs and integration measure. `FeynGravSimplify`
does not supply those diagram factors. FeynArts may already supply them in
its amplitudes; follow the documented conversion and avoid counting them twice.
Likewise, state whether the assembled diagram expression represents
$i\mathcal M$ or $\mathcal M$ before comparing amplitudes.

The distinction between an integrand and an integral is essential:

- With no declared loop momentum and no saved integral coefficients, the
  simplifier performs algebra on the expression.
- With `LoopMomenta -> {l}` and enabled tensor decomposition, supported
  one-loop integrands are represented by integral coefficients. This is an
  equality of integrals, not pointwise equality of integrands.
- Saved `PaVe` or other supported integral coefficients can undergo subsequent
  algebra and reduction without integrating their multipliers again.

Tensor decomposition follows FeynCalc's `ToPaVe` normalisation, with an
`I Pi^2` prefactor. It does **not** insert $(2\pi)^{-D}$ or a renormalisation
scale. If your diagram uses the Fourier measure written above, account for
that measure explicitly when assembling the integral.

The default `PaVeToABCD -> True` converts supported final `PaVe` objects to named functions
such as `A0`, `B0`, `C0` and `D0`. This notation change does not evaluate those
functions or subtract their divergences. Unsupported or exceptional
coefficients can remain formal.

Routing shifts assume a translation-invariant regularisation.
`DropScaleless -> True` discards demonstrably scaleless integrals within the
supported decomposition; the default `False` retains formal scaleless terms.
This operation does not separate ultraviolet and infrared divergences.
See [one-loop decomposition](../CalcFormConverter/UserGuide.md#one-loop-tensor-decomposition)
for the exact scope and options.

## Model conventions and verification

Use each model's action to determine potential signs, factorials, derivative
couplings and definitions such as $X$. An overall action sign, a curvature-sign
choice and a redefinition of $X$ cannot be interchanged without translating all
affected terms and couplings.

The [model index](Models.md) covers the implemented sectors, including those
without native bundles. The [GR page](Models/GeneralRelativity.md) gives its
action and gauge fixing; the [native-model catalogue](../FeynArts/Models/ModelCatalogue.md)
links to the remaining supplied model guides. Shared conventions do not
establish that every rule in those sectors has been verified.

**Horndeski G4 source and shipped libraries are accepted within their recorded
verification scopes.** The [Horndeski page](Models/Horndeski.md#translation-from-the-literature)
fixes the translated action for (+---) and X without a half: the G4 and G5
Hessian coefficients are $-2G_{4,X}$ and $+G_{5,X}/3$. The
[RULE-01 source checks](Verification/HorndeskiG4-2026-11-01.md) compare the corrected
construction with an independent action reference; the
[RULE-05 migration](Verification/HorndeskiLibraryMigration-2026-11-01.md) separately
verifies all 14 shipped G4 libraries and their public imports. Generic G4 b=5
verification remains incomplete after a timeout; arbitrary orders and loop
amplitudes are not certified. Earlier printed-action matches alone do not
validate the corrected target. The
[implementation audits](Verification/HorndeskiImplementationPlan.md) record the
accepted cases and limits, while the [G4 review record](Verification/HorndeskiG4ContractedBranch.md)
retains the historical findings.
The axion epsilon-density/action convention also remains unresolved.

This page was checked against the package's
[conventions report implementation](../Conventions.wl), main-package
definitions and linked guides on 1 November 2026. The existing
[conventions verification report](Verification/Conventions.md) records
representative linear-order and display checks; it does not prove all
nonlinear interactions. No new mathematical validation is implied by this
documentation update.
