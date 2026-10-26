# GR + SU(N) Yang–Mills model

[Directory overview](README.md)

`GR-SU-N-Yang-Mills` supplies a standalone native model for pure SU(N)
Yang–Mills coupled to Einstein gravity, through $\kappa^2$ per vertex.
It contains no quarks or other matter.

## Action and conventions

Use $g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}$, all vertex momenta incoming,
the package curvature convention and Fourier phase $e^{+ipx}$. The action is

\[
S=\int d^Dx\,\sqrt{-g}\left[-\frac{2}{\kappa^2}R
 -\frac14 g^{\mu\rho}g^{\nu\sigma}F^a_{\mu\nu}F^a_{\rho\sigma}
 -\frac12(\nabla_\mu A^{a\mu})^2
 +g^{\mu\nu}\partial_\mu\bar c^a
   (\partial_\nu c^a+g_s f^{abc} A^b_\nu c^c)\right]
 +S_{\mathrm{gf,GR}}+S_{\mathrm{FP,GR}},
\]

where $F^a_{\mu\nu}=\partial_\mu A^a_\nu-\partial_\nu A^a_\mu
+g_s f^{abc}A^b_\mu A^c_\nu$. This is the symbolic-D continuation of the
four-dimensional action. No four-dimensional tensor identity is imposed.
The fixed source settings are `GaugeFixingEpsilon = 2` and
`GaugeFixingEpsilonSUNYM = -1`. The latter gives the covariant Feynman-gauge
term displayed above. Changing package settings after loading does not alter
this explicit snapshot.

The unassigned parameters are ``FeynGrav`\[Kappa]``,
``FeynGrav`YangMillsCoupling`` ($g_s$) and ``FeynCalc`SUNN`` ($N$).
The Lorentz dimension is `D`; the adjoint dimension is $N^2-1$.
Generator normalisation is $\mathrm{tr}(T^a T^b)=\delta^{ab}/2$.
Specialise parameters by replacement after conversion, for example
`expr /. {YangMillsCoupling -> gs, SUNN -> 3}`. Do not assign numeric values
before model initialisation.

## Fields, propagators and ghosts

| Class | Field | Indices |
| --- | --- | --- |
| `T[1]` | Graviton | Two Lorentz indices |
| `V[1]` | Massless gauge field | One Lorentz and one adjoint index |
| `F[1]`, `-F[1]` | Gravitational ghost, antighost | One Lorentz index |
| `U[1]`, `-U[1]` | YM ghost, antighost | One adjoint index; Lorentz scalar |

`F` here is a **Grassmann vector ghost**, not a spinor. Its generic propagator
and couplings contain no Dirac matrices. This model-local use of a supported
FeynArts head lets the two ghost species have distinct kinematic index lists
without modifying FeynArts. Both have native Grassmann statistics and arrows.
Do not reuse the GR-only model's `U[1]` interpretation in this model.

With the ordered ghost–antighost convention, the gravitational ghost line is
$-i\eta_{\mu\nu}/(p^2+i0)$, the gauge line is
$-i\delta^{ab}\eta_{\mu\nu}/(p^2+i0)$ and the YM ghost line is
$+i\delta^{ab}/(p^2+i0)$. The graviton propagator and GR vertices equal the
accepted `GR` rules. All fields are massless.

The GR gauge condition depends only on the metric. Ordering gauge conditions
as GR then YM, and transformations as diffeomorphisms then YM, gives a triangular
Faddeev–Popov operator:

\[
\mathcal M=\begin{pmatrix}M_{\rm diff}&0\\
\delta_{\rm diff}(\nabla\!\cdot\!A)&M_{\rm YM}\end{pmatrix},\qquad
\det\mathcal M=\det M_{\rm diff}\det M_{\rm YM}.
\]

The model represents these two diagonal determinants with independent ghosts.
The off-diagonal entry need not vanish; the factorised determinant representation
does not include a mixed ghost coupling for it. This statement concerns the
gauge-fixed determinant, not equality of arbitrary ghost Green functions with
an unredefined full BRST ghost action. The existing linear GR ghost action has
only its cubic interaction. The covariant YM ghost action supplies the contacts
listed below.

## Complete finite inventory

Here `T`, `V`, `F`, `U` denote the classes above; a pair of ghost letters means
one antighost and one ghost, with that order in coupling rows.

| Families | Coupling powers |
| --- | --- |
| `TTT`, `TTTT`, `TFF` | $\kappa$, $\kappa^2$, $\kappa$ |
| `TVV`, `TTVV` | $\kappa$, $\kappa^2$ |
| `VVV`, `TVVV`, `TTVVV` | $g_s$, $\kappa g_s$, $\kappa^2 g_s$ |
| `VVVV`, `TVVVV`, `TTVVVV` | $g_s^2$, $\kappa g_s^2$, $\kappa^2 g_s^2$ |
| `TUU`, `TTUU` | $\kappa$, $\kappa^2$ |
| `VUU`, `TVUU`, `TTVUU` | $g_s$, $\kappa g_s$, $\kappa^2 g_s$ |

There are 16 families. Quadratic terms are in propagators, not two-leg insertions.
`TTVVV`, `TVVVV` and `TTVUU` have five legs; `TTVVVV` has six.
Use `Adjacencies -> {3,4,5,6}` for processes admitting them. Restricting to
`{3,4}` omits permitted contacts. The cutoff is on each vertex, not the total
coupling order of a graph assembled from several vertices.

## Load and construct diagrams

Start a fresh kernel:

```wl
$LoadAddOns = {"FeynArts"};
<< FeynGrav`;
model = "GR-SU-N-Yang-Mills";
InitializeModel[model, GenericModel -> model];
topologies = CreateTopologies[0, 2 -> 2, Adjacencies -> {3, 4, 5, 6}];
insertions = InsertFields[topologies, {V[1], V[1]} -> {V[1], V[1]},
  Model -> model, GenericModel -> model, InsertionLevel -> {Classes}];
Paint[insertions, ColumnsXRows -> {3, 3}, Numbering -> Simple];
raw = CreateFeynAmp[insertions, Truncated -> True,
  MomentumConservation -> True, PreFactor -> 1];
diagrams = FCFAConvert[raw, IncomingMomenta -> {p1, p2},
  OutgoingMomenta -> {p3, p4}, ChangeDimension -> D,
  LorentzIndexNames -> {\[Mu]1, \[Mu]2, \[Mu]3, \[Mu]4},
  List -> True, SMP -> False, DropSumOver -> False];
```

This produces seven diagrams: the four-gauge-field contact, three gauge exchanges
and three graviton exchanges. For `{V[1],V[1]} -> {V[1],T[1]}` there are four:
the `TVVV` contact and three gauge exchanges. For
`{V[1],V[1]} -> {T[1],T[1]}` there are four: the `TTVV` contact, s-channel
graviton exchange and t/u-channel gauge exchange. Supply five or six external
Lorentz-index names, respectively. Native graph factors already supply the tree
weights; do not multiply by guessed symmetry factors.

`MomentumConservation -> True` resolves internal routing at each vertex.
`PreFactor -> 1` retains the source normalisation. `Truncated -> True` removes
external wavefunctions: the converted result has one free index per gauge
field and two per graviton. Contract with your D-dimensional polarisation
vectors/tensors, conjugating outgoing states, then impose the desired kinematics.
The generic outgoing graviton marker is not a FeynCalc tensor shortcut; the
truncated open-index workflow avoids treating it as one.

Keep colour sums during conversion. For a fixed external-colour amplitude,
only explicitly marked external `SumOver[...,External]` factors can be stripped
as external-state markers. An internal adjoint sum must either remain explicit
or be translated to a valid repeated-index contraction before `SUNSimplify`.
Check each expanded monomial, not the occurrence count of a factored expression.
Never blanket-replace all `SumOver`/`IndexSum` objects by 1. The model uses a
symbolic `NoUnfold[N^2-1]` range, rather than an eight-component SU(3) list.

The same standalone workflow can load just FeynCalc and patched FeynArts: append
this model directory to ``FeynArts`$ModelPath`` before `InitializeModel`. Neither
model file executes FeynGrav, reads `Libs`, calls FORM nor invokes a builder.
See the [common loading guide](../ModelCatalogue.md#common-loading-workflow).
The optional pure-GR bridge retains its existing process restrictions.

For an evaluated $AA \to Ah$ calculation, see
[example 12](../../../Examples/12.%20Yang_Mills_Graviton_Emission_Tree_Level_FeynArts.nb).
It displays all four diagrams, constructs the fixed-colour on-shell amplitude
and checks each gauge-field leg plus the graviton with an arbitrary gauge vector.
Its final subsection gives a four-dimensional helicity expression in h1, h2, h3,
h4, s and t, with graviton helicity 2h4. Its explicit scalar-product assignments
come after the general symbolic-D amplitude and Ward checks.
The [notebook report](../../../Documentation/Verification/Example12YangMills-2026-10-26.md)
records its exact comparisons and display limitations.

## Verification and limits

The [verification report](../../../Documentation/Verification/GRSUNYM-2026-10-26.md)
records independent generic off-shell action comparisons for all ten nonzero-
graviton YM/ghost families, exact native conversion of all 16 contacts, native
Grassmann-sign fixtures, and all external-leg Ward tests for the three processes
above. The tree checks keep D and N symbolic. Untested external gravitons use
factorised transverse traceless states; each tested graviton is replaced by a
pure-gauge tensor with an arbitrary remaining vector. These are selected physical
tree checks, not a proof of every tensor component or process.

Loop amplitudes, renormalisation, quarks, other gauge choices and higher orders
are not validated by this delivery. Existing scalar/GR bundles are unchanged.
The cubic/quartic gluon source routines needed a missing $\kappa^n$ correction;
the report records the four affected regenerated libraries and the exact audit.
Restart the kernel and recompute affected expressions after updating; old saved
notebook outputs do not update themselves.
