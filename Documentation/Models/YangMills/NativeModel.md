# GR + SU(N) Yang–Mills model

[Documentation home](../../README.md)

`GR-SU-N-Yang-Mills` supplies a standalone native model for SU(N)
Yang–Mills and one fundamental Dirac quark flavour coupled to Einstein gravity,
through $\kappa^2$ per vertex. The quark mass remains arbitrary.

## Action and conventions

Use $g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}$, all vertex momenta incoming,
the package curvature convention. The bosonic/ghost action is

$$
S=\int d^Dx\,\sqrt{-g}\left[-\frac{2}{\kappa^2}R
 -\frac14 g^{\mu\rho}g^{\nu\sigma}F^a_{\mu\nu}F^a_{\rho\sigma}
 -\frac12(\nabla_\mu A^{a\mu})^2
 +g^{\mu\nu}\partial_\mu\bar c^a
   (\partial_\nu c^a+g_s f^{abc} A^b_\nu c^c)\right]
 +S_{\mathrm{gf,GR}}+S_{\mathrm{FP,GR}},
$$

where $F^a_{\mu\nu}=\partial_\mu A^a_\nu-\partial_\nu A^a_\mu
+g_s f^{abc}A^b_\mu A^c_\nu$. This is the symbolic-D continuation of the
four-dimensional action. No four-dimensional tensor identity is imposed.
The fixed source settings are `GaugeFixingEpsilon = 2` and
`GaugeFixingEpsilonSUNYM = -1`. The latter gives the covariant Feynman-gauge
term displayed above. Changing package settings after loading does not alter
this explicit snapshot.

Add the symmetrised Dirac action

$$
S_q=\int d^Dx\,\sqrt{-g}\left[
 \frac{i}{2}\bar q e_a{}^\mu\gamma^a\overleftrightarrow{D}_\mu q
 -m\bar q q\right],\qquad D_\mu=\nabla_\mu-i g_s T^a A^a_\mu.
$$

The spin connection is torsion-free and the vierbein is the symmetric square
root of the metric. The flat interaction is $+g_s\bar q\gamma^\mu T^a A^a_\mu q$.
There is one flavour, not an implicit flavour sum.

Raw FeynGrav fermion sources use $e^{+ipx}$. Native FeynArts spinor conventions
use the opposite Fourier routing. For a canonical `{-F,F,...}` row, the builder
maps the source fermion momentum to `-mom2`, the antifermion momentum to `-mom1`,
and **every graviton source momentum to `-momj`**. Gamma order is preserved.
Reversing only the fermion momenta gives an incorrect two-graviton spin term.
This convention conversion was checked against an independent action expansion;
it does not alter interaction sources or stored libraries.

The unassigned parameters are ``FeynGrav`\[Kappa]``,
``FeynGrav`YangMillsCoupling`` ($g_s$), ``FeynGrav`QuarkMass`` ($m$), and ``FeynCalc`SUNN`` ($N$).
The Lorentz dimension is `D`; the adjoint dimension is $N^2-1$.
Generator normalisation is $\mathrm{tr}(T^a T^b)=\delta^{ab}/2$.
Specialise parameters by replacement after conversion, for example
`expr /. {QuarkMass -> m, YangMillsCoupling -> gs, SUNN -> 3}`. Do not assign numeric values
before model initialisation.

## Fields, propagators and ghosts

| Class | Field | Indices |
| --- | --- | --- |
| `T[1]` | Graviton | Two Lorentz indices |
| `V[1]` | Massless gauge field | One Lorentz and one adjoint index |
| `F[1]`, `-F[1]` | Quark, antiquark | One fundamental colour index; ordered Dirac chain |
| `S[1]`, `-S[1]` | Gravitational ghost, antighost | One Lorentz index |
| `U[1]`, `-U[1]` | YM ghost, antighost | One adjoint index; Lorentz scalar |

`S` here is a **Grassmann vector ghost**, not a physical scalar. Its generic propagator
and couplings contain no Dirac matrices. This model-local use of a supported
FeynArts head lets the two ghost species have distinct kinematic index lists
without modifying FeynArts. Both ghosts and the quark have native Grassmann statistics and arrows.
The old pure-YM bundle used `F[1]` for the gravitational ghost. Update explicit
ghost selections to `S[1]` and restart; the model name is unchanged.
Do not reuse the GR-only model's `U[1]` interpretation in this model.

With the ordered ghost–antighost convention, the gravitational ghost line is
$-i\eta_{\mu\nu}/(p^2+i0)$, the gauge line is
$-i\delta^{ab}\eta_{\mu\nu}/(p^2+i0)$ and the YM ghost line is
$+i\delta^{ab}/(p^2+i0)$. The graviton propagator and GR vertices equal the
accepted `GR` rules. Only the quark is massive. Along its physical fermion
flow the propagator is $i(\not p+m)/(p^2-m^2+i0)$. The native generic encoding
uses `I (-slash[mom]+Mass)` because FeynArts routes `mom` oppositely on that
ordered line. Its external spinors follow the same convention. Fundamental
colour runs over symbolic N, independently of `QuarkMass`.

The GR gauge condition depends only on the metric. Ordering gauge conditions
as GR then YM, and transformations as diffeomorphisms then YM, gives a triangular
Faddeev–Popov operator:

$$
\mathcal M=\begin{pmatrix}M_{\rm diff}&0\\
\delta_{\rm diff}(\nabla\!\cdot\!A)&M_{\rm YM}\end{pmatrix},\qquad
\det\mathcal M=\det M_{\rm diff}\det M_{\rm YM}.
$$

The model represents these two diagonal determinants with independent ghosts.
The off-diagonal entry need not vanish; the factorised determinant representation
does not include a mixed ghost coupling for it. This statement concerns the
gauge-fixed determinant, not equality of arbitrary ghost Green functions with
an unredefined full BRST ghost action. The existing linear GR ghost action has
only its cubic interaction. The covariant YM ghost action supplies the contacts
listed below.

## Complete finite inventory

Here `T`, `V`, `F`, `S`, `U` denote the classes above; a pair of ghost letters means
one antighost and one ghost, with that order in coupling rows.

| Families | Coupling powers |
| --- | --- |
| `TTT`, `TTTT`, `TSS` | $\kappa$, $\kappa^2$, $\kappa$ |
| `TVV`, `TTVV` | $\kappa$, $\kappa^2$ |
| `VVV`, `TVVV`, `TTVVV` | $g_s$, $\kappa g_s$, $\kappa^2 g_s$ |
| `VVVV`, `TVVVV`, `TTVVVV` | $g_s^2$, $\kappa g_s^2$, $\kappa^2 g_s^2$ |
| `TUU`, `TTUU` | $\kappa$, $\kappa^2$ |
| `VUU`, `TVUU`, `TTVUU` | $g_s$, $\kappa g_s$, $\kappa^2 g_s$ |
| `FFV` | $g_s$ |
| `FFT`, `FFTT` | $\kappa$, $\kappa^2$ |
| `FFTV`, `FFTTV` | $\kappa g_s$, $\kappa^2 g_s$ |

There are 21 families. `FF` means an antiquark–quark pair. `FFTTV` is the
five-leg $hh\bar q q A$ contact. There is no $\bar q q AA$ contact in this
first-order Dirac action and no quark–ghost vertex. Quadratic terms, including
the quark mass, are in propagators, not separate two-leg insertions.
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
See the [common loading guide](../FeynArts.md#Common%20loading%20workflow).
The optional pure-GR bridge retains its existing process restrictions.

For an evaluated $AA \to Ah$ calculation, see
example 12 (`Examples/12. Yang_Mills_Graviton_Emission_Tree_Level_FeynArts.nb`).
It displays all four diagrams, constructs the fixed-colour on-shell amplitude
and checks each gauge-field leg plus the graviton with an arbitrary gauge vector.
Its final subsection gives a four-dimensional helicity expression in h1, h2, h3,
h4, s and t, with graviton helicity 2h4. Its explicit scalar-product assignments
come after the general symbolic-D amplitude and Ward checks.
See the [per-notebook verification and saved-output status](../../Examples/README.md#Verification%20and%20saved-output%20status) for the recorded evaluation scope and display limitations.

## Verification and limits

Independent off-shell action comparisons cover the ten Yang–Mills/ghost families with gravitons and the quark kinetic, mass and spin-connection interactions. Exact native-contact comparisons cover all 21 supplied families, including all five quark contacts. Native Grassmann-sign checks cover the stated ghost and fermion conventions. Direct external-leg Ward checks pass for the selected AA → Ah and AA → hh trees with symbolic D and N. Each tested graviton is replaced by a pure-gauge tensor with an arbitrary remaining vector; other external gravitons use factorised transverse traceless states. These are selected physical tree checks, not a proof of every tensor component or process. See [scientific scope](../../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions).

Loop amplitudes, renormalisation, extra flavours, other gauge choices and higher orders are not validated by these checks.

**Upgrading older rules:** missing $\kappa^n$ factors were restored in mixed cubic/quartic gluon and quark–gluon rules and their one-/two-graviton libraries. Restart the kernel and recompute affected expressions after updating; old saved notebook outputs do not update themselves.

## Quark workflow and verification

Use `{-F[1],F[1]} -> {T[1],V[1]}` for $\bar q q\to hA$. There are four diagrams:
the contact, s-channel gluon and t/u quark exchanges. For $\bar q q\to AA$,
the four diagrams are s-channel gluon/graviton and t/u quarks. For
$\bar q q\to hh$, they are the contact, s-channel graviton and t/u quarks.
The $qq\to qq$ process has t/u gluon and graviton exchanges with their native
identical-fermion signs.

Example 13 (`Examples/13. Quark_Graviton_Emission_Tree_Level_FeynArts.nb`)
loads the package/model, displays all four $\bar q q\to hA$ graphs, sets massive
on-shell Mandelstam kinematics, computes the open Dirac matrix with
`FeynGravSimplify[..., Method -> "FORM"]`, and attaches external spinors for
presentation. It uses conjugated outgoing polarisations, keeps D/N/m symbolic,
and tests both gauge symmetries with the external Dirac equations. Before FORM,
expand composite momentum arguments and slashed sums so momentum conservation
also reaches internal Dirac chains. No helicity or D = 4 limit is required.

The checked quark sector includes independent off-shell kinetic/mass/spin-connection rules, all five native contacts, massive and massless reference comparisons, and selected tree Ward tests. Wrong-normalisation, missing-contact and sign changes fail the corresponding checks. See [scientific scope](../../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions) and [notebook status](../../Examples/README.md#Verification%20and%20saved-output%20status). The five-leg comparison is a contact check, not full 2→3 scattering. The closed ghost comparison checks its combinatorial sign, not a loop amplitude. Setting $g_s=0$ recovers the tested quark–gravity sector; it does not remove gravity from the other fields. Setting $\kappa=0$ recovers the tested massive quark–gluon sector.

[Native-model order limits](./HigherOrders.md).
