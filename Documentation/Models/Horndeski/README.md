# Horndeski scalar–tensor gravity

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [General relativity](../GeneralRelativity.md)

**Applies to the 4.2.0 development checkout; reviewed 1 November 2026.**
FeynGrav provides four monomial interaction interfaces, [`HorndeskiG2`](./HorndeskiG2.md) through
[`HorndeskiG5`](./HorndeskiG5.md), and a finite set of stored vertices about flat spacetime.
There is no general Horndeski FeynArts model or automatic compiler for arbitrary
functions of the scalar and its kinetic invariant.

The action below defines the convention used by the interaction commands.
The shipped libraries have finite coverage, and higher-order construction has
explicit bounds. Read [Scientific scope](../../ScientificScope.md#Horndeski)
for the checked cases and limitations before assembling an amplitude.

<a name="Contents"></a>

## Contents

- [Fields and conventions](#Fields%20and%20conventions)
- [Action](#Action)
- [Translation from the literature](#Translation%20from%20the%20literature)
- [G4 action and vertex derivation](./G4Derivation.md)
- [Monomials and coupling normalisation](#Monomials%20and%20coupling%20normalisation)
- [Free fields, gauge fixing and ghosts](#Free%20fields%2C%20gauge%20fixing%20and%20ghosts)
- [Commands and library inventory](#Commands%20and%20library%20inventory)
- [Examples and FeynArts support](#Examples%20and%20FeynArts%20support)
- [Rule generation and higher orders](#Rule%20generation%20and%20higher%20orders)
- [Verification and open work](#Verified%20scope%20and%20limitations)

<a name="Fields and conventions"></a>

## Fields and conventions

The physical theory contains a metric and one real scalar. The perturbative
rules use

$$
g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu},
\qquad \eta_{\mu\nu}=\operatorname{diag}(+1,-1,-1,-1),
\qquad \bar\phi=0.
$$

The scalar argument of the interaction is the fluctuation about zero. A
nonzero background requires a separate expansion and a corresponding check of
the quadratic action. It is not selected by assigning a value to a coupling.

Use the curvature and positive Fourier phase in
[Common conventions](../../Conventions.md#Curvature). Define

$$
X=g^{\mu\nu}\partial_\mu\phi\,\partial_\nu\phi,
\qquad H_{\mu\nu}=\nabla_\mu\nabla_\nu\phi
=\partial_\mu\partial_\nu\phi-\Gamma^\rho{}_{\mu\nu}\partial_\rho\phi.
$$

There is **no factor of one-half in X**. With all contractions performed using
the full metric, write

$$
\begin{aligned}
\left[H\right]&=g^{\mu\nu}H_{\mu\nu}=\Box\phi,\\
[H^2]&=H^\mu{}_{\nu}H^\nu{}_{\mu}=H_{\mu\nu}H^{\mu\nu},\\
[H^3]&=H^\mu{}_{\nu}H^\nu{}_{\rho}H^\rho{}_{\mu},\\
\mathcal H_2&=[H]^2-[H^2],\\
\mathcal H_3&=[H]^3-3[H][H^2]+2[H^3].
\end{aligned}
$$

The minus sign in the covariant Hessian follows from the connection acting on
the covector $\partial_\nu\phi$. Changing metric signature does not change that
definition to a plus sign.

<a name="Action"></a>

## Action

In these conventions the four-dimensional Horndeski action is

$$
S_{\mathrm H}=\int d^4x\,\sqrt{-g}
\left(\mathcal L_2+\mathcal L_3+\mathcal L_4+\mathcal L_5\right),
$$

with

$$
\begin{aligned}
\mathcal L_2&=G_2(\phi,X),\\
\mathcal L_3&=G_3(\phi,X)[H],\\
\mathcal L_4&=G_4(\phi,X)R-2G_{4,X}(\phi,X)\mathcal H_2,\\
\mathcal L_5&=G_5(\phi,X)G^{\mu\nu}H_{\mu\nu}
+\frac13G_{5,X}(\phi,X)\mathcal H_3.
\end{aligned}
$$

Here $G_{i,X}=\partial G_i/\partial X$ and
$G_{\mu\nu}=R_{\mu\nu}-g_{\mu\nu}R/2$. The function $G_2$ and the Einstein
tensor $G_{\mu\nu}$ are different objects. The coefficients $-2$ and $+1/3$
are required by this definition of X and the stated metric/curvature convention;
the translation is given below.

**Convention difference from the reference paper.** This action is written in
FeynGrav conventions, which differ from those of
[Kobayashi, Yamaguchi and Yokoyama (2011), DOI: 10.1143/PTP.126.511](https://doi.org/10.1143/PTP.126.511).
Their metric signature is $(-,+,+,+)$ and their kinetic variable is
$\widehat X=-\widehat g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi/2$;
FeynGrav uses $(+,-,-,-)$ and $X=g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi$.
Consequently, their displayed Lagrangian has different signs and numerical
coefficients. The [translation below](#Translation%20from%20the%20literature)
relates the two descriptions of the same Horndeski action, including the
necessary redefinition of the functions $G_i$.

The standard GR and canonical massive-scalar terms are obtained with

$$
G_4=-\frac{2}{\kappa^2}+\Delta G_4,
\qquad G_2=\frac12X-\frac12m^2\phi^2-V_{\mathrm{int}}(\phi)+\Delta G_2.
$$

When using the conventional pure-GR and minimal-scalar commands separately,
add only the additional interactions: do not count their action terms again
through a Horndeski monomial. Scalar tadpoles, vacuum energy, field mixing and
kinetic normalisation must be consistent with the chosen flat background.

The rules retain symbolic D as a dimensional continuation of the
four-dimensional action. This is not a claim to implement the most general
second-order scalar–tensor theory in every dimension, or perturbations about
a cosmological background.

<a name="Translation from the literature"></a>

## Translation from the literature

The reference is T. Kobayashi, M. Yamaguchi and J. Yokoyama,
[“Generalized G-Inflation: Inflation with the Most General Second-Order Field Equations,”
*Progress of Theoretical Physics* **126**, 511–529 (2011)](https://doi.org/10.1143/PTP.126.511),
[arXiv:1105.5723, Eqs. (1)–(5)](https://arxiv.org/html/1105.5723v4#S2).

We mark the paper's quantities with hats to distinguish its conventions from
FeynGrav's. The paper uses the mostly-plus metric,
$\widehat X=-\widehat g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi/2$,
and $\widehat{\mathcal L}_3=-\widehat G_3\widehat\Box\phi$.
Its quadratic- and cubic-Hessian coefficients are
$+\widehat G_{4,\widehat X}$ and $-\widehat G_{5,\widehat X}/6$,
respectively; FeynGrav's are $-2G_{4,X}$ and $+G_{5,X}/3$.
These coefficients must be compared together with the metric, kinetic variable
and function definitions, rather than transferred unchanged between conventions.
Keep the mixed-index Riemann definition fixed and set
$\widehat g_{\mu\nu}=-g_{\mu\nu}$. Then

$$
\begin{gathered}
\widehat X=X/2,\qquad
\widehat\Gamma^\rho{}_{\mu\nu}=\Gamma^\rho{}_{\mu\nu},\qquad
\widehat H_{\mu\nu}=H_{\mu\nu},\\
\widehat R=-R,\qquad
\widehat G_{\mu\nu}=G_{\mu\nu},\qquad
\widehat{[H]}=-[H],\qquad
\widehat{\mathcal H}_2=\mathcal H_2,\qquad
\widehat{\mathcal H}_3=-\mathcal H_3.
\end{gathered}
$$

Define the functions in this page by

$$
\begin{aligned}
G_2(\phi,X)&=\widehat K(\phi,X/2),&
G_3(\phi,X)&=\widehat G_3(\phi,X/2),\\
G_4(\phi,X)&=-\widehat G_4(\phi,X/2),&
G_5(\phi,X)&=\widehat G_5(\phi,X/2).
\end{aligned}
$$

Consequently $\widehat G_{4,\widehat X}=-2G_{4,X}$ and
$\widehat G_{5,\widehat X}=2G_{5,X}$ at corresponding arguments. Substituting
these relations gives the action above. This translates both the signature
and the kinetic variable; it does not change only the sign of a Hessian term.

**Difference from the original FeynGrav presentation.**
[FeynGrav 3.0, Eq. (27)](https://arxiv.org/html/2406.14872#S3.E27)
states $X=(\nabla\phi)^2$ but prints $+G_{4,X}\mathcal H_2$ and
$-G_{5,X}\mathcal H_3/6$. With the conventions fixed here, those relative
coefficients must instead be $-2G_{4,X}$ and $+G_{5,X}/3$.
The corrected expressions are used by the current source rules and shipped
libraries. The [G4 derivation](./G4Derivation.md) explains the geometric kernels;
[scientific scope](../../ScientificScope.md#Horndeski) and the
[release notes](../../ReleaseNotes-4.2.0.md#Horndeski%20scientific%20corrections)
state the checked orders and which earlier results require recomputation.

Two useful convention checks follow by integration by parts:

$$
\begin{aligned}
\mathcal H_2&\simeq R_{\mu\nu}\partial^\mu\phi\partial^\nu\phi,\\
-\frac{\beta}{2}XR+\beta\mathcal H_2
&\simeq \beta G^{\mu\nu}\partial_\mu\phi\partial_\nu\phi,\\
-\beta\phi G^{\mu\nu}H_{\mu\nu}
&\simeq \beta G^{\mu\nu}\partial_\mu\phi\partial_\nu\phi.
\end{aligned}
$$

Here $\simeq$ means equality of actions up to a boundary term. Thus John can
be represented either by $\Delta G_4=-\beta X/2$ or by $G_5=-\beta\phi$.
These are alternative representations, not two contributions to add. The
existing native John model uses the second route. For another check,
$G_3=f(\phi)$ gives $G_3\Box\phi\simeq-f_{,\phi}X$; a constant G5 is a boundary
term. The full scalar and graviton permutation sums and momentum conservation
matter when testing such identities as vertices.

<a name="Monomials and coupling normalisation"></a>

## Monomials and coupling normalisation

The command interfaces use coefficients of $\lambda\phi^a X^b$ in each
function, with explicit non-negative integers a and b. For one coefficient,
the interaction densities in the corrected convention are

$$
\begin{aligned}
\Delta\mathcal L_2&=\lambda_2\phi^aX^b,\\
\Delta\mathcal L_3&=\lambda_3\phi^aX^b[H],\\
\Delta\mathcal L_4&=\lambda_4\phi^a
\left(X^bR-2bX^{b-1}\mathcal H_2\right),\\
\Delta\mathcal L_5&=\lambda_5\phi^a
\left(X^bG^{\mu\nu}H_{\mu\nu}+\frac b3X^{b-1}\mathcal H_3\right).
\end{aligned}
$$

For b = 0, omit the differentiated-function term before evaluation; do not
construct $0X^{-1}$. The G5 stored selections have been checked against this
action within the scope below. The G4 source is also checked against this
action; the shipped G4 libraries and ordinary interface now return the corrected expressions.

The coupling argument is the coefficient of the monomial itself. There is no
implicit $1/a!$, $1/b!$ or $2^{-b}$ in that definition. If a model is written
with such factors, include them in the coupling supplied to the command.
The library convention includes the vertex phase, κ powers and field
differentiation multiplicities; do not add another factor of i or a graviton
factorial. The six historical G4 b=0 files omitted a!; the migration restores
it without changing the coupling convention. Recompute affected old results. In particular, G2's
graviton multiplicity was corrected separately
from its scalar permutation sum.

| Family | Number of scalar momenta s | Graviton data | Meaning of a |
| --- | --- | --- | --- |
| G2 | $a+2b$ | Flat list of n index pairs | $a=s-2b$ |
| G3 | $a+2b+1$ | Flat list of n index–index–momentum triples | $a=s-2b-1$ |
| G4 | $a+2b$ | Flat list of n triples | $a=s-2b$ |
| G5 | $a+2b+1$ | Flat list of n triples | $a=s-2b-1$ |

For these additional monomials n counts attached gravitons and the metric
expansion contributes $\kappa^n$; a chosen coupling can itself contain κ.
The separately supplied pure-GR r-graviton rule instead carries
$\kappa^{r-2}$. All scalar and graviton momenta are incoming.

<a name="Free fields, gauge fixing and ghosts"></a>

## Free fields, gauge fixing and ghosts

For the canonical GR plus scalar free action, use `ScalarPropagator[p, m]`,
`GravitonPropagator[mu, nu, alpha, beta, p]` and the gravitational ghost sector
described on the [GR page](../GeneralRelativity.md#Gauge%20fixing%20and%20ghosts).
Its linear gauge choice is

$$
F_\mu=\partial^\nu h_{\mu\nu}-\frac12\partial_\mu h,
\qquad S_{\mathrm{gf}}=\frac{2}{\varepsilon}\int d^D x\,F_\mu F^\mu,
\qquad \varepsilon=\texttt{GaugeFixingEpsilon}.
$$

The default is 2. With this metric-only gauge function there is no new
independent scalar gauge symmetry or scalar FP species. The vector ghost and
antighost retain the GR kinetic term and cubic graviton coupling, including
the negative ordered ghost propagator.

These propagators are not an automatic inversion of a general Horndeski
quadratic action. If the chosen functions or background alter the scalar
kinetic term, graviton normalisation or scalar–graviton mixing, first determine
the appropriate free fields or explicitly treat the additional quadratic terms
as perturbative insertions. Importing a Horndeski library does neither operation.

<a name="Commands and library inventory"></a>

## Commands and library inventory

Use the ordinary FeynGrav interface in a fresh kernel, with D, κ and the
couplings unassigned. Import only the families required for the calculation:

```mathematica
<< FeynGrav`
FeynGravImportHorndeskiG2[];
FeynGravImportHorndeskiG3[];
```

The four-argument public calls are:

| Command | Required importer |
| --- | --- |
| [`HorndeskiG2[pairs, momenta, b, lambda]`](HorndeskiG2.md) | `FeynGravImportHorndeskiG2[]` |
| [`HorndeskiG3[triples, momenta, b, lambda]`](HorndeskiG3.md) | `FeynGravImportHorndeskiG3[]` |
| [`HorndeskiG4[triples, momenta, b, lambda]`](HorndeskiG4.md) | `FeynGravImportHorndeskiG4[]`; all 14 shipped files independently checked |
| [`HorndeskiG5[triples, momenta, b, lambda]`](HorndeskiG5.md) | `FeynGravImportHorndeskiG5[]`; audited stored selections; John unchanged |

For the stored G2 monomial with a = 1, b = 1 and one graviton:

```mathematica
v2 = HorndeskiG2[{\[Mu], \[Nu]}, {p1, p2, p3}, 1, \[Lambda]2];
```

For the stored G3 monomial with a = 0, b = 1 and one graviton:

```mathematica
v3 = HorndeskiG3[{\[Mu], \[Nu], k}, {p1, p2, p3}, 1, \[Lambda]3];
```

The graviton's momentum is unnecessary as an explicit G2 argument because
that monomial has no metric derivatives. For G3 it is supplied in the triple;
all-incoming conservation is $k+p_1+p_2+p_3=0$. These calls retrieve individual
vertices, without imposing that relation or calculating a scattering amplitude.

Horndeski importers accept options but **no order argument**. They import every
available matching file. `Verbose -> False` is the default. To print an
inventory use `FeynGravImportHorndeskiG2[Verbose -> True]`; to inspect a successful
import and its file identities, evaluate:

```mathematica
FeynGravLibraryInformation[FeynGravImportHorndeskiG2]
```

The extensionless filenames are `HorndeskiGi_a_b_n`. This checkout contains
the following selections; each listed pair has **both n = 1 and n = 2**:

| Family | Installed (a,b) pairs | Files |
| --- | --- | --- |
| G2 | (0,2), (0,3), (1,1), (1,2), (2,1), (2,2), (3,1), (4,1) | 16 |
| G3 | (0,1), (0,2), (1,1), (1,2), (2,0), (2,1), (3,0), (3,1), (4,0), (5,0) | 20 |
| G4 | (0,1), (0,2), (1,1), (2,0), (2,1), (3,0), (4,0) | 14 |
| G5 | (0,1), (1,0), (1,1), (2,0), (3,0) | 10 |

This is a file inventory, not mathematical certification. An unmatched call
does not mean a vertex is zero. The importer does not generate absent orders
or compile a general G function. No zero-graviton Horndeski libraries are
shipped: purely scalar derivative contacts can still exist and must be included
when assembling a complete process. The inventory is not a complete amplitude
prescription for an arbitrary choice of functions.

<a name="Examples and FeynArts support"></a>

## Examples and FeynArts support

There is no native `Horndeski` model. The supplied
[John bundle](./John/NativeModel.md) implements GR, a canonical
massive scalar and $\beta G^{\mu\nu}\partial_\mu\phi\partial_\nu\phi$ through κ²
per vertex. It uses G5 with a = 1, b = 0 and coupling $-\beta$, so the
G5 differentiated-function term is absent. Its independent verification does
not validate the general G4 family or G5 cases with b > 0.

Use example 10 (`Examples/10. Scalar_Scattering_John_Tree_Level_FeynArts.nb`)
for scalar elastic scattering in that model. The separate
[EsGB bundle and example](../ScalarGaussBonnet/NativeModel.md)
use explicit scalar–Gauss–Bonnet rules; that workflow does not establish support
for a non-polynomial Horndeski representation. See the
[example index](../../Examples/README.md) for the remaining calculations.

`FeynGravSimplify` can simplify supported expressions assembled from these
vertices. It does not choose the action, repair a rule, supply omitted contacts,
derive a free propagator or establish gauge invariance automatically.

<a name="Rule generation and higher orders"></a>

## Rule generation and higher orders

The routines in `Rules/` are distinct from the four-argument library interface:
their public G2–G5 constructors and `Uncontracted` variants take **three**
arguments, with unit monomial coupling. Use a separate fresh generator kernel
to avoid collisions between identical short names. Read the
[rule contracts](../../Services/RuleConstruction/Contracts.md) and [generator guide](../../Services/LibraryGeneration/Guide.md).

`GenerateHorndeskiG2Specific[a, b, n]` and the corresponding G3–G5 commands
construct a selected library; n must be positive. The generator uses the
uncontracted rule before algebraic processing.

**Batch selection and exclusions.** In `GenerateHorndeskiG2[S, n]` and the
corresponding G3–G5 commands, S is the maximum number of scalar legs, and the
batch selects graviton orders 1 through n. For nonnegative integer a and b,
the generator (`Libs/Generator.wl`) selects the following monomials:

| Batch command | Scalar legs s | Selection condition |
| --- | --- | --- |
| `GenerateHorndeskiG2[S, n]` | a + 2b | b >= 1 and 3 <= s <= S |
| `GenerateHorndeskiG3[S, n]` | a + 2b + 1 | 3 <= s <= S |
| `GenerateHorndeskiG4[S, n]` | a + 2b | 2 <= s <= S |
| `GenerateHorndeskiG5[S, n]` | a + 2b + 1 | 3 <= s <= S |

Thus G2 batches omit every b = 0 potential term and the a = 0, b = 1 kinetic
term. G4 batches omit a = b = 0 (the pure curvature term) and a = 1, b = 0
(the scalar–curvature term). G3 and G5 batches omit all terms with fewer than
three scalar legs. In particular, **no G5 batch includes the John interaction**
a = 1, b = 0, even when S is increased. Use
`GenerateHorndeskiG5Specific[1, 0, n]` to generate its library at the selected
graviton order; `GenerateHorndeskiG5[2, n]` selects no libraries.

The `Specific` commands bypass these batch filters, but remain subject to the
source construction bounds below. Conversely, selection by a batch does not
guarantee source support: the batch filters do not enforce those bounds.

**G2 and G3 construction bounds.** The shared
`CTensorGeneral` (`Rules/CTensorGeneral.wl`) constructor supports at
most seven inverse-metric pairs. The
G2 rule (`Rules/HorndeskiG2.wl`) requires b pairs, hence 0 <= b <= 7.
The connection term in the G3 rule (`Rules/HorndeskiG3.wl`) requires
b + 2 pairs, hence 0 <= b <= 5 at every supported graviton order n >= 1.
Both allow nonnegative integer a. These are implementation bounds, not
restrictions on the Horndeski action, guarantees of practical runtime or
claims that every allowed case has been independently verified. They also
do not imply that a corresponding stored library is shipped.

G5's TII contribution now supports every graviton order. Its four classes contain
zero through three connections; further gravitons enter through the determinant
and inverse metrics. The direct source bounds are b <= 4 for n = 1, b <= 3 for
n = 2 and b <= 2 for n >= 3, with nonnegative a and b. They reflect the shared
seven-inverse-metric limit, not a runtime guarantee. The b = 0 route remains
separate. G2 accepts an empty graviton list; direct G3–G5 require at least one
triple. The checks summarised in [Scientific scope](../../ScientificScope.md#Horndeski)
include the complete four-graviton a = 0, b = 1 vertex and selected T8 sectors.
No higher-order stored library is added by this extension.

For any extension, first validate the relevant source against the corrected
action, including permutations, coupling and Fourier factors. Generate into
a separate candidate directory, compare the candidate and public import
against that reference, then document the supported domain. The corrected G4
source has dependency bounds b <= 5 for one graviton and b <= 4 for two or more,
with nonnegative a and b. Its checked cases and the incomplete generic b=5 comparison
are summarised in [Scientific scope](../../ScientificScope.md#Horndeski); these bounds
are not arbitrary-order runtime guarantees. All 14 shipped G4 libraries have been regenerated and checked. The checks cover
ordinary public imports and the John identities.
Native model files require their own rebuild and checks only if an affected rule
feeds them; none of the shipped bundles uses general G4.

<a id="verification-and-open-work"></a>
<a name="Verified scope and limitations"></a>

## Verified scope and limitations

The [scientific-scope summary](../../ScientificScope.md#Horndeski) distinguishes
source comparisons, stored-library checks and complete processes. All 60 shipped
selections have recorded provenance. The 14 G4 libraries and four G5 b=1 files
were corrected; G2/G3 and the six G5 b=0 files are unchanged by that migration.
The [John model](./John/NativeModel.md#Verification) has
its own cubic/quartic off-shell and selected tree Ward checks.

Higher-order G5 construction includes all connection classes, but its finite
checks do not certify every vertex. The generic G4 b=5 comparison is incomplete;
no full five-graviton G5 or general Horndeski loop validation is claimed.

Restart the kernel and recompute results using any old G4 library or the four
affected G5 b=1 files. A global rescaling is generally insufficient. Existing
notebook outputs are not recomputed by an import or package update.
