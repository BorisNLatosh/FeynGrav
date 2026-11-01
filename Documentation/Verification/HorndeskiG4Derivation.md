# Horndeski G4: action, vertex kernels and verification meaning

## Action and conventions

For nonnegative integers a and b, the interaction monomial is

$$
S_{a,b}=\lambda\int d^Dx\,\sqrt{-g}\,\phi^a
\left[X^bR-2bX^{b-1}\mathcal H_2\right],
\qquad
X=g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi,
$$

$$
H_{\mu\nu}=\nabla_\mu\nabla_\nu\phi,
\qquad
\mathcal H_2=(g^{\mu\nu}H_{\mu\nu})^2
-g^{\mu\alpha}g^{\nu\beta}H_{\mu\nu}H_{\alpha\beta}.
$$

There is no factor one half in X. For b=0 the Hessian term is absent; no
negative power of X is evaluated. The displayed action has no factorials
absorbed into the coupling. The rule kernels use unit coupling and the
physical vertex is multiplied by $\lambda$.

Use signature (+---), $g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}$,
incoming Fourier modes $e^{ip\cdot x}$, and the vertex factor i times the
functional derivative of the action. The curvature convention is

$$
R^\rho{}_{\sigma\mu\nu}=
\partial_\mu\Gamma^\rho{}_{\nu\sigma}
-\partial_\nu\Gamma^\rho{}_{\mu\sigma}
+\Gamma^\rho{}_{\mu\lambda}\Gamma^\lambda{}_{\nu\sigma}
-\Gamma^\rho{}_{\nu\lambda}\Gamma^\lambda{}_{\mu\sigma}.
$$

All identities below retain symbolic D and unrestricted off-shell momenta.
No momentum-conservation, helicity, transversality, or Gram constraint is
needed to compare the local vertices.

## Five geometric kernels

Define the first-kind connection and scalar derivatives by

$$
C_{\alpha\mu\nu}=\tfrac12(\partial_\mu g_{\alpha\nu}+\partial_\nu g_{\alpha\mu}-\partial_\alpha g_{\mu\nu}),
\quad \Gamma^\alpha{}_{\mu\nu}=g^{\alpha\rho}C_{\rho\mu\nu},
\quad v_\mu=\partial_\mu\phi,\quad A_{\mu\nu}=\partial_\mu\partial_\nu\phi,
$$

and let $Q^{\mu\nu\alpha\beta}=g^{\mu\nu}g^{\alpha\beta}-g^{\mu\alpha}g^{\nu\beta}$. The density is the sum
of two curvature terms and three Hessian terms:

$$
\begin{aligned}
\mathcal L_{R1}&=\sqrt{-g}\,\phi^aX^b
g^{\mu\nu}g^{\alpha\beta}\partial_\mu(C_{\nu\alpha\beta}-C_{\alpha\beta\nu}),\\
\mathcal L_{R2}&=\sqrt{-g}\,\phi^aX^b
g^{\mu\nu}g^{\alpha\beta}g^{\rho\sigma}
(C_{\mu\alpha\rho}C_{\nu\sigma\beta}-C_{\mu\alpha\beta}C_{\nu\rho\sigma}),\\
\mathcal L_{H0}&=-2b\sqrt{-g}\,\phi^aX^{b-1}Q^{\mu\nu\alpha\beta}A_{\mu\nu}A_{\alpha\beta},\\
\mathcal L_{H1}&=+4b\sqrt{-g}\,\phi^aX^{b-1}Q^{\mu\nu\alpha\beta}g^{\rho\sigma}C_{\rho\mu\nu}v_\sigma A_{\alpha\beta},\\
\mathcal L_{H2}&=-2b\sqrt{-g}\,\phi^aX^{b-1}Q^{\mu\nu\alpha\beta}g^{\rho\sigma}g^{\lambda\tau}
C_{\rho\mu\nu}C_{\lambda\alpha\beta}v_\sigma v_\tau.
\end{aligned}
$$

The curvature identity is pointwise. Lowering the first index of Riemann
first gives

$$
R_{\rho\sigma\mu\nu}=\partial_\mu C_{\rho\nu\sigma}
-\partial_\nu C_{\rho\mu\sigma}
+g^{\alpha\beta}(C_{\alpha\nu\rho}C_{\beta\mu\sigma}-C_{\alpha\mu\rho}C_{\beta\nu\sigma}).
$$

Contracting with $g^{\rho\mu}g^{\sigma\nu}$ gives the two displayed
curvature kernels. Derivatives of the inverse metric are included through
the connection products in this lowered-index formula. No integration by
parts is used, so multiplying by $\phi^aX^b$ requires no additional
derivatives of that factor.

The three Hessian kernels follow by expanding
$H_{\mu\nu}=A_{\mu\nu}-g^{\rho\sigma}C_{\rho\mu\nu}v_\sigma$. The two cross terms are equal after
dummy-index exchange, giving the coefficients $-2b,+4b,-2b$.
Because C is linear in h, the second curvature and two-connection Hessian
kernels vanish at one graviton. Other graviton dependence comes entirely
from the inverse metrics and $\sqrt{-g}$.

## Fourier phase and differentiation multiplicities

Every kernel has 2b+2 derivatives. If the tensor primitives omit their
Fourier factors i, the n-graviton vertex therefore has the common factor

$$
i\lambda(-1)^{b+1}\kappa^n.
$$

An independent geometric expansion which already substitutes
$v(p)=ip$, $H(p)=-pp-i\Gamma p$, and the Fourier-valued connection
must instead multiply by only $i\lambda\kappa^n$. Applying both
prescriptions would count the derivative phase twice.

There are N=a+2b labelled scalar legs. For a curvature kernel, select 2b
labels and partition them into b unordered pairs. Each such matching has
multiplicity

$$
a!\,2^b b!.
$$

For a Hessian kernel, designate its two Hessian scalar labels and match
2b-2 of the remaining labels into kinetic pairs. With an **ordered** Hessian
pair the multiplicity is

$$
w=a!\,2^{b-1}(b-1)!.
$$

An unordered pair has weight 2w only when its kernel is symmetric in those
two slots. The zero-connection kernel is symmetric. The one-connection
source kernel distinguishes the connection-times-gradient slot from the
flat Hessian slot and requires
$w[K(u,v)+K(v,u)]$. The two-connection source kernel is symmetric after
the full graviton permutation sum; retaining both scalar orientations with
weight w is also always valid. The independent action reference uses the
symmetric bilinear
$B(H_u,H_v)=\operatorname{tr}H_u\operatorname{tr}H_v-
\operatorname{tr}(H_uH_v)$, including both one-connection cross terms,
so its unordered Hessian sum carries 2w and the action coefficient -2b.

These are identities of permutations of labelled positions. Equal momentum
values do not identify external legs. For b=0 the scalar multiplicity is
a!, even though every scalar momentum is absent from the kernel.

The C tensor is an averaged Taylor coefficient. Full permutations of the
graviton positions supply its differentiation multiplicity: equivalently,
one distinguished differentiated graviton has spectator factor (n-1)!,
two ordered distinguished gravitons have factor (n-2)!, and a kernel with
none has factor n!. Zero kernels are removed before negative factorials
could arise.

## Symmetries, implementation domain and independent checks

Each graviton index pair is symmetric. The fully differentiated vertex is
symmetric under exchanges of labelled graviton triples and labelled scalar
legs. Dummy indices must be fresh in independent raw tensor factors; their
names are not external labels and must never introduce accidental contractions.

The analytic implementation depends on at most seven inverse-metric pairs
in `CTensorGeneral`. T1 and T4 require b+2 pairs, T3 requires b+1, and T2/T5
require b+3 when at least two gravitons are present. Thus its current domain
is b<=5 for one graviton and b<=4 for two or more gravitons. This dependency
limit is distinct from the mathematical action, which permits every
nonnegative b. The scalar combinatorics permit arbitrary nonnegative a,
without implying an unlimited practical calculation size.

The independent reference expands the metric inverse, determinant,
connection, curvature and Hessian directly with nilpotent labels for each
external graviton. It does not use the production C, I or Gamma tensors.
Its explicit volume formula currently covers one through three gravitons.

For a full symbolic tensor comparison one may set
$h_i^{\mu\nu}=e_i^\mu e_i^\nu$ with unrestricted auxiliary vectors.
For a separately symmetric multilinear tensor, equality as a polynomial in
all e_i determines the complete tensor by the polarization identity. The
pair symmetry and multilinearity must be checked separately: a rank-one
projection alone cannot detect an antisymmetric component. Using generic
scalar-product variables is not a fixed-dimensional component sample.

Exact comparisons at specified n,a,b establish equality for those complete
off-shell vertices. The pointwise geometric identity and orbit counting
prove their structural and combinatorial extensions; finite tests alone
do not verify every C-tensor coefficient at every higher graviton order.
Direct public calls, contracted/uncontracted agreement, index hygiene,
coincident-label multiplicities, and invalid-argument behaviour test
additional implementation properties. A grouped comparison must be
reported as such, even when its exact orbit identity avoids the need to
execute all factorially many positional terms.

These checks concern local G4 vertices. They do not establish loop results,
on-shell amplitudes, renormalisation, all stored-library contents, or the
other Horndeski sectors. An interrupted or resource-limited comparison is
inconclusive. Stored-library regeneration and verification remain separate
from the action-level derivation.
