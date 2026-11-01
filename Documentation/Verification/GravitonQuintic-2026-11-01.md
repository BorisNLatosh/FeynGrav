# Independent five-graviton vertex — VERIFY-02

1 November 2026. **Accepted independent off-shell vertex comparison.**
Base checkout: `FeynArts_Integration`,
`5e9e9224267e91efa509c70dd81d7a514369552f`. The stored rule is unchanged.
The [machine-readable record](GravitonQuintic-2026-11-01.json) gives the source,
script, output and bounded-execution identities.

## Action and independent derivation

Use the [common conventions](../Conventions.md) and
[GR action](../Models/GeneralRelativity.md):

\[
S_{\rm EH}=-\frac{2}{\kappa^2}\int d^D x\,\sqrt{-g}\,R,
\qquad g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu},
\qquad \eta=\operatorname{diag}(+,-,\ldots,-).
\]

The curvature convention is
\(R^\rho{}_{\sigma\mu\nu}=\partial_\mu\Gamma^\rho{}_{\nu\sigma}
-\partial_\nu\Gamma^\rho{}_{\mu\sigma}
+\Gamma^\rho{}_{\mu\lambda}\Gamma^\lambda{}_{\nu\sigma}
-\Gamma^\rho{}_{\nu\lambda}\Gamma^\lambda{}_{\mu\sigma}\).
Fourier modes are \(e^{ip\cdot x}\), with all momenta incoming; the vertex is
\(i\) times the action derivative. Background de Donder gauge fixing is
quadratic in h and contributes no quintic contact.

Integrating the two differentiated-connection terms by parts, using
\(\partial_\rho\sqrt{-g}=\sqrt{-g}\,\Gamma^\lambda{}_{\lambda\rho}\)
and metric compatibility, gives

\[
\sqrt{-g}R=\sqrt{-g}\,g^{\mu\nu}
 \left(\Gamma^\rho{}_{\mu\sigma}\Gamma^\sigma{}_{\nu\rho}
       -\Gamma^\rho{}_{\mu\nu}\Gamma^\sigma{}_{\rho\sigma}\right)
 +\partial_\rho B^\rho,
\quad
B^\rho=\sqrt{-g}\left(g^{\mu\nu}\Gamma^\rho{}_{\mu\nu}
                  -g^{\rho\mu}\Gamma^\nu{}_{\mu\nu}\right).
\]

Discard the surface term for the momentum-space interaction. This fixes the
representative of the off-shell vertex used for comparison. The total-momentum
delta function is suppressed. The sign is additionally checked against a
separate direct-Ricci expansion at cubic order, using only
\(p_3=-p_1-p_2\); no mass-shell condition is used in that control.

Introduce independent labels and unrestricted auxiliary vectors,
\(h_i=e_i e_i\), with \(t_i^2=0\). This retains the coefficient in which each
external graviton occurs exactly once. For the spectator set S define

\[
H_S{}^\mu{}_\nu=\sum_{r\in S}t_r e_r^\mu e_{r\nu},\qquad
G_S=(1+H_S)^{-1}=1-H_S+H_S^2-\cdots,\qquad
V_S=\sqrt{\det(1+H_S)}.
\]

All contractions use the formal background metric. No numerical components,
fixed-dimensional vector basis or Gram relation enter. The determinant lemma
makes the coefficient of \(t_A=\prod_{r\in A}t_r\) in the determinant the
principal Gram minor \(d_A=\det(e_r\cdot e_s)_{r,s\in A}\). The volume follows
from \(V_S^2=\det(1+H_S)\):

\[
v_\varnothing=1,\qquad
v_A=\frac12\left(d_A-
 \sum_{\varnothing\ne B\subsetneq A}v_Bv_{A\setminus B}\right).
\]

The inverse-metric coefficients follow directly from the Neumann series,
with every permutation of the chosen labels appearing once. For example,

\[
[t_A]\,(u\cdot G_S\cdot v)=(-1)^{|A|}
\sum_{\pi\in\mathrm{Perm}(A)}
(u\cdot e_{\pi_1})(e_{\pi_1}\cdot e_{\pi_2})\cdots
(e_{\pi_{|A|}}\cdot v).
\]

Write the first-kind connection of leg i as \(i\kappa C_i/2\), where

\[
C_{i,abc}=e_{ia}p_{ib}e_{ic}+e_{ia}e_{ib}p_{ic}
                         -p_{ia}e_{ib}e_{ic}.
\]

The **complete five-graviton vertex**, contracted with the five unrestricted
\(e_i e_i\), is therefore the following finite coefficient formula:

\[
\boxed{
\mathcal V_5(e_1,\ldots,e_5)=\frac{i\kappa^3}{2}
\sum_{i\ne j}
[t_S]\,V_S C_{i,abc}C_{j,def}
\left(G_S^{af}G_S^{be}G_S^{cd}
     -G_S^{ae}G_S^{bc}G_S^{df}\right),
\qquad S=\{1,2,3,4,5\}\setminus\{i,j\}.
}
\]

The normalisation is \(i(-2)(i/2)^2=i/2\). There is no additional 5!:
the coefficient of all five distinct labels already performs functional
differentiation with respect to the five fields. The ordered i,j sum chooses
the two differentiated legs; determinant and inverse-metric expansions supply
all remaining insertions.

`Rules/Verification/GravitonQuinticReference.wl` implements precisely this
construction. It does not call the production `GravitonVertex`, C/I/Gamma
helpers, or stored libraries. Those libraries enter only the comparison side.
The direct-Ricci cubic control uses the separate test-only
`HorndeskiReference.wl`; it is not an input to the quintic reference.

## Why this tests the complete tensor

The five auxiliary vectors are unrestricted, not physical polarisations.
For a tensor symmetric in each external index pair, its polynomial contraction
with \(\prod_i e_i^{\mu_i}e_i^{\nu_i}\) determines every component: polynomial
coefficient extraction, or the polarisation identity in each slot, recovers
the symmetric tensor. This is not a sample of helicities or numerical points.

All five pair symmetries are checked **on the actual unprojected stored rule**.
The reference is symmetric by construction from h. The exact polynomial
residual is zero, so these checks together establish complete tensor equality.
The four adjacent whole-leg swaps are also checked directly on the stored
rule; they generate all permutations of the five legs.

No on-shell, transversality, tracelessness, helicity or momentum-conservation
substitution is made in the quintic comparison. All formal scalar products
remain generic. D is never assigned. The untraced expression has no explicit
D from closed metric traces; single and double traces provide explicit
symbolic-D checks. On the stored side traces identify and contract index slots;
on the reference side they use one half of the auxiliary-vector Laplacian.

## Results

- The 76,230 expanded stored tensor terms project to **4,990** scalar-product
  monomials. The independent reference has exactly the same 4,990 monomials.
  Their full difference is literal zero.
- Cubic and quartic stored vertices agree independently with the same action
  construction. The volume-square and inverse-metric identities pass.
- All five pair swaps and four adjacent whole-leg swaps pass exactly.
- Each auxiliary vector has degree two, the total momentum degree is two,
  and the coupling degree is three. No unresolved Lorentz-index object remains.
- Single- and double-trace comparisons are exact zeros. Their expanded results
  have 1,940 and 654 terms and polynomial D degrees one and two, respectively.
- Wrong sign, wrong normalisation and a deliberately altered tensor coefficient
  fail equality as intended. Replacing D by four in the trace also fails the
  symbolic-D comparison.

The portable `GravitonQuinticChecks.wls` has three separately bounded suites:
`Lower` (11 assertions), `Quintic` (23), and `Trace` (10). See the
[execution instructions](../../Rules/Verification/README.md#five-graviton-einstein-hilbert-checks).
The initial evidence-only scripts and final installed suites are retained
separately. The initial quintic metadata mistakenly counted the top-level
factors of the reference (3) rather than expanded terms; the installed suite
corrects that measurement to 4,990. Its altered-coefficient control also uses
an actual expanded tensor monomial; the initial additive perturbation control
was less specific. Neither reporting/test correction changes the equality result.

| Scope | Assertions | Kernel elapsed (s) | Retained cgroup peak (bytes) |
| --- | ---: | ---: | ---: |
| lower | 11 | 5.197 | 203,608,064 |
| quintic | 23 | 36.856 | 485,904,384 |
| trace | 10 | 12.419 | 406,646,784 |
| installed-lower | 11 | 5.048 | 203,939,840 |
| installed-quintic | 23 | 36.825 | 486,109,184 |
| installed-trace | 10 | 12.532 | 406,962,176 |

All 44 final installed assertions pass; the preceding 44 assertions are
retained development evidence, not additional independent identities.

Every scope uses a fresh sequential kernel with aggregate 1 GiB RAM, zero swap,
120 seconds plus 10 seconds cleanup grace and fresh available host memory of
at least 2 GiB. All scopes exit normally, without diagnostics, resource events,
swap use or surviving task-owned processes. Peaks are the highest retained
cgroup memory samples, not isolated kernel RSS or guaranteed final peaks.
No FORM process, FeynArts initialisation, library regeneration or notebook
calculation is needed for these exact polynomial comparisons.

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-01-verify02/`.

## Closure and remaining boundaries

**VERIFY-02 is resolved.** No defect was found in the stored quintic rule.
Its SHA-256 remains
`c9deb4b247b58d2c7b5b6ddf9ad7f286c450df073517fc1d06956095f1ccce42`.

This acceptance concerns the conventional Einstein–Hilbert five-graviton
contact and the stated lower-order controls. It does not validate all higher
orders merely because the reference accepts more labels. No new five-point
amplitude, gauge-cancellation, Ward-identity or soft-limit calculation was run.
The native FeynArts order-three cost/completeness workflow remains **PERF-02**;
the shipped model still stops at κ² per vertex. No Cheung–Remmen, ghost-loop,
renormalisation or new native-model claim is added. Verification and source
acceptance do not imply publication.
