# Quark–gluon gravitational coupling correction — 26 October 2026

## Finding and correction

The contracted and uncontracted `GravitonQuarkGluonVertex` source routines in
`Rules/GravitonSUNYM.wl` omitted the overall κⁿ for n attached gravitons. Their
`CETensor` supplies geometric Taylor coefficients of the metric determinant and
inverse vierbein, with κ factored out. Neither library generation nor the public
importer restored that factor. The stored order-one and order-two libraries
therefore had the same defect.

Both source variants now multiply their existing expression by κⁿ. The existing
n! is retained. Only `Libs/GravitonQuarkGluonVertex_1` and `_2` were regenerated,
using the existing generator and serial FORM in a fresh temporary destination.
Each regenerated expression equals its previous stored expression times κⁿ
exactly. No colour, gamma-matrix, Lorentz-tensor or scalar coefficient was changed.
The zero-graviton source vertex remains unchanged.

## Independent normalisation check

In the existing convention the flat vertex is
\(i g_s T^a\gamma^\lambda\). Its curved-space density contains
\(g_s\sqrt{-g}\,\bar q\gamma^a e_a{}^\mu T^b A_\mu^b q\).
Write \(g=\eta+\kappa h\) and \(H^\mu{}_\nu=\eta^{\mu\rho}h_{\rho\nu}\).
In the symmetric vierbein gauge,

\[
\sqrt{-g}=1+\frac{\kappa}{2}\operatorname{tr}H
+\kappa^2\left[\frac{(\operatorname{tr}H)^2}{8}
-\frac{\operatorname{tr}(H^2)}4\right]+O(\kappa^3),
\qquad
 e^{-1}=\mathbf1-\frac\kappa2 H+\frac{3\kappa^2}{8}H^2+O(\kappa^3).
\]

Consequently,

\[
\sqrt{-g}\,e^{-1}=\mathbf1+
\frac\kappa2[(\operatorname{tr}H)\mathbf1-H]
+\kappa^2\left[
\left(\frac{(\operatorname{tr}H)^2}{8}-\frac{\operatorname{tr}(H^2)}4\right)\mathbf1
-\frac14(\operatorname{tr}H)H+\frac38H^2\right]+O(\kappa^3).
\]

Differentiating with respect to symmetric labelled graviton fields supplies the
one- and two-graviton references without invoking C/CE tensor helpers. In
particular, the one-graviton rule is

\[
V^{\mu\nu;\lambda,a}
=i g_s\kappa T^a\left[
\frac12\eta^{\mu\nu}\gamma^\lambda
-\frac14(\eta^{\mu\lambda}\gamma^\nu+
\eta^{\nu\lambda}\gamma^\mu)\right].
\]

The full two-graviton comparison includes all 15 Lorentz/gamma monomials from
the quadratic expression. D remains symbolic and every gamma matrix is
D-dimensional. These local contacts are momentum independent; no on-shell,
helicity, momentum-conservation or four-dimensional identity is imposed.

## Accepted checks

- **29/29 source/generation assertions:** the original omission; exact
  independent comparisons at n = 0, 1, 2; contracted/uncontracted agreement;
  equality to the old sources times κⁿ; homogeneous κⁿg_s powers; graviton-pair
  symmetries; missing-κ negative controls; exactly two selected/generated
  libraries; old-library and independent-reference equality; dimensional and
  resolved-expression checks.
- **12/12 fresh public-import assertions:** normal `FeynGravImportSUNYM[2]`,
  both independent reference comparisons, coupling powers and zero-gravity
  limits, dimensional/resolved structures, reimport preservation, absence of
  gauge parameters and exact installed source/library hashes.

The two final accepted batches emitted no Wolfram diagnostics or payload stderr.
They used Wolfram 15.0.1 and FeynCalc 10.2.1. The generation batch contained two
sequential serial FORM calls; the public-import batch contained none.

| Accepted batch | Kernel/child payload wall time | Peak aggregate cgroup memory |
| --- | ---: | ---: |
| Source and generation | 4.344 s | 242,778,112 bytes |
| Public import | 3.272 s | 237,662,208 bytes |

Every launch enforced whole-tree 1 GiB RAM, zero swap, 120 seconds plus 10 seconds
termination grace and fresh host available memory above 2 GiB. No sampled memory
limit/OOM event, swap use or surviving task-owned calculator occurred. User
kernels were not touched.

Two test-harness failures are retained rather than counted as passes. The first
reference assignment ended at a newline before its two remaining second-order
terms; a separate bounded inspection isolated that error before generation.
The first public test introduced a Global-context κ shadow while reading the
reference, and its gauge-symbol pattern evaluated to gauge values. Corrected
harnesses use a grouped reference assignment, the public reference context and
localised gauge values for the symbol check. No further production changes were
needed. Five fresh kernels were used in total: those two failed harness runs,
one diagnostic inspection, and the two accepted batches above.

## Source identity and retained evidence

Base commit: `a306478ff948f022b4218408327d69a025742166`, branch
`FeynArts_Integration`. The maintainer authorised committing this correction
on 26 October 2026; no push or merge is included.

| Installed file | SHA-256 |
| --- | --- |
| `Rules/GravitonSUNYM.wl` | `3072d40fa73e4d13a5dd99f0993aed1ffa89c584e6498ac249a2cc46c87f6c90` |
| `Libs/GravitonQuarkGluonVertex_1` | `4d73e5f05496b836bc0f0e5b625789a23220608bbb895ad2e2aeb1484896f73d` |
| `Libs/GravitonQuarkGluonVertex_2` | `79232c603c9bc7c68612bad8fe6db2dcc2f86178c6ce6dcd09e555e049221dd2` |

Evidence is retained locally at
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-26-quark-gluon-kappa/`:
before/candidate sources and libraries, exact test scripts/diffs, independent
reference, raw diagnostics, comparisons, hashes, installation receipt and
resource/termination records. This path is provenance, not a runtime dependency.

The non-quark source text is byte-preserved after removing only this patch.
All five existing native model bundles, all notebooks and unrelated libraries
are unchanged. The pure-YM model contains no quarks and requires no rebuild.
Its manifest intentionally retains the source hash of its historical build;
it does not assert that the entire current source file is byte-identical.

## Use and limits

Restart the kernel and call `FeynGravImportSUNYM[2]` to load the corrected stored
rules. Previously evaluated expressions and saved notebook outputs must be
recomputed if they used these mixed quark–gluon vertices. Do not manually
multiply the newly imported vertices by another κⁿ.

This resolves the κ-power follow-up from
[FA-YM-01](GRSUNYM-2026-10-26.md). The general source prefactor is repaired, but
independent checks and regenerated stored libraries cover only n = 0, 1, 2.
No native quark model, complete quark scattering/Ward test, loop calculation,
higher-order tensor validation or full regression suite is claimed.
