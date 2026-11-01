# G5 higher-order construction — RULE-02

1 November 2026. **Accepted source extension; not committed.** Checkout:
`FeynArts_Integration`, HEAD `a7c2a9aae891a854028d86fbf2fa988822fccf52`.
The [earlier G5 audit](HorndeskiAudit-2026-11-01.md) established the corrected
supported-order kernels. This delivery extends their assembly beyond three
gravitons. The [JSON manifest](HorndeskiG5HigherOrders-2026-11-01.json) records
source, script, result and resource identities.

## Construction and domain

Use the [documented action and conventions](../Models/Horndeski.md#action):

\[
\mathcal L_5=\lambda\sqrt{-g}\,\phi^a
\left[X^bG^{\mu\nu}H_{\mu\nu}
 +\frac b3X^{b-1}\mathcal H_3\right],\qquad
H_{\mu\nu}=\partial_\mu\partial_\nu\phi
 -\Gamma^\rho{}_{\mu\nu}\partial_\rho\phi.
\]

Here X has no factor of one half. The differentiated-function term is omitted
when b=0, before any negative power of X is constructed. Let H=S-A, where A is
the connection term. Write traces using the full inverse metric and define the
symmetric trilinear form

\[
K(U,V,W)=[U][V][W]-[U][VW]-[V][UW]-[W][UV]+[UVW]+[UWV].
\]

It satisfies K(H,H,H)=\(\mathcal H_3\), so

\[
K(S-A,S-A,S-A)=K(S,S,S)-3K(A,S,S)+3K(A,A,S)-K(A,A,A).
\]

There are exactly four connection-placement classes, at **every** total graviton
order. The lowered connection is linear in a differentiated metric perturbation.
Its raised index supplies one inverse metric. Additional gravitons enter through
the determinant and inverse metrics, already present in the source coefficients.
Thus T5–T8 remain exhaustive; no new fourth-connection kernel or extra binomial
factor is required. For n>=3 the dispatcher uses all four; for n=0,1,2 it uses
only those with at most n connections.

The remaining n-r graviton labels enter an averaged C coefficient. The outer
labelled permutation sum selects the r differentiated legs and supplies the
remaining (n-r)! multiplicity. Scalar permutations remain labelled even when
momenta coincide. The existing T5–T8 factors (-1/3,1,-1,1/3), public relative
factor -2b and common Fourier/coupling normalisation are unchanged.

For r connections, X^(b-1), the three Hessian contractions and the connections
require b+2+r inverse metrics. The shared helper supports at most seven pairs:

\[
b+2+\min(n,3)\leq7.
\]

The Einstein-sector kernels require the same maxima. Both public **three-argument
source** constructors now enforce these bounds before generating permutations:

| Gravitons n | Allowed b | Scalar count |
| --- | --- | --- |
| 1 | 0 through 4 | a+2b+1, a>=0 |
| 2 | 0 through 3 | a+2b+1, a>=0 |
| 3 or more | 0 through 2 | a+2b+1, a>=0 |

The b=0 route remains independent of TII and permits every positive n. These
are algebraic dependency bounds, not promises of affordable evaluation at every
order. The four-argument public library interface still exposes its shipped
selections; this extension adds no stored library or FeynArts interaction.

## Implementation

`Rules/HorndeskiG5.wl` changes only usage text, the TII n>=3 dispatch arm and
inverse-metric guards. T1–T8 and TI are byte-identical to the accepted pre-extension
source. Label permutations, fresh dummy indices, successful-result-only caching,
dependency failure propagation and abort handling are preserved.

The independent verification reference needed its own higher-order volume:
the preceding file had a hardcoded n<=3 formula. For unrestricted rank-one
perturbations t_i e_i e_i, the determinant lemma gives each labelled determinant
coefficient as the corresponding principal Gram minor. In the ring t_i²=0,
solve V²=det recursively with V[0]=1. Inverse metrics come from a geometric
series; connection, Ricci tensor and Hessians are derived independently. This
reference calls no production C, I, Gamma or T1–T8 routine. Its n=3 volume and
complete compact reference are checked against the earlier formula.

## Exact checks

The **complete n=4,a=0,b=1 vertex** agrees with that independent action. Source
T1–T8 are evaluated as actual tensor kernels, contracted with unrestricted
auxiliary e_i e_i, and summed over every one of the 24 graviton and six scalar
permutations. Einstein and cubic-Hessian sectors agree separately; their sum
therefore agrees exactly. No summand or physical polarisation is sampled.
Polynomial equality for unrestricted auxiliary vectors determines the complete
separately symmetric tensor. D stays symbolic; no mass shell, momentum
conservation, null/transverse polarisation or helicity condition is applied.

Checks include all adjacent graviton/scalar swaps, quadratic auxiliary-vector
homogeneity, κ⁴ and six-derivative homogeneity, nonzero sectors and absence of
unresolved indices. Wrong sector normalisation and omitted nonzero T8 controls
reject equality. Independent fixed-connection T8 checks additionally cover
n=4,b=2 (the seven-inverse-metric boundary) and n=5,b=1 (two spectator metric
labels). Those are complete **sector** checks, not full vertices at those orders.

Marked-kernel tests check dispatcher n=0..6, public n4/n5 multiplicities, b=0
isolation, early bounds, failure/cache/retry and abort propagation. They verify
control flow and assembly only. Actual lower-order action, traced symbolic-D,
coincident-momentum and index-collision checks remain in the 48-check regression.

| Accepted scope | Assertions | Last observed elapsed (s) | Retained cgroup peak (bytes) |
| --- | ---: | ---: | ---: |
| Dispatch, bounds, recovery and reference primitives | 25 | 3.64 | 215,650,304 |
| Independent complete n4 reference and n3 equality | 5 | 95.50 | 348,635,136 |
| Actual n4 T1–T4 source seeds | 8 | 6.17 | 288,280,576 |
| Actual n4 T5–T8 and projection controls | 10 | 14.37 | 503,296,000 |
| Independent n4,b2 and n5,b1 T8 sectors | 8 | 5.35 | 236,691,456 |
| Complete n4 Einstein sector | 17 | 54.55 | 497,430,528 |
| Complete n4 Hessian sector | 18 | 117.04 | 679,907,328 |
| Lower-order portable regression | 48 | 23.22 | 354,877,440 |
| Fresh installed portable higher-order suite | 33 | 5.76 | 237,084,672 |

The nine accepted scopes contain **172 passing assertions**, including the
33-check fresh installed suite. Repeated assertions across stages are not 172
independent mathematical identities. No failed run is counted as accepted.

## Bounds and retained unsuccessful attempts

All thirteen launched scopes were fresh and sequential, with aggregate 1 GiB
RAM, zero swap, 120 seconds plus 10 seconds cleanup grace, and at least 2 GiB
fresh available host memory. FORM children used the explicit serial executable
with one worker. The highest retained passing cgroup peak was **679,907,328
bytes**; this is charged process-tree memory, not isolated kernel RSS. The final
Hessian comparison took about 117 seconds within its scope, close to the time
cap. These are verification timings, not public-constructor performance claims.

Four earlier unsuccessful scopes are preserved:

- `reference-n4`: the direct full labelled reference reached the 120-second
  bound without a comparison. A symmetry-reduced, algebraically complete scalar
  differentiation was then checked at n=3 and completed at n=4.
- `seeds-hessian`: the first uncontracted T5 export reached the memory cap before
  a FORM child was observed. Free indices were subsequently replaced by their
  unrestricted auxiliary momenta **before** export, an exact linear contraction
  also checked against explicit lower-order projections.
- `compare-n4`: materialising all 144 labelled copies and both action sectors
  reached the memory cap. Sequential summand accumulation and separate sector
  scopes avoided that oversized intermediate without changing the sum.
- `compare-einstein`: the independent block equality and seven further checks
  passed, then the homogeneity fixture failed on an unnormalised symbolic factor
  inside `Momentum`. The corrected fixture scales explicit scalar-product atoms
  and validates their momentum/dimension vocabulary. Its single corrected run
  passes all 17 assertions; no production formula changed.

System logs confirm both OOM kills; sampled counters alone did not capture the
final event. Original logs and checks are retained. All observed task-owned
processes ended; only verified empty failed task scopes were reset. No user
kernel was touched. Limits were never raised. Successful scopes have no unexpected
diagnostic, swap use or resource event.

## Reproduction and preservation

`Rules/Verification/HorndeskiG5HigherOrderChecks.wls` is the portable 33-check
contract/sector suite; `HorndeskiChecks.wls` retains 48 lower-order checks with
the former n4 rejection replaced by the actual inverse-metric boundary.
See the [verification instructions](../../Rules/Verification/README.md).
The extended independent reference is shared with those suites.

The full n4 derivation/comparison uses retained stage scripts and pinned WXF
intermediates under:

`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-01-rule02-extension/`

The accepted order is `contracts`, `reference-compact`, `seeds-einstein`,
`seeds-hessian-r2`, `higher-sectors`, `compare-einstein-r2`,
`compare-hessian-r2`, `portable-regression`, `installed-higher`. Per-stage files
include the exact script copies, input hashes, predeclared assertions, raw
outputs/diagnostics, process/resource observations and termination records.
The manifest identifies comparison WXF prerequisites. Do not overwrite those
historical directories or reuse consumed runner attempts; a future rerun needs
fresh paths, a freshly reviewed source identity and external resource bounds.

All 60 shipped Horndeski libraries are byte-identical to the RULE-02 starting
snapshot, preserving the preceding RULE-05 migration. All model and notebook
files and unrelated source are unchanged. No commit, push, branch change,
library regeneration, model rebuild or notebook execution was performed.

## Remaining limits

RULE-02 is closed for this source extension and construction. Full n5 vertices,
full n4,b2 vertices, all higher-b boundary points and arbitrary-order runtime
completion are not claimed. Higher-order generation/import must be verified
before any new library ships. No general loop/Ward or native scattering result
follows from this source check. The earlier G4 b5 timeout and broader QED/FORM
limitations remain separate; closing RULE-02 does not change their status.
