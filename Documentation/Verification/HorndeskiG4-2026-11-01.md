# RULE-01: corrected Horndeski G4 source

**Later delivery:** [RULE-05 is now complete](HorndeskiLibraryMigration-2026-11-01.md), including all 14 G4 libraries and ordinary imports. The checkpoint below records the earlier source/audit delivery; its pending-library statements and JSON hashes describe that earlier state.

1 November 2026. **RULE-01 is resolved for the source generators and their documented domain.**
The 14 stored G4 libraries remain unchanged and require the separate RULE-05 migration.
In particular, the ordinary four-argument `FeynGrav` library interface does not yet
return the corrected vertices. No commit, model change or notebook change is included.

## Corrected construction

For unit coupling, both three-argument source routines now differentiate

$$
\sqrt{-g}\,\phi^a\left[X^bR-2bX^{b-1}\mathcal H_2\right],\qquad
X=(\partial\phi)^2,\quad
\mathcal H_2=(\Box\phi)^2-H_{\mu\nu}H^{\mu\nu}.
$$

The signature, curvature and Fourier conventions are those in the
[Horndeski model page](../Models/Horndeski.md). The Hessian still has
$H_{\mu\nu}=\partial_\mu\partial_\nu\phi-\Gamma^\rho{}_{\mu\nu}\partial_\rho\phi$.
The change is the consistently translated relative coefficient, not a reversal
of the connection sign in this definition.

One set of five analytic kernels replaces the rejected labelled-action route
and duplicated formulae. The curvature kernels are unchanged. Zero-, one- and
two-connection Hessian kernels have coefficients $-2b,+4b,-2b$. These kernels
strip derivative phases, so their common vertex factor is
$i(-1)^{b+1}\kappa^n$, once. A reference which already inserts $ip$ derivatives
instead uses only the final factor $i\kappa^n$.

Both scalar and graviton permutations act on labelled positions, including
when values coincide. The $b=0$ branch includes the missing $a!$. Every raw
kernel owns fresh internal indices, and uncontracted results are uncached.
The contracted route contracts canonical kernels before the linear external
permutation sums and memoises successful results only. Invalid arguments and
unsupported ranges return `Failure`; dependency failures propagate, and aborts
remain aborts. There is no fallback to the rejected action builder.

The [derivation](HorndeskiG4Derivation.md) gives the pointwise curvature identity,
Hessian expansion, Fourier counting, and exact permutation weights. It also
explains why projecting a separately symmetric graviton tensor onto unrestricted
auxiliary products $e_i^\mu e_i^\nu$ retains the complete tensor. These are not
physical polarisations and no transversality or helicity constraints are used.

## Verification and limits

The independent reference expands inverse metrics, determinant, connection,
Ricci tensor and covariant Hessian with labelled nilpotent fields. It calls no
production C/I/Gamma tensor or G4 kernel. It extends the independently checked
common reference used by the G2/G3/G5 audit.

All vertex comparisons retain symbolic D and generic off-shell momenta; only
the two John integration-by-parts checks impose total incoming momentum conservation.
No four-dimensional component basis, Gram constraints, field equations or
numerical sampling are used to establish the equalities.

| Check | Result and precise scope |
| --- | --- |
| Portable source regression | 95/95: actual contracted and uncontracted public calls at `(n,a,b)=(1,2,0),(1,0,1),(1,0,2),(2,0,1),(2,0,2)`; complete `(3,0,2)` action from actual kernels; pair/leg symmetries, homogeneity, phase and altered-factor controls |
| Complete three-graviton rule | Curvature and all three Hessian sectors agree separately. Contracted source seeds are summed over every labelled scalar/graviton permutation. The genuine order-three T5 removal produces a nonzero residual. This is exact full-tensor equality, not a timing claim for the large unsimplified public wrapper |
| Source interfaces | 67/67: early malformed/domain failures, intentional Failure/$Failed/Abort propagation and recovery, cache behaviour, fresh raw dummies, products, colliding names, composite/equal momenta, factorial and identical-graviton multiplicities |
| Higher-b and spectator checkpoints | 34 completed checks: orbit sums equal literal permutations block by block at `(2,0,2)`; full `(2,1,2)`, `(2,0,3)` and `(2,0,4)` action comparisons, counting and supporting symmetries |
| Installed-source smoke | 18/18 in a fresh kernel: installed source/reference loading, one-graviton action and phase checks, b=0 factorial and domain rejection; installed files match the tested candidate byte for byte |
| Stored/public-library audit | 48/48: every stored file agrees with its ordinary public import; all 14 discrepancies with the corrected action are explained below; both John identities and source/library arity separation pass |

The higher-order batch subsequently timed out at 600 seconds during the extra
generic `(1,0,5)` calculation. Its 34 completed assertions and full b3/b4 WXF
checkpoints are retained; **the batch is not a 67/67 pass and generic b5 equality
is not claimed**. The later portable suite completes the required public/n3
checks which had not been reached by that batch. It does not repeat b5.

The dependency domain is n >= 1, a >= 0, with integer b <= 5 for one graviton
and b <= 4 for multiple gravitons. The largest C-tensor rank is respectively
b+2 or b+3 inverse-metric pairs; the dependency implements seven. The geometric
construction and labelled-differentiation argument justify that domain, while
finite tests certify the stated cases. Arbitrary-order runtime, all n/b
combinations, generic b5 completion, zero-graviton interfaces, loops and a general
Horndeski FeynArts model are not claimed.

## Publication and stored-library relationship

The curvature decomposition in [FeynGrav 3.0, Eq. (39)](https://arxiv.org/html/2406.14872#S3.E39)
is consistent with this construction. The Hessian sector in its Eqs. (40)–(42)
uses the historical +b monomial normalisation. Under the documented (+---),
X-without-a-half convention, that complete Hessian sector must be multiplied by
-2, while the curvature sector is retained. The convention translation from
[Kobayashi, Yamaguchi and Yokoyama](https://arxiv.org/html/1105.5723v4#S2)
is derived on the model page. This is not a blanket certification or rejection
of every published expression. The omitted b=0 factorial is an implementation
error; the publication's full labelled-permutation prescription includes it.

Let R denote the complete curvature vertex and H the complete **corrected**
Hessian vertex, with the same coupling and kappa context. Exact comparisons give:

| Stored selections (each at n=1,2) | Files | Required change |
| --- | --- | --- |
| `(a,b)=(0,1),(0,2),(1,1),(2,1)` | 8 | Stored result is `R-H/2`; corrected result is `R+H`. There is no single overall rescaling |
| `(a,b)=(2,0),(3,0),(4,0)` | 6 | Corrected result is `a!` times stored result, i.e. factors 2, 6 and 24 |

The machine-readable per-file table is in the [manifest](HorndeskiG4-2026-11-01.json).
`GenerateHorndeskiG4Specific` and `GenerateHorndeskiG4` use the corrected
uncontracted source for future generation; neither was run to publish libraries.
`FeynGravImportHorndeskiG4` still imports the old files unchanged. RULE-05 must
regenerate and verify all 14 files, exercise the public importer and inspect
consumers before the ordinary G4 library interface can be marked migrated.
The native John model uses the independently checked G5 b=0 route; no model,
example, library or notebook is modified by this source delivery.

## Evidence and reproducibility

The portable entry point is [HorndeskiG4Checks.wls](../../Rules/Verification/HorndeskiG4Checks.wls),
with [its independent companion](../../Rules/Verification/HorndeskiG4Reference.wl)
and the existing common reference. See the [verification instructions](../../Rules/Verification/README.md).
It requires a fresh externally bounded kernel and retains logs and exact inputs/outputs.

Full local evidence:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-11-01-rule01-final/`.
Source/script hashes, exact results, resource samples, protected-file snapshots,
launch ledgers and the post-install check are recorded in the manifest. Eleven
scientific scopes were used within the predeclared maximum of twelve; none is
left running. A denied service-bus access before the installed smoke launched
no kernel; the same bounded runner then completed with the required service access.
Micro/interface scopes use 1 GiB/120 seconds; larger scopes use 4 GiB/600 seconds.
Both policies enforce zero swap, 10-second cleanup grace, fresh >=2 GiB host
headroom, sequential task-owned kernels and at most two serial FORM calls.
The timed batch peaked at 983,310,336 sampled cgroup bytes, with zero memory
limit/OOM events and zero swap; its scope and processes were confirmed absent
after cleanup. The portable suite completed in about 72.3 seconds of observed scope time, with
a 627,970,048-byte retained peak; the installed smoke used about 7 seconds and
232,886,272 bytes. Both had clean diagnostics, zero memory events/swap, and no
surviving owned processes. Cgroup measurements are not isolated kernel RSS.

Two initial package-context failures, one unbalanced test expression, and the
undeclared test scaling variable are preserved. They were corrected in the
harness, without suppressing unexpected diagnostics. The interface runner's
broad scanner misread `Abort` in successful test names; saved evidence was
revalidated after removing only the exact predeclared PASS lines, without a
rerun. Earlier rejected source and xPert evidence remain historical and are not
used as proof of the corrected action. No resource limits were increased.
