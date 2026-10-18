# Resolution — 18 October 2026

The potential blocker below is resolved. The user authorised the source correction and regeneration; eight checks passed after regeneration without a memory cap. GRScalarPotential is delivered. The historical observations below describe the pre-correction source. See [current scalar-model report](ScalarModelExtensions.md) for delivery, John verification limits and the separate EsGB blocker.

---

# Scalar-potential normalisation: implementation blocker

Checked: 18 October 2026. Evidence: **exact check — failed source/action agreement**.

The newly authorised GRScalarPotential, GRJohn and GREsGB work stopped at the first model's independent normalisation gate. No new model was built or published. Existing PureGR/GRScalar model files, interaction rules and libraries are unchanged. John and EsGB checks are **not tested**, not failed.

## Counterexample and tensor identity

The selected action contains `+lambda phi^n/n!`, for n = 3 or 4. Differentiating the scalar monomial cancels n! in both cases. With g = eta + kappa h,

$$
\sqrt{-g}=1+\frac{\kappa}{2}h+
\frac{\kappa^2}{8}(h^2-2h_{\mu\nu}h^{\mu\nu})+O(\kappa^3).
$$

Taking two symmetric graviton-field derivatives gives the vertex

$$
V^{\mu\nu,\rho\sigma}_{\rm action}
=\frac{i\lambda\kappa^2}{4}
(\eta^{\mu\nu}\eta^{\rho\sigma}
-\eta^{\mu\rho}\eta^{\nu\sigma}
-\eta^{\mu\sigma}\eta^{\nu\rho}).
$$

The imported `GravitonScalarPotentialVertex[{mu,nu,rho,sigma},lambda]` contains **i lambda kappa^2/8**, with precisely this tensor bracket. The retained symbolic residual `2 source - expected` is exactly zero.

An independent component check removes symmetric-index differentiation ambiguity: set only h00 = x + y on the mostly-minus background. Then sqrt(-g) = sqrt(1+kappa(x+y)). The mixed derivative with respect to the two independently labelled insertions is -kappa^2/4. The stored tensor projected onto four time indices instead gives -kappa^2/8. The one-graviton check agrees at i lambda kappa/2.

This is a mismatch between the documented full vertex and the action derivative. It is not a scalar factorial, on-shell, loop, resource or FeynArts insertion issue. The stored expression has the normalisation of a symmetrised expansion coefficient rather than the full two-graviton differentiated vertex. The source routine multiplies `CTensorGeneral` directly; the importer loads the file without an additional graviton factorial. This localises a likely missing differentiation multiplicity, but does not establish a general repair for other tensor families or orders.

## Retained execution

Evidence directory:
`/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-18-three-scalar-models/`

- `source-identity.json` and `source-snapshot/`: actual uncommitted input snapshot and SHA256 identities.
- `scripts/check.wls`: independent derivatives and source comparison.
- `run-CHECK/potential-normalisation.wl`: full tensors and exact residuals.
- `run-CHECK/check-result.json`: five passing checks, one failed source/action comparison.
- `run-CHECK/kernel-stdout.log`: no unexpected Wolfram messages.
- `run-CHECK/wrapper-scope.log`: 7.108 s kernel wall time; kernel exit 1 from the failed assertion.
- Highest retained cgroup memory peak: 328,597,504 bytes (not process RSS).
- Enforced 1 GiB whole-tree cap, zero swap, 120 s plus 10 s grace; initial host headroom 7,705,488 KiB.
- No sampled memory-limit or OOM events, no timeout, no task-owned survivors. Existing user kernels were untouched.
- Protected files and repository state were identical before and after the calculation. Documentation was added afterwards.

Only one bounded kernel launch was made. The runner rejected the nonzero result; there is no passing build receipt. No retry was attempted.

## Required decision

The implementation plan forbids changing interaction formulae, regenerating libraries or silently compensating in a model. Therefore the three-model assignment is blocked pending an explicit source-normalisation correction decision. Resolve and verify the potential rule first, including the relevant identical-graviton multiplicity, then resume model construction. Do not automatically multiply unrelated rules by two or reinterpret previous GRScalar checks as independent validation of this potential family.

## Authorised repair attempt — 18 October 2026

The user subsequently authorised correcting the source and regenerating the affected library, followed by the three-model implementation. Both potential routines in `Rules/GravitonScalarVertex.wl` now multiply the averaged determinant coefficient by `Factorial[Length[indexArray]/2]`. The shared C tensors and kinetic rules were not changed. Only potential libraries of orders one and two are present; order one is unaffected.

**Verification incomplete: resource stop.** A single new bounded run passed the zero-graviton check for both variants, then reached the 1 GiB whole-tree memory cap before library generation. `indexArraySymmetrization` calls `RuleValidation`RuleParallelMap`, which invokes `ParallelMap`; the retained scope observations identify the main Wolfram kernel and ten additional Wolfram kernels. These inherited the same aggregate cap. Highest retained cgroup memory peak: 1,073,741,824 bytes; sampled `memory.events max` reached 222931. The last sample recorded zero OOM-kill counters; the scope nevertheless exited with signal 9, so do not assert a sampled OOM kill. Elapsed observer time was about 24.1 seconds. No task-owned processes survived; user kernels were not disturbed.

No corrected library was produced or installed. Existing PureGR/GRScalar model files and all stored libraries remain unchanged. The source correction is pending verification; the three new models are not delivered. Unexecuted builder drafts remain outside the repository and are not accepted implementation.

Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-18-scalar-extension-work/repair/run-CHECK/`. This contains the scripts as run, source-protection hashes, stdout, assertions, live cgroup samples and termination receipt. The preceding exact source/action discrepancy is still valid for the shipped library.

The explicit resource-event stop rule prevents automatic retry. Recommended next step: authorise one serial verification/regeneration run under the same 1 GiB and 120-second limits, preventing automatic subkernel launch in the test harness without changing production parallelism. Once verified, install only the corrected order-two potential library and resume the model work. No commit has been made.
