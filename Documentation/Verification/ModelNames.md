# Model naming and scalar merge — 19 October 2026

This is an uncommitted naming-only update on `FeynArts_Integration` following commit `7713935`. The exact names are `GR`, `Single-Scalar-Tensor-Minimal-Coupling`, `John` and `Einstein-scalar-Gauss-Bonnet`. The latter three include a real scalar of arbitrary symbolic mass. The merged scalar model has independent positive cubic and quartic potential couplings; both may be set to zero after conversion.

## Changes and preserved evidence

The former PureGR, GRScalarPotential, GRJohn and GREsGB generic/classes files were moved and renamed **without changing their bytes**. Manifests now use the corresponding names in ModelName and Profile; their model-file hashes are unchanged. Historical source hashes remain historical provenance. The redundant GRScalar bundle and its standalone minimal-only builder were removed; their prior versions and tests remain in Git and retained evidence.

The three scalar developer scripts and their common helper use the new names. The public GR builder, bridge profile checks, startup listing, current guides and navigation were updated. No old-name aliases remain. Historical verification reports keep their original names and claims; their user-guide links point to current destinations. The supplied graviton notebook has one changed model-selection input line; saved output remains untouched. Its old paths/model names are historical output, not the current configuration.

No interaction formula, generated library, loop pin or benchmark notebook changed. Existing exact algebra evidence is reused because the model expressions are byte-identical. The scalar-only zero-potential limit was also checked directly in the renamed model. Builders were inspected statically, not rerun to regenerate equivalent model expressions.

## Fresh-kernel checks

Each model was loaded in a separate fresh kernel with FeynCalc-patched FeynArts. All 32 checks passed. These cover automatic discovery, matching manifest names, warning-free initialisation, exact vertex counts, a native cubic contact converted by FCFAConvert, unassigned parameters, scalar masses and nonminimal couplings where applicable. GR additionally passed the companion model/profile and process validators. The merged model's six potential coupling rows vanish when both couplings are zero, and its native scalar cubic contact is i ScalarLambda3.

| Model | Passed checks | Kernel wall seconds | Highest retained cgroup memory peak, bytes |
| --- | ---: | ---: | ---: |
| `GR` | 8 | 5.439 | 291483648 |
| `Single-Scalar-Tensor-Minimal-Coupling` | 8 | 6.555 | 291794944 |
| `John` | 8 | 7.408 | 291172352 |
| `Einstein-scalar-Gauss-Bonnet` | 8 | 4.974 | 281034752 |

These are single smoke checks, not benchmarks or a repeated full algebra suite. Each used 1 GiB whole-tree memory, zero swap, 120 seconds plus 10 seconds grace, and at least 2 GiB initial host headroom. There were no unexpected warnings, resource events or task-owned surviving processes. Existing user kernels were untouched.

Evidence: `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-19-model-names/`. `rename-receipt.json` records the exact mapping and prior hashes. Per-model directories retain scripts, assertion results, native contacts, resource observations and termination records. Final navigation/wording edits were checked statically after these runs.

Loop validation, full on-shell amplitudes and higher-order EsGB source auditing remain outside this change. No commit was made.
