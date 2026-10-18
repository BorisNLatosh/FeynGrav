# GRScalar native tree release — 18 October 2026

## Delivered scope

The separate `FeynArts/Models/GRScalar` bundle implements conventional GR plus one real, minimally coupled scalar with symbolic mass, through $\kappa^2$. It contains `TTT`, `TTTT`, `TUU`, `TSS` and `TTSS`, preserves symbolic `D` and the selected negative ordinary-GR ghost propagator, and introduces no scalar potential or non-minimal interaction. See the [native guide](../../FeynArts/Models/GRScalar/README.md).

The model files were copied byte-for-byte from the tested staged bundle. The existing PureGR generic/classes files and manifest, interaction libraries, notebooks, Contractor, source formulae and loop verification pins were unchanged by this delivery. Existing uncommitted work was preserved. No commit accompanied this report.

## Identity

The checkout was `FeynArts_Integration` at `1685e07619c231d75cb05f4ea867b135abb1aa5e` with existing uncommitted source/documentation changes. HEAD alone does not identify the tested snapshot.

| Component | SHA-256 |
| --- | --- |
| `FeynGrav.wl` tested source | `a3b24436ef92f078f6c05cdd0bf8c4ec2cacb3d9274b20ea0245f69fffed6ceb` |
| `FeynArtsIntegration/BuildGRScalar.wl` | `ef3e18d832e0382d0bab689390414c80a90ca2909b56cc5e4a816ee5f8555d9a` |
| `GRScalar.gen` | `19bcdd15e8f73514f54c3a39de895b8a02416f8d3549c851f5fab9a8e240898f` |
| `GRScalar.mod` | `4cfdf88293d264c5d4cc3f7f7c0e45e32eb42d698506645189567a9917509313` |
| `manifest.json` | `6e58db870798c1be28ee359b5fdd0ab1c9a39bcaba5ada55641cba82cda309d7` |

The manifest records the four source libraries and their hashes, the fixed gauge, field/coupling inventory and model snapshot. Its absolute source paths describe the build machine; the model files do not read them when loading. Basis sizes are `TTT=231`, `TTTT=4158`, `TUU=9`, `TSS=4`, `TTSS=18`.

Dependencies tested: Wolfram 15.0.1 on Linux x86-64, FeynCalc 10.2.1, FeynCalc-patched FeynArts 3.12. These are tested versions, not established minimum requirements.

## Completed checks

| Area | Result and qualification |
| --- | --- |
| Build | 24/24 passed: source/import prerequisites, tensor bases, propagators, finite inventory, self-contained generated files, native initialisation and cubic conversion |
| Fresh native workflow | 29/29 passed; native model initialised and trees constructed before loading FeynGrav or its reference libraries |
| TSS | Exact source comparison and an independently written minimally coupled action normalisation; scalar interchange checked |
| TTSS | Exact source comparison including mass terms; exchange of complete graviton index pairs and scalar legs checked |
| Scalar–scalar scattering | Three graviton exchanges identified by species/routing; each vertex and the complete propagator/scalar remainder agree exactly with source rules; no scalar four-point contact |
| Scalar–scalar to two gravitons | Contact, graviton exchange and both massive scalar exchanges; exact source-factor checks preserve actual signed endpoint momenta and native factors |
| Mass | Actual converted scalar poles carry ``FeynGrav`ScalarMass``; both scalar exchanges also agree at mass zero |
| Crossing | Crossed `S T -> S T` TTSS contact checked; complete crossed-amplitude equality not tested |
| Missing coupling | Excluding TTSS removes the contact and retains the three exchanges; options and canonical field-point availability restored |
| Preservation | Protected source/model hashes agree before/after; copied bundle matches the tested candidate |

There are 31 saved exact-zero residual artefacts in the final native batch. These overlap its assertions and are not 31 additional independent tests. Source-factor comparisons establish translation and routing for the tested diagrams; most compare two representations of the same source rule. The independent cubic check does not establish general physical-amplitude, gauge/Ward or loop correctness.

## Bounded execution and retained failures

| Attempt | Result | Kernel wall time | Highest retained cgroup memory peak |
| --- | --- | --- | --- |
| Build | 24 passed | 6.507 s | 354,119,680 bytes |
| First native check | 20 passed, channel-routing assertion failed | 5.177 s | 269,303,808 bytes |
| First correction | 25 passed, noncanonical field-point lookup assertion failed | 5.325 s | 269,352,960 bytes |
| Separately authorised final check | 29 passed, no warnings | 5.663 s | 269,291,520 bytes |

Each launch used an enforced aggregate 1 GiB cap, zero swap, 120 seconds plus 10 seconds termination grace and at least 2 GiB fresh available host memory. No memory-limit/OOM events occurred and no owned processes survived. User kernels were neither evaluated nor controlled. These are single observations; cgroup memory includes the owned tree and charged cache and is not kernel RSS or a performance benchmark.

The first test expected a channel route literally instead of modulo overall momentum conservation; the corrected classifier leaves signed source-vertex comparisons unchanged. The second queried `CheckFieldPoint` without FeynArts' canonical `VSort`; the actual negative test already removed the right contact. Canonicalising the query made the final check pass. Neither correction altered model bytes. All failed attempts remain retained; no failed attempt is relabelled successful.

Scripts-as-run, JSON receipts, raw logs, residuals, hashes and resource observations are retained in the maintainer evidence archive under the dated GRScalar build, route-correction and final-check records. The private verification note indexes their exact locations. Reuse requires matching relevant source, dependency and model identities.

## Exclusions

No complete exchange-product expansion, on-shell/helicity/Ward validation, loop calculation, Full benchmark, higher-order graviton model, scalar self-potential, other theory or interaction-library regeneration was performed. The optional bridge remains pure GR; GRScalar uses native FeynArts. The PureGR loop gate is unchanged. The supplied model is a finite tree implementation, not a claim that arbitrary multiplicities or loops are complete.

## Subsequent directory layout

The later [model-folder standardisation](ModelLayout.md) moved PureGR into its named subdirectory and updated only FeynGrav's path registration. All model and manifest bytes remain identical to this report's tested snapshot. Its source hashes remain historical identities; see the layout record for the changed loader and focused loading checks.
