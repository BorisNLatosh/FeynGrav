# FeynGrav documentation

**Current target: 4.2.0, unreleased.** See the [draft release notes](ReleaseNotes-4.2.0.md)
for the unified interface, calculation improvements and verification limits.

- [4.1.0 release notes](ReleaseNotes-4.1.0.md): the published baseline, not the additional loop-reduction work on this branch.
- [4.1.0 release preparation](Verification/ReleasePreparation-4.1.0.md): historical packaging checks and publication boundaries.

This development branch combines the published 4.1.0 changes with the one-loop tensor decomposition, repeated-power and scalar-master reduction work. It is not the published 4.1.0 snapshot. Earlier verification retains its recorded dates and source identities; merging does not imply that every check has been rerun. Consult `?FunctionName` and `Options[FunctionName]` in a fresh kernel after changing package files.


Latest scoped FORM acceptance: [PERF-05 strategy, dependency review and retained checks](Verification/PERF05Final-2026-10-30.md).
The [PERF-04 measurements](Verification/PERF04-2026-10-25.md) remain the historical baseline.

## Choose a route

| Reader | Start here | Continue with |
| --- | --- | --- |
| New user | [Installation and requirements](UserGuide.md#requirements) | [Function reference](Reference.md), then [example notebooks](../Examples/README.md) |
| FORM user | [Converter guide](../CalcFormConverter/UserGuide.md) | Supported vocabulary, conventions and troubleshooting in that guide; [benchmarks](../Benchmark/UserGuide.md) for local timings |
| FeynArts user | [Supplied model guides](../FeynArts/Models/ModelCatalogue.md) | Ready-to-use GR trees and [Single-Scalar-Tensor-Minimal-Coupling mixed trees](../FeynArts/Models/Single-Scalar-Tensor-Minimal-Coupling/ModelGuide.md); [optional pure-GR bridge](../FeynArtsIntegration/BridgeGuide.md). Loop compatibility is deferred; the corrected GR snapshot is rejected by the unchanged bridge loop gate. |
| Library developer | [Interaction-rule contracts](../Rules/Contracts.md) | [Library generation](../Libs/Generator.md); use a separate fresh kernel from ordinary FeynGrav calculations |
| Maintainer | [Converter architecture and tests](../CalcFormConverter/DEVELOPER.md) | [Saved formats](../CalcFormConverter/FORMAT.md), verification records below |

The reference covers main-package commands, public gauge parameters and the automatically loaded Nieuwenhuizen helpers. Generator-side routines have separate signatures: do not substitute them for library-backed main-package functions simply because their short names match.

## Verification and history

- [Example syntax and interface audit](Verification/ExamplesSyntax-2026-10-25.md): all 11 notebooks, six narrow input repairs, preserved outputs and explicit execution limits.

- [Main/FORM merge checks](Verification/FORMMainMerge-2026-10-20.md): focused checks on the combined development branch, with explicit limits.

- [Uniform model folders](Verification/ModelLayout.md): common layout, automatic registration and fresh-kernel loading checks.
- [GR tree release](Verification/PureGRTreeRelease.md): accepted ghost convention and native three-/four-graviton source checks; loop compatibility deferred.
- [Historical minimal-scalar tree release](Verification/GRScalarTreeRelease.md): scalar mass, contact and exchange checks, model identities and bounded execution; physical amplitude and loop limits.

- [Conventions report checks](Verification/Conventions.md): current settings, held import records, static rendering and representative curvature/Fourier checks.
- [Documentation checks](Verification/DocumentationVerification.md): coverage, links, representative commands and preservation checks for this update.
- [Generator migration](Verification/GeneratorVerification.md): family comparisons and failure-handling verification, with its own scope and date.
- [Earlier integrand optimisation and independent review](../CalcFormConverter/Tests/Reports/EarlyDimensionCompaction.md#follow-up-complete-calculation-optimisation): complete-call observations, power qualifications, retained changes and 1,964 regression checks after review corrections.
- [One-loop tensor decomposition](../CalcFormConverter/Tests/Reports/TensorDecomposition.md): coefficient conventions, routing and bounded rank checks before later reduction stages.
- [Repeated-power coefficient reduction](../CalcFormConverter/Tests/Reports/LoopCoefficientReduction.md): fixed-dimension reduction and subsequent collection/import work.
- [Scalar-master reduction](../CalcFormConverter/Tests/Reports/ScalarMasterReduction.md) and [raw performance data](../CalcFormConverter/Tests/Reports/ScalarMasterPerformance.json): current development-stage checks, exceptional-case retention and incomplete realistic-workload trials. These timings do not establish completion of the large scalar-master calculation.
- [Converter performance history](Verification/ConverterPerformance.md): earlier measurements, baselines and qualifications; not current machine-independent guarantees.
- [Epsilon](../CalcFormConverter/Tests/Reports/Epsilon.md), [Dirac translation](../CalcFormConverter/Tests/Reports/DiracColour.md), [Dirac processing](../CalcFormConverter/Tests/Reports/DiracAlgebra.md) and [colour processing](../CalcFormConverter/Tests/Reports/ColourAlgebra.md): implementation records with their original verification boundaries.

User guides describe supported behaviour. Verification reports record what was tested at a particular stage; an earlier report is not evidence that every later revision was rerun through the same checks. Documentation is ordinary Markdown: no site build, network service or native Mathematica F1 installation is required.

For library-generation timings, use [benchmark 06](../Benchmark/06_Library_Generation.nb) and the [benchmark guide](../Benchmark/UserGuide.md). Start in a fresh kernel, separate from the main-package workflow.

## Native scalar extensions — 18 October 2026

`Single-Scalar-Tensor-Minimal-Coupling`, `John` and `Einstein-scalar-Gauss-Bonnet` are supplied alongside `GR`, with matching generic/classes files in individual model folders. Use a fresh kernel and the same name for `Model` and `GenericModel`. The optional pure-GR bridge remains restricted to its existing processes. See [model index](../FeynArts/Models/ModelCatalogue.md) for actions, native examples and verification limits. The two-graviton EsGB source and library have been corrected against an independent curvature derivation; higher-order EsGB sources require a separate audit. Vertex order is limited to kappa squared; potential contacts still require up to six legs. Loops are not validated.

- [Model naming and scalar merge](Verification/ModelNames.md): four exact model names, unchanged algebra and 32 fresh-kernel checks.

- [Minimal scalar normalisation and rebuilt-model Ward checks](Verification/ScalarWardCorrection.md)
