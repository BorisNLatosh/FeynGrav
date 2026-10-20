# FeynGrav documentation

- [4.1.0 release notes](ReleaseNotes-4.1.0.md): migration, scientific corrections and remaining limitations (20 October 2026).
- [4.1.0 release preparation](Verification/ReleasePreparation-4.1.0.md): packaging checks and publication boundaries.

This documentation describes the development source checked on 6 October 2026, with the conventions report added on 9 October and converter optimisation documentation reviewed on 10 October 2026. The four native model guides use the names selected on 19 October 2026; earlier verification reports retain their historical names. Installed releases may differ; consult `?FunctionName` and `Options[FunctionName]` in your kernel. Begin a new kernel after changing package files.

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

- [Uniform model folders](Verification/ModelLayout.md): common layout, automatic registration and fresh-kernel loading checks.
- [GR tree release](Verification/PureGRTreeRelease.md): accepted ghost convention and native three-/four-graviton source checks; loop compatibility deferred.
- [Historical minimal-scalar tree release](Verification/GRScalarTreeRelease.md): scalar mass, contact and exchange checks, model identities and bounded execution; physical amplitude and loop limits.

- [Conventions report checks](Verification/Conventions.md): current settings, held import records, static rendering and representative curvature/Fourier checks.
- [Documentation checks](Verification/DocumentationVerification.md): coverage, links, representative commands and preservation checks for this update.
- [Generator migration](Verification/GeneratorVerification.md): family comparisons and failure-handling verification, with its own scope and date.
- [Current converter optimisation and independent review](../CalcFormConverter/Tests/Reports/EarlyDimensionCompaction.md#follow-up-complete-calculation-optimisation): complete-call observations, power qualifications, retained changes and 1,964 regression checks after review corrections.
- [Converter performance history](Verification/ConverterPerformance.md): earlier measurements, baselines and qualifications; not current machine-independent guarantees.
- [Epsilon](../CalcFormConverter/Tests/Reports/Epsilon.md), [Dirac translation](../CalcFormConverter/Tests/Reports/DiracColour.md), [Dirac processing](../CalcFormConverter/Tests/Reports/DiracAlgebra.md) and [colour processing](../CalcFormConverter/Tests/Reports/ColourAlgebra.md): implementation records with their original verification boundaries.

User guides describe supported behaviour. Verification reports record what was tested at a particular stage; an earlier report is not evidence that every later revision was rerun through the same checks. Documentation is ordinary Markdown: no site build, network service or native Mathematica F1 installation is required.

For library-generation timings, use [benchmark 06](../Benchmark/06_Library_Generation.nb) and the [benchmark guide](../Benchmark/UserGuide.md). Start in a fresh kernel, separate from the main-package workflow.

## Native scalar extensions — 18 October 2026

`Single-Scalar-Tensor-Minimal-Coupling`, `John` and `Einstein-scalar-Gauss-Bonnet` are supplied alongside `GR`, with matching generic/classes files in individual model folders. Use a fresh kernel and the same name for `Model` and `GenericModel`. The optional pure-GR bridge remains restricted to its existing processes. See [model index](../FeynArts/Models/ModelCatalogue.md) for actions, native examples and verification limits. The two-graviton EsGB source and library have been corrected against an independent curvature derivation; higher-order EsGB sources require a separate audit. Vertex order is limited to kappa squared; potential contacts still require up to six legs. Loops are not validated.

- [Model naming and scalar merge](Verification/ModelNames.md): four exact model names, unchanged algebra and 32 fresh-kernel checks.

- [Minimal scalar normalisation and rebuilt-model Ward checks](Verification/ScalarWardCorrection.md)
