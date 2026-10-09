# FeynGrav documentation

This documentation describes the development source checked on 6 October 2026, with the conventions report added on 9 October 2026. Installed releases may differ; consult `?FunctionName` and `Options[FunctionName]` in your kernel. Begin a new kernel after changing package files.

## Choose a route

| Reader | Start here | Continue with |
| --- | --- | --- |
| New user | [Installation and requirements](../README.md#requirements) | [Function reference](Reference.md), then [example notebooks](../Examples/README.md) |
| FORM user | [Converter guide](../CalcFormConverter/README.md) | Supported vocabulary, conventions and troubleshooting in that guide; [benchmarks](../Benchmark/README.md) for local timings |
| Library developer | [Interaction-rule contracts](../Rules/README.md) | [Library generation](../Libs/Generator.md); use a separate fresh kernel from ordinary FeynGrav calculations |
| Maintainer | [Converter architecture and tests](../CalcFormConverter/DEVELOPER.md) | [Saved formats](../CalcFormConverter/FORMAT.md), verification records below |

The reference covers main-package commands, public gauge parameters and the automatically loaded Nieuwenhuizen helpers. Generator-side routines have separate signatures: do not substitute them for library-backed main-package functions simply because their short names match.

## Verification and history

- [Conventions report checks](Verification/Conventions.md): current settings, held import records, static rendering and representative curvature/Fourier checks.
- [Documentation checks](Verification/DocumentationVerification.md): coverage, links, representative commands and preservation checks for this update.
- [Generator migration](Verification/GeneratorVerification.md): family comparisons and failure-handling verification, with its own scope and date.
- [Converter performance history](Verification/ConverterPerformance.md): earlier measurements, baselines and qualifications; not current machine-independent guarantees.
- [Epsilon](../CalcFormConverter/Tests/Reports/Epsilon.md), [Dirac translation](../CalcFormConverter/Tests/Reports/DiracColour.md), [Dirac processing](../CalcFormConverter/Tests/Reports/DiracAlgebra.md) and [colour processing](../CalcFormConverter/Tests/Reports/ColourAlgebra.md): implementation records with their original verification boundaries.

User guides describe supported behaviour. Verification reports record what was tested at a particular stage; an earlier report is not evidence that every later revision was rerun through the same checks. Documentation is ordinary Markdown: no site build, network service or native Mathematica F1 installation is required.

For library-generation timings, use [benchmark 06](../Benchmark/06_Library_Generation.nb) and the [benchmark guide](../Benchmark/README.md). Start in a fresh kernel, separate from the main-package workflow.
