# FeynGrav documentation

**Version: 4.2.0.** Use the documentation supplied with your
checkout. The [4.1.0 release notes](./ReleaseNotes-4.1.0.md) describe the published
baseline; the [4.2.0 release notes](./ReleaseNotes-4.2.0.md) describe additions and corrections.

## Choose a route

- [Getting started](./GettingStarted.md): installation, loading and a first vertex.
- [Models and interactions](./Models.md): actions, commands and available libraries.
- [Service functions](./Services/README.md): calculation, imports and library generation.
- [Constants and parameters](./Parameters/README.md).
- [Alphabetical symbol index](./Symbols.md).
- [Common conventions](./Conventions.md): signs, momenta, dimensions and normalisation.

| What you want to do | Read next |
| --- | --- |
| Install or troubleshoot FeynGrav | [User guide](./UserGuide.md) |
| Construct an amplitude | [Example notebooks](./Examples/README.md) |
| Simplify an expression or reduce a one-loop integral | [Calculation commands](./Services/Calculation/README.md) and [FORM guide](./Services/Calculation/Guide.md) |
| Generate diagrams with FeynArts | [Native model catalogue](./Models/FeynArts.md) |
| Generate additional vertex libraries | [Library-generation commands](./Services/LibraryGeneration/README.md) and [workflow](./Services/LibraryGeneration/Guide.md) |
| Measure performance on your computer | [Benchmark notebooks](./Benchmarks/README.md) |

Main-package retrieval commands and rule constructors can have identical short
names but different signatures. Run library generation in a separate fresh kernel.

<a id="verification-and-history"></a>
## Scientific scope and corrections

Read [Scientific scope](./ScientificScope.md) for the checked interaction orders,
known limitations and the distinction between a vertex check and a complete
amplitude. The release notes explain changes that require recomputing old results.
An available or successfully imported rule is not automatically a verified prediction.

<a id="native-scalar-extensions--18-october-2026"></a>
## Native scalar models

The [minimal scalar](./Models/Scalar/README.md), [John](./Models/Horndeski/John/README.md)
and [Einstein–scalar–Gauss–Bonnet](./Models/ScalarGaussBonnet/README.md) bundles
provide selected scalar interactions through κ² per vertex. Use matching model
and generic-model names in a fresh kernel. Consult each model's action,
contact inventory and limits before combining diagrams. Native loop validation
remains outside the supplied tree-level checks.

## Reading locally

Open `Documentation/` as a vault in Obsidian, or open this page in a Markdown viewer. All user-guide pages are inside this folder. Mathematical rendering depends on the viewer; Obsidian reads the supplied Markdown and mathematical notation directly.

Notebook examples, benchmarks and source files remain in the complete FeynGrav package. See [Opening notebooks and package files](./PackageFiles.md) for their locations and how to open them. Mathematica is needed to execute notebooks, not to read this manual. These Markdown pages do not install native Mathematica F1 documentation.

## Documentation structure

Model overviews connect actions and conventions to individual command pages.
Service pages explain calculations, imports and optional generation workflows;
parameter pages document values and when settings take effect. The alphabetical
index lists the interfaces covered by this manual. Existing model and reference
paths remain available.
