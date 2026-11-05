# FeynGrav 4.2.0

**Version 4.2.0.** This checkout adds the unified
`FeynGravSimplify` interface, one-loop tensor decomposition and scalar-master
reduction to the published 4.1.0 baseline.

Wolfram Language Feynman rules for gravity and matter, using FeynCalc notation,
with FORM calculation support and six native FeynArts models.

See the [4.2.0 release notes](Documentation/ReleaseNotes-4.2.0.md) for changes,
compatibility and verification limits. The [4.1.0 release notes](Documentation/ReleaseNotes-4.1.0.md)
remain the record of the published 20 October 2026 release.

## Start here

- [Getting started with FeynGrav](Documentation/GettingStarted.md)
- [Common conventions](Documentation/Conventions.md)
- [Installation, requirements and troubleshooting](Documentation/UserGuide.md#installation-and-help)
- [Documentation index](Documentation/README.md) and [function reference](Documentation/Reference.md)
- [Numbered examples](Documentation/Examples/README.md)
- [FeynArts models](Documentation/Models/FeynArts.md)
- [FORM converter](Documentation/Services/Calculation/Guide.md)
- [Library generation](Documentation/Services/LibraryGeneration/Guide.md) and [rule contracts](Documentation/Services/RuleConstruction/Contracts.md)
- [Benchmarks](Documentation/Benchmarks/README.md)

Requires FeynCalc 10.2.1 or newer and Wolfram Language 12.2 or newer, subject to
FeynCalc's requirements. FORM is optional for ordinary package loading. After
installation, restart the kernel and load with `` << FeynGrav` ``.

The native models cover GR, a massive scalar with cubic/quartic potential,
John, Einstein–scalar–Gauss–Bonnet, quadratic gravity and GR + SU(N) Yang–Mills with one massive quark flavour through κ² per vertex. Selected tree
checks are documented; loop validation remains deferred.

Evaluate `FeynGravHowToCite[]` for citation guidance. See the
[citations and licence](Documentation/UserGuide.md#citations-and-licence),
[version history](Documentation/UserGuide.md#version-history) and [LICENSE](LICENSE).

<!-- Compatibility anchors for links to the former detailed README. -->
<a id="contents"></a><a id="requirements"></a><a id="installation-and-help"></a><a id="automatic-installation"></a><a id="manual-installation-and-help"></a><a id="interaction-libraries"></a><a id="feynarts-models"></a><a id="calcformconverter"></a><a id="examples"></a><a id="benchmarks"></a><a id="package-structure"></a><a id="troubleshooting-and-support"></a><a id="version-history"></a><a id="version-410--unreleased"></a><a id="version-40"></a><a id="version-30"></a><a id="version-20"></a><a id="version-10"></a><a id="citations-and-licence"></a><a id="native-scalar-extensions--18-october-2026"></a>
Detailed sections: [UserGuide](Documentation/UserGuide.md).
