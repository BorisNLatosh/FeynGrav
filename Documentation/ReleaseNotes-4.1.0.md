# FeynGrav 4.1.0 — release notes

**Status: unreleased; prepared for review.** Intended tag: `v4.1.0`.
No release date is assigned until publication.

## Versioning policy

Version 4.1.0 is the agreed transition to MAJOR.MINOR.PATCH numbering. It
includes the incompatible changes below. Subsequent incompatible public-interface
changes increment MAJOR, compatible additions MINOR, and compatible corrections
PATCH. Mathematical corrections must state their scientific impact even when
they preserve the calling interface. Published versions are immutable snapshots;
further changes receive another version. A public development branch is not a
formal release. Release candidates may use `4.1.0-rc.1` and subsequent suffixes.

## New functionality

- CalcFormConverter provides supported FeynCalc/FORM translation, execution and
  reconstruction, including Lorentz, epsilon, ordinary Dirac and fundamental
  SU(N) colour processing. Propagator grouping, guarded numerator cancellation,
  coefficient simplification and incremental import reduce intermediate work.
  See the [converter guide](../CalcFormConverter/UserGuide.md) for exact exclusions.
- Four standalone native FeynArts models: `GR`,
  `Single-Scalar-Tensor-Minimal-Coupling`, `John` and
  `Einstein-scalar-Gauss-Bonnet`. They retain vertices through κ² per vertex;
  products of vertices can contribute at higher total order. See the
  [model index](../FeynArts/Models/ModelCatalogue.md).
- D-dimensional polarisation helpers, a FORM-based library generator,
  reproducible benchmark tools and expanded numbered examples.
- `FeynGravConventions[]`, `FeynGravLibraryInformation[]` and
  `FeynGravHowToCite[]` provide conventions, import provenance and citation guidance.
- The installer checks dependencies, verifies a staged package in a fresh kernel
  and retains a backup when replacing an existing installation.

## Migration and incompatible changes

| Previous interface or layout | Replacement |
| --- | --- |
| `import…` commands | `FeynGravImport…`, retaining each suffix and positional arguments |
| `printOutput` option, including its private form | `Verbose -> False` by default; `True` enables diagnostics |
| `PureGR` | `GR` |
| `GRScalar` or `GRScalarPotential` | `Single-Scalar-Tensor-Minimal-Coupling` |
| `GRJohn` | `John` |
| `GREsGB` | `Einstein-scalar-Gauss-Bonnet` |
| `Libs/FeynGravLibrariesGenerator.wl` | `Libs/Generator.wl` |
| Unnumbered example filenames | Numbered files listed in [Examples/README.md](../Examples/README.md) |

Old importer/option and model names are not compatibility aliases. For the
unified scalar model, set both potential couplings to zero after conversion to
recover the minimal-only theory. Use the same quoted name for `Model` and
`GenericModel`. The former local `Calc` implementation is removed; use supported
FeynCalc operations or `CalcFormCalculate` as appropriate to the expression.
Restart the kernel and update notebook setup cells after upgrading. Previously
computed expressions do not change when new libraries are installed.

## Scientific corrections

- The ordinary gravitational ghost propagator is
  `-I MTD[mu,nu] FAD[p]`, with the existing cubic ghost interaction and native
  Grassmann signs. See the [GR verification record](Verification/PureGRTreeRelease.md).
- Restore missing graviton differentiation multiplicities in scalar kinetic/mass
  and potential sources and affected vector, Yang–Mills, G2 and axion-vector
  routines. Terms already constructed through explicit permutations are not
  multiplied again. Affected shipped libraries were regenerated. In particular,
  the former minimal two-scalar/two-graviton vertex was half its correct value,
  matching the discrepancy reported in
  [GitHub issue #2](https://github.com/BorisNLatosh/FeynGrav/issues/2).
- Correct the order-two Gauss–Bonnet source against an independent linearised
  curvature derivation. Rebuild the scalar models with the corrected sources.
- Axion regeneration also replaces legacy epsilon text with explicit
  D-dimensional momentum/index slots. This is a notation migration with a
  remaining action-convention limitation, not a claim of full axion validation.

These corrections can change amplitudes obtained with older versions. Rerun
relevant calculations rather than comparing only textual forms. See the
[multiplicity record](Verification/RuleMultiplicityCorrection.md),
[scalar-model record](Verification/ScalarModelExtensions.md) and
[scalar Ward verification](Verification/ScalarWardCorrection.md).

## Verification and known limitations

The selected native scalar–scalar to graviton–graviton trees pass both outgoing
Ward identities with symbolic D, mass and couplings. John cubic/quartic tensors
also have full independent off-shell action comparisons. This evidence does not
establish every amplitude or loop calculation. Loop validation is deferred.

**Higher-order EsGB is not certified:** the shipped `ScalarGaussBonnet_3` and
legacy higher-order source branch require a separate mathematical audit. The
native EsGB model uses the verified order-two source only. Do not rely on the
higher-order library for physical predictions without independent validation.
No automatic rejection of that legacy library has been introduced in this release.

**Axion convention remains unresolved:** factorial and explicit-slot comparisons
do not establish the epsilon-density/action convention. The broader multiplicity
checks are regressions, not independent derivations of every affected action.

High-valence FeynArts model loading and amplitude construction can be expensive.
Incremental FORM import reduces temporary storage but the final expression and
individual coefficients must still fit in memory. GR plus Yang–Mills and
quadratic-gravity native FeynArts bundles are not supplied in this release.

## Dependencies and release checks

FeynCalc 10.2.1 or newer and Wolfram Language 12.2 or newer are required, also
subject to FeynCalc's own requirements. Development checks used Wolfram 15.0.1
and FeynCalc 10.2.1; the full older-version range is not tested. FORM is optional
for package loading and required for FORM calculations and library generation.
Native diagram workflows require FeynCalc-patched FeynArts.

The [release-preparation record](Verification/ReleasePreparation-4.1.0.md)
distinguishes new packaging checks from retained mathematical evidence. No Full
benchmark or complete algebra-regression rerun is implied. Publication, tagging
and any final archive verification remain separate release actions.
