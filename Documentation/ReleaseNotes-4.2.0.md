# FeynGrav 4.2.0 — draft release notes

**Status: unreleased development version.** Version 4.2.0 was selected on
25 October 2026 for the `FORM_Simplification` branch. No release date is assigned;
`v4.2.0` is the intended future tag, not a tag created by this preparation.
The [published 4.1.0 record](ReleaseNotes-4.1.0.md) remains unchanged.

## Version and compatibility

This is a minor-version update: it adds public calculation commands and
capabilities while retaining the existing `CalcForm*` entry points. A 4.1.1
patch would describe compatible corrections alone. No intermediate 4.1.1 release
is required. The older unpublished Contractor interface has its own migration
notes; its changes are not changes to the published 4.1.0 interface.

The package startup label identifies this checkout as `FeynGrav 4.2.0
(development)`. That label does not imply publication or a new complete
release-validation run.

## New calculation interface

- `FeynGravSimplify[expr, opts]` selects the full FORM pipeline by default.
  `Method -> Automatic` and `Method -> "FORM"` use the converter;
  `"Contractor"` and `"FORMBenchmark"` select explicit restricted methods.
  Unsupported settings fail rather than silently selecting another backend.
- `FeynGravFORMExport`, `FeynGravFORMImport`, `FeynGravFORMCheck` and
  `FeynGravFORMInstall` wrap the existing converter commands with independent
  option stores. `CalcFormCalculate` and the other `CalcForm*` names remain
  available; existing code need not be renamed to use this version.
- Loading recovery, option preservation, abort/failure reporting and the
  separation of mapping, export, import and runtime responsibilities have
  received focused maintenance and verification. The installer recognises the
  companion modules and retains its staged verification and backup policy.

See the [detailed interface contract](ReleaseNotes-API01-Development.md),
[function reference](Reference.md#simplified-calculation-front-end-development-unreleased)
and [converter guide](../CalcFormConverter/UserGuide.md).

## One-loop reduction and coefficient handling

Declaring one loop momentum enables tensor-integral decomposition into
coefficient functions. The default `ReduceToScalarMasters -> True` reduces
supported families through boxes to scalar PaVe masters, with repeated-power
reduction where needed. `TensorDecomposition -> False` retains integrand algebra;
`ReduceToScalarMasters -> False` stops at coefficient functions. Previously
computed coefficients can be reduced without declaring a new loop momentum.

Export planning avoids redundant registration and unused denominator families.
Shared, factored output and incremental import reduce temporary representation
cost. Eligible final master coefficients receive exact rational cancellation in
FORM. Polynomial polarisation structures can be grouped and simplified within
private work limits, with complete-original restoration when the allowance is
exhausted. This preserves a valid result without claiming maximal simplification.
No transversality, on-shell condition, helicity relation or Wolfram `$Assumptions`
is introduced by that grouping.

Symbolic `D`, incoming-momentum conventions and the documented FeynCalc
`I Pi^2` conversion normalisation are retained. `DropScaleless -> False` preserves
formal scaleless terms. Singular or unsupported families may remain unreduced
with a partial-reduction notice. This is not master-integral evaluation,
a Laurent expansion, renormalisation or general multiloop integration.

## Examples and scientific scope

The numbered examples use `FeynGravSimplify`. Example 07 presents the
tadpole-dependent two-point Ward identities. Example 08 uses native FeynArts
scalar-model diagrams with potential couplings set to zero, the corrected
identical-graviton bubble weight, and an explanation of the omitted scaleless
higher-order contact.

The [all-example syntax/interface audit](Verification/ExamplesSyntax-2026-10-25.md)
covers 11 notebooks, 274 executable cells and 133 option calls. Six input cells
were repaired: four `B0` collection patterns in 07, the gravity-only contribution
in 09 and the relation checks in 11 that previously assigned definitions to
`Contract`. Ten focused assertions passed. All 123 saved Output cells were
preserved; they are historical results, not newly recomputed results.

Restart the kernel after updating. In particular, restart if the old assignment
cell in 11 was evaluated, and recompute affected presentation/check cells. Example
08's downstream reduction, UV poles and infrared analysis still require
recomputation with its corrected bubble weight. See the
[examples guide](../Examples/README.md#syntax-and-interface-audit).

## Native GR + SU(N) Yang–Mills

The new `GR-SU-N-Yang-Mills` bundle contains all 16 interaction families through
κ² per vertex for a pure SU(N) gauge field coupled to GR, including covariant
YM gauge fixing, two ghost sectors and five/six-leg contacts. D and N remain
symbolic, with unassigned `YangMillsCoupling`. No quarks are included.

The audit found a missing κ^n factor in both contracted and uncontracted cubic/
quartic gluon source routines. These four routines are corrected, and the four
order-one/two gluon libraries were regenerated and independently checked. This
changes affected mixed gluon rules; restart the kernel and recompute previous
results that used them. Existing GR and scalar model bundles are unchanged.
The four corrected stored libraries are tracked repository files. The new
model is self-contained and does not read them at runtime.

Independent off-shell action comparisons, all native contacts and selected
AA → AA, AA → Ah and AA → hh Ward checks passed; see the [verification
report](Verification/GRSUNYM-2026-10-26.md). Native loop validation remains deferred.

## Quark–gluon rule correction

The same missing κⁿ factor was confirmed and corrected in both quark–gluon
source variants. The two stored libraries with one/two gravitons were regenerated
and checked exactly against an independent determinant–inverse-vierbein
expansion with symbolic D. Their colour, gamma and tensor coefficients are
unchanged; the flat vertex is unchanged. Restart the kernel, reload with
`FeynGravImportSUNYM[2]`, and recompute affected results. The native pure-YM
model contains no quarks and is unchanged. See the
[verification report](Verification/QuarkGluonKappa-2026-10-26.md).

## Verification and performance

[PERF-04](Verification/PERF04-2026-10-25.md) records the retained three-box
workload: one warmup followed by complete measured calls of **25.332, 28.473 and
28.621 seconds**, each below its 60-second target. All 17 masters agree exactly
with the retained reference, and an altered-result negative control is rejected.
Each output is **4,355,957 bytes**. These are workload- and host-specific
observations, not general runtime guarantees.

Peak memory during those calls was 337.89–338.80 MB; the complete evidence scopes
peaked at 462.66–464.86 MB after result retention and inspection. The difference
and the comparison with the older approximately 392 MB reference were investigated
and are documented. The final small QED fixtures retain both exact cancellations
and the verified sample reduction. The linked report records focused regressions,
source identities, expected diagnostics and historical failed attempts.

Retained checks used fresh task-owned processes with a whole-tree 1 GiB cap,
zero swap, 120 seconds plus 10 seconds termination grace and at least 2 GiB
available host memory. This version-label update reruns no calculations.
The latest notebook audit used Wolfram 15.0.1/FeynCalc 10.2.1, and PERF-04 used
FORM 5.0.2. Full compatibility across all older advertised dependency versions
and operating systems has not been established by these checks.

Remaining limits include the complete selected QED tadpole, its 232-group
reconstruction target and the 1,490-term group's full symbolic comparison.
There is no new Full-suite, arbitrary-amplitude or loop-physics certification.
The notebook audit's front-end previews have a documented shutdown qualification;
its core parsing/interface/focused checks have clean lifecycle records.
Real Contractor execution is not newly certified by the facade's process-double
tests. Earlier dated verification reports retain their own scope and diagnostics.

## Installation and publication

Installation still defaults to GitHub `main`; it does not automatically select
the newest release or this local development branch. A future `v4.2.0` tag can
be used only after publication. Keep the current published-version example for
`v4.1.0` and use a full local checkout when testing this development version.
See the [installation guide](UserGuide.md#installation-and-help).

The version selection and draft notes do not merge branches, commit, push, tag,
publish or establish that the eventual release archive has passed installation
checks. Those operations and the final release review remain separate.
