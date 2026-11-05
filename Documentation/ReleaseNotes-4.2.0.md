# FeynGrav 4.2.0 — draft release notes

**Status: unreleased development version.** No release date or `v4.2.0` tag
is available yet. The [4.1.0 release notes](./ReleaseNotes-4.1.0.md) describe the
published baseline.

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
- `FeynGravSimplify` now defaults to `PaVeToABCD -> True`, converting supported
  final coefficients to named integral functions. `False` preserves the existing
  output. Raw converter/import/export behaviour is unchanged; unsupported
  coefficients remain. See the [Scientific scope](./ScientificScope.md#Calculation%20and%20performance).
- `FeynGravFORMExport`, `FeynGravFORMImport`, `FeynGravFORMCheck` and
  `FeynGravFORMInstall` wrap the existing converter commands with independent
  option stores. `CalcFormCalculate` and the other `CalcForm*` names remain
  available; existing code need not be renamed to use this version.
- Loading recovery, option preservation, abort/failure reporting and the
  separation of mapping, export, import and runtime responsibilities have
  received focused maintenance and verification. The installer recognises the
  companion modules and retains its staged verification and backup policy.

See the [Calculation interface](./Services/Calculation/FeynGravSimplify.md),
[function reference](./Reference.md#Simplified%20calculation%20front%20end%20%28development%2C%20unreleased%29)
and [converter guide](./Services/Calculation/Guide.md).

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

Eligible coefficients are simplified before and after reduction. Symbolic
comparisons cover selected three-box, QED and scalar/gravitation examples;
see [Scientific scope](./ScientificScope.md#Calculation%20and%20performance).
These cases do not guarantee a speedup or completion for every larger integral.

Symbolic `D`, incoming-momentum conventions and the documented FeynCalc
`I Pi^2` conversion normalisation are retained. `DropScaleless -> False` preserves
formal scaleless terms. Singular or unsupported families may remain unreduced
with a partial-reduction notice. This is not master-integral evaluation,
a Laurent expansion, renormalisation or general multiloop integration.

<a name="Horndeski scientific corrections"></a>

## Horndeski scientific corrections

The documented (+---), X-without-a-half action requires -2 G4,X H2 and
+G5,X H3/3. Independent G2/G3/G5 checks repair labelled-leg multiplicities and
temporary-index collisions. G5 also receives corrected cubic-Hessian
normalisation, symmetric connection placements and its third connection index.
In the G5 family, `HorndeskiG5_0_1_1`, `_0_1_2`, `_1_1_1` and `_1_1_2` change; G2/G3 libraries
and the six G5 b = 0 libraries remain unchanged. Public signatures are unchanged.

Restart the kernel and recompute results using the corrected b = 1 G5 vertices;
no single global factor converts old amplitudes. Direct calculations with
coincident momenta or colliding temporary indices also need recomputation.
The John model's b = 0 vertices and all model/notebook files are unchanged.
[Scientific scope](./ScientificScope.md#Horndeski)
states the finite coverage. Both G4 source variants now use one corrected analytic construction, including
the -2b Hessian coefficient, Fourier phase, b=0 scalar factorial, labelled
permutations and fresh internal indices. The contracted multi-graviton b>=2
branch is implemented within its documented dependency bounds.

**G4 library migration is complete.** All 14 shipped files have been regenerated
and independently checked; ordinary four-argument G4 calls now use the corrected
rules. Eight files receive the relative Hessian correction. Six b=0 files gain
the missing a! factors (2, 6 and 24). Public signatures and monomial coupling
normalisations are unchanged. See [Scientific scope](./ScientificScope.md#Horndeski) for the checked cases, and `FeynGravLibraryInformation` for the file identities in your installation.

Restart the kernel and recompute results using old G4 libraries. An isolated
b=0 vertex changes by the stated factorial; b>0 changes relative action blocks
and cannot be repaired by a global factor. Source/library updates do not modify
saved notebook outputs. No general Horndeski, arbitrary-order runtime or
loop-amplitude certification is implied.

The direct G5 source now supports cubic-Hessian contributions beyond three
gravitons, with the existing inverse-metric bounds: b<=4 at n=1, b<=3 at n=2,
b<=2 at n>=3. [Scientific scope](./ScientificScope.md#Horndeski)
includes the full four-graviton a=0,b=1 action comparison at symbolic D and generic
off-shell momenta, selected higher connection sectors and lower-order regressions.
No additional stored library or native FeynArts model order is introduced.

## Five-graviton source verification

The unchanged stored κ³ Einstein–Hilbert vertex now has an independent complete
off-shell comparison with symbolic D, pair/leg symmetries and trace controls.
The independent comparison establishes the complete local tensor for the checked action. It adds no native FeynArts model order and does not
certify a complete five-point amplitude or resolve its separate performance issue.

## Examples and scientific scope

The numbered examples use `FeynGravSimplify`. Example 07 presents the
tadpole-dependent two-point Ward identities. Example 08 uses native FeynArts
scalar-model diagrams with potential couplings set to zero, the corrected
identical-graviton bubble weight, and an explanation of the omitted scaleless
higher-order contact.

Input repairs in examples 07, 09 and 11 correct B0 collection patterns,
the gravity-only contribution and relation checks that previously assigned
definitions to `Contract`. Saved outputs were preserved and must not be treated
as newly recomputed results.

Restart the kernel after updating. In particular, restart if the old assignment
cell in 11 was evaluated, and recompute affected presentation/check cells. Example
08's downstream reduction, UV poles and infrared analysis still require
recomputation with its corrected bubble weight. See the
[examples guide](./Examples/README.md#Syntax%20and%20interface%20audit).

## Native quadratic gravity

The `QuadraticGravity` FeynArts model provides the full three-pole tensor propagator, cubic and quartic graviton vertices and conventional vector ghosts. The generated local rules agree with the action-based FeynGrav comparisons at symbolic D and generic off-shell momenta. The complete massless four-graviton tree passes selected four-dimensional Ward and Einstein-limit checks. See the [native model guide](./Models/QuadraticGravity/NativeModel.md) for the mass conventions and checked scope.

## Native GR + SU(N) Yang–Mills

The new `GR-SU-N-Yang-Mills` bundle contains all 21 interaction families through
κ² per vertex for an SU(N) gauge field and one Dirac quark flavour coupled to GR, including covariant
YM gauge fixing, two ghost sectors and five/six-leg contacts. D and N remain
symbolic, with unassigned `YangMillsCoupling` and `QuarkMass`. The five new
quark families include the five-leg hh qbar q A contact. `F[1]` now denotes
the quark and `S[1]` the gravitational vector ghost; update old explicit ghost
selections and restart the kernel. The bundle name is unchanged.

The audit found a missing κ^n factor in both contracted and uncontracted cubic/
quartic gluon source routines. These four routines are corrected, and the four
order-one/two gluon libraries were regenerated and independently checked. This
changes affected mixed gluon rules; restart the kernel and recompute previous
results that used them. Existing GR and scalar model bundles are unchanged.
The four corrected stored libraries are tracked repository files. The new
model is self-contained and does not read them at runtime.

Independent off-shell action comparisons, all native contacts and selected
AA → AA, AA → Ah and AA → hh Ward checks passed; see the [Scientific scope](./ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions). Native loop validation remains deferred.

## Quark–gluon rule correction

The same missing κⁿ factor was confirmed and corrected in both quark–gluon
source variants. The two stored libraries with one/two gravitons were regenerated
and checked exactly against an independent determinant–inverse-vierbein
expansion with symbolic D. Their colour, gamma and tensor coefficients are
unchanged; the flat vertex is unchanged. Restart the kernel, reload with
`FeynGravImportSUNYM[2]`, and recompute affected results. At that correction checkpoint the native pure-YM
model had no quarks and was unchanged. The subsequent quark extension now
uses these corrected libraries; no further source/library change was needed. See the
[Scientific scope](./ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions).

## Verification and performance

Read [Scientific scope](./ScientificScope.md) for the independently checked
interaction orders and selected calculation examples. Native loop validation,
higher-order EsGB and the axion convention remain outside that scope. Larger
QED full-tadpole and 1,490-term symbolic comparisons remain unverified.
Supported reduction does not promise maximal compactness or fixed runtimes.
Use the [benchmarks](./Benchmarks/README.md) to measure your own workload.

Compatibility across all older supported dependency versions and operating
systems has not been established. Real Contractor execution is not established
by checks using a process substitute; follow its separate method restrictions.

## Installation and publication

Installation still defaults to GitHub `main`; it does not automatically select
the newest release or this local development branch. A future `v4.2.0` tag can
be used only after publication. Keep the current published-version example for
`v4.1.0` and use a full local checkout when testing this development version.
See the [installation guide](./UserGuide.md#Installation%20and%20help).

Restart the kernel after upgrading and recompute affected expressions.
Previously saved notebook results do not change when package files are updated.

## Native quark extension

Independent Dirac-action comparisons establish the kinetic, mass and spin terms
through κ². The native adapter reverses every source momentum to match FeynArts
spinor/Fourier conventions, including graviton momenta. All five new contacts
and selected massive quark trees pass; See [Scientific scope](./ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions) for the action and selected tree checks.
Example 13 (`Examples/13. Quark_Graviton_Emission_Tree_Level_FeynArts.nb`)
computes quark annihilation with graviton emission using `FeynGravSimplify`.
Existing notebooks and the four other model bundles are preserved.
