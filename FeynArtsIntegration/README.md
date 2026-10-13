# Optional pure-GR FeynArts bridge

This companion is **explicitly loaded**; ordinary `FeynGrav.wl` loading neither starts FeynArts/FORM nor creates a model. The finite model snapshots the existing conventional-metric FeynGrav rules at `GaugeFixingEpsilon = 2`, symbolic `D` and κ: one symmetric `T[1]` graviton, oriented non-self-conjugate `U[1]` ghost/antighost, exact source propagators, cubic/quartic graviton vertices and the oriented cubic ghost rule. It imports the existing graviton libraries through `importGravitons[2]`; no shipped rule, formula or dependency is modified. The newly created generic/classes files and SHA-256 manifest live in a **new** caller-selected directory. An existing destination is refused; a failed validation leaves its staging directory for inspection.

## Start with the standard FeynArts model

For the simplest route, use ordinary `InitializeModel`, `CreateTopologies`, `InsertFields`, `CreateFeynAmp` and `FCFAConvert` on the `.gen`/`.mod` files in [`FeynArts/Models`](../FeynArts/Models/README.md), **without loading this companion**. The 59 direct source-factor assertions behind that route cover the tree vertices and both propagators, the ghosts and the complete first 1PI source products. `MomentumConservation -> True` enforces local vertex routing; it does not eliminate an external momentum or impose mass shells. The convenience interface below is separate from that model-level proof.

## Small explicit tree workflow

Use a fresh kernel with the installed **FeynCalc-patched FeynArts 3.12** and existing converter (no installation or automatic dependency changes). Adjust `root` to your checkout and choose an absolute new temporary directory:

```wl
$LoadFeynArts = True;
root = "/home/latosh/.Wolfram/Applications/FeynGrav";
Get[FileNameJoin[{root, "FeynGrav.wl"}]];
Get[FileNameJoin[{root, "FeynArtsIntegration", "FeynArtsIntegration.wl"}]];
model = FeynGrav`FeynArtsIntegration`FeynGravBuildModel[
  FileNameJoin[{$TemporaryDirectory, "fgfa-puregr-" <> CreateUUID[]}]];
process = <|"Model" -> "PureGR", "Incoming" -> {FeynArts`T[1], FeynArts`T[1]},
  "Outgoing" -> {FeynArts`T[1], FeynArts`T[1]}, "LoopOrder" -> 0|>;
tree = FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[
  model, process, {p1, p2}, {q1, q2}];
(* Optional drawing, no algebra or FORM: FeynArts`Paint[tree["Insertions"]] *)
(* Explicit, bounded algebra on each complete diagram: *)
FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[
  tree, FeynCalc`Dimension -> D, System`TimeConstraint -> 20,
  CalcFormConverter`FORMThreads -> 1]
```

The last step requires the **existing** `CalcFormConverter` package to be loaded (load `CalcFormConverter/CalcFormConverter.wl` explicitly if `DownValues[CalcFormConverter`CalcFormCalculate] === {}`). A three-point example uses one incoming and two outgoing `T[1]` fields with matching distinct momentum lists. Consult `?FeynGrav`FeynArtsIntegration`FeynGravValidateProcess` for the exact whitelist. Pass only explicitly supported, fully qualified converter options. The constructor records per-diagram SHA-256 integrity values; a changed expression, graph ID or denominator count is rejected before **real** FORM algebra (an accidental-mutation guard, not a secret authentication scheme). If one diagram fails, `"Status"` is `"Incomplete"`, its failure and ID remain in `"Diagrams"`, and `"Total"` is `Missing` rather than a partial answer. FORM files are retained when `CalcFormConverter`KeepFiles -> True` is explicitly requested. A custom `CalculationFunction` is a **test seam only**, never physical evidence.

The result contains all individual `"Diagrams"` (open-index FeynCalc expressions and FeynArts graph IDs), `"RawAmplitude"` and `"Insertions"` for FeynArts drawing. External tensor markers are truncated, external propagators are **not** added, and the FeynArts graph/statistics factor is applied **once**; no hidden spin sum, external on-shell condition, momentum integration measure, counterterm or master-integral reduction is inserted. The four-point tree has exactly one contact and all three single-propagator exchange diagrams. These are off-shell integrand expressions, not squared amplitudes or cross sections.

## First validated 1PI two-point **integrand**, without loop algebra

With `model` from the tree example above, request one incoming/outgoing graviton with the **same off-shell external momentum**, and declare one distinct loop momentum explicitly:

```wl
selfEnergy = <|"Model" -> "PureGR", "Incoming" -> {FeynArts`T[1]},
  "Outgoing" -> {FeynArts`T[1]}, "LoopOrder" -> 1,
  "OneParticleIrreducible" -> True|>;
loop = FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[
  model, selfEnergy, {p}, {p}, {ell}];
Lookup[loop["Diagrams"], {"TopologyFamily", "LoopRouting"}]
(* Optional drawing: FeynArts`Paint[loop["Insertions"]] *)
loop["TotalIntegrand"]
```

This path accepts **only** the independently checked model/source fingerprint (new model bytes require fresh ghost/tadpole/bubble verification). It retains precisely the graviton quartic tadpole, two-cubic graviton bubble and one oriented ghost bubble, with denominator inventories `{1,2,2}` and loop routes `ell` and `ell-p`. The complete unintegrated total alpha-renames **only each diagram's internal dummy Lorentz indices**; its four open external indices remain shared. All graph symmetry factors and the one closed-ghost-loop minus are included once by FeynArts. There is **no** integration measure, scaleless-tadpole deletion, counterterm, on-shell assignment, external propagator or integral evaluation. For bounded local runs, use an operating-system aggregate process-tree memory cap; a per-process virtual-address `ulimit` was insufficient here. `FeynGravCalculateAmplitude[loop]` deliberately returns `LoopAlgebraUnavailable`: the present branch validates construction of the physical 1PI **integrand**, not heavy FORM loop algebra or the separate loop-reduction backend.

## Validation boundary

Fresh bounded direct-kernel checks show exact generated-vs-source graviton and ghost propagators, an exact cubic tree and quartic contact, and **exact independent source-factor checks** of both cubic vertices and the graviton propagator for each s/t/u exchange. The propagator comparison uses typed `MomentumExpand` to reconcile `Momentum[p+q,D]` with the source's sum of `Momentum` objects; no on-shell condition or expansion of a large vertex product is used. A small real one-diagram `CalcFormCalculate` call returned successfully. Source cubic ghost interactions match exactly under both whole-leg permutations, with FeynArts' minus sign for the reversed external Grassmann order. Reinitialising an already-used identical FeynArts model forcibly in the same kernel produced a stale-state failure in a diagnostic; this wrapper uses its existing initialised identity and validates file hashes before every call. Start a fresh kernel when changing model files.

The public physical requests are **three-/four-graviton trees and the pinned one-loop 1PI graviton two-point integrand**. Under an independently verified 1-GiB aggregate process cap, exact source-factor checks pass for the quartic tadpole (including its graph half and the reversed internal whole-leg momenta), both cubic vertices and both graph-half propagators of the graviton bubble, and both oriented cubic vertices, two propagator metrics/routings and closed-loop minus of the ghost bubble. `MomentumExpand` **and** FeynCalc `ExpandScalarProduct` are needed to reconcile equivalent composite momentum spellings in the ghost source; no compensating ghost sign or rule change was made. A separate standard-library numerical implementation of the published ghost vertex agrees at 256/256 components for each cubic and three nonzero contracted bubble samples; that test is numerical, not a substitute for the exact symbolic checks. The published fixed-gauge FP operator has no two-graviton ghost contact *in this gauge*; do not infer this for other gauges. A deliberately changed model snapshot is rejected before topology generation. The later FORM loop-reduction branch is not part of this checkout.

External ghosts are for diagnostics, not supported asymptotic processes. Matter, other gravity models, five- or higher-graviton vertices, counterterms, higher-multiplicity trees, one-loop four-point, multiloops, automatic higher-order model extension, loop integration, implicit measures, on-shell conditions, polarisation sums and library regeneration are outside scope. Requests beyond the allowlist fail **before** FeynArts topology/model generation. No library or dependency should be edited to overcome a failed gate.
