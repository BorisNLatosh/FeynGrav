# Optional pure-GR FeynArts bridge

This companion is **explicitly loaded**; ordinary `FeynGrav.wl` loading neither starts FeynArts/FORM nor creates a model. The finite model snapshots the existing conventional-metric FeynGrav rules at `GaugeFixingEpsilon = 2`, symbolic `D` and κ: one symmetric `T[1]` graviton, oriented non-self-conjugate `U[1]` ghost/antighost, exact source propagators, graviton self-interactions and the oriented cubic ghost rule. **`FeynGravBuildModel` builds the cubic and quartic graviton vertices and the validated ghost sector by default** (`GravitonOrder -> 2`), which is inexpensive: a measured default build took 5.2 s, produced a 696,151-byte generic model, initialised in FeynArts with three vertices at a 0.32 GB peak, and its two model files were **byte-identical** to the shipped `FeynArts/Models` snapshot. **The five-graviton vertex requires the explicit `GravitonOrder -> 3`**, which imports through `importGravitons[3]`; its rule has 76,230 basis terms against 231 cubic and 4,158 quartic terms, the generic model grows to about 15.4 MB, and its FeynArts initialisation was measured once at 486 s with a 6.07 GB peak (single measurements on one machine under a user cgroup, not portable expectations, and `CreateFeynAmp` over a five-point tree did not finish inside 900 s). The requested order is validated before any import or rule construction, and an unrecognised option is reported rather than ignored. Because `importGravitons` caps at the highest installed order, a request for order 3 without `Libs/GravitonVertex_3` **fails with `LibraryOrderUnavailable`** instead of publishing a snapshot that silently omits the quintic. The manifest records the included `GravitonOrders` and `InteractionFamilies` for the requested order. No shipped rule, formula or dependency is modified. The newly created generic/classes files and SHA-256 manifest live in a **new** caller-selected directory. An existing destination is refused; a failed validation leaves its staging directory for inspection.

## GR-01 frozen contract: finite source-to-model mapping

Accepted 15 October 2026 following source inspection at `a8c1fb8` and a bounded fresh-kernel check passing 73/73 assertions (42 helper fixtures and 31 inventory checks). Executed scripts and evidence are retained separately. Acceptance covers this finite contract and its inventory; it does not establish independent physical normalisation, Faddeev–Popov completeness or amplitude correctness. Those checks remain separate tasks.

**Scope.** Conventional pure GR, $g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}$, through coupling order $\kappa^2$: imported graviton library orders `{1,2}`, fixed `GaugeFixingEpsilon = 2` (initialised in `FeynGrav.wl`), symbolic `D` and symbolic unassigned κ. For a pure-graviton Einstein–Hilbert vertex with $r$ legs the coupling power is $\kappa^{r-2}$: the action supplies $\kappa^{-2}$ and each metric perturbation one $\kappa$, so library order $n$ corresponds to $n+2$ graviton legs. Orders `{1,2}` are therefore the three- and four-graviton vertices. Order and leg count are related, not identical, and a per-vertex cutoff does not by itself bound the total amplitude order.

| Object | Source signature and location | Slots and index spaces | Momenta | Coupling | Model representation |
| --- | --- | --- | --- | --- | --- |
| Graviton propagator | `GravitonPropagator[mu,nu,alpha,beta,p]`, `FeynGrav.wl` | Two Lorentz indices per leg; massless quadratic part | Line momentum `p` | — (quadratic) | `AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]` in `PureGR.gen`; full tensor numerator with factor `I`, `FAD[p]` pole and the current gauge value 2 |
| FP vector-ghost propagator | `GhostVectorPropagator[mu,nu,p]`, `FeynGrav.wl` | One Lorentz index per leg; oriented | Line momentum `p` | — (quadratic) | Internal `U` propagator `I*FAMetricTensor[li1,li2]*FAPropagatorDenominator[mom,0]` in `PureGR.gen` |
| Three-graviton vertex | `GravitonVertex[mu1,nu1,p1,mu2,nu2,p2,mu3,nu3,p3]`, `Libs/GravitonVertex_1` via `importGravitons[1]` | Three ordered graviton triples (index pair, momentum) | All incoming | $\kappa$ | Generic `AnalyticalCoupling` with three `T` legs (231-term kinematic vector); classes `C[T[1],T[1],T[1]]` scalar coefficients ($\pm I/4\,\kappa$, …) |
| Four-graviton vertex | `GravitonVertex[...]` with four triples, `Libs/GravitonVertex_2` via `importGravitons[2]` | Four ordered graviton triples | All incoming | $\kappa^2$ | Generic `AnalyticalCoupling` with four `T` legs (4,158-term kinematic vector); classes `C[T[1],T[1],T[1],T[1]]` |
| Graviton–ghost–antighost vertex | `GravitonGhostVertex[rho,sigma,k,mu,pGhost,nu,pAntiGhost]`, `FeynGrav.wl` | Graviton triple first, then ghost $(\mu,p_{\rm ghost})$, then antighost $(\nu,p_{\rm anti})$; the derivative acts on the antighost (`FVD[pAntiGhost,...]`) | All incoming | $\kappa$ (prefactor $I\kappa/2$) | Classes row `C[T[1],-U[1],U[1]]`: antighost `-U[1]` in slot 2, ghost `U[1]` in slot 3; the builder evaluates the source with ghost first and maps it onto the model slot order (`ModelBuilder.wl`) |

**Fields and conjugation.** `T[1]` is the self-conjugate massless graviton (`SelfConjugate->True`, two transported Lorentz indices via `KinematicIndices[T]={Lorentz,Lorentz}`). `U[1]` is the oriented, non-self-conjugate Faddeev–Popov vector ghost (`SelfConjugate->False`, `PropagatorArrow->Forward`, one Lorentz index via `KinematicIndices[U]={Lorentz}`); `-U[1]` is its antighost, and FeynArts treats them as distinct Grassmann lines with the ghost number flowing from the `-U[1]` slot to the `U[1]` slot at the `TUU` coupling. Both external graviton Lorentz indices are transported through the model into the converted amplitude; nothing in the generic file contracts or drops one index of the pair.

**Momenta.** Every source vertex takes *all* momenta incoming. A scattering process instead has physically incoming and outgoing legs; the conversion passes the user's distinct incoming/outgoing momentum lists to `FCFAConvert`, which performs the required sign handling, with `MomentumConservation -> True` supplying local per-vertex routing only (it does not eliminate an external momentum or impose mass shells).

**Generic versus classes files.** The `.gen` file supplies the kinematic machinery: index declarations, propagator definitions and the kinematic tensor vector of each coupling. The `.mod` file supplies the particle classes and the matching scalar coupling-coefficient vectors (couplings, factors of `I`, signs). Kinematic tensors do not belong in the classes coefficients; the two vectors must stay aligned under FeynArts' closure (`CloseKinematicVector`/`CloseCouplingVector`), which the shipped snapshot applies to both.

**Amputation and external states.** External graviton legs carry the `FAPolarizationTensor` marker (`AnalyticalPropagator[External]`); `M$TruncationRules={_FAPolarizationTensor->1}` together with `Truncated -> True` removes that marker, yielding amputated open-index tensors. External propagators are never attached; polarisation attachment is a separate explicit step, not part of the default amplitude.

**Prefactors.** FeynArts' native `CreateFeynAmp` default is `PreFactor -> -I (2 Pi)^(-4 LoopNumber)` (installed `FeynArts/Analytic.m`), which would *not* reproduce the source normalisation; the bridge therefore sets `PreFactor -> 1` explicitly. FeynCalc's `FCFAConvert` defaults `Prefactor -> 1` and the bridge passes it explicitly as well. Vertex phases (factors of `I` and κ) live in the classes coefficients exactly as in the source rules; graph/statistics factors (including one closed-ghost-loop minus) are applied once by FeynArts. Because each factor now has one traced home, the bridge must not add an unexplained overall phase, symmetry factor or integration measure: any unresolved residual must be investigated and reported, never compensated with an unexplained extra sign. An unresolved comparison alone does not demonstrate a mathematical defect.

**Ghost-sector boundary.** The documented conclusion that the selected fixed-gauge FP operator is linear in $h$, so no two-graviton ghost contact is required *in this gauge*, is preserved from the dated verification record. Its independent traceable re-derivation is GR-02's task; this inventory neither repeats nor re-verifies it.

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

Fresh bounded direct-kernel checks show exact generated-vs-source graviton and ghost propagators, an exact cubic tree and quartic contact, and **exact independent source-factor checks** of both cubic vertices and the graviton propagator for each s/t/u exchange. The propagator comparison uses typed `MomentumExpand` to reconcile `Momentum[p+q,D]` with the source's sum of `Momentum` objects; no on-shell condition or expansion of a large vertex product is used. A small real one-diagram `CalcFormCalculate` call returned successfully. Source cubic ghost interactions match exactly under both whole-leg permutations, with FeynArts' minus sign for the reversed external Grassmann order. Field insertion is checked for completeness **before any amplitude is built**: every requested topology must receive an insertion, compared through its propagator skeleton rather than by counting, so a model that supplies no vertex for a requested topology returns `IncompleteInsertion` with the missing positions instead of a silently truncated diagram set. Reinitialising an already-used identical FeynArts model forcibly in the same kernel produced a stale-state failure in a diagnostic; this wrapper uses its existing initialised identity and validates file hashes before every call. Start a fresh kernel when changing model files.

The public physical requests are **three-/four-graviton trees and the pinned one-loop 1PI graviton two-point integrand**. Under an independently verified 1-GiB aggregate process cap, exact source-factor checks pass for the quartic tadpole (including its graph half and the reversed internal whole-leg momenta), both cubic vertices and both graph-half propagators of the graviton bubble, and both oriented cubic vertices, two propagator metrics/routings and closed-loop minus of the ghost bubble. `MomentumExpand` **and** FeynCalc `ExpandScalarProduct` are needed to reconcile equivalent composite momentum spellings in the ghost source; no compensating ghost sign or rule change was made. A separate standard-library numerical implementation of the published ghost vertex agrees at 256/256 components for each cubic and three nonzero contracted bubble samples; that test is numerical, not a substitute for the exact symbolic checks. The published fixed-gauge FP operator has no two-graviton ghost contact *in this gauge*; do not infer this for other gauges. A deliberately changed model snapshot is rejected before topology generation. The later FORM loop-reduction branch is not part of this checkout.

External ghosts are for diagnostics, not supported asymptotic processes. Matter, other gravity models, five- or higher-graviton vertices, counterterms, higher-multiplicity trees, one-loop four-point, multiloops, automatic higher-order model extension, loop integration, implicit measures, on-shell conditions, polarisation sums and library regeneration are outside scope. Requests beyond the allowlist fail **before** FeynArts topology/model generation. No library or dependency should be edited to overcome a failed gate.
