# Optional pure-GR FeynArts bridge

The supplied `GR` model is ready for native three-/four-graviton trees. Ordinary FeynGrav loading does not load this optional companion or initialise a model. Use the [native model guide](../FeynArts/Models/GR/README.md) to construct and convert diagrams without rebuilding anything.

## Separate Single-Scalar-Tensor-Minimal-Coupling native model

For GR plus one minimally coupled real scalar of arbitrary mass and independent positive cubic/quartic potential couplings, use [Single-Scalar-Tensor-Minimal-Coupling](../FeynArts/Models/Single-Scalar-Tensor-Minimal-Coupling/README.md). It includes five- and six-leg potential contacts through kappa squared; set both potential couplings to zero for the minimal-only theory. The [scalar extension report](../Documentation/Verification/ScalarModelExtensions.md) records its verification. This companion's process allowlist remains GR-only; use native FeynArts for scalar models. [BuildSingleScalarTensorMinimalCoupling.wl](BuildSingleScalarTensorMinimalCoupling.wl) is the optional developer builder. The former minimal-only builder and bundle were removed when the models were merged.

## Current tree release — 18 October 2026

The shipped model now matches the verified candidate: ordinary ghost propagator `-I MTD[mu,nu] FAD[p]`, unchanged cubic ghost vertex and native Grassmann signs. The classes file and graviton rules are unchanged. The matched generic/classes files and manifest are installed together. [Verification and limits](../Documentation/Verification/PureGRTreeRelease.md).

**Loop compatibility checks are deferred.** This is a tree-level release. Existing loop fingerprints remain unchanged; the corrected model returns `GhostCompletenessGate` before bridge loop construction. Do not bypass the gate or relabel historical loop evidence as verification of this snapshot. Native FeynArts calls do not apply this bridge safeguard.

<a id="current-transition--15-october-2026"></a>

### Historical transition

Before this release, the source used a negative ghost propagator while the shipped model retained a positive one. Accepted source, temporary-model and native ghost checks resolved that discrepancy. The first native tree test then stopped at a late-companion-loading warning; loading the companion before model initialisation passed all 32 checks without warnings. Use a fresh kernel after changing model files.

## Optional model building

`FeynGravBuildModel[absoluteNewDirectory]` snapshots current conventional-metric rules at gauge epsilon 2 with symbolic D and κ. `GravitonOrder -> 2` includes the cubic/quartic graviton vertices and existing ghost sector. Rebuilding is unnecessary for the supplied model. Existing destinations are refused; failed validation retains its staging directory.

`GravitonOrder -> 3` additionally includes the five-graviton vertex and requires `Libs/GravitonVertex_3`. It is outside this tree release's verified scope: the basis has 76,230 components, a historical initialisation took 486 s at a 6.07 GB peak, and five-point amplitude construction did not finish within 900 s. These are single-machine historical measurements. Missing requested libraries fail with `LibraryOrderUnavailable` rather than silently truncating the snapshot. Invalid orders and unknown options are rejected. Preserve order 2 for ordinary three-/four-point use.

## Finite source-to-model contract

Accepted 15 October 2026 following source inspection at `a8c1fb8` and a bounded fresh-kernel check passing 73/73 assertions (42 helper fixtures and 31 inventory checks). Executed scripts and evidence are retained separately. Acceptance covers this finite contract and its inventory; it does not establish independent physical normalisation, Faddeev–Popov completeness or amplitude correctness. The original inventory is historical; the table below includes the subsequently verified negative ghost line. Current ghost/tree acceptance is summarised above, while general physical-amplitude validation remains separate.

**Scope.** Conventional pure GR, $g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}$, through coupling order $\kappa^2$: imported graviton library orders `{1,2}`, fixed `GaugeFixingEpsilon = 2` (initialised in `FeynGrav.wl`), symbolic `D` and symbolic unassigned κ. For a pure-graviton Einstein–Hilbert vertex with $r$ legs the coupling power is $\kappa^{r-2}$: the action supplies $\kappa^{-2}$ and each metric perturbation one $\kappa$, so library order $n$ corresponds to $n+2$ graviton legs. Orders `{1,2}` are therefore the three- and four-graviton vertices. Order and leg count are related, not identical, and a per-vertex cutoff does not by itself bound the total amplitude order.

| Object | Source signature and location | Slots and index spaces | Momenta | Coupling | Model representation |
| --- | --- | --- | --- | --- | --- |
| Graviton propagator | `GravitonPropagator[mu,nu,alpha,beta,p]`, `FeynGrav.wl` | Two Lorentz indices per leg; massless quadratic part | Line momentum `p` | — (quadratic) | `AnalyticalPropagator[Internal][s1 T[j1,mom,{li1,li2}->{li3,li4}]]` in `GR.gen`; full tensor numerator with factor `I`, `FAD[p]` pole and the current gauge value 2 |
| FP vector-ghost propagator | `GhostVectorPropagator[mu,nu,p]`, `FeynGrav.wl` | One Lorentz index per leg; oriented | Line momentum `p` | — (quadratic) | Internal `U` propagator `(-I)*FAMetricTensor[li1,li2]*FAPropagatorDenominator[mom,0]` in `GR.gen` |
| Three-graviton vertex | `GravitonVertex[mu1,nu1,p1,mu2,nu2,p2,mu3,nu3,p3]`, `Libs/GravitonVertex_1` via `FeynGravImportGravitons[1]` | Three ordered graviton triples (index pair, momentum) | All incoming | $\kappa$ | Generic `AnalyticalCoupling` with three `T` legs (231-term kinematic vector); classes `C[T[1],T[1],T[1]]` scalar coefficients ($\pm I/4\,\kappa$, …) |
| Four-graviton vertex | `GravitonVertex[...]` with four triples, `Libs/GravitonVertex_2` via `FeynGravImportGravitons[2]` | Four ordered graviton triples | All incoming | $\kappa^2$ | Generic `AnalyticalCoupling` with four `T` legs (4,158-term kinematic vector); classes `C[T[1],T[1],T[1],T[1]]` |
| Graviton–ghost–antighost vertex | `GravitonGhostVertex[rho,sigma,k,mu,pGhost,nu,pAntiGhost]`, `FeynGrav.wl` | Graviton triple first, then ghost $(\mu,p_{\rm ghost})$, then antighost $(\nu,p_{\rm anti})$; the derivative acts on the antighost (`FVD[pAntiGhost,...]`) | All incoming | $\kappa$ (prefactor $I\kappa/2$) | Classes row `C[T[1],-U[1],U[1]]`: antighost `-U[1]` in slot 2, ghost `U[1]` in slot 3; the builder evaluates the source with ghost first and maps it onto the model slot order (`ModelBuilder.wl`) |

**Fields and conjugation.** `T[1]` is the self-conjugate massless graviton (`SelfConjugate->True`, two transported Lorentz indices via `KinematicIndices[T]={Lorentz,Lorentz}`). `U[1]` is the oriented, non-self-conjugate Faddeev–Popov vector ghost (`SelfConjugate->False`, `PropagatorArrow->Forward`, one Lorentz index via `KinematicIndices[U]={Lorentz}`); `-U[1]` is its antighost, and FeynArts treats them as distinct Grassmann lines with the ghost number flowing from the `-U[1]` slot to the `U[1]` slot at the `TUU` coupling. Both external graviton Lorentz indices are transported through the model into the converted amplitude; nothing in the generic file contracts or drops one index of the pair.

**Momenta.** Every source vertex takes *all* momenta incoming. A scattering process instead has physically incoming and outgoing legs; the conversion passes the user's distinct incoming/outgoing momentum lists to `FCFAConvert`, which performs the required sign handling, with `MomentumConservation -> True` supplying local per-vertex routing only (it does not eliminate an external momentum or impose mass shells).

**Generic versus classes files.** The `.gen` file supplies the kinematic machinery: index declarations, propagator definitions and the kinematic tensor vector of each coupling. The `.mod` file supplies the particle classes and the matching scalar coupling-coefficient vectors (couplings, factors of `I`, signs). Kinematic tensors do not belong in the classes coefficients; the two vectors must stay aligned under FeynArts' closure (`CloseKinematicVector`/`CloseCouplingVector`), which the shipped snapshot applies to both.

**Amputation and external states.** External graviton legs carry the `FAPolarizationTensor` marker (`AnalyticalPropagator[External]`); `M$TruncationRules={_FAPolarizationTensor->1}` together with `Truncated -> True` removes that marker, yielding amputated open-index tensors. External propagators are never attached; polarisation attachment is a separate explicit step, not part of the default amplitude.

**Prefactors.** FeynArts' native `CreateFeynAmp` default is `PreFactor -> -I (2 Pi)^(-4 LoopNumber)` (installed `FeynArts/Analytic.m`), which would *not* reproduce the source normalisation; the bridge therefore sets `PreFactor -> 1` explicitly. FeynCalc's `FCFAConvert` defaults `Prefactor -> 1` and the bridge passes it explicitly as well. Vertex phases (factors of `I` and κ) live in the classes coefficients exactly as in the source rules; graph/statistics factors (including one closed-ghost-loop minus) are applied once by FeynArts. Because each factor now has one traced home, the bridge must not add an unexplained overall phase, symmetry factor or integration measure: any unresolved residual must be investigated and reported, never compensated with an unexplained extra sign. An unresolved comparison alone does not demonstrate a mathematical defect.

**Ghost-sector boundary.** The documented conclusion that the selected fixed-gauge FP operator is linear in $h$, so no two-graviton ghost contact is required *in this gauge*, is preserved from the dated verification record. The source/native ghost checks are now accepted within the scope described in the verification summary; the original inventory did not itself re-derive this statement.

## Start with the standard FeynArts model

Use ordinary `InitializeModel`, `CreateTopologies`, `InsertFields`, `CreateFeynAmp` and `FCFAConvert` with the [supplied model and native recipe](../FeynArts/Models/GR/README.md). The current tree checks cover contacts, whole-leg permutations, renamed/composite inputs and every s/t/u exchange factor. They establish source-translation fidelity, not general physical-amplitude correctness. The optional convenience interface below adds complete-insertion and result-integrity checks.

## Small explicit tree workflow

Use a fresh kernel with the installed **FeynCalc-patched FeynArts 3.12** and existing converter (no installation or automatic dependency changes). The example uses the supplied model, with no build. Load the companion before model initialisation:

```wl
$LoadFeynArts = True;
root = DirectoryName[FindFile["FeynGrav`"]];
<< FeynGrav`;
Get[FileNameJoin[{root, "FeynArtsIntegration", "FeynArtsIntegration.wl"}]];
modelDirectory = FileNameJoin[{root, "FeynArts", "Models", "GR"}];
model = <|"Directory" -> modelDirectory, "ModelName" -> "GR",
  "Manifest" -> FileNameJoin[{modelDirectory, "manifest.json"}]|>;
process = <|"Model" -> "GR", "Incoming" -> {FeynArts`T[1], FeynArts`T[1]},
  "Outgoing" -> {FeynArts`T[1], FeynArts`T[1]}, "LoopOrder" -> 0|>;
tree = FeynGrav`FeynArtsIntegration`FeynGravCreateAmplitude[
  model, process, {p1, p2}, {q1, q2}];
(* Optional drawing, no algebra or FORM: FeynArts`Paint[tree["Insertions"]] *)
(* Optional FORM algebra; 20 s bounds each FORM execution, not total runtime: *)
FeynGrav`FeynArtsIntegration`FeynGravCalculateAmplitude[
  tree, FeynCalc`Dimension -> D, System`TimeConstraint -> 20,
  CalcFormConverter`FORMThreads -> 1]
```

The last step requires the **existing** `CalcFormConverter` package to be loaded (load `CalcFormConverter/CalcFormConverter.wl` explicitly if ``DownValues[CalcFormConverter`CalcFormCalculate] === {}``). A three-point example uses one incoming and two outgoing `T[1]` fields with matching distinct momentum lists. Consult ``?FeynGrav`FeynArtsIntegration`FeynGravValidateProcess`` for the exact whitelist. Pass only explicitly supported, fully qualified converter options. The constructor records per-diagram SHA-256 integrity values; a changed expression, graph ID or denominator count is rejected before **real** FORM algebra (an accidental-mutation guard, not a secret authentication scheme). If one diagram fails, `"Status"` is `"Incomplete"`, its failure and ID remain in `"Diagrams"`, and `"Total"` is `Missing` rather than a partial answer. FORM files are retained when ``CalcFormConverter`KeepFiles -> True`` is explicitly requested. A custom `CalculationFunction` is a **test seam only**, never physical evidence.

The result contains all individual `"Diagrams"` (open-index FeynCalc expressions and FeynArts graph IDs), `"RawAmplitude"` and `"Insertions"` for FeynArts drawing. External tensor markers are truncated, external propagators are **not** added, and the FeynArts graph/statistics factor is applied **once**; no hidden spin sum, external on-shell condition, momentum integration measure, counterterm or master-integral reduction is inserted. The four-point tree has exactly one contact and all three single-propagator exchange diagrams. These are off-shell integrand expressions, not squared amplitudes or cross sections.

## One-loop route — deferred for this release

The existing fifth-argument loop interface remains in the code, but its accepted fingerprint predates the corrected ghost model. The current shipped model therefore returns `Failure["GhostCompletenessGate", ...]` before topology generation. This is intentional. No loop checks, fingerprint refresh or loop integration accompany this release.

Historical tests covered tadpole, graviton-bubble and oriented ghost-bubble source factors for the earlier snapshot. Their results are preserved as historical evidence only. Revalidation of the corrected candidate is deferred. Do not change model bytes or verification pins merely to make a loop request run. Tree construction is unaffected.

## Validation boundary

The corrected model passed 28 native ghost requirements plus six arrow fixtures; the subsequent tree batch passed 32 checks with 15 exact zero residuals. It compared native tensors against fresh FeynGrav calls and identified the contact plus all three exchanges by topology, fields and routing. Exchange factors were compared separately, preserving the entire propagator/scalar remainder. No complete exchange-product expansion or on-shell assumption was required. The real missing-quartic case returned `IncompleteInsertion` before amplitude construction and restored native settings afterwards.

These checks reused the accepted temporary build and verified their exact model/source identities. The shipped files are byte-identical to that candidate. See the [verification summary](../Documentation/Verification/PureGRTreeRelease.md) for measurements, the preserved failed first attempt and untested boundaries. Start a fresh kernel when changing models; if using this companion, load it before native initialisation to avoid the observed late-load namespace warning.

Physical on-shell amplitudes, helicity/Ward tests and corrected-model loop compatibility remain unverified. The current release exposes complete open-index tree diagrams, not automatically normalised cross sections. The existing calculation helper uses CalcFormConverter explicitly; this release did not rerun FORM algebra or establish new performance guarantees.

External ghosts are for diagnostics, not supported asymptotic processes. Matter, other gravity models, five- or higher-graviton vertices, counterterms, higher-multiplicity trees, one-loop four-point, multiloops, automatic higher-order model extension, loop integration, implicit measures, on-shell conditions, polarisation sums and library regeneration are outside scope. Requests beyond the allowlist fail **before** FeynArts topology/model generation. No library or dependency should be edited to overcome a failed gate.

## Native scalar extensions — 18 October 2026

`Single-Scalar-Tensor-Minimal-Coupling`, `John` and `Einstein-scalar-Gauss-Bonnet` are supplied alongside `GR`, with matching generic/classes files in individual model folders. Use a fresh kernel and the same name for `Model` and `GenericModel`. The optional pure-GR bridge remains restricted to its existing processes. See [model index](../FeynArts/Models/README.md) for actions, native examples and verification limits. The two-graviton EsGB source and library have been corrected against an independent curvature derivation; higher-order EsGB sources require a separate audit. Vertex order is limited to kappa squared; potential contacts still require up to six legs. Loops are not validated.
