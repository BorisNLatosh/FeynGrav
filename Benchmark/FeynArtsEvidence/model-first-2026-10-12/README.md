# Direct FeynArts model check — 12 October 2026

## Purpose

Validate the shortest chain: **existing FeynGrav rules → standard FeynArts `.gen`/`.mod` files → diagrams and amplitudes**, without loading `FeynArtsIntegration` or calling its public wrappers. This is the model-first stage requested after the broader stabilisation proposal was set aside. No FORM calculation, new physical model, library generation or wrapper redesign belongs to this check.

## What the model files contain

The `model/` subdirectory is a byte-identical copy of the earlier successful model snapshot, preserved here so these checks no longer depend on that `/tmp` directory. It is an original generated FeynGrav model, not a copy of FeynArts' built-in models. Its name is `FGPureGR5a149c98161e2b2e`.

| Standard FeynArts definition | This GR model |
| --- | --- |
| `KinematicIndices` in `.gen` | `T` transports two Lorentz indices; `U` transports one |
| `M$GenericPropagators` | Full source tensor and ghost propagators, with their `I` factors; graviton external marker |
| `M$GenericCouplings` | Three-/four-graviton and graviton–ghost–antighost kinematic vectors |
| `M$ClassesDescription` in `.mod` | One massless self-conjugate `T[1]`; oriented non-self-conjugate `U[1]` and `-U[1]` |
| `M$CouplingMatrices` | Matching scalar coefficients containing the original coupling ``FeynGrav`κ`` and normalisation |
| `CloseKinematicVector`, `CloseCouplingVector` | Standard FeynArts closure of the paired generic/classes vectors |
| `P$NonCommuting`, `$FermionLines`, `P$ChainBuildOrder` | Ghost statistics and chain ordering, not a commuting-vector substitute |
| `M$TruncationRules` | Removes the external graviton marker on an explicitly truncated amplitude |

The sources are `GravitonVertex`, `GravitonPropagator`, `GravitonGhostVertex` and `GhostVectorPropagator`; the existing builder evaluates and translates them once. The files do not call those routines when loaded. Only the cubic/quartic graviton orders are explicitly imported for these checks using `importGravitons[2]`. Normal loading of FeynGrav has its existing broader library defaults; **the model itself contains no matter fields or interactions**.

This follows the installed `FeynArts/manual/FA3Guide.tex`, **Definition of a New Model** (lines 2644 onward). **Model-file Generation** (lines 3733 onward) describes `.mmod` templates and `ModelMaker`; lines 3820–3823 explicitly warn that `ModelMaker` hard-codes some `Lorentz.gen` conventions. We therefore reuse the existing source-rule translation, not a second Lagrangian derivation or an assumed universal template generator.

Verified before execution:

- Generic SHA-256: `9fff5547d9bb9cb9c548f5861699844006fe8dc7cdcc56620004c6deb48f67d9` (696,151 bytes).
- Classes SHA-256: `536144daf0106bf49c9d7c56e6858a447ba68c8287edb704272836606e5828a0` (134,327 bytes).
- Both shipped graviton-library bytes still match the copied manifest.
- The manifest's original source paths describe provenance; they are not callbacks inside the model files.

## Minimal direct workflow

In a fresh kernel with installed FeynCalc-patched FeynArts, set `root` to the checkout:

```wl
$LoadFeynArts = True;
root = "/path/to/FeynGrav";
Get[FileNameJoin[{root, "FeynGrav.wl"}]];
modelDir = FileNameJoin[{root, "Benchmark", "FeynArtsEvidence",
  "model-first-2026-10-12", "model"}];
model = "FGPureGR5a149c98161e2b2e";
FeynArts`$ModelPath = Prepend[FeynArts`$ModelPath, modelDir];
FeynArts`InitializeModel[model, FeynArts`GenericModel -> model];
top = FeynArts`CreateTopologies[0, 1 -> 2, FeynArts`Adjacencies -> {3, 4}];
ins = FeynArts`InsertFields[top, {FeynArts`T[1]} -> {FeynArts`T[1], FeynArts`T[1]},
  FeynArts`Model -> model, FeynArts`GenericModel -> model,
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
(* Optional drawing: FeynArts`Paint[ins] *)
amp = FeynArts`CreateFeynAmp[ins, FeynArts`PreFactor -> 1,
  FeynArts`GaugeRules -> {}, FeynArts`MomentumConservation -> True,
  FeynArts`Truncated -> True];
fc = FeynCalc`FCFAConvert[amp, FeynCalc`IncomingMomenta -> {p},
  FeynCalc`OutgoingMomenta -> {q1, q2}, FeynCalc`ChangeDimension -> D,
  FeynCalc`Contract -> False, List -> True, FeynCalc`Prefactor -> 1];
source = FeynGrav`GravitonVertex[Lor1, Lor2, p,
  Lor3, Lor4, -q1, Lor5, Lor6, -q2];
Expand[First[fc] - source] === 0
```

This is the product of the original Feynman rules and graph/statistics factors, not an integrated or squared amplitude. Outgoing physical momenta enter source vertices with minus signs. The explicit settings add no external propagators, on-shell constraints, loop measure or external-momentum elimination. **`MomentumConservation -> True` is essential for standard internal routing:** it enforces conservation at each vertex. The manual's option table and momentum-routing section, corroborated by `FeynArts/Analytic.m:114–127`, distinguish this from eliminating an external momentum. The first four-point test used `False` as mistakenly proposed in the plan; its contact and permutation tests passed but its three expected exchange routes were absent. That failed test and log are preserved as `direct-four-unrouted.wls/.log`; it is not a source-rule defect. The plan and current tests now state the correct local-routing convention. `Paint` is the normal optional drawing interface; no graphical rendering is claimed by the headless tests.

## Retained direct checks

| Script and raw log | Current result |
| --- | --- |
| `direct-three.wls/.log` | **Exact PASS:** 10 assertions; tensor/ghost index transport, graviton propagator, cubic vertex, six external indices, outgoing-momentum swap and within-pair symmetry; no integration companion loaded |
| `direct-four.wls/.log` | **Exact PASS:** 22 assertions; contact plus all three exchange channels, nine independent exchange-factor comparisons, external-momentum permutation and quartic whole-leg symmetry |
| `direct-ghost.wls/.log` | **Exact PASS:** 8 assertions; ghost propagator and both external field orders with their expected opposite Grassmann signs |
| `direct-loop.wls/.log` | **Exact PASS:** 19 assertions; graph families identified from topology valences and inserted fields, joined by GraphID; complete tadpole/graviton-bubble/ghost-bubble source factors, routes, symmetry weights and one closed-ghost-loop minus |

The four final fresh-kernel runs passed **59 assertions**, with shell exits zero and explicit completion markers. Recorded aggregate memory peaks were 280,039,424 / 292,724,736 / 280,317,952 / 280,158,208 bytes for cubic/four-point/ghost/loop checks respectively (about 267–279 MiB). Every final scope reported zero `max`, `oom`, `oom_kill` and `oom_group_kill` memory events. These are exact representation/source-product checks for this finite snapshot, **not** an independent derivation of the entire GR source library or validation of wrapper/backend robustness.

`setup.wl` only provides shared loading, model-hash checks, cap verification and assertion reporting for these small scripts. It is not a new model API or FORM runtime. `run-provenance.json` records 30 hashes covering the scripts, logs, model bundle, relevant source libraries and inspected dependency files, together with final exit/assertion counts and rounded pre-run host headroom. The snapshot is uncommitted; HEAD alone does not identify its contents.

## Ghost completeness and independent graph weights

The [existing FP action audit](../2026-10-12-gate-a/fp-operator-audit.md) gives the selected background-linear gauge functional and its variation. In the conventional `g = eta + kappa h` parametrisation, the field variation has an `h`-independent part and a part linear in `h`. Applying the linear gauge functional gives `M = M0 + kappa M1[h]`: there is no `h h antighost ghost` contact **in this fixed gauge**. The audit cites the published operator and explicit source rules, and retains its auxiliary-field-normalisation caveat; we do not alter the source propagator or infer the same contact statement in other gauges.

The new source-factor test uses the following independent Wick-counting weights, with labelled external gravitons and the source vertices already symmetrised:

- **Quartic tadpole:** 4 choices for the first external contraction and 3 for the second; the remaining pair contracts once. Thus `4*3/4! = 1/2`.
- **Two-cubic graviton bubble:** 2 assignments of the labelled external legs to vertices, 3 choices of an external slot at each vertex and 2 pairings of the remaining internal slots. Thus `(2*3*3*2)/(2!*(3!)^2) = 1/2`.
- **Oriented ghost bubble:** 2 assignments of the external gravitons cancel the expansion's `2!`; ghost number fixes the internal contractions. The graph weight is 1, with **one closed Grassmann-loop minus**. Reversing the same cycle does not add another diagram. Both source ghost propagators retain their `I`, so the propagator product and loop minus must be compared together.

`direct-loop.wls` checks these products against the three actual FeynArts contributions, keeping each vertex factored. It does not merely compare FeynArts' reported factors to themselves. Ghost cubic equality uses `ExpandScalarProduct[MomentumExpand[...]]` to distribute momenta inside typed pairs; it requires no fitted sign. The earlier [tiny commuting/noncommuting sign control](../2026-10-12-gate-a/check-ghost-sign.log) is retained supporting evidence, not a newly run test and not a substitute for the physical three-graph comparison. Its tadpole exclusion was diagnostic only; the new full 1PI construction excludes neither tadpoles nor scaleless terms.

## Bounded reproduction

The scripts derive the checkout from their location. They read the model from `FGFA_MODEL_DIR` when that variable is set and otherwise fall back to the packaged copy in `FeynArts/Models`. The original model snapshot in this directory has been removed and the packaged copy is now canonical; the deleted files were byte-identical, so the recorded hashes still apply. From `Benchmark/`, with sufficient host headroom, use a fresh user cgroup and preserve a **new log name** rather than overwrite historical evidence:

```bash
unit="fgfa-direct-$(date +%s)-$$"
systemd-run --user --scope --same-dir --unit="$unit" \
  -p MemoryAccounting=yes -p MemoryMax=1G -p MemorySwapMax=0 \
  -p OOMPolicy=kill -p KillMode=control-group -p RuntimeMaxSec=180s \
  -p TimeoutStopSec=7s -p TasksMax=48 \
  -- timeout --kill-after=5s 165s \
  WolframKernel -noinit -script FeynArtsEvidence/model-first-2026-10-12/direct-three.wls \
  > "FeynArtsEvidence/model-first-2026-10-12/${unit}.log" 2>&1
```

`direct-three-after-move.log` records this fallback working: `MODEL_DIR=.../FeynGrav/FeynArts/Models`, `MODEL_HASHES_VERIFIED=True`, 10 assertions passed. `run-provenance.json` still lists the old model paths because it is the frozen record of the original run; those bytes now live in `FeynArts/Models`.

Run one kernel at a time. Each kernel refuses to proceed without a live 1-GiB aggregate limit, zero swap and group OOM policy. Completed logs record `memory.peak`, `memory.events`, assertion count and failures before the transient scope disappears. Check the process exit **and** assertions: a timeout or missing completion line is not a pass. This bounds the checked workload; it does not close the separate RESOURCE-01 issue for larger importer/FORM calculations.
