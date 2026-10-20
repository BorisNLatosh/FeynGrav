# Scalar-model extensions and source correction

**Subsequent status:** the original checks below are historical. The full John cubic/quartic derivation is summarised in the [John guide](../../FeynArts/Models/John/ModelGuide.md#verification); the later minimal-contact correction, rebuilt scalar models and selected passing Ward identities are recorded in [ScalarWardCorrection.md](ScalarWardCorrection.md). These additions do not validate higher orders or loops.

Checked: 18 October 2026, uncommitted working tree on `FeynArts_Integration`. No commit was made. All evidence below is retained under `/home/latosh/Documents/Codex/FeynGrav-evidence/2026-10-18-scalar-extension-work/`.

## Delivery and evidence limits

| Model or change | Outcome | Evidence |
| --- | --- | --- |
| Potential source and order-two library | Corrected graviton differentiation multiplicity; eight exact checks passed | `repair-uncapped/run-CHECK`, `potential-library-install.json` |
| GRScalarPotential | Installed standalone bundle; fifteen final native checks passed | `potential-native/run-CHECK` |
| GRJohn | Installed standalone bundle; six construction and fourteen final native checks passed | `john-build/run-CHECK`, `john-native-correction/run-CHECK` |
| GREsGB | Corrected and delivered: nine source/regeneration, six construction and twenty native checks passed | `esgb-build-corrected/run-CHECK`, `esgb-native/run-CHECK`; regeneration in sibling `2026-10-18-esgb-repair/` |

The two existing PureGR and GRScalar bundles were preserved. Their unchanged GR/ghost evidence is reused. No loop or general on-shell Ward validation was performed. Manifest hashes identify the generated files and input libraries; they are provenance records, not a proof of physical correctness.

## Potential normalisation

The potential routines now multiply the averaged n-graviton expansion coefficient by n!. Scalar factorials are already cancelled by differentiation and are not multiplied again. Only `Libs/GravitonScalarPotentialVertex_2` was regenerated: its complete tensor equals twice the previous tensor and agrees with the differentiated action. The one-graviton library is unchanged. Independent time-component derivatives were checked through order three, and the full order-two tensor was compared exactly. This does not establish higher-order libraries that were not generated.

The initial capped verification hit its memory limit while automatic workers were launched. That evidence remains in `repair/`. The user then authorised regeneration without a memory cap. The successful eight-check regeneration in `repair-uncapped/` used serial FORM and no memory cap. This exception did not remove the model-check limits.

Potential construction retained an assertion-syntax failure and then a FeynArts closure warning. For constant scalar-only bases `{1}`, applying the tensor closure helper created an orphan optional pattern. The final builder skips closure for these scalar-only constant bases in both generic and classes files, retaining closure for tensor/ghost bases. The corrected candidate passed standalone native initialisation and all fifteen checks without warnings. Failed attempts are preserved, not counted as passes.

All six added potential contacts, including five- and six-leg contacts, agree exactly with the action. The scalar elastic process has one quartic contact, three scalar exchanges and three graviton exchanges with the checked species and routes. Crossed 1-to-3 construction and scalar-sector factors were checked; full crossed gravitational-amplitude equality was not newly established. Mixed minimal channels reuse existing source evidence. A missing-contact negative test detects omission.

## John normalisation and native checks

The source mapping is G5 = -beta phi, with a = 1 and b = 0. Integration by parts gives the selected positive John action without an extra half. Existing G5 libraries were preserved.

Independent exact checks use g_mu_nu = (1 + kappa H) eta_mu_nu and expand the densitised Einstein tensor through second order. Both conformal projections agree for symbolic D and routed generic momenta. **This is not a full independent component derivation.** Native cubic/contact and exchange-factor comparisons establish translation of the stored source rules; they do not fill that independent-proof gap.

The first native attempt stopped at a test locator that omitted graviton momenta when locating the mixed contact. The one test-only correction included those momenta; fourteen checks then passed. Both beta-linear and beta-quadratic products are retained, with no duplicate coupling rows or propagator correction. Zero beta recovers the minimal scalar rules. A missing-contact negative test passed.

## EsGB: historical failure before the authorised correction

The independent reference is twice the bilinear cross term of the linearised curvatures:

$$V=2i\kappa^2(R_1^{abcd}R_{2,abcd}-4R_1^{ab}R_{2,ab}+R_1R_2).$$

Each graviton is symmetrised. No separate conservation of the two graviton momenta is imposed. The scalar carries their remaining momentum.

For mostly-minus metric, kappa = coupling = 1, graviton components (00) and (11), and contravariant momenta k1 = (1,2,3,4), k2 = (5,6,7,8), the stored source gives **-4080 i** while direct linearised curvature contraction gives **-32 i**. Their residual is **-4048 i**. An independent exact-rational component implementation confirms -32 i (`gb-independent-components.json`). The retained symbolic comparison has different coefficient ratios, so one overall rescaling cannot repair it.

The original EsGB batch passed its inventory check and failed its action check, then stopped. That rejected candidate and its evidence remain unchanged. The user subsequently authorised a source correction, regeneration and completion of GREsGB; the later successful checks below supersede the blocker. No compensating change was made inside the model.

## Resources and reproducibility

| Run | Kernel wall seconds | Highest retained cgroup memory peak, bytes |
| --- | ---: | ---: |
| Potential final native | 7.074 | 262402048 |
| John construction | 11.285 | 335216640 |
| John final native | 10.407 | 274309120 |
| EsGB construction, stopped on discrepancy | 8.414 | 285941760 |

These are single verification runs, not performance benchmarks. Model checks used fresh sequential kernels, whole-tree 1 GiB memory, zero swap, 120 seconds plus 10 seconds termination grace and at least 2 GiB initial host headroom. Retained wrappers, warning logs, assertions, script hashes, resource observations and termination records are in each run directory. The listed final runs had no resource event or unexpected warning. Existing user kernels were not touched. Earlier failed attempts remain available.

No expensive regression suite, stored-library inventory regeneration, notebook rewrite or loop-pin refresh was performed. Documentation/startup edits after the checks were reviewed statically; they were not grounds to repeat kernel calculations.

## EsGB correction and completed delivery

The order-two source now constructs linearised Riemann curvature using antisymmetrised derivatives of `GammaTensor`, traces it to Ricci/scalar curvature, and contracts the full Gauss–Bonnet combination with the two-graviton differentiation factor. This bypasses the incorrect legacy `TensorT` contraction only at order two. An independent reference constructs the curvatures directly from the symmetric metric perturbation. Full symbolic residuals vanish, as do both graviton Ward contractions and the zero-total-scalar-momentum limit; graviton interchange is exact.

Nine source/regeneration checks passed. Serial FORM generated only `ScalarGaussBonnet_2` into a temporary directory; read-back agrees with the independent full tensor. The verified file was installed with its previous hash and new hash retained in `2026-10-18-esgb-repair/library-install.json`. Regeneration had no imposed memory cap, as requested, and a 120-second wall limit; it completed in 7.428 seconds overall. The systemd memory summary is not used as a reliable peak measurement.

The new candidate passed six construction checks: exact family inventory, full action comparison, separate g1/g2 normalisations, zero-coupling recovery and initialisation. Twenty native checks then passed, including standalone loading, TTS and combined TTSS contacts, signed source factors and propagators in selected channels, independent couplings, massless scalar propagator limit and a real missing-contact negative test. The mixed scalar–scalar to graviton–graviton process has six diagrams, including two g1-squared graviton exchanges. Four-graviton scattering has seven diagrams, including three massive-scalar exchanges proportional to g1 squared and kappa^4. The unchanged pure-GR factors reuse earlier evidence; the new scalar exchanges were checked factor by factor.

Construction took 11.398 seconds with highest retained cgroup memory peak 313430016 bytes. Native verification took 51.469 seconds with highest retained peak 288223232 bytes. Both used the original whole-tree 1 GiB/no-swap/120-second limits, fresh host headroom above 2 GiB, and no unexpected warnings, resource events or surviving owned processes. These are single checks, not benchmarks. No test-only retry was needed after the authorised source repair.

The tested `.gen`, `.mod` and manifest were copied unchanged to `FeynArts/Models/GREsGB/`. Existing model bundles were preserved. Final documentation and usage-message updates were checked statically, without another kernel launch. Higher-order EsGB sources and libraries were not changed or validated. No loops, general physical on-shell amplitudes, full benchmark run or commit are included.
