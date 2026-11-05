# Learning FeynGrav through examples

Notebook names below are paths within the complete FeynGrav package, outside this Markdown vault. See [Opening notebooks](../PackageFiles.md#Open%20a%20notebook).

Start with a fresh Mathematica kernel and use **Shift+Enter** to evaluate input cells. Evaluate the sections of each example in order. Opening a notebook does not run calculations. The package and its dependencies must already be installed; see the [installation guide](../UserGuide.md#Installation%20and%20help).

These examples target the [unreleased FeynGrav 4.2.0](../ReleaseNotes-4.2.0.md) and use `FeynGravSimplify`, whose default `Method -> Automatic` selects the full FORM pipeline. The command is not available in published FeynGrav 4.1.0.

## Numbered examples and suggested reading order

The numbers define the suggested reading order, from operator algebra and tree-level scattering to loop calculations. All notebooks require FeynGrav and FeynCalc. Examples 01–08, 13 and 14 also use FORM. TFORM is optional. Examples 09–12 evaluate their tree tensors directly with FeynCalc. Additional tools are listed below; see [optional dependency setup and checks](../OptionalDependencies.md) before running a notebook that needs them.

| No. | Notebook | What you learn | Additional requirements |
| --- | --- | --- | --- |
| 01 | `Examples/01. Nieuwenhuizen_Operators.nb` | Tensor indices, propagator denominators, completeness and algebraic checks | None |
| 02 | `Examples/02. Scalars_Gravitational_Scattering_Tree_Level.nb` | Exchange amplitude, Mandelstam variables, differential cross section and nonrelativistic limit | None |
| 03 | `Examples/03. Graviton_Scattering_Tree_Level.nb` | Manual construction, helicity amplitudes, contact terms and longitudinal checks | None |
| 04 | `Examples/04. Graviton_Scattering_Tree_Level_FeynArts.nb` | Native construction of the complete 2 → 2 tree, off-shell tensors and on-shell helicity comparisons | FeynCalc-patched FeynArts |
| 05 | `Examples/05. Graviton_Self_Energy.nb` | Graviton and ghost loops, tensor-integral reduction and projector representation | None |
| 06 | `Examples/06. Matter_Self_Energy_Graviton_Contribution.nb` | Matter two-point functions with gravitational loop corrections | None |
| 07 | `Examples/07. Graviton_Self_Energy_Matter_Contribution.nb` | Fermion, massive-vector, massless-vector and Yang–Mills loops in the graviton two-point function | None |
| 08 | `Examples/08. Graviton_Scalar_Vertex_One_Loop.nb` | Native construction of five minimal scalar–graviton 1PI diagrams, checked routing and weights; downstream ultraviolet/infrared results need recomputation | FeynCalc-patched FeynArts and FeynHelpers with its bundled Package-X component |
| 09 | `Examples/09. Scalar_Scattering_Tree_Level_FeynArts.nb` | Seven scalar-scattering diagrams, on-shell kinematics and the massive amplitude with independent cubic/quartic couplings | FeynCalc-patched FeynArts |
| 10 | `Examples/10. Scalar_Scattering_John_Tree_Level_FeynArts.nb` | John scalar scattering, three graviton exchanges, and minimal/mixed/John amplitude contributions | FeynCalc-patched FeynArts |
| 11 | `Examples/11. Scalar_Annihilation_EsGB_Tree_Level_FeynArts.nb` | Six scalar–scalar to graviton–graviton diagrams and on-shell polarisation-dependent amplitude; helicity amplitude and two generic graviton Ward checks using the corrected model | FeynCalc-patched FeynArts |
| 12 | `Examples/12. Yang_Mills_Graviton_Emission_Tree_Level_FeynArts.nb` | SU(N) gauge-field scattering with graviton emission, four diagrams, symbolic-D on-shell amplitude, gauge/graviton Ward checks and a four-dimensional helicity expression | FeynCalc-patched FeynArts |
| 13 | `Examples/13. Quark_Graviton_Emission_Tree_Level_FeynArts.nb` | Massive quark annihilation into a gluon and graviton, FORM Dirac algebra and both Ward identities | FeynCalc-patched FeynArts and FORM |
| 14 | `Examples/14. Graviton_Scattering_Quadratic_Gravity_FeynArts.nb` | Four-graviton scattering in quadratic gravity, with the on-shell helicity amplitudes compared to example 04 | FeynCalc-patched FeynArts |

Example 09 can be read immediately after example 04: it gives the scalar counterpart of the native FeynArts workflow, with incoming momenta `p1`, `p2` and outgoing momenta `p3`, `p4`. It displays the contact, scalar-exchange and graviton-exchange contributions to `M` after removing the overall `I` from the diagram result.

Example 12 uses `GR-SU-N-Yang-Mills` for $A^a(p_1) + A^b(p_2) \to A^c(p_3) + h(p_4)$. It separates the common $g_s\kappa f^{abc}$ colour/coupling factor, uses conjugate outgoing polarisations, and keeps D and N symbolic. Three gauge-field Ward checks and one graviton check with an arbitrary gauge vector vanish; removing the contact gives a nonzero gauge residual. A complete kernel evaluation is recorded within the [scope](../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions) and saved-output limits listed below. The final subsection defines polarisation scalar products and gives one helicity expression in `h1`, `h2`, `h3`, `h4`, `s` and `t`; the graviton helicity is `2 h4`. FORM is not needed.

The two larger matter notebooks contain separate field sectors. Example 06 begins with the scalar sector; example 07 begins with fermions, then covers massive vectors, massless vectors and Yang–Mills theory. One-loop reduction and symbolic phase-space integration can take appreciably longer than the small projector checks.

<a name="Verification and saved-output status"></a>

## Verification and saved-output status

The table distinguishes recorded checks from the results currently saved in each file. A syntax/interface audit checks whether input can be parsed and uses the intended interfaces; it does **not** evaluate the complete notebook. A model or diagram check validates only the stated calculation. A recorded full kernel evaluation is not a fresh run against every subsequent package change or a Mathematica front-end certification.

All fourteen example notebooks currently contain saved output. Examples 01–11 retained their outputs when their calculation calls were migrated to `FeynGravSimplify`; those outputs were not refreshed by that migration. For a result obtained with your installed package, start a fresh kernel and evaluate the required calculation and all its dependencies.

| Example | Recorded verification scope | Saved-output status and action |
| --- | --- | --- |
| 01 — Operators | Syntax/interface audit; no complete reevaluation in that audit. | Historical output retained; recompute the identities with the installed package. |
| 02 — Scalar scattering | Syntax/interface audit; no complete reevaluation in that audit. | Historical output retained; recompute amplitude and cross section. |
| 03 — Graviton scattering | Syntax/interface audit; no complete reevaluation in that audit. | Historical output retained; recompute amplitude, helicities and longitudinal checks. |
| 04 — Native graviton scattering | Syntax/interface audit; native model contact/exchange factors checked separately. | Historical output retained; those model checks do not refresh the notebook’s amplitude/helicity results. |
| 05 — Graviton self-energy | Syntax/interface audit; complete loop calculation not rerun. | Historical output retained; recompute reductions and projector coefficients. |
| 06 — Matter self-energy | Syntax/interface audit; complete loop calculation not rerun. | Historical output retained; recompute each required matter sector. |
| 07 — Matter contribution to graviton self-energy | Syntax/interface audit and focused repair of `B0` collection patterns; complete loop calculation not rerun. | Historical output retained, including tables affected by the repair; recompute them. |
| 08 — Scalar–graviton one-loop vertex | Partial evaluation: five native diagram products, routing and weights checked with symbolic D. Downstream reduction, poles and infrared analysis not rerun. | Output is present, but downstream results are not certified after the bubble-weight correction. Recompute before using them. |
| 09 — Native scalar scattering | Seven-diagram tree amplitude and six notebook checks evaluated, including the repaired gravity-only extraction. | Earlier saved output retained; rerun the gravity-only contribution and dependent displays/checks. |
| 10 — John scalar scattering | Syntax/interface audit; John action and selected model trees checked separately. | Historical output retained; model verification does not certify the saved scalar-scattering decomposition. Recompute it. |
| 11 — EsGB annihilation | Repaired setup relations checked; corrected-model graviton Ward tests checked separately. No complete reevaluation of the current notebook recorded by the syntax audit. | Historical output retained; rerun in a fresh kernel, especially if an old cell assigning to `Contract` was evaluated. |
| 12 — Yang–Mills graviton emission | Complete kernel evaluation recorded, including gauge/graviton Ward tests and helicity comparisons. | Saved results retained from the checked workflow and subsequent front-end save; not a rerun against later package changes. |
| 13 — Quark graviton emission | All fourteen input cells evaluated; both Ward tests and coupling-limit checks passed. | Computed results and diagram graphics saved with the checked notebook; not a rerun against later package changes or front-end certification. |
| 14 — Quadratic gravity | The four diagram contractions and all three GR helicity comparisons passed in a fresh kernel with symbolic mass parameters. | Existing diagram graphics retained. The availability, diagram count and three helicity checks have saved Boolean outputs. |

The loop limitations of example 08 remain in force even though its native diagram construction passes. See its detailed scope below and the [scientific limitations](../ScientificScope.md#Examples%20and%20saved%20outputs).

## Reading the code

- **Vertices and propagators:** FeynGrav supplies the Feynman rules. Vertex momenta are incoming; repeated indices are contracted. Read the local notation paragraph before adapting signs or external legs.
- **`FeynGravSimplify`:** sends supported Lorentz, epsilon, Dirac and fundamental SU(N) colour algebra to FORM, then imports the result. Automatic selection prefers up to eight TFORM workers, capped by the processor count, with serial fallback when TFORM is missing. With no declared loop momentum it performs algebra only. Example 08 explicitly uses `TensorDecomposition -> False` with `LoopMomenta -> {l}` to retain the integrand for the subsequent FeynCalc `TID` step. Its `reduceVertex` helper removes denominator-free polynomials as scaleless integrals before calling `TID`.
- **`TID[..., k, ToPaVe -> True]`:** FeynCalc performs tensor-integral reduction with loop momentum `k`, leaving scalar Passarino–Veltman functions such as `A0` and `B0`.
- **`FeynAmpDenominatorExplicit`:** rewrites propagator denominators as ordinary expressions, useful for tree-level algebra and projector identities.
- **Semicolons:** suppress long intermediate output. Evaluate a stored expression's name separately to inspect it. `AbsoluteTiming` measures elapsed wall time, including external FORM execution where used.
- **Checks:** `True` confirms the displayed equality; `False` means it did not match. An unevaluated equality is unresolved. A longitudinal residual equal to zero verifies the particular contraction shown, not every possible Ward identity.

Use `?GravitonVertex`, `?GravitonScalarVertex`, `?TID`, or the corresponding function name to see usage information. `FeynGravCommands[]` lists the package interface.

## Conventions and rerunning

The notebooks retain the Feynman-rule factors of `i` and `κ`. Example 08 now obtains its diagram weights from native FeynArts: the two-graviton bubble carries `1/2`, and the two triangles and each mixed bubble carry `1`. Its five source-rule products, loop routes and weights were checked exactly with symbolic `D`. This does not validate every loop process supported by the model; other manually constructed examples need their own combinatorial checks. Loop results require attention to the stated integration normalisation before comparison with a different convention. Replacing `D` by 4 in tensor coefficients is not ultraviolet subtraction or a Laurent expansion of the master integrals.

Run each notebook in a fresh kernel: `SetMandelstam`, explicit scalar-product definitions, and `$Assumptions` persist in a session. After changing kinematics or a diagram, reevaluate its dependent cells. The examples use named intermediate expressions rather than `%` or `%%`, so inserting a display cell does not change later comparisons.

For performance measurements use the separate [Benchmark notebooks](../Benchmarks/README.md). These examples emphasise how to construct and interpret calculations.

The updated loop examples extract the pure-gravity and massive-fermion coefficient forms from the current rules rather than asserting the older hard-coded formulas. Their reconstruction checks verify algebraic decomposition, not an independent physical reference. Other reference comparisons are retained where they agree with the computed expressions.

All algebra previously performed through the legacy Calc chain now uses `FeynGravSimplify`. FeynCalc propagator shorthand is expanded with `Explicit`. In examples 01–11, fermion and colour expressions first use FeynCalc’s dedicated algebra functions (including `DiracOrder -> True` for open chains); `Collect` passes their bosonic coefficients to FORM. Examples 05–06 and 08 use explicit FeynCalc `TID` steps for their bubble/vertex reductions. The tadpole calls in 05–06 and the loop reductions in 07 instead declare `LoopMomenta -> {k}` to `FeynGravSimplify`; its enabled tensor decomposition then returns integral coefficients. Calls on those already-reduced coefficients leave `LoopMomenta` empty.

This preprocessing describes the notebooks as currently written. It is not a requirement to remove all Dirac or colour structures before using the converter: supported chains, traces and colour tensors can now be processed directly. See the [converter vocabulary](../Services/Calculation/Guide.md#Supported%20vocabulary). For examples 01–11, the command migration preserved saved outputs without recalculating them with the new entry point; the table above gives each notebook’s status. Examples 12–14 have separately recorded kernel evaluations.

### Example 08: scope of the native one-loop construction

Example 08 loads `Single-Scalar-Tensor-Minimal-Coupling` and sets `ScalarLambda3` and `ScalarLambda4` to zero by replacement, retaining the scalar mass. It excludes potential field points and reducible graphs, draws five diagrams, and groups them into V1–V4 without depending on diagram numbering. Both mixed bubbles are already present; no extra scalar-leg exchange is added.

The model omits the order-κ³ `hhhφφ` vertex. Closing two of its graviton legs gives a massless tadpole with a polynomial numerator, which is scaleless and vanishes in dimensional regularisation. Thus the example covers the non-vanishing minimal 1PI correction under this prescription, rather than the complete order-κ³ vertex inventory. Its κ² cutoff is per vertex, not per diagram.

The native construction and comparisons establish only the partial scope listed above ([scientific scope](../ScientificScope.md#Examples%20and%20saved%20outputs)). The subsequent off-shell reduction, Package-X poles and infrared analysis were preserved but not rerun as part of that construction check. Their results must be recalculated with the corrected bubble weight; this check does not establish a complete Ward identity or a renormalised observable.

<a name="Syntax and interface audit"></a>

### Syntax and interface audit

Earlier notebooks needed repairs to the `B0` collection patterns in example 07, the gravity-only contribution in example 09 and checks that assigned to `Contract` in example 11. Restart the kernel if the old assignment cell in example 11 was evaluated. Saved outputs were preserved; rerun affected presentation/check cells before relying on them. This syntax/interface audit did not rerun the complete loop calculations. See [example scope](../ScientificScope.md#Examples%20and%20saved%20outputs).

### Example 13: massive quark graviton emission

The existing SU(N) bundle now contains one quark flavour with arbitrary mass.
Example 13 uses `FeynGravSimplify` directly for the supported open Dirac matrix,
Lorentz and colour algebra. Spinors are attached afterwards. All fourteen input
cells were evaluated; both Ward checks and the coupling limits return `True`.
D/N/m remain symbolic. The notebook retains generated diagram graphics and
outputs; kernel/box validation does not certify a Mathematica front-end session.
These are recorded checks of the delivered notebook, not a fresh run against subsequent package changes. See [scientific scope](../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions).

### Example 14: four gravitons in quadratic gravity

Evaluate the five sections in order in a fresh kernel. Evaluate section 1 as a group, including its collapsed FORM helper subsection. All helper definitions are inside the notebook. FeynArts generates the contact and three exchanges with the routing of example 04. Section 4 sets the external massless momentum and polarisation products. Section 5 applies them inside each FORM contraction to reduce peak RAM before the result is imported. It prints the bytes written for each diagram. The final cells explain why the complete massless four-graviton tree equals GR at finite masses and link to the published calculation. They set $D=4$ and compare three helicity amplitudes with example 04, keeping both mass parameters symbolic. See the [quadratic-gravity model guide](../Models/QuadraticGravity/NativeModel.md) for the internal poles and the Einstein limit.
