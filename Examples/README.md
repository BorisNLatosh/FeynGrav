# Learning FeynGrav through examples

Start with a fresh Mathematica kernel for each notebook and evaluate its input cells from top to bottom with **Shift+Enter**. Opening a notebook does not run calculations. The package and its dependencies must already be installed; see the [installation guide](../Documentation/UserGuide.md#installation-and-help).

These examples target the [unreleased FeynGrav 4.2.0](../Documentation/ReleaseNotes-4.2.0.md) and use `FeynGravSimplify`, whose default `Method -> Automatic` selects the full FORM pipeline. The command is not available in published FeynGrav 4.1.0.

## Numbered examples and suggested reading order

The numbers define the suggested reading order, from operator algebra and tree-level scattering to loop calculations. All notebooks require FeynGrav and FeynCalc. Examples 01–08 also use FORM; TFORM is optional. Examples 09–11 evaluate their tree tensors directly with FeynCalc. Additional tools are listed below.

| No. | Notebook | What you learn | Additional requirements |
| --- | --- | --- | --- |
| 01 | [01. Nieuwenhuizen_Operators.nb](01.%20Nieuwenhuizen_Operators.nb) | Tensor indices, propagator denominators, completeness and algebraic checks | None |
| 02 | [02. Scalars_Gravitational_Scattering_Tree_Level.nb](02.%20Scalars_Gravitational_Scattering_Tree_Level.nb) | Exchange amplitude, Mandelstam variables, differential cross section and nonrelativistic limit | None |
| 03 | [03. Graviton_Scattering_Tree_Level.nb](03.%20Graviton_Scattering_Tree_Level.nb) | Manual construction, helicity amplitudes, contact terms and longitudinal checks | None |
| 04 | [04. Graviton_Scattering_Tree_Level_FeynArts.nb](04.%20Graviton_Scattering_Tree_Level_FeynArts.nb) | Native construction of the complete 2 → 2 tree, off-shell tensors and on-shell helicity comparisons | FeynCalc-patched FeynArts |
| 05 | [05. Graviton_Self_Energy.nb](05.%20Graviton_Self_Energy.nb) | Graviton and ghost loops, tensor-integral reduction and projector representation | None |
| 06 | [06. Matter_Self_Energy_Graviton_Contribution.nb](06.%20Matter_Self_Energy_Graviton_Contribution.nb) | Matter two-point functions with gravitational loop corrections | None |
| 07 | [07. Graviton_Self_Energy_Matter_Contribution.nb](07.%20Graviton_Self_Energy_Matter_Contribution.nb) | Fermion, massive-vector, massless-vector and Yang–Mills loops in the graviton two-point function | None |
| 08 | [08. Graviton_Scalar_Vertex_One_Loop.nb](08.%20Graviton_Scalar_Vertex_One_Loop.nb) | Native construction of five minimal scalar–graviton 1PI diagrams, verified routing and weights, ultraviolet poles and infrared analysis | FeynCalc-patched FeynArts, FeynHelpers and Package-X |
| 09 | [09. Scalar_Scattering_Tree_Level_FeynArts.nb](09.%20Scalar_Scattering_Tree_Level_FeynArts.nb) | Seven scalar-scattering diagrams, on-shell kinematics and the massive amplitude with independent cubic/quartic couplings | FeynCalc-patched FeynArts |
| 10 | [10. Scalar_Scattering_John_Tree_Level_FeynArts.nb](10.%20Scalar_Scattering_John_Tree_Level_FeynArts.nb) | John scalar scattering, three graviton exchanges, and minimal/mixed/John amplitude contributions | FeynCalc-patched FeynArts |
| 11 | [11. Scalar_Annihilation_EsGB_Tree_Level_FeynArts.nb](11.%20Scalar_Annihilation_EsGB_Tree_Level_FeynArts.nb) | Six scalar–scalar to graviton–graviton diagrams and on-shell polarisation-dependent amplitude; helicity amplitude and two generic graviton Ward checks using the corrected model | FeynCalc-patched FeynArts |

Example 09 can be read immediately after example 04: it gives the scalar counterpart of the native FeynArts workflow, with incoming momenta `p1`, `p2` and outgoing momenta `p3`, `p4`. It displays the contact, scalar-exchange and graviton-exchange contributions to `M` after removing the overall `I` from the diagram result.

The two larger matter notebooks contain separate field sectors. Example 06 begins with the scalar sector; example 07 begins with fermions, then covers massive vectors, massless vectors and Yang–Mills theory. One-loop reduction and symbolic phase-space integration can take appreciably longer than the small projector checks.

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

For performance measurements use the separate [Benchmark notebooks](../Benchmark/UserGuide.md). These examples emphasise how to construct and interpret calculations.

The updated loop examples extract the pure-gravity and massive-fermion coefficient forms from the current rules rather than asserting the older hard-coded formulas. Their reconstruction checks verify algebraic decomposition, not an independent physical reference. Other reference comparisons are retained where they agree with the computed expressions.

All algebra previously performed through the legacy Calc chain now uses `FeynGravSimplify`. FeynCalc propagator shorthand is expanded with `Explicit`. In these notebooks, fermion and colour expressions first use FeynCalc’s dedicated algebra functions (including `DiracOrder -> True` for open chains); `Collect` passes their bosonic coefficients to FORM. Examples 05–06 and 08 use explicit FeynCalc `TID` steps for their bubble/vertex reductions. The tadpole calls in 05–06 and the loop reductions in 07 instead declare `LoopMomenta -> {k}` to `FeynGravSimplify`; its enabled tensor decomposition then returns integral coefficients. Calls on those already-reduced coefficients leave `LoopMomenta` empty.

This preprocessing describes the notebooks as currently written. It is not a requirement to remove all Dirac or colour structures before using the converter: supported chains, traces and colour tensors can now be processed directly. See the [converter vocabulary](../CalcFormConverter/UserGuide.md#supported-vocabulary). The command migration preserves saved outputs; these outputs have not been recalculated with the new entry point.

### Example 08: scope of the native one-loop construction

Example 08 loads `Single-Scalar-Tensor-Minimal-Coupling` and sets `ScalarLambda3` and `ScalarLambda4` to zero by replacement, retaining the scalar mass. It excludes potential field points and reducible graphs, draws five diagrams, and groups them into V1–V4 without depending on diagram numbering. Both mixed bubbles are already present; no extra scalar-leg exchange is added.

The model omits the order-κ³ `hhhφφ` vertex. Closing two of its graviton legs gives a massless tadpole with a polynomial numerator, which is scaleless and vanishes in dimensional regularisation. Thus the example covers the non-vanishing minimal 1PI correction under this prescription, rather than the complete order-κ³ vertex inventory. Its κ² cutoff is per vertex, not per diagram.

The native construction and comparisons are recorded in [Example 08 verification](../Documentation/Verification/Example08NativeFeynArts-2026-10-23.md). The subsequent off-shell reduction, Package-X poles and infrared analysis were preserved but not rerun during this migration. Their results must be recalculated with the corrected bubble weight; this check does not establish a complete Ward identity or a renormalised observable.

### Syntax and interface audit

All 11 notebooks (274 executable cells) were audited on 25 October 2026. The
repairs concern four `B0[__]` collection patterns in 07, the gravity-only
contribution in 09, and equality checks instead of assignments to `Contract` in
11. Restart the kernel if the old assignment cell in 11 was evaluated. Saved
outputs are preserved; rerun affected presentation/check cells before relying
on them. This audit did not rerun the complete loop calculations. See the
[verification ledger](../Documentation/Verification/ExamplesSyntax-2026-10-25.md)
for exact checks, notebook identities and front-end verification limits.
