# Learning FeynGrav through examples

Start with a fresh Mathematica kernel for each notebook and evaluate its input cells from top to bottom with **Shift+Enter**. Opening a notebook does not run calculations. The package and its dependencies must already be installed; see the [main README](../Documentation/UserGuide.md#installation-and-help).

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
| 07 | [07. Graviton_Self_Energy_Matter_Contribution.nb](07.%20Graviton_Self_Energy_Matter_Contribution.nb) | Scalar, fermion, vector and Yang–Mills loops in the graviton two-point function | None |
| 08 | [08. Graviton_Scalar_Vertex_One_Loop.nb](08.%20Graviton_Scalar_Vertex_One_Loop.nb) | Vertex construction, consistency checks, ultraviolet poles and infrared behaviour | FeynHelpers and Package-X |
| 09 | [09. Scalar_Scattering_Tree_Level_FeynArts.nb](09.%20Scalar_Scattering_Tree_Level_FeynArts.nb) | Seven scalar-scattering diagrams, on-shell kinematics and the massive amplitude with independent cubic/quartic couplings | FeynCalc-patched FeynArts |
| 10 | [10. Scalar_Scattering_John_Tree_Level_FeynArts.nb](10.%20Scalar_Scattering_John_Tree_Level_FeynArts.nb) | John scalar scattering, three graviton exchanges, and minimal/mixed/John amplitude contributions | FeynCalc-patched FeynArts |
| 11 | [11. Scalar_Annihilation_EsGB_Tree_Level_FeynArts.nb](11.%20Scalar_Annihilation_EsGB_Tree_Level_FeynArts.nb) | Six scalar–scalar to graviton–graviton diagrams and on-shell polarisation-dependent amplitude; helicity amplitude and two generic graviton Ward checks using the corrected model | FeynCalc-patched FeynArts |

Example 09 can be read immediately after example 04: it gives the scalar counterpart of the native FeynArts workflow, with incoming momenta `p1`, `p2` and outgoing momenta `p3`, `p4`. It displays the contact, scalar-exchange and graviton-exchange contributions to `M` after removing the overall `I` from the diagram result.

The two larger matter notebooks contain separate field sectors. Begin with the scalar sector before moving to fermions or Yang–Mills theory. One-loop reduction and symbolic phase-space integration can take appreciably longer than the small projector checks.

## Reading the code

- **Vertices and propagators:** FeynGrav supplies the Feynman rules. Vertex momenta are incoming; repeated indices are contracted. Read the local notation paragraph before adapting signs or external legs.
- **`CalcFormCalculate`:** sends supported Lorentz, epsilon, Dirac and fundamental SU(N) colour algebra to FORM, then imports the result. Automatic selection prefers up to eight TFORM workers, capped by the processor count, with serial fallback when TFORM is missing. This function does not integrate loops.
- **`TID[..., k, ToPaVe -> True]`:** FeynCalc performs tensor-integral reduction with loop momentum `k`, leaving scalar Passarino–Veltman functions such as `A0` and `B0`.
- **`FeynAmpDenominatorExplicit`:** rewrites propagator denominators as ordinary expressions, useful for tree-level algebra and projector identities.
- **Semicolons:** suppress long intermediate output. Evaluate a stored expression's name separately to inspect it. `AbsoluteTiming` measures elapsed wall time, including external FORM execution where used.
- **Checks:** `True` confirms the displayed equality; `False` means it did not match. An unevaluated equality is unresolved. A longitudinal residual equal to zero verifies the particular contraction shown, not every possible Ward identity.

Use `?GravitonVertex`, `?GravitonScalarVertex`, `?TID`, or the corresponding function name to see usage information. `FeynGravCommands[]` lists the package interface.

## Conventions and rerunning

The notebooks retain the displayed diagram weights and Feynman-rule factors of `i` and `κ`. They do not derive the combinatorial factors. Loop results require attention to the stated integration normalisation before comparison with a different convention. Replacing `D` by 4 in tensor coefficients is not ultraviolet subtraction or a Laurent expansion of the master integrals.

Run each notebook in a fresh kernel: `SetMandelstam`, explicit scalar-product definitions, and `$Assumptions` persist in a session. After changing kinematics or a diagram, reevaluate its dependent cells. The examples use named intermediate expressions rather than `%` or `%%`, so inserting a display cell does not change later comparisons.

For performance measurements use the separate [Benchmark notebooks](../Benchmark/UserGuide.md). These examples emphasise how to construct and interpret calculations.

The updated loop examples extract the pure-gravity and massive-fermion coefficient forms from the current rules rather than asserting the older hard-coded formulas. Their reconstruction checks verify algebraic decomposition, not an independent physical reference. Other reference comparisons are retained where they agree with the computed expressions.

All algebra previously performed through the legacy Calc chain now uses `CalcFormCalculate`. FeynCalc propagator shorthand is expanded with `Explicit`. In these notebooks, fermion and colour expressions first use FeynCalc’s dedicated algebra functions (including `DiracOrder -> True` for open chains); `Collect` passes their bosonic coefficients to FORM. Loop integration and tensor reduction remain in FeynCalc.

This preprocessing describes the notebooks as currently written. It is not a requirement to remove all Dirac or colour structures before using the converter: supported chains, traces and colour tensors can now be processed directly. See the [converter vocabulary](../CalcFormConverter/UserGuide.md#supported-vocabulary). The notebooks themselves are unchanged by this documentation update.
