# Learning FeynGrav through examples

Start with a fresh Mathematica kernel for each notebook and evaluate its input cells from top to bottom with **Shift+Enter**. Opening a notebook does not run calculations. The package and its dependencies must already be installed; see the [main README](../README.md#installation-and-help).

## Suggested reading order

| Notebook | What you learn | Additional requirements |
| --- | --- | --- |
| [Nieuwenhuizen_Operators.nb](Nieuwenhuizen_Operators.nb) | Tensor indices, propagator denominators, spin projectors and algebraic checks | FORM (TFORM optional) |
| [Scalars_Gravitational_Scattering_Tree_Level.nb](Scalars_Gravitational_Scattering_Tree_Level.nb) | Constructing an exchange amplitude, Mandelstam variables, differential cross sections and the nonrelativistic limit | FORM (TFORM optional) |
| [Graviton_Scattering_Tree_Level.nb](Graviton_Scattering_Tree_Level.nb) | Joining vertices, permuting external legs, helicity amplitudes, contact terms and longitudinal checks | FORM; TFORM is optional |
| [Graviton_Self_Energy.nb](Graviton_Self_Energy.nb) | Graviton and ghost loops, FORM contraction, tensor-integral reduction and a projector representation | FORM; TFORM is optional |
| [Matter_Self_Energy_Graviton_Contribution.nb](Matter_Self_Energy_Graviton_Contribution.nb) | Gravitational loop corrections to matter two-point functions | FORM (TFORM optional) |
| [Graviton_Self_Energy_Matter_Contribution.nb](Graviton_Self_Energy_Matter_Contribution.nb) | Matter loops in the graviton two-point function, including Dirac and colour algebra | FORM (TFORM optional) |
| [Graviton_Scalar_Vertex_at_First_Loop.nb](Graviton_Scalar_Vertex_at_First_Loop.nb) | A complete one-loop vertex workflow, scalar integrals, ultraviolet poles and a momentum-transfer limit | FORM, FeynHelpers and Package-X |

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

For performance measurements use the separate [Benchmark notebooks](../Benchmark/README.md). These examples emphasise how to construct and interpret calculations.

The updated loop examples extract the pure-gravity and massive-fermion coefficient forms from the current rules rather than asserting the older hard-coded formulas. Their reconstruction checks verify algebraic decomposition, not an independent physical reference. Other reference comparisons are retained where they agree with the computed expressions.

All algebra previously performed through the legacy Calc chain now uses `CalcFormCalculate`. FeynCalc propagator shorthand is expanded with `Explicit`. In these notebooks, fermion and colour expressions first use FeynCalc’s dedicated algebra functions (including `DiracOrder -> True` for open chains); `Collect` passes their bosonic coefficients to FORM. Loop integration and tensor reduction remain in FeynCalc.

This preprocessing describes the notebooks as currently written. It is not a requirement to remove all Dirac or colour structures before using the converter: supported chains, traces and colour tensors can now be processed directly. See the [converter vocabulary](../CalcFormConverter/README.md#supported-vocabulary). The notebooks themselves are unchanged by this documentation update.
