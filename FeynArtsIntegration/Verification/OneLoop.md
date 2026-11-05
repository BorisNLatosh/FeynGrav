# One-loop verification for native FeynArts models

## First calculation

Use the shipped conventional `GR` model to compute the one-loop, one-particle-irreducible graviton two-point function at symbolic dimension `D` and generic off-shell external momentum (`p² ≠ 0`). The tree checks and the old bridge fingerprint cover different evidence. The corrected shipped model currently returns `GhostCompletenessGate` from the optional bridge. Use native FeynArts for the initial comparison and retain this safeguard throughout validation.

1. Pin the model files, manifest and source rules. Generate the complete two-point diagram inventory. Preserve the quartic graviton tadpole, the two-cubic graviton bubble and the oriented Faddeev–Popov ghost bubble, including the massless tadpole before scaleless integration.
2. Independently assemble all three integrands from FeynGrav rules. Check loop routing up to legitimate loop-momentum shifts, free and dummy indices, propagator signs, the closed ghost-loop minus sign, symmetry factors and powers of `κ`. Record the FeynArts and conversion prefactors explicitly. Compare each graph before summing. An omitted graph and a deliberately reversed ghost sign must be detected.
3. State the loop measure and dimensional-regularisation convention. Use `FeynGravSimplify` for contraction and scalar reduction, then compare the summed result with an independent calculation in the same gauge and conventions. Distinguish an identically zero integrand from a scaleless integral that vanishes after integration. Record any unreduced masters. Check the appropriate gravitational Slavnov–Taylor or Ward identity, including any lower-point or ghost terms it requires.
4. Retain exact residuals, negative controls, diagnostics, elapsed times and peak process-tree memory from bounded fresh kernels. Only after the independent checks pass, update the bridge fingerprint for the verified snapshot and repeat the case through the public bridge. Verify that a changed, unvalidated snapshot still fails before diagram generation.

## Coverage beyond pure GR

Inventory the remaining native bundles separately: the minimally coupled scalar with potential, John, Einstein–scalar–Gauss–Bonnet, SU(N) Yang–Mills with one massive quark flavour, and quadratic gravity. For each, select a representative one-loop observable and identify its required vertices, gauge fixing, ghost sectors and independent reference. Record which model and process have passed, and any missing interaction or unresolved comparison. A successful pure-GR calculation establishes only that model and process.

This is a verification plan. The public guide describes the currently available [loop interface and its limits](../../Documentation/Services/FeynArtsIntegration/Guide.md).
