# Extending the GR + SU(N) model beyond κ²

[Model guide](ModelGuide.md)

The supplied files are a verified finite snapshot. Follow this sequence to add
higher orders; merely importing a larger library does not extend the model.

1. **Derive the inventory from the same action.** At target order K, include
   pure GR valences through K+2, and h^n AA, h^n AAA, h^n AAAA, h^n ghost-antighost
   and h^n A ghost-antighost at every allowed n through K. Also include
   h^n qbar q (n≥1) and h^n qbar q A (n≥0), with the same single massive
   flavour and no invented qbar q AA or quark–ghost contacts. Reassess the FP operator
   if changing either gauge; preserve the distinction between vector gravitational
   ghosts and scalar YM ghosts. Include contacts with more than six legs when
   required. A per-vertex cutoff is not an amplitude-order cutoff.
2. **Verify sources before generating libraries.** Read
   `Rules/GravitonSUNYM.wl`, `Rules/GravitonFermionVertex.wl`,
   `Rules/Contracts.md` and `Libs/Generator.md`. Independently expand the Dirac
   kinetic, mass and torsion-free spin-connection terms; preserve ordered gamma
   products. Apply the full source-to-native Fourier map from the model guide,
   including all graviton momenta, rather than fitting signs to a Ward test.
   For n gravitons the cubic/quartic gluon rules must contain κ^n, not merely
   graviton symmetrisation factorials. Independently differentiate the action,
   including covariant gauge fixing and the two ghost determinants, with labelled
   identical legs. Compare at generic momenta and symbolic D/N. Generate only
   affected files in a fresh destination/kernel under an explicit resource budget;
   retain old files until the comparison succeeds. Orders above two have not
   been validated by the present repair.
3. **Extend the private builder deliberately.**
   `FeynArtsIntegration/YangMillsModelBuilder.wl` reuses `Basis.wl` and
   `ModelBuilder.wl` for translation, basis reconstruction and encoding.
   `FermionModelBasis.wl` handles ordered Dirac words. Its present one/three-gamma
   support is finite: derive any new structures and their flipping rules before
   extending it. Keep `F` for quarks, `S` for vector GR ghosts and `U` for YM ghosts. The builder's
   family list, imports, power guards and `ymDensity` derivatives are currently
   fixed to orders 0–2. Extend all these together, with the manifest inventory,
   provenance and cutoff. Preserve the independently verified quartic colour
   reconstruction and matched `CloseKinematicVector`/`CloseCouplingVector` calls.
   Do not silently truncate an unavailable source or compensate for a source
   defect in a generated coupling. Measure closure growth before a large build.
4. **Build into a new directory.** The current explicit entry point is:

   ```sh
   FG_MODEL_TARGET=/absolute/new/candidate \
     WolframKernel -noinit -script FeynArtsIntegration/BuildGRSUNYM.wl
   ```

   This command alone does not enforce resource limits: use a reviewed external
   whole-process-tree limiter. The script rejects an existing destination.
   The tested entry point supplies the target through `FG_MODEL_TARGET`. Building
   does not install, initialise or certify the model.
5. **Validate before replacing the bundle.** Use fresh kernels and matching
   generic/classes files. Check exact source reconstruction, field/colour sums,
   permutation symmetry, coupling powers and ghost statistics. Construct and
   convert every new contact using sufficient `Adjacencies`. Check complete
   species-labelled exchange channels and Ward identities with symbolic D/N,
   plus a missing-contact negative control. Keep routing, weights and gauge
   parameters explicit. The portable selected-tree check is described in
   `FeynArtsIntegration/Verification/README.md`; new orders need new tests.
6. **Publish a matched, documented snapshot.** Replace `.gen`, `.mod` and
   `manifest.json` together only after review. Update this model's guide,
   verification limits and common catalogue, and restart kernels. Preserve the
   other model bundles. Neither higher-order tree verification nor native loop
   generation grants loop-validation status.
