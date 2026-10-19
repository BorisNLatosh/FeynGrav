## Adding higher-order graviton vertices

[Model overview](README.md) · [Model guide](ModelGuide.md)

The order refers to the power of κ **in each vertex**. A pure-graviton vertex with `r` legs has order `n = r - 2` and is stored in `Libs/GravitonVertex_n`. Thus orders 1, 2 and 3 supply three-, four- and five-graviton vertices. Keeping vertices through a given order does not restrict the total coupling order of diagrams assembled from them.

Extend the source libraries and rebuild the matching generic/classes pair; do not append a tensor expression to `GR.mod` alone. The builder translates each source rule into a kinematic vector in `GR.gen` and its aligned coefficient vector in `GR.mod`, including permutation closure.

### Five gravitons: the existing order-three builder

1. **Check the source inventory.** Orders `GravitonVertex_1`, `_2` and `_3` must all be installed in `Libs`. This checkout already contains all three. Their presence alone does not establish independent mathematical validation of the five-graviton rule. If a library is missing, generate it in a separate fresh kernel using `Libs/Generator.wl` and `GenerateGravitonVertexSpecific[3, OutputDirectory -> absoluteTemporaryDirectory]`. The destination is a string naming a temporary directory. Validate the generated rule before installing it in `Libs`; do not overwrite existing libraries merely to build a model. See the [generator guide](../../../Libs/Generator.md).
2. **Build a separate candidate.** In a fresh kernel, load patched FeynArts, FeynGrav and the integration companion, in that order. Keep `D` and κ unassigned and `GaugeFixingEpsilon = 2`. Do not initialise the shipped model first. For example, after replacing the two absolute paths:

   ```wl
   $LoadAddOns = {"FeynArts"};
   << FeynGrav`;
   Get["/absolute/path/to/FeynGrav/FeynArtsIntegration/FeynArtsIntegration.wl"];
   candidate = FeynGravBuildModel["/absolute/existing/parent/GR-order-3",
     GravitonOrder -> 3];
   ```

   The parent directory must exist and the destination must not exist. The builder imports the requested orders itself, verifies the recorded inventory, writes `GR.gen`, `GR.mod` and `manifest.json`, and attempts native initialisation before publishing the candidate directory. Stop on a `Failure` and inspect its diagnostics. Do not replace the supplied bundle at this stage.
3. **Inspect the candidate inventory.** Its manifest must report `GravitonOrders -> {1,2,3}` and families `TTT`, `TTTT`, `TTTTT`, `TUU`. Check the source and model hashes, gauge and dimension. The accepted negative ghost propagator and existing ghost coupling must remain intact; increasing the pure-graviton order does not require inventing additional ghost contacts for this fixed gauge.
4. **Load the intended candidate in another fresh kernel.** Prepend its absolute directory to `FeynArts`$ModelPath` before calling `InitializeModel["GR", GenericModel -> "GR"]`, using the qualified FeynArts names shown above. The builder still calls the model `GR`: the candidate directory name does not create a new model identifier. Path precedence and a fresh kernel prevent accidental reuse of the shipped model.
5. **Verify the new contact and complete trees.** Compare the native five-graviton contact, converted with `FCFAConvert`, with the source `GravitonVertex` using five incoming momentum/index triples. Check both indices on every leg, momentum routing, κ³, permutation symmetry and normalisation. For `2 -> 3`, request `Adjacencies -> {3,4,5}` and check that the five-leg contact and every required exchange channel survive insertion. Compare topology identities and field assignments, not only counts. FeynArts can silently omit a topology with an absent coupling. Native construction is required here: the optional bridge's process allowlist does not currently admit `2 -> 3`.
6. **Publish only after review.** Retain exact source-to-model comparisons, an independent check of the new source rule, warnings and resource outcomes. A successful build or insertion inventory is not proof of the full amplitude. Replace the generic file, classes file and manifest together only after the relevant checks pass, and update this README's scope and provenance. Existing lower-order verification and deferred loop checks do not automatically validate the enlarged model.

Order three is an available development path, **not part of this release's accepted tree scope**. Historical five-graviton initialisation took about 486 seconds and a 6.07 GB peak; constructing the full five-point amplitude did not finish within 900 seconds. These are single-machine observations, not resource guarantees. The builder has a 600-second initialisation timeout, but no whole-process memory cap; use an explicitly chosen external resource limit before attempting this expensive build.

### Six or more gravitons: extend the builder first

The current public builder accepts only `GravitonOrder -> 2` or `3`. Supplying a larger value is not sufficient, even if the corresponding library exists. For a maximum order `n >= 4`:

- Generate and independently validate the required source libraries through `GravitonVertex_n`, retaining all lower orders and the same gauge and normalisation.
- Extend [ModelBuilder.wl](../../../FeynArtsIntegration/ModelBuilder.wl): the accepted order values, `faOrderSpecification` family names and basis labels, and validation of placeholder indices/momenta in `faRoleFreeQ`. The latter currently checks only the slots needed through five legs. Audit the remaining translation and closure machinery for assumptions about maximum valence; changing the option validator alone would leave incomplete metadata and checks.
- Update public usage messages and option diagnostics. If the optional bridge is to accept the new snapshot, extend its manifest validation in `Diagram.wl` and review its separate process restrictions in `Process.wl`. Do not widen process support merely because the model now contains another contact, and do not refresh loop-verification pins without loop evidence.
- Build into a fresh candidate directory. Verify reconstruction and native permutation closure at each new valence, unchanged lower-order rules and ghost conventions, and complete representative contact/exchange channels with all required adjacencies through `n + 2`. Include a missing-contact test that must be rejected as incomplete.
- Record what was actually checked and any uncompleted calculations, then publish the matched bundle and documentation together. Keep the source libraries as the rule source rather than maintaining independent handwritten copies in the model files.
