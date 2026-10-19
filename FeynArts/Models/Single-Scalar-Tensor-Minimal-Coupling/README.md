# GR with a massive scalar and cubic/quartic potential

This native FeynArts model contains one real scalar of arbitrary symbolic mass and vertices through kappa squared per vertex. The action is

$$S=\int d^4x\sqrt{-g}\left[-2R/\kappa^2+\tfrac12(\partial\phi)^2-\tfrac12m^2\phi^2+\lambda_3\phi^3/3!+\lambda_4\phi^4/4!\right]+S_{gf}+S_{FP}.$$

Use ``FeynGrav`ScalarMass``, ``FeynGrav`ScalarLambda3`` and ``FeynGrav`ScalarLambda4`` as unassigned symbols. Specialise converted amplitudes by replacement. Both potential signs are positive in the action. The mass term is already included in the propagator and minimal gravitational couplings.

## Loading and calculation

In a fresh kernel:

```wl
$LoadAddOns = {"FeynArts"};
Needs["FeynGrav`"];
FeynArts`InitializeModel["Single-Scalar-Tensor-Minimal-Coupling",
  FeynArts`GenericModel -> "Single-Scalar-Tensor-Minimal-Coupling"];
top = FeynArts`CreateTopologies[0, 2 -> 2,
  FeynArts`Adjacencies -> {3,4,5,6}];
ins = FeynArts`InsertFields[top,
  {FeynArts`S[1],FeynArts`S[1]} -> {FeynArts`S[1],FeynArts`S[1]},
  FeynArts`Model -> "Single-Scalar-Tensor-Minimal-Coupling",
  FeynArts`GenericModel -> "Single-Scalar-Tensor-Minimal-Coupling",
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
raw = FeynArts`CreateFeynAmp[ins, FeynArts`PreFactor -> 1,
  FeynArts`GaugeRules -> {}, FeynArts`MomentumConservation -> True,
  FeynArts`Truncated -> True];
diagrams = FeynCalc`FCFAConvert[raw,
  FeynCalc`IncomingMomenta -> {p1,p2}, FeynCalc`OutgoingMomenta -> {q1,q2},
  FeynCalc`LorentzIndexNames -> {mu,nu,rho,sigma,a,b,c,d},
  FeynCalc`ChangeDimension -> D, FeynCalc`Contract -> False,
  List -> True, FeynCalc`Prefactor -> 1];
```

There are seven scalar 2-to-2 diagrams: the quartic contact, three scalar exchanges and three graviton exchanges. The model files are standalone: alternatively register this folder in FeynArts`$ModelPath after loading FeynCalc-patched FeynArts, without loading FeynGrav.

## Inventory and limits

The inherited families are TTT, TTTT, TUU, TSS and TTSS. Additional families are SSS, TSSS, TTSSS, SSSS, TSSSS and TTSSSS. Consequently five- and six-leg contacts are required despite the kappa-squared cutoff. Restricting topology adjacency to three and four omits these contacts in higher-multiplicity processes. The cutoff is per vertex, not total amplitude order.

GaugeFixingEpsilon is fixed at 2; the gravitational ghost propagator is negative imaginary with native Grassmann signs. The physical action is four-dimensional; symbolic D is its dimensional-regularisation continuation. External-state conditions and physical on-shell kinematics are not imposed by generation. No loops have been validated. Native scalar 1-to-3 construction is an algebraic crossing check, not a physical decay of one massive scalar to three identical massive scalars.

The six potential contacts passed exact action comparisons; 15 native checks passed. Unchanged minimal gravity factors reuse Single-Scalar-Tensor-Minimal-Coupling evidence. Full crossed gravitational-amplitude equality and on-shell Ward checks were not newly tested. See [verification](../../../Documentation/Verification/ScalarModelExtensions.md). Use the explicit developer script `FeynArtsIntegration/BuildSingleScalarTensorMinimalCoupling.wl` with a fresh absolute output directory to rebuild; normal model use needs no build or FORM process.

## Minimal-scalar limit and naming

This single bundle replaces both former `GRScalar` and `GRScalarPotential` models. It contains an arbitrary scalar mass and independent cubic and quartic couplings. Set ``FeynGrav`ScalarLambda3`` and ``FeynGrav`ScalarLambda4`` to zero by replacement after conversion to recover the massive minimally coupled scalar theory without self-interactions. No separate minimal-only bundle is supplied. Its model name is a string, including the hyphens; always quote it.

## Adding higher orders in κ

The supplied model is complete through **κ² per vertex**. Extending it means increasing the number of gravitons attached to the existing interactions; it does not introduce new scalar powers or couplings. The scalar potential remains λ₃φ³/3! + λ₄φ⁴/4!, with an arbitrary scalar mass.

For a maximum vertex order `N`, retain every lower order and include:

| Sector | Required vertices | Coupling order | Source |
| --- | --- | --- | --- |
| Pure gravity | `h^(n+2)`, `1 <= n <= N` | κⁿ | `GravitonVertex_n` |
| Scalar kinetic and mass terms | `h^n phi^2`, `1 <= n <= N` | κⁿ | `GravitonScalarVertex_n` |
| Cubic scalar potential | `h^n phi^3`, `0 <= n <= N` | κⁿλ₃ | `GravitonScalarPotentialVertex_n` for `n >= 1` |
| Quartic scalar potential | `h^n phi^4`, `0 <= n <= N` | κⁿλ₄ | The same potential tensor for `n >= 1` |
| Gravitational ghosts | Existing `h antighost ghost` | κ | Existing fixed-gauge ghost rule |

The zero-graviton potential rules remain `I ScalarLambda3` and `I ScalarLambda4`. The scalar mass is already included in the scalar propagator and its gravitational vertices; do not add a separate mass insertion. Preserve the negative ghost propagator and the current linear gauge-fixing convention, which needs no additional multi-graviton ghost contacts. The pure-gravity order counts `n + 2` legs, whereas the scalar-library order counts `n` graviton legs.

For example, extending from κ² to κ³ adds the five-graviton vertex and `TTTSS`, `TTTSSS`, `TTTSSSS`. The last of these has **seven legs**. A vertex-order cutoff is not a total diagram-order cutoff: several allowed vertices may produce higher powers of κ in an amplitude.

### 1. Prepare and verify the source libraries

For every missing order `n`, use the [library generator](../../../Libs/Generator.md) in a separate fresh kernel:

```wl
Get["/absolute/path/to/FeynGrav/Libs/Generator.wl"];
GenerateGravitonVertexSpecific[n, OutputDirectory -> temporaryDirectory];
GenerateGravitonScalarsSpecific[n, OutputDirectory -> temporaryDirectory];
```

Here `n` must be an explicit positive integer and `temporaryDirectory` an absolute directory path. The second command produces both `GravitonScalarVertex_n` and `GravitonScalarPotentialVertex_n`. Generate into temporary storage, inspect failures and verify the expressions before installing accepted files in `Libs`. Keep existing files and their provenance until the replacements have been accepted. Generator and main-package/model workflows should use separate fresh kernels.

Check the potential tensor against the differentiated expansion of `Sqrt[-Det[g]]` at the new graviton order. In particular, retain the graviton differentiation multiplicity: the order-two source previously required a normalisation correction. That correction and the checks at order two do **not** establish the correctness of pre-existing higher-order libraries. Scalar differentiation already cancels the `3!` or `4!`; do not multiply the supplied tensor by either factorial again. Independently check the minimal scalar tensor against the kinetic and mass terms at each added order. See the [source-correction report](../../../Documentation/Verification/ScalarModelExtensions.md).

### 2. Extend the scalar-model builder

The current [developer entry point](../../../FeynArtsIntegration/BuildSingleScalarTensorMinimalCoupling.wl) accepts a fresh output directory. Its [shared implementation](../../../FeynArtsIntegration/ScalarModelBuilder.wl) is fixed at order two; it has **no higher-order option**. `GravitonOrder -> 3` belongs to the separate pure-GR builder and does not extend this scalar model.

Make the following coordinated changes before rebuilding:

1. Introduce a validated maximum order for this model, retaining two as the default. Keep the John and Einstein–scalar–Gauss–Bonnet branches at their existing scope unless they are extended and verified separately.
2. Import graviton and scalar libraries through that order. Check the recorded orders for all three source families with `FeynGravLibraryInformation`; an import returning without a `Failure` does not by itself prove that every requested order was available.
3. Replace the fixed `TTT`/`TTTT` and `TSS`/`TTSS` assignments with generation of all required families. A pure-gravity rule uses `n + 2` index-pair/momentum triples. A minimal scalar rule uses `n` graviton index pairs and the two scalar momenta in slots `n + 1` and `n + 2`, with `ScalarMass` unchanged.
4. Extend the potential loop from `{nh,0,2}` to `{nh,0,N}` for both scalar multiplicities. Continue using one common gravitational potential tensor with the respective coupling and one coupling row per field content.
5. Enlarge the placeholder arrays: `2 (N + 2)` Lorentz indices cover the pure-gravity vertices, and `N + 4` momentum slots cover the largest potential contact. Audit translation, basis extraction and permutation closure for any further fixed-valence assumptions.
6. Update `GravitonOrders`, `VertexOrderCutoff`, `InteractionFamilies`, source-library records and hashes in the manifest to describe the actual result. Reuse the aligned generic/class vector construction; do not hand-edit only the `.mod` file. Preserve the special treatment of constant scalar-only bases, which do not use the tensor closure helper.

Keep `D`, κ, the scalar mass and both couplings symbolic, with `GaugeFixingEpsilon = 2`. Preserve all current propagators and lower-order rules.

### 3. Build and test a separate candidate

Run the updated developer script in a fresh kernel with a new absolute destination whose parent exists. Its existing command-line form is:

```sh
WolframKernel -noinit -script /absolute/path/to/FeynGrav/FeynArtsIntegration/BuildSingleScalarTensorMinimalCoupling.wl /absolute/existing/parent/scalar-candidate
```

This command alone still builds only through κ² until the builder has been extended as described above. Document any new order-selection interface when implementing it. A `BUILT` message confirms file production, not native validation: this builder leaves model initialisation and physical checks to a separate verification step.

In another fresh kernel, register the candidate directory first in `$ModelPath` and initialise matching `Model` and `GenericModel` names. The candidate retains the name `Single-Scalar-Tensor-Minimal-Coupling`; a different directory does not create a different model identifier. Avoid loading the installed snapshot accidentally through path precedence or cached model state.

Before accepting the extension:

- Check the exact family inventory, coupling powers, scalar mass dependence, absence of unresolved source heads, and matching generic/class basis reconstruction.
- Construct and convert every new contact with native FeynArts and `FCFAConvert`. Compare with verified source rules using consistent incoming momenta; check graviton and identical-scalar permutations and both potential couplings independently.
- Confirm that setting λ₃ and λ₄ to zero recovers the extended minimal scalar theory, and that all previously supplied rules are unchanged.
- Test representative trees that actually exercise the new contacts. The scalar `2 -> 2` example alone cannot test higher-valence vertices. Permit the necessary adjacencies, up to `N + 4` when covering the full inventory.
- Check required field-labelled contacts and exchange channels individually. Equal topology counts are insufficient: FeynArts may omit a topology when a coupling is missing. Include a deliberately missing-contact case that the verification must identify as incomplete.

Use explicit resource bounds and retain warnings, failures and incomplete trials. Increasing the valence can make generic-basis closure and amplitude construction expensive; the [pure-GR extension notes](../GR/README.md#adding-higher-order-graviton-vertices) describe the existing five-graviton limitation. Successful lower-order checks do not establish performance or correctness at the new order.

### 4. Publish the matched bundle

After review, install the `.gen`, `.mod` and `manifest.json` together, update this README's inventory and verification limits, and record the source identities and completed checks. Preserve a recoverable copy of the previous bundle. The generated files must remain standalone, with no run-time dependency on `Libs`, the builder or FORM.

Loop validation remains deferred. Adding higher-order vertices does not establish loop completeness or change the optional pure-GR bridge's supported process list; this scalar model is used through native FeynArts calls.

## Minimal scalar normalisation update — 19 October 2026

The rebuilt bundle uses the corrected two-graviton minimal scalar contact. Both
graviton Ward identities for scalar–scalar to graviton–graviton scattering pass
exactly with symbolic D, mass and model couplings. This supersedes earlier
statements that these particular on-shell checks had not been performed; it
does not add loop validation. Restart the kernel before loading the updated
model. See [the verification record](../../../Documentation/Verification/ScalarWardCorrection.md).
