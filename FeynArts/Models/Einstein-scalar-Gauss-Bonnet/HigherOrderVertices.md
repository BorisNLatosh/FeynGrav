## Adding higher orders in κ

[Model overview](README.md) · [Model guide](ModelGuide.md)

The supplied model is implemented and verified within the stated tree-level scope through **κ² per vertex**. Extending the cutoff to an integer `N > 2` requires source verification and a builder extension; changing a filename or importing more libraries does not extend the saved model.

| Sector | Required vertices | Coupling order | Source library |
| --- | --- | --- | --- |
| Pure gravity | `h^(n+2)`, `1 <= n <= N` | κⁿ | `GravitonVertex_n` |
| Minimal scalar kinetic and mass terms | `h^n phi^2`, `1 <= n <= N` | κⁿ | `GravitonScalarVertex_n` |
| Linear scalar–Gauss–Bonnet term | `h^n phi`, `2 <= n <= N` | g₁κⁿ | `ScalarGaussBonnet_n` |
| Quadratic scalar–Gauss–Bonnet term | Contribution to `h^n phi^2`, `2 <= n <= N` | g₂κⁿ | The same `ScalarGaussBonnet_n` |
| Gravitational ghosts | Existing `h antighost ghost` | κ | Existing fixed-gauge ghost rule |

Order three therefore adds `TTTTT`, `TTTS` and `TTTSS`; the last row combines minimal and g₂ contributions. Preserve all lower orders, the existing propagators and the fixed linear gauge's ghost sector. Do not add scalar self-potential terms or a separate mass insertion. The scalar factorial in the g₂ action cancels under differentiation: do not multiply that rule by another `2!`. There are still no zero- or one-graviton Gauss–Bonnet vertices.

### 1. Audit and prepare the source libraries

**Higher-order Gauss–Bonnet sources are not covered by the order-two correction.** The routines in [ScalarGaussBonnet.wl](../../../Rules/ScalarGaussBonnet.wl) retain the legacy higher-order contraction. Independently derive and audit each new order before accepting its generated library. Do not assume that an existing file or successful generation establishes correctness, and do not compensate for a source discrepancy inside the FeynArts model.

Expand the full density

$$\sqrt{-g}\left(g_1\phi+\frac{g_2}{2!}\phi^2\right)
\left(R_{\mu\nu\rho\sigma}R^{\mu\nu\rho\sigma}
-4R_{\mu\nu}R^{\mu\nu}+R^2\right),
\qquad g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}.$$

Include the determinant, inverse metrics in contractions, nonlinear curvatures and graviton differentiation multiplicities. Products of linearised curvatures suffice only at order two. Compare full tensors with symbolic D and generic off-shell momenta, imposing total momentum conservation including the scalar legs. Do not set the sum of graviton momenta to zero separately. Preserve the curvature and Fourier conventions, and introduce neither four-dimensional topological identities nor a `1/(D-4)` factor.

After any required source correction, generate missing orders into an absolute temporary directory in a separate fresh generator kernel:

```wl
Get["/absolute/path/to/FeynGrav/Libs/Generator.wl"];
GenerateGravitonVertexSpecific[n, OutputDirectory -> temporaryDirectory];
GenerateGravitonScalarsSpecific[n, OutputDirectory -> temporaryDirectory];
GenerateScalarGaussBonnetSpecific[n, OutputDirectory -> temporaryDirectory];
```

Here `n >= 3` is the added order. The scalar command also produces a potential tensor, which this model does not use. Check failures, normalisation and graviton/scalar permutations before installing accepted libraries in `Libs`. Verify new minimal and pure-gravity sources as well, retaining hashes and comparison evidence.

### 2. Extend the builder consistently

[BuildEinsteinScalarGaussBonnet.wl](../../../FeynArtsIntegration/BuildEinsteinScalarGaussBonnet.wl) delegates to [ScalarModelBuilder.wl](../../../FeynArtsIntegration/ScalarModelBuilder.wl). The current builder has fixed order-two rules and **no higher-order option**. The pure-GR builder's `GravitonOrder` option does not extend this model.

1. Add a validated maximum order, keeping two as the default and preserving the other model branches unless separately extended.
2. Import gravity, minimal scalar and Gauss–Bonnet libraries through `N`. Check actual orders with `FeynGravLibraryInformation`: importers can cap requests at available orders, so successful return alone is insufficient.
3. Generate gravity and minimal scalar rules for every order through `N`. For each `n >= 2`, form a flat list of `n` graviton index-pair/momentum triples and call:

   ```wl
   ScalarGaussBonnet[gravitonTriples, GaussBonnetG1]
   ScalarGaussBonnet[gravitonTriples, GaussBonnetG2]
   ```

   Put the first expression in the `n`-graviton, one-scalar row. Add the second to the minimal two-scalar rule **before extracting the common kinematic basis**, keeping one coupling row per field content. Scalar momenta are absent from this source tensor because the derivatives act on gravity; their legs still belong in the model and in momentum conservation.
4. Replace fixed placeholder limits and hard-coded family assignments. Allow at least `2 (N + 2)` Lorentz-index slots and `N + 2` momentum slots for this model, preserving any larger requirements of other shared branches. Check translation and permutation closure at every new valence.
5. Update the manifest's orders, cutoff, exact inventory, source hashes and model hashes. Keep generic and classes kinematic vectors aligned. Editing only `.mod` is insufficient.

Keep D, κ, the scalar mass and both couplings symbolic. Retain `GaugeFixingEpsilon = 2` and the negative gravitational ghost propagator.

### 3. Build and validate a separate candidate

After implementing the extension, build into a fresh absolute destination whose parent exists:

```sh
WolframKernel -noinit -script /absolute/path/to/FeynGrav/FeynArtsIntegration/BuildEinsteinScalarGaussBonnet.wl /absolute/existing/parent/esgb-candidate
```

Until the builder is extended, this command still produces only the κ² model. Document the new order-selection interface when adding it. Register the candidate directory first in `$ModelPath` in another fresh kernel and initialise matching `Model` and `GenericModel` names; avoid testing a cached or installed snapshot accidentally.

Before acceptance:

- Check exact field inventories, coupling powers, mass dependence, basis reconstruction and absence of unresolved source functions or private symbols.
- Construct and convert every new contact with native FeynArts and `FCFAConvert`, comparing with the independent tensors under consistent signed momentum routing and permutations.
- Check g₁ and g₂ independently. Setting both to zero must recover the minimal massive scalar theory through the same order; all lower-order rules must remain unchanged.
- Exercise new contacts in selected complete trees and check the corresponding on-shell graviton Ward identities before helicity specialisation. A nonlinear contact need not satisfy a standalone Ward identity: include the required exchange diagrams.
- Allow the required topology valences, up to `N + 2`, through `Adjacencies`. Check species-labelled channels and include a missing-contact negative case; topology counts alone do not prove completeness. The existing four-point example cannot validate higher-valence contacts.

Use explicit resource bounds and retain warnings and incomplete trials. Higher-valence basis closure and amplitude construction can be expensive; see the [GR extension guide](../GR/HigherOrderVertices.md#adding-higher-order-graviton-vertices). Products of allowed vertices may have total κ order above `N`; apply a separate amplitude-order restriction if required.

### 4. Install the matched bundle

After review, install the candidate `.gen`, `.mod` and `manifest.json` together, preserving a recoverable previous bundle. Update this README's inventory, cutoff and verification record. Loading must remain standalone, without source libraries, a builder or FORM execution.

Loop validation remains deferred. Adding higher-order vertices does not establish loop completeness or extend the optional pure-GR bridge's process restrictions.
