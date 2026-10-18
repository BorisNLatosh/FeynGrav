# GR with one minimally coupled massive scalar

`GRScalar` is a supplied native FeynArts model for one real scalar and conventional general relativity. It includes vertices through $\kappa^2$ and is ready for the tree workflows below with **FeynCalc-patched FeynArts**. No model build or FORM execution is needed to construct and convert these diagrams.

## Action, fields and scope

The four-dimensional action is

$$
S=\int d^4x\,\sqrt{-g}\left[-\frac{2}{\kappa^2}R
+\frac12 g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi
-\frac12 m^2\phi^2\right]+S_{\mathrm{gf}}+S_{\mathrm{FP}}.
$$

The expansion is about flat space and $\phi=0$, with $g_{\mu\nu}=\eta_{\mu\nu}+\kappa h_{\mu\nu}$, the existing curvature/Fourier conventions, mostly-minus signature and `GaugeFixingEpsilon = 2`. Tensors retain symbolic `D` as a dimensional-regularisation setup. Source-vertex momenta are all incoming; native incoming/outgoing process lists handle the corresponding signs.

| Field | Meaning | Free propagator |
| --- | --- | --- |
| `T[1]` | Self-conjugate graviton, two Lorentz indices | Existing fixed-gauge GR propagator |
| `U[1]`, `-U[1]` | Oriented gravitational ghost and antighost, one Lorentz index | $-i\eta_{\mu\nu}/(p^2+i0)$ |
| `S[1]` | One real self-conjugate scalar, no Lorentz indices | $i/(p^2-m^2+i0)$ |

| Coupling | Order | Contents |
| --- | --- | --- |
| `TTT`, `TUU`, `TSS` | $\kappa$ | Cubic GR, ghost and scalar couplings |
| `TTTT`, `TTSS` | $\kappa^2$ | Quartic GR and two-graviton/two-scalar couplings |

The mass is the unassigned symbol ``FeynGrav`ScalarMass``. Both scalar vertices include their mass-term contribution; the mass is already in the free propagator, so there is no separate two-leg mass insertion. There is no scalar self-interaction, non-minimal coupling or scalar ghost. The gravitational ghost vertex and native Grassmann signs are unchanged from [PureGR](../README.md). The per-vertex cutoff does not impose a total coupling-order bound on arbitrary diagrams.

## Load the supplied bundle

Start a fresh kernel, with `D`, the momenta and the mass/coupling symbols unassigned:

```wl
$LoadAddOns = {"FeynArts"};
Needs["FeynCalc`"];
root = DirectoryName[FindFile["FeynGrav`"]];
modelDirectory = FileNameJoin[{root, "FeynArts", "Models", "GRScalar"}];
FeynArts`$ModelPath = Prepend[
  DeleteCases[FeynArts`$ModelPath, modelDirectory], modelDirectory];
FeynArts`InitializeModel["GRScalar", FeynArts`GenericModel -> "GRScalar"];
```

`FindFile` locates the installed package without loading it. If FeynGrav is outside Mathematica's application path, set `root` to its absolute repository directory instead. This explicit registration recipe does not load FeynGrav. Alternatively, loading FeynGrav with FeynArts already present automatically registers all supplied model folders, including GRScalar; see the [common loading workflow](../README.md#common-loading-workflow). Always specify both model names. The generated `.gen` and `.mod` files need neither FeynGrav loading nor interaction-library imports. Their manifest's original source paths are provenance, not runtime dependencies.

The optional FeynGrav FeynArts bridge remains a pure-GR interface. Use **native FeynArts calls** for GRScalar; its process allowlist and loop pins have not been extended.

## Construct a scalar–scalar to graviton–graviton tree

```wl
top = FeynArts`CreateTopologies[0, 2 -> 2,
  FeynArts`Adjacencies -> {3, 4}];
ins = FeynArts`InsertFields[top,
  {FeynArts`S[1], FeynArts`S[1]} -> {FeynArts`T[1], FeynArts`T[1]},
  FeynArts`Model -> "GRScalar", FeynArts`GenericModel -> "GRScalar",
  FeynArts`InsertionLevel -> {FeynArts`Classes}];
raw = FeynArts`CreateFeynAmp[ins,
  FeynArts`PreFactor -> 1, FeynArts`GaugeRules -> {},
  FeynArts`MomentumConservation -> True, FeynArts`Truncated -> True];
diagrams = FeynCalc`FCFAConvert[raw,
  FeynCalc`IncomingMomenta -> {p1, p2},
  FeynCalc`OutgoingMomenta -> {q1, q2},
  FeynCalc`LorentzIndexNames -> {mu1, nu1, mu2, nu2, a, b, c, d},
  FeynCalc`ChangeDimension -> D, FeynCalc`Contract -> False,
  List -> True, FeynCalc`Prefactor -> 1];
(* Optional: FeynArts`Paint[ins] *)
```

The four diagrams are the `TTSS` contact, one graviton exchange and two scalar exchanges. The output retains two free indices per external graviton; index names are allocated during conversion, so inspect each diagram's free indices before attaching external polarisations. `Truncated -> True` removes external tensor markers. These are open-index expressions: external states, mass-shell conditions, helicity sums and observable normalisation are subsequent explicit steps.

`PreFactor -> 1` preserves the source-rule normalisation. FeynArts supplies its native graph/statistics factors. `MomentumConservation -> True` resolves internal routing; it does not impose external mass shells. Keep exchange products factored when inspecting them.

For scalar–scalar scattering, replace the outgoing fields by `{S[1], S[1]}` and regenerate `ins`, `raw` and `diagrams`. There are three graviton exchanges and no scalar four-point contact. For the cubic graviton-to-two-scalar tensor use `1 -> 2`, `{T[1]} -> {S[1], S[1]}`, one incoming momentum, two outgoing momenta and two external Lorentz-index names. This last case is a vertex/conversion check, not a claim of physical decay kinematics.

Keep the mass symbolic during generation. Specialise the **converted** expression afterwards, for example:

```wl
massiveDiagrams = diagrams /. FeynGrav`ScalarMass -> m;
masslessDiagrams = diagrams /. FeynGrav`ScalarMass -> 0;
```

Both numerator mass terms and denominator masses are then changed consistently. Do not assign a global numerical value to the model's mass symbol. Setting the mass does not impose external on-shell scalar products.

## Completeness and verification limits

This finite model contains only the five coupling families listed above. FeynArts can silently omit a topology with no allowed coupling. Check the required species and channels for each chosen process; equal topology counts are not a general completeness criterion for matter theories. Higher multiplicities may require vertices beyond the supplied cutoff.

The build passed 24 checks; the final fresh-kernel native check passed 29, with 31 saved zero residuals. It covers the two scalar contacts, an independent cubic action-normalisation comparison, all exchanges for the two processes above, scalar/graviton contact permutations, the massless limit, a crossed `S T -> S T` **contact** and a real missing-contact test. Full crossed-amplitude equality, on-shell/Ward tests and loops were not checked. [Verification and provenance](../../../Documentation/Verification/GRScalarTreeRelease.md).

## Optional rebuilding

Most users should use the supplied files. Maintainers can reproduce a separate snapshot with [BuildGRScalar.wl](../../../FeynArtsIntegration/BuildGRScalar.wl), from a fresh kernel and with the current FeynGrav scalar/graviton libraries available:

```sh
GRSCALAR_TARGET=/absolute/new/output-directory \
  WolframKernel -noinit -script /absolute/FeynGrav/FeynArtsIntegration/BuildGRScalar.wl
```

The parent directory must exist; an existing destination is refused. `GRSCALAR_RESULT=/absolute/result.json` optionally selects the build report. The script performs local construction checks and one native cubic smoke comparison; it does not regenerate the interaction libraries or run the complete native-tree verification batch. It reuses the companion's basis/encoding helpers without changing the PureGR builder. Ordinary model loading never invokes it. A rebuilt snapshot needs checks matching its changed inputs; a successful build alone is not full amplitude validation.
