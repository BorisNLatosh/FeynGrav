# Quadratic gravity

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0 development. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

The interface combines the Einstein–Hilbert rule with scalar-curvature-squared and Ricci-tensor-squared interactions. In the source mass parametrisation the curvature density is

$$
\mathcal L_{\mathrm Q}=-\frac{2R}{\kappa^2}
+\frac{1}{3\kappa^2}\left(\frac{2}{m_2^2}+\frac{1}{m_0^2}\right)R^2
-\frac{2}{\kappa^2m_2^2}R_{\mu\nu}R^{\mu\nu}.
$$

This expression records the coefficients assembled in the rule source (`Rules/QuadraticGravityVertex.wl`); it is not a new independent action verification. Use the package curvature convention and metric expansion. `m0` and `m2` are explicit arguments, not package constants. The massive spin-2 pole parameter is `m2`; `m0` equals the scalar pole mass only in four dimensions. The implemented continuation has

$$
m_s^2=\frac{3(D-2)m_0^2m_2^2}{(D-4)m_0^2+2(D-1)m_2^2}.
$$

## Gauge fixing, libraries and native support

The conventional propagator uses `GaugeFixingEpsilon`, initially 2. The higher-derivative propagator uses `GaugeFixingEpsilonHD`, `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`, initially unassigned. The HD ghost propagator and cubic ghost vertex use the last two. These direct definitions read current settings; parameter and projector denominators require nonsingular limits.

For an r-graviton stored self-interaction use `FeynGravImportQuadraticGravity[r-2]`. These libraries are optional and not loaded initially. The cubic HD ghost command is a fixed interface, not a generator of arbitrary ghost contacts. There is no supplied quadratic-gravity native FeynArts bundle.

## Commands

| Command | Purpose |
| --- | --- |
| [GhostVectorPropagatorHD](./GhostVectorPropagatorHD.md) | Ghost propagator for higher-derivative gauge fixing; depends on `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`, including their inverse combinations. |
| [GravitonGhostVertexHD](./GravitonGhostVertexHD.md) | Same leg order with higher-derivative ghost gauge fixing; depends on `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`. |
| [QuadraticGravityPropagator](./QuadraticGravityPropagator.md) | Quadratic-gravity propagator with conventional gauge fixing, using the current `GaugeFixingEpsilon`. |
| [QuadraticGravityPropagatorHD](./QuadraticGravityPropagatorHD.md) | Quadratic-gravity propagator with higher-derivative gauge fixing, using all three HD gauge parameters. |
| [QuadraticGravityVertex](./QuadraticGravityVertex.md) | `r >= 3` graviton legs in one flat list, followed by the two quadratic-gravity mass parameters. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](../../Services/RuleConstruction/Contracts.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

The displayed source coefficients do not establish an independent complete quadratic-action inversion, all-order rule audit, or native amplitude validation. The [model catalogue](../FeynArts.md) identifies the bundles that actually exist.
