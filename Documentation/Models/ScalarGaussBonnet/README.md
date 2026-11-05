# Einstein–scalar–Gauss–Bonnet

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

The supplied native bundle contains GR and a real massive scalar with

$$
S=\int d^Dx\sqrt{-g}\left[-\frac{2R}{\kappa^2}+\frac12(\partial\phi)^2-\frac12m^2\phi^2+\left(g_1\phi+\frac{g_2}{2!}\phi^2\right)\mathcal G\right]+S_{\mathrm{gf}}+S_{\mathrm{FP}},
$$

$$
\mathcal G=R_{abcd}R^{abcd}-4R_{ab}R^{ab}+R^2.
$$

The native parameters are `ScalarMass`, `GaussBonnetG1` and `GaussBonnetG2`; leave them symbolic while loading. Expansion is about flat spacetime and zero scalar background. Each curvature starts at one graviton, so the new interaction begins at two gravitons. No `1/(D-4)` rescaling or four-dimensional curvature identity is applied.

## Gauge fixing, libraries and native support

The GR gauge/ghost sector is inherited with gauge value 2 in the native bundle. The canonical scalar and graviton free propagators remain unchanged. Import stored vertices with `FeynGravImportScalarGaussBonnet[n]`, for `n >= 2`; the family is not loaded initially.

The `Einstein-scalar-Gauss-Bonnet` native model stops at κ² per vertex. Its new contacts are two gravitons with one or two scalars; the latter combines with the minimal scalar contact. Products of allowed vertices can have higher powers of κ in a diagram.

## Commands

| Command | Purpose |
| --- | --- |
| [ScalarGaussBonnet](./ScalarGaussBonnet.md) | `n >= 2` graviton legs, followed by the scalar–Gauss–Bonnet coupling; no separate scalar momentum argument. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](./NativeModel.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

The two-graviton rule has an independent linearised-curvature comparison. The native model includes the corrected minimal scalar contact and passes the selected on-shell tree checks described in [Scientific scope](../../ScientificScope.md#Scalar%20models). Higher-order legacy sources, including the supplied order-three library, require separate validation; loops are not certified.

For the supplied FeynArts bundle, see the [native model guide](./NativeModel.md) and [order limits](./HigherOrders.md).
