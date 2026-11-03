# Minimally coupled scalar

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0 development. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

One real scalar couples minimally to the metric. The native scalar bundle uses

$$
S=\int d^Dx\sqrt{-g}\left[-\frac{2R}{\kappa^2}+\frac12g^{\mu\nu}\partial_\mu\phi\partial_\nu\phi-\frac12m^2\phi^2+\frac{\lambda_3}{3!}\phi^3+\frac{\lambda_4}{4!}\phi^4\right]+S_{\mathrm{gf}}+S_{\mathrm{FP}}.
$$

The two displayed potential coefficients have **positive signs in the action**. The direct potential command accepts the coefficient of a general signed action monomial `lambda phi^r/r!`; it is not restricted to cubic and quartic powers. The mass term belongs to the minimal vertex and propagator.

The native parameter symbols are `ScalarMass`, `ScalarLambda3` and `ScalarLambda4`. Leave them symbolic when loading and specialise converted expressions by replacement. The direct commands instead take explicit mass and coupling arguments.

## Gauge fixing, libraries and native support

Use the conventional GR gauge and gravitational ghosts from the [GR overview](../GeneralRelativity/README.md#Gauge%20fixing%20and%20ghosts). There is no additional scalar gauge symmetry. Successful initialisation imports minimal and potential libraries through two attached gravitons. Higher orders need installed files and `FeynGravImportScalars[n]`.

The native `Single-Scalar-Tensor-Minimal-Coupling` model stops at κ² per vertex. Its potential contacts can have five or six legs; restricting topology adjacency to three and four can omit required interactions.

## Commands

| Command | Purpose |
| --- | --- |
| [GravitonScalarPotentialVertex](./GravitonScalarPotentialVertex.md) | Scalar-potential vertex with the supplied potential coupling; no scalar momentum arguments. The convention is the potential monomial `lambda phi^r/r!`. |
| [GravitonScalarVertex](./GravitonScalarVertex.md) | Two incoming scalar momenta and their mass; minimal scalar coupling to `n` gravitons. |
| [ScalarPropagator](./ScalarPropagator.md) | Scalar propagator `I FAD[{p,m}]`; `m = 0` is supported. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](./NativeModel.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

The corrected two-graviton contact agrees with the canonical scalar action; selected scalar-to-graviton trees satisfy both outgoing Ward identities. These checks do not establish arbitrary-process or loop correctness. See [Scientific scope](../../ScientificScope.md#Scalar%20models).

For the supplied FeynArts bundle, see the [native model guide](./NativeModel.md) and [order limits](./HigherOrders.md).
