# SU(N) Yang–Mills, quarks and ghosts

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

The sector contains an SU(N) gauge field and its ghosts coupled to GR. The native bundle also includes one fundamental Dirac quark of arbitrary mass. Its physical action contains

$$
\mathcal L=-\frac{2R}{\kappa^2}-\frac14F^a_{\mu\nu}F^{a\mu\nu}
+\frac{i}{2}\bar q e_a{}^\mu\gamma^a\overleftrightarrow D_\mu q-m\bar q q,
$$

with `D_mu = nabla_mu - i g_s T^a A^a_mu`, plus the gauge-fixing and two ghost determinants detailed in the native guide. The fundamental generators obey `tr(T^a T^b) = delta^(ab)/2`. `SUNN` denotes N and is independent of spacetime dimension D.

The native parameters are `YangMillsCoupling`, `QuarkMass`, κ and FeynCalc's `SUNN`. Leave them symbolic and specialise converted expressions by replacement. Source spinor momenta use the package positive Fourier phase; the native guide documents the necessary FeynArts routing conversion.

## Gauge fixing, libraries and native support

Use `FeynGravImportSUNYM[n]` for the six stored families: two-, three- and four-gluon, quark–gluon, Yang–Mills ghost and gluon–ghost. The optional family is not loaded initially. `GaugeFixingEpsilonSUNYM`, initially -1, is applied during import.

The `GR-SU-N-Yang-Mills` native bundle fixes conventional gravity gauge 2 and Yang–Mills gauge -1 and stops at κ² per vertex. It contains separate gravitational vector ghosts and Yang–Mills scalar ghosts. The model-local field labels and ghost arrows are specified in the guide; they are not interchangeable with those in the pure-GR bundle.

## Commands

| Command | Purpose |
| --- | --- |
| [GravitonGluonGhostVertex](./GravitonGluonGhostVertex.md) | Gluon–ghost interaction in the existing library interface, with three index–colour–momentum groups. Retain all nine leg arguments, including the ghost-position Lorentz placeholders. |
| [GravitonGluonVertex](./GravitonGluonVertex.md) | `l = 2, 3, 4` gluons, each supplied as momentum–Lorentz-index–colour-index. Selects the two-, three- or four-gluon library by argument count. |
| [GravitonQuarkGluonVertex](./GravitonQuarkGluonVertex.md) | Quark–antiquark–gluon coupling; the gluon is specified by its Lorentz and colour indices. No momentum arguments. |
| [GravitonYMGhostVertex](./GravitonYMGhostVertex.md) | Two Yang–Mills ghost momenta and colour labels. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](./NativeModel.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

See [Scientific scope](../../ScientificScope.md#Yang%E2%80%93Mills%20and%20fermions) for the checked contacts and selected trees. Native tree checks do not certify arbitrary loops or all higher-order generated vertices.

For the supplied FeynArts bundle, see the [native model guide](./NativeModel.md) and [order limits](./HigherOrders.md).
