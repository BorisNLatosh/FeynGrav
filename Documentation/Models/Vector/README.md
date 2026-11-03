# Massive and massless Abelian vectors

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0 development. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

This group contains two interfaces. The massive field is Proca matter, with physical density

$$
\mathcal L_{\mathrm{Proca}}=-\frac14F_{\mu\nu}F^{\mu\nu}+\frac12m^2 A_\mu A^\mu,
\qquad F_{\mu\nu}=\partial_\mu A_\nu-\partial_\nu A_\mu.
$$

The massless interface includes the Maxwell sector, covariant vector gauge fixing and its Faddeev–Popov ghosts. It is not a complete charged-matter QED model. The gravitational gauge sector is described in the [GR overview](../GeneralRelativity/README.md).

The Proca mass is an explicit argument. The massless-vector gauge parameter is `GaugeFixingEpsilonVector`, initially `-1`. Its value is applied when importing vector libraries, so change it before reimporting the desired order. The Proca propagator contains `1/m^2`; setting its mass to zero does not produce a gauge-fixed photon propagator.

## Gauge fixing, libraries and native support

Initialisation imports massive-vector, massless-vector and vector-ghost libraries through two gravitons. Use `FeynGravImportVectors[n]` for additional installed orders. The massive-vector and ghost vertices take graviton index pairs; the massless-vector vertex takes index–index–momentum triples. There is no supplied standalone native FeynArts bundle for either sector.

## Commands

| Command | Purpose |
| --- | --- |
| [GravitonMassiveVectorVertex](./GravitonMassiveVectorVertex.md) | Each vector is specified by index then momentum, followed by their mass. |
| [GravitonVectorGhostVertex](./GravitonVectorGhostVertex.md) | Two incoming momenta of the massless-vector Faddeev–Popov ghosts. |
| [GravitonVectorVertex](./GravitonVectorVertex.md) | Massless-vector vertex; depends on the vector gauge value used during import. |
| [ProcaPropagator](./ProcaPropagator.md) | Massive-vector propagator, with numerator `-I (MTD[mu,nu] - FVD[p,mu] FVD[p,nu]/m^2)`. Its massless limit cannot be obtained by substituting `m = 0`. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](../../Services/RuleConstruction/Contracts.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

Corrected differentiation multiplicities have bounded comparisons, but these do not settle every vector action or amplitude. See [vertex multiplicities](../../ScientificScope.md#Vertex%20multiplicities) and [remaining interaction limits](../../ScientificScope.md#Other%20interactions). The selected Yang–Mills native checks have their own scope.
