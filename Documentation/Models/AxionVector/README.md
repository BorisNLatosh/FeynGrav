# Axion-like scalar–vector interactions

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0 development. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

The interface supplies an axion-like scalar coupling to two Abelian vectors with attached gravitons and an explicit coupling `theta`. The intended sector involves an epsilon tensor and field strengths. **The precise epsilon-density/action convention remains unresolved.** No fully checked covariant action is asserted here.

In particular, treating a Levi-Civita density as an ordinary tensor can change the metric dependence of a topological interaction. The existence of stored graviton vertices must not be used to settle that question.

## Gauge fixing, libraries and native support

Use `FeynGravImportAxionVectorVertex[n]` before retrieval; this optional family is not loaded initially. The first vertex argument is a flat list of graviton index pairs, followed by two vector index–momentum groups and the coupling. There is no axion momentum argument. Inspect import records for the epsilon setting used when loading.

There is no supplied native FeynArts bundle. A complete process additionally needs a consistent free action and the remaining interactions chosen by the user.

## Commands

| Command | Purpose |
| --- | --- |
| [GravitonAxionVectorVertex](./GravitonAxionVectorVertex.md) | Two vector index–momentum groups and axion coupling `theta`; no axion momentum argument. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](../../Services/RuleConstruction/Contracts.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

The epsilon-density/action convention remains unresolved. Corrected multiplicities and epsilon slots do not establish that convention. See [Scientific scope](../../ScientificScope.md#Other%20interactions) before using the stored vertices in a prediction.
