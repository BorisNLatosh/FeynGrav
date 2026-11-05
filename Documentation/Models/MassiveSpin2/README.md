# Massive spin-2 propagator

[Model index](../../Models.md) · [Common conventions](../../Conventions.md) · [Documentation index](../../README.md)

**Baseline:** 4.2.0. These pages describe the installed interfaces and their supported limits.

## Fields, action and parameters

This group contains a propagator utility for a symmetric rank-two field. It uses an explicit mass argument and the implemented `1/(D-1)` trace subtraction. There is no accompanying nonlinear massive-gravity action, interaction library or native model.

Do not infer a complete interacting Fierz–Pauli, dRGT or other massive-gravity theory from this function alone. The free tensor structure and its singular parameter limits are the scope of the interface.

## Gauge fixing, libraries and native support

The propagator is available after loading FeynGrav, without an extra library import or gauge option. Treat mass and dimension symbolically before considering limits. In particular, substituting zero mass is not a route to the GR massless propagator.

## Commands

| Command | Purpose |
| --- | --- |
| [GravitonPropagatorMassive](./GravitonPropagatorMassive.md) | Massive-gravity propagator with the implemented `1/(D-1)` trace subtraction. Requires nonsingular mass and dimension choices; `m = 0` is not a massless-propagator shortcut. |

Read the individual pages for exact argument order and retrieval examples. A rule that remains unevaluated can indicate a missing library or an unmatched signature; it is not evidence of a vanishing interaction.

## Examples and further reading

- [Detailed guide and contracts](../GeneralRelativity/README.md)
- [Worked notebook index](../../Examples/README.md)
- [Library imports and provenance](../../Reference.md#Library%20importers)

## Verification and limitations

The documentation describes the implemented expression; it adds no interacting-model, unitarity or amplitude certification.
