# GhostVectorPropagator

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Returns the ordered ghost–antighost propagator for the gravitational Faddeev–Popov vector ghosts in conventional GR.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GhostVectorPropagator[mu, nu, p]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `mu, nu` | Ghost-line Lorentz indices in the ordered contraction. |
| `p` | Ghost-line momentum. |

## Availability

Direct definition, available after loading FeynGrav; no additional vertex library or FORM executable is needed.

## Options and settings

No options. This conventional ghost propagator does not depend on `GaugeFixingEpsilon`. See [gauge parameters and coupling](../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns `-I MTD[mu, nu] FAD[p]`. The minus sign is the current source and native `GR` convention. Preserve ghost ordering and diagram statistics; do not replace it with the historical positive propagator.

Rules use the [shared momentum, dimension and normalisation conventions](../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
ghostPropagator = GhostVectorPropagator[mu, nu, p];
FCI[ghostPropagator] === FCI[-I MTD[mu, nu] FAD[p]]
```

Expected result: `True`. The gravitational ghost propagator is `-I MTD[mu, nu] FAD[p]`; the minus sign is part of the implemented convention.

## Limitations

This is the conventional GR ghost line. Higher-derivative quadratic-gravity ghosts use `GhostVectorPropagatorHD`; the conventional command is not a CR ghost prescription. Supplying a propagator does not add diagram statistics or a loop-integration measure.

## Verification scope

The [scientific-scope summary](../../ScientificScope.md#General%20relativity) states the checked GR interactions and ghost convention. Native-model loop compatibility remains deferred.

## See also

- [GravitonGhostVertex](./GravitonGhostVertex.md)
- [GravitonPropagator](./GravitonPropagator.md)
- Main-package source (`FeynGrav.wl`)
- [Graviton library import](../../Reference.md#Library%20importers)
- [Native GR guide](./NativeModel.md)
