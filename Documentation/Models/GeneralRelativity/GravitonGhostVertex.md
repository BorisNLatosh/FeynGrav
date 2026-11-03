# GravitonGhostVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Returns the cubic interaction of one conventional GR graviton with a ghost and an antighost.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
GravitonGhostVertex[rho, sigma, k, mu, pGhost, nu, pAntiGhost]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `rho, sigma, k` | Graviton index pair and incoming momentum. |
| `mu, pGhost` | Ghost Lorentz index and incoming momentum. |
| `nu, pAntiGhost` | Antighost Lorentz index and incoming momentum. |

## Availability

Direct definition, available after loading FeynGrav; no extra library import or FORM executable is needed.

## Options and settings

No options. The conventional cubic ghost vertex does not depend on `GaugeFixingEpsilon`. See [gauge parameters and coupling](../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns the contracted D-dimensional FeynCalc expression, including the implemented factor `I κ/2` multiplying its tensor structure. The ghost precedes the antighost and every momentum is incoming. Keep this ordering when joining ghost lines.

Rules use the [shared momentum, dimension and normalisation conventions](../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
ghostVertex = GravitonGhostVertex[rho, sigma, k, mu, pGhost, nu, pAntiGhost];
ghostVertex =!= Null && FreeQ[ghostVertex, _GravitonGhostVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `ghostVertex` contains neither an unevaluated `GravitonGhostVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `ghostVertex` and the loading messages. Evaluate `ghostVertex` on its own to display the expression.

## Limitations

This fixed seven-argument interface is not an arbitrary multi-graviton ghost generator. In the documented background gauge the ghost action has one cubic h–ghost–antighost coupling and no two-graviton ghost contact. This does not apply to every gravitational gauge or supply the CR ghost sector. Malformed calls are not a zero interaction.

## Verification scope

See the [gauge and ghost action](./README.md#Gauge%20fixing%20and%20ghosts) and the [Scientific scope](../../ScientificScope.md#General%20relativity) for ordering, source-to-model and sign checks; loop compatibility is not inferred.

## See also

- [GhostVectorPropagator](./GhostVectorPropagator.md)
- [GravitonVertex](./GravitonVertex.md)
- Main-package source (`FeynGrav.wl`)
- [Graviton library import](../../Reference.md#Library%20importers)
- [Native GR guide](./NativeModel.md)
