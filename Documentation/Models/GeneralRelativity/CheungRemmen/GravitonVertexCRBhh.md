# GravitonVertexCRBhh

[Model overview](./README.md) · [Common conventions](../../../Conventions.md) · [Command index](../../../Reference.md)

Returns the cubic interaction of one auxiliary field B and two CR gravitons.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonVertexCRBhh[alpha, rho, sigma, mu1, nu1, p1, mu2, nu2, p2]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `alpha, rho, sigma` | Auxiliary-field Lorentz indices. |
| `mu1, nu1, p1; mu2, nu2, p2` | Two graviton index–index–momentum triples, with incoming momenta. |

## Availability

Direct definition after loading FeynGrav. No conventional graviton library import or FORM executable is required.

## Options and settings

No options. All nine arguments are separate. The auxiliary field has no explicit momentum argument. See [gauge parameters and coupling](../../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns the symmetrised symbolic interaction with the implemented overall factor `-I κ^(4-D)`. Preserve its auxiliary-first leg order and normalisation.

Rules use the [shared momentum, dimension and normalisation conventions](../../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
crMixedVertex = GravitonVertexCRBhh[alpha, rho, sigma, mu1, nu1, p1, mu2, nu2, p2];
crMixedVertex =!= Null && FreeQ[crMixedVertex, _GravitonVertexCRBhh | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `crMixedVertex` contains neither an unevaluated `GravitonVertexCRBhh` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `crMixedVertex` and the loading messages. Evaluate `crMixedVertex` on its own to display the expression.

## Limitations

Use only the stated signature; an unmatched call or diagnostic is not a zero interaction. Keep symbolic dimensions and avoid singular parameter substitutions without a separate analysis. These bosonic rules do not provide a complete gauge-fixed CR calculation: there is no supplied CR FeynArts bundle or dedicated public CR ghost-vertex command.

## Verification scope

These definitions and signatures are present in the source. The conventional GR verification does not validate CR normalisations or amplitudes. See the [CR verification boundary](./README.md#Verification%20boundary).

## See also

- [GravitonPropagatorCR](./GravitonPropagatorCR.md)
- [GravitonPropagatorAuxiliaryCR](./GravitonPropagatorAuxiliaryCR.md)
- [GravitonVertexCRhhh](./GravitonVertexCRhhh.md)
- [GravitonVertexCRBBh](./GravitonVertexCRBBh.md)
- Main-package source (`FeynGrav.wl`)
