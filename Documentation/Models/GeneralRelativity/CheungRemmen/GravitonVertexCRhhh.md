# GravitonVertexCRhhh

[Model overview](./README.md) · [Common conventions](../../../Conventions.md) · [Command index](../../../Reference.md)

Returns the cubic interaction of three CR graviton fields.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonVertexCRhhh[mu1, nu1, p1, mu2, nu2, p2, mu3, nu3, p3]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `mui, nui, pi` | Lorentz-index pair and incoming momentum for each of the three graviton legs. |

## Availability

Direct definition after loading FeynGrav. No conventional graviton library import or FORM executable is required.

## Options and settings

No options. Pass three triples as separate arguments, without an enclosing list. See [gauge parameters and coupling](../../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns the symmetrised symbolic interaction with the implemented $\kappa^{5-D}$ prefactor. Do not supply an additional conventional GR phase or rescale it by the conventional library-order rule.

Rules use the [shared momentum, dimension and normalisation conventions](../../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
crVertex = GravitonVertexCRhhh[mu1, nu1, p1, mu2, nu2, p2, mu3, nu3, p3];
crVertex =!= Null && FreeQ[crVertex, _GravitonVertexCRhhh | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `crVertex` contains neither an unevaluated `GravitonVertexCRhhh` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `crVertex` and the loading messages. Evaluate `crVertex` on its own to display the expression.

## Limitations

Use only the stated signature; an unmatched call or diagnostic is not a zero interaction. Keep symbolic dimensions and avoid singular parameter substitutions without a separate analysis. These bosonic rules do not provide a complete gauge-fixed CR calculation: there is no supplied CR FeynArts bundle or dedicated public CR ghost-vertex command.

## Verification scope

These definitions and signatures are present in the source. The conventional GR verification does not validate CR normalisations or amplitudes. See the [CR verification boundary](./README.md#Verification%20boundary).

## See also

- [GravitonPropagatorCR](./GravitonPropagatorCR.md)
- [GravitonPropagatorAuxiliaryCR](./GravitonPropagatorAuxiliaryCR.md)
- [GravitonVertexCRBhh](./GravitonVertexCRBhh.md)
- [GravitonVertexCRBBh](./GravitonVertexCRBBh.md)
- Main-package source (`FeynGrav.wl`)
