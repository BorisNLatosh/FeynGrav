# GravitonVertexCRBBh

[Model overview](./README.md) · [Common conventions](../../../Conventions.md) · [Command index](../../../Reference.md)

Returns the cubic interaction of two auxiliary fields B and one CR graviton.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonVertexCRBBh[alpha1, rho1, sigma1, alpha2, rho2, sigma2, mu, nu]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `alpha1, rho1, sigma1; alpha2, rho2, sigma2` | Two groups of auxiliary-field Lorentz indices. |
| `mu, nu` | Graviton Lorentz-index pair. |

## Availability

Direct definition after loading FeynGrav. No conventional graviton library import or FORM executable is required.

## Options and settings

No options. There are eight separate index arguments and no momentum arguments. See [gauge parameters and coupling](../../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns the symmetrised metric-tensor interaction with the implemented $\kappa^{3-D}$ prefactor. Keep this algebraic vertex and its normalisation distinct from the conventional graviton vertex.

Rules use the [shared momentum, dimension and normalisation conventions](../../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
crAuxiliaryVertex = GravitonVertexCRBBh[
  alpha1, rho1, sigma1, alpha2, rho2, sigma2, mu, nu];
crAuxiliaryVertex =!= Null && FreeQ[crAuxiliaryVertex, _GravitonVertexCRBBh | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `crAuxiliaryVertex` contains neither an unevaluated `GravitonVertexCRBBh` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `crAuxiliaryVertex` and the loading messages. Evaluate `crAuxiliaryVertex` on its own to display the expression.

## Limitations

Use only the stated signature; an unmatched call or diagnostic is not a zero interaction. Keep symbolic dimensions and avoid singular parameter substitutions without a separate analysis. These bosonic rules do not provide a complete gauge-fixed CR calculation: there is no supplied CR FeynArts bundle or dedicated public CR ghost-vertex command.

## Verification scope

These definitions and signatures are present in the source. The conventional GR verification does not validate CR normalisations or amplitudes. See the [CR verification boundary](./README.md#Verification%20boundary).

## See also

- [GravitonPropagatorCR](./GravitonPropagatorCR.md)
- [GravitonPropagatorAuxiliaryCR](./GravitonPropagatorAuxiliaryCR.md)
- [GravitonVertexCRhhh](./GravitonVertexCRhhh.md)
- [GravitonVertexCRBhh](./GravitonVertexCRBhh.md)
- Main-package source (`FeynGrav.wl`)
