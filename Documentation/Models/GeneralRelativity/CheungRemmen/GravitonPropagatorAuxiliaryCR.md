# GravitonPropagatorAuxiliaryCR

[Model overview](./README.md) · [Common conventions](../../../Conventions.md) · [Command index](../../../Reference.md)

Returns the algebraic auxiliary-field propagator in Cheung–Remmen variables.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
GravitonPropagatorAuxiliaryCR[lambda1, mu1, nu1, lambda2, mu2, nu2]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `lambda1, mu1, nu1` | Three Lorentz indices of the first auxiliary field. |
| `lambda2, mu2, nu2` | Three Lorentz indices of the second auxiliary field. |

## Availability

Direct definition after loading FeynGrav. No conventional graviton library import or FORM executable is required.

## Options and settings

No options or gauge-fixing parameters. There is no momentum argument. See [gauge parameters and coupling](../../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns a metric-tensor expression with the implemented overall factor `I κ^(2-D)`. It is independent of momentum and has no momentum propagator denominator.

Rules use the [shared momentum, dimension and normalisation conventions](../../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
auxiliaryPropagator = GravitonPropagatorAuxiliaryCR[
  lambda1, mu1, nu1, lambda2, mu2, nu2];
auxiliaryPropagator =!= Null && FreeQ[auxiliaryPropagator, _GravitonPropagatorAuxiliaryCR | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `auxiliaryPropagator` contains neither an unevaluated `GravitonPropagatorAuxiliaryCR` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `auxiliaryPropagator` and the loading messages. Evaluate `auxiliaryPropagator` on its own to display the expression.

## Limitations

Use only the stated signature; an unmatched call or diagnostic is not a zero interaction. Keep symbolic dimensions and avoid singular parameter substitutions without a separate analysis. These bosonic rules do not provide a complete gauge-fixed CR calculation: there is no supplied CR FeynArts bundle or dedicated public CR ghost-vertex command.

## Verification scope

These definitions and signatures are present in the source. The conventional GR verification does not validate CR normalisations or amplitudes. See the [CR verification boundary](./README.md#Verification%20boundary).

## See also

- [GravitonPropagatorCR](./GravitonPropagatorCR.md)
- [GravitonVertexCRhhh](./GravitonVertexCRhhh.md)
- [GravitonVertexCRBhh](./GravitonVertexCRBhh.md)
- [GravitonVertexCRBBh](./GravitonVertexCRBBh.md)
- Main-package source (`FeynGrav.wl`)
