# GravitonPropagatorCR

[Model overview](./README.md) · [Common conventions](../../../Conventions.md) · [Command index](../../../Reference.md)

Returns the graviton propagator in Cheung–Remmen variables.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonPropagatorCR[mu, nu, alpha, beta, p]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `mu, nu; alpha, beta` | Two graviton Lorentz-index pairs. |
| `p` | Line momentum. |

## Availability

Direct definition after loading FeynGrav. No conventional graviton library import or FORM executable is required.

## Options and settings

No options. Reads the current `GaugeFixingEpsilonCR`, initialised to `-1/2` when FeynGrav loads. See [gauge parameters and coupling](../../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns the implemented D-dimensional propagator with `I FAD[p]` and the inverse Nieuwenhuizen-operator structure. Its gauge and field normalisation belong to CR variables.

Rules use the [shared momentum, dimension and normalisation conventions](../../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
crPropagator = GravitonPropagatorCR[mu, nu, alpha, beta, p];
crPropagator =!= Null && FreeQ[crPropagator, _GravitonPropagatorCR | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `crPropagator` contains neither an unevaluated `GravitonPropagatorCR` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `crPropagator` and the loading messages. Evaluate `crPropagator` on its own to display the expression.

## Limitations

Use only the stated signature; an unmatched call or diagnostic is not a zero interaction. Keep symbolic dimensions and avoid singular parameter substitutions without a separate analysis. These bosonic rules do not provide a complete gauge-fixed CR calculation: there is no supplied CR FeynArts bundle or dedicated public CR ghost-vertex command.

## Verification scope

These definitions and signatures are present in the source. The conventional GR verification does not validate CR normalisations or amplitudes. See the [CR verification boundary](./README.md#Verification%20boundary).

## See also

- [GravitonPropagatorAuxiliaryCR](./GravitonPropagatorAuxiliaryCR.md)
- [GravitonVertexCRhhh](./GravitonVertexCRhhh.md)
- [GravitonVertexCRBhh](./GravitonVertexCRBhh.md)
- [GravitonVertexCRBBh](./GravitonVertexCRBBh.md)
- Main-package source (`FeynGrav.wl`)
