# GravitonPropagator

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Returns the conventional general-relativity graviton propagator in FeynCalc notation.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
GravitonPropagator[mu, nu, alpha, beta, p]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `mu, nu` | Lorentz-index pair on the first graviton end. |
| `alpha, beta` | Lorentz-index pair on the second graviton end. |
| `p` | Line momentum. |

## Availability

Direct definition, available after loading FeynGrav; no additional vertex library or FORM executable is needed.

## Options and settings

No options. Reads the current `GaugeFixingEpsilon`, initialised to `2` on package loading. Changing it affects later calls but does not rebuild the fixed-gauge native `GR` model. See [gauge parameters and coupling](../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Includes the factor `I`, a massless `FAD[p]` denominator and D-dimensional tensors. At `GaugeFixingEpsilon = 2`,

$$
\Delta_{\mu\nu,\alpha\beta}(p)=\frac{i}{p^2}
\left[\frac12(\eta_{\mu\alpha}\eta_{\nu\beta}+\eta_{\mu\beta}\eta_{\nu\alpha})
-\frac{\eta_{\mu\nu}\eta_{\alpha\beta}}{D-2}\right].
$$

The displayed fraction suppresses prescription notation. Other gauge values add the implemented longitudinal terms.

Rules use the [shared momentum, dimension and normalisation conventions](../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
propagator = Block[{GaugeFixingEpsilon = 2},
  GravitonPropagator[mu, nu, alpha, beta, p]];
propagator =!= Null && FreeQ[propagator, _GravitonPropagator | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `propagator` contains neither an unevaluated `GravitonPropagator` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `propagator` and the loading messages. Evaluate `propagator` on its own to display the expression.

## Limitations

The D = 2 singularity is not resolved by direct substitution. Singular dimension or gauge choices are not automatically diagnosed. Do not interpret an unmatched call or a diagnostic as a zero propagator.

## Verification scope

See the [model conventions and propagator discussion](./README.md#Propagators) and [Scientific scope](../../ScientificScope.md#General%20relativity). Their recorded scopes do not establish arbitrary-gauge amplitude or native-loop validation.

## See also

- [GhostVectorPropagator](./GhostVectorPropagator.md)
- [GravitonVertex](./GravitonVertex.md)
- Main-package source (`FeynGrav.wl`)
- [Graviton library import](../../Reference.md#Library%20importers)
- [Native GR guide](./NativeModel.md)
