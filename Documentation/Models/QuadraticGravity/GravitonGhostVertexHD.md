# GravitonGhostVertexHD

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Same leg order with higher-derivative ghost gauge fixing; depends on `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
GravitonGhostVertexHD[rho, sigma, k, mu, p1, nu, p2]
```

## Arguments

`rho, sigma, k` specify one graviton; `mu, p1` and `nu, p2` specify the two gravitational ghost legs, with incoming momenta.

## Availability

No library import is needed.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
result = GravitonGhostVertexHD[rho, sigma, k, mu, p1, nu, p2];
result =!= Null && FreeQ[result, _GravitonGhostVertexHD | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `GravitonGhostVertexHD` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

Uses the current `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`. This fixed cubic interface does not construct arbitrary multi-graviton ghost vertices.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
