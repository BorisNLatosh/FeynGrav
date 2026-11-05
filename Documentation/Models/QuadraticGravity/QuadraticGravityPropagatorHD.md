# QuadraticGravityPropagatorHD

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Quadratic-gravity propagator with higher-derivative gauge fixing, using all three HD gauge parameters.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
QuadraticGravityPropagatorHD[mu, nu, alpha, beta, p, m0, m2]
```

## Arguments

`mu, nu` and `alpha, beta` label the tensor legs; `p` is momentum. The two mass parameters are defined in the model overview.

## Availability

No library import is needed.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc propagator in `FAD` notation. No on-shell condition or singular-limit prescription is chosen by the command.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
result = QuadraticGravityPropagatorHD[mu, nu, alpha, beta, p, m0, m2];
result =!= Null && FreeQ[result, _QuadraticGravityPropagatorHD | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `QuadraticGravityPropagatorHD` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

Uses all three HD gauge parameters: `GaugeFixingEpsilonHD`, `GaugeFixingEpsilonHD0` and `GaugeFixingEpsilonHD1`. They are initially unassigned; singular choices are not diagnosed.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
