# QuadraticGravityPropagator

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Quadratic-gravity propagator with conventional gauge fixing, using the current `GaugeFixingEpsilon`.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
QuadraticGravityPropagator[mu, nu, alpha, beta, p, m0, m2]
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
result = QuadraticGravityPropagator[mu, nu, alpha, beta, p, m0, m2];
result =!= Null && FreeQ[result, _QuadraticGravityPropagator | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `QuadraticGravityPropagator` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

Uses the current `GaugeFixingEpsilon`, initially 2. Treat projector and parameter denominators symbolically before taking limits.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
