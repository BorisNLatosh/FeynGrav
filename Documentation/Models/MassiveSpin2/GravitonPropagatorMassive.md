# GravitonPropagatorMassive

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Massive-gravity propagator with the implemented `1/(D-1)` trace subtraction. Requires nonsingular mass and dimension choices; `m = 0` is not a massless-propagator shortcut.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
GravitonPropagatorMassive[mu, nu, alpha, beta, p, m]
```

## Arguments

`mu, nu` and `alpha, beta` label the two symmetric tensor legs; `p` is momentum and `m` is the nonzero mass.

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
result = GravitonPropagatorMassive[mu, nu, alpha, beta, p, m];
result =!= Null && FreeQ[result, _GravitonPropagatorMassive | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `GravitonPropagatorMassive` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

The trace subtraction uses `1/(D-1)`. Singular masses or dimensions are not diagnosed. This utility supplies no interacting massive-gravity model.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
