# ScalarPropagator

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Scalar propagator `I FAD[{p,m}]`; `m = 0` is supported.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
ScalarPropagator[p, m]
```

## Arguments

`p` is the scalar momentum and `m` its mass; the massless value is supported.

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
result = ScalarPropagator[p, m];
FCI[result] === FCI[I FAD[{p, m}]]
```

Expected result: `True`. The scalar propagator is `I FAD[{p, m}]`, including its Feynman-rule phase. This compares the returned expression with that definition.

## Limitations and verification



Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
