# ProcaPropagator

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Massive-vector propagator, with numerator `-I (MTD[mu,nu] - FVD[p,mu] FVD[p,nu]/m^2)`. Its massless limit cannot be obtained by substituting `m = 0`.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
ProcaPropagator[mu, nu, p, m]
```

## Arguments

`mu, nu` are vector Lorentz indices, `p` is momentum, and `m` is the nonzero vector mass.

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
result = ProcaPropagator[mu, nu, p, m];
FCI[result] === FCI[-I (MTD[mu, nu] - FVD[p, mu] FVD[p, nu]/m^2) FAD[{p, m}]]
```

Expected result: `True` for an unassigned nonzero mass parameter. This compares the returned propagator with its documented tensor form; substituting zero mass is singular.

## Limitations and verification

Do not substitute `m = 0` into this propagator; the longitudinal term contains `1/m^2`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
