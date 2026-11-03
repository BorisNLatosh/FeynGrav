# PolarizationTensor

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`PolarizationTensor``. **Baseline:** 4.2.0 development.

Four-dimensional product of two polarisation vectors with the same momentum and phase.

## Syntax and arguments

```mathematica
PolarizationTensor[p, mu, nu, phase : I, opts]
```

Momentum comes first. `p`, `mu` and `nu` (where present) must be standalone symbols. The optional positional phase defaults to `I`; pass `I` or `-I`. The latter denotes complex conjugation, not a helicity or multiplicative factor.

## Options and result

The sole option is `Transversality -> False`. An explicitly supplied value is forwarded. When omitted, the internal `Polarization` object has no explicit transversality option and its current FeynCalc default applies.

`Transversality -> True` imposes a zero momentum–polarisation contraction only. It does not impose mass shell, null polarisation or normalisation. A factorised spin-2 tensor is transverse and traceless for a nonzero massless on-shell momentum when the underlying vector is transverse and null. The D-dimensional shortcut is not a complete polarisation basis.

## Example

```mathematica
<< FeynGrav`
PolarizationTensor[p, mu, nu, -I, Transversality -> True]
```

No vertex library is required. Unmatched patterns can remain unevaluated rather than returning `Failure`. The four-dimensional `PolarizationVector` is supplied by FeynCalc, not introduced by FeynGrav.

See [the group overview](./README.md) and [common polarisation conventions](../../Conventions.md).
