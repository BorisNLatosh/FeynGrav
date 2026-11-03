# Polarisation objects

[Documentation index](../../README.md) · [Complete symbol index](../../Symbols.md)

These shortcuts require no vertex libraries. `p`, `mu` and `nu` must be standalone symbols. Their argument order begins with the **momentum**.

| Command | Dimension and result |
| --- | --- |
| [`PolarizationVectorD[p, mu, phase : I, opts]`](PolarizationVectorD.md) | D-dimensional `Pair[LorentzIndex[mu,D], Momentum[Polarization[p,phase,...],D]]`. |
| [`PolarizationTensor[p, mu, nu, phase : I, opts]`](PolarizationTensor.md) | Four-dimensional product of two polarisation vectors with the same momentum and phase. |
| [`PolarizationTensorD[p, mu, nu, phase : I, opts]`](PolarizationTensorD.md) | The corresponding D-dimensional factorised product. |

Here `phase : I` indicates an optional positional argument with default `I`; call with `I` or `-I`, not that pattern notation. `-I` denotes complex conjugation of the vector. These labels are neither multiplicative factors nor helicity labels.

The sole option is `Transversality -> False`. Omitting it leaves the internal `Polarization` object without an explicit transversality option; an explicit option is forwarded. `Transversality -> True` imposes only the momentum–polarisation contraction being zero. It does not impose mass shell, the null-vector condition or normalisation.

For nonzero massless on-shell momentum, the factorised spin-2 tensor is transverse and traceless when its underlying vector is transverse and null. The D-dimensional shortcut does not construct a complete polarisation basis. Wrong patterns can remain unevaluated rather than producing `Failure`.

```mathematica
PolarizationTensorD[p, mu, nu, -I, Transversality -> True]
```

FeynCalc supplies the four-dimensional `PolarizationVector[p, mu, ...]`; it is not a new FeynGrav function.

## Individual pages

| Symbol | Kind | Audience |
| --- | --- | --- |
| [``FeynGrav`PolarizationTensor``](./PolarizationTensor.md) | command | user |
| [``FeynGrav`PolarizationTensorD``](./PolarizationTensorD.md) | command | user |
| [``FeynGrav`PolarizationVectorD``](./PolarizationVectorD.md) | command | user |
