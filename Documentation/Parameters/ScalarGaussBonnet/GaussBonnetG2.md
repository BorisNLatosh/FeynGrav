# GaussBonnetG2

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`GaussBonnetG2``. **Baseline:** 4.2.0.

Coefficient of $+g_2\phi^2\mathcal G/2!$ in the native Einstein–scalar–Gauss–Bonnet action. Differentiating the two scalar fields cancels the factorial.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`GaussBonnetG2 -> value
```

`expression` is a converted EsGB amplitude. Replacing `GaussBonnetG2` by zero removes the coupling quadratic in the scalar. Set `GaussBonnetG1` to zero as well to recover the minimal scalar sector; the scalar mass remains independent.

## Related documentation

- [Model or conventions](../../Models/ScalarGaussBonnet/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
