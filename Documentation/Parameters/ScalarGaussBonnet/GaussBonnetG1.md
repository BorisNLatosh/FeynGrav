# GaussBonnetG1

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`GaussBonnetG1``. **Baseline:** 4.2.0.

Coefficient of $+g_1\phi\mathcal G$ in the native Einstein–scalar–Gauss–Bonnet action.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`GaussBonnetG1 -> value
```

`expression` is a converted EsGB amplitude. Replacing `GaussBonnetG1` by zero removes the coupling linear in the scalar. Set `GaussBonnetG2` to zero as well to recover the minimal scalar sector; the scalar mass remains independent.

## Related documentation

- [Model or conventions](../../Models/ScalarGaussBonnet/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
