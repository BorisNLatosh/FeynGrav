# D

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``System`D``. **Baseline:** 4.2.0.

Spacetime dimension in FeynCalc Lorentz tensors and the dimensional continuation of the four-dimensional physical actions. This is the Wolfram System symbol, not a new FeynGrav constant. It is independent of the number of colours.

## Initial value and use

**Initial/default status:** Retain symbolic. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. System`D -> d
```

Here `expression` is a previously calculated expression and `d` is the dimension you want to substitute. This replacement changes only that expression; do not assign a global value to the protected Wolfram symbol `D`. Replacing `D` by 4 is not a Laurent expansion, ultraviolet subtraction or renormalisation.

## Related documentation

- [Model or conventions](../../Conventions.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
