# JohnBeta

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``FeynGrav`JohnBeta``. **Baseline:** 4.2.0 development.

Coefficient of $+\beta G^{\mu\nu}\partial_\mu\phi\partial_\nu\phi$ in the native John action, with no extra one-half. Equivalent to G5 = -beta phi up to a boundary term.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`JohnBeta -> value
```

`expression` is a converted John-model amplitude. Choose `value = 0` in the replacement to remove the John interaction and retain the minimally coupled massive scalar sector. This is a substitution in the amplitude, not an assignment to the model parameter.

## Related documentation

- [Model or conventions](../../../Models/Horndeski/John/README.md)
- [Conventions report](../../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../../Reference.md#Library%20import%20records)
