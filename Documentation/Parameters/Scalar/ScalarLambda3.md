# ScalarLambda3

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`ScalarLambda3``. **Baseline:** 4.2.0 development.

Native scalar action coefficient of $+\lambda_3\phi^3/3!$. The sign is positive in the action. Set it to zero by replacement after conversion for the minimal-scalar limit.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`ScalarLambda3 -> value
```

`expression` is a converted scalar-model amplitude. Replacing `ScalarLambda3` by zero removes the cubic potential coupling; set `ScalarLambda4` to zero as well when taking the minimal scalar limit. The mass and gravitational coupling remain independent.

## Related documentation

- [Model or conventions](../../Models/Scalar/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
