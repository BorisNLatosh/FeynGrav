# ScalarMass

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`ScalarMass``. **Baseline:** 4.2.0 development.

Arbitrary scalar mass in the native minimal-scalar, John and Einstein–scalar–Gauss–Bonnet models. The direct scalar commands take an explicit mass argument instead.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`ScalarMass -> value
```

`expression` is a converted native-model amplitude and `value` is the scalar mass. For a massless specialisation, use ``expression /. FeynGrav`ScalarMass -> 0``. Direct scalar-rule commands instead take their mass as an explicit argument; replacing `ScalarMass` will not change an unrelated mass symbol used in those calls.

## Related documentation

- [Model or conventions](../../Models/Scalar/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
