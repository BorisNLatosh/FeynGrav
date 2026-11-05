# QuarkMass

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`QuarkMass``. **Baseline:** 4.2.0.

Mass of the one fundamental Dirac flavour in the native `GR-SU-N-Yang-Mills` model; zero is a supported specialisation.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`QuarkMass -> value
```

`expression` is a converted native quark-model amplitude and `value` is the quark mass. A zero-mass substitution also requires consistent external on-shell kinematics and spinors; replacing the parameter alone does not rebuild those choices.

## Related documentation

- [Model or conventions](../../Models/YangMills/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
