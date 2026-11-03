# YangMillsCoupling

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`YangMillsCoupling``. **Baseline:** 4.2.0 development.

Native SU(N) gauge coupling g_s. The covariant derivative convention is `D_mu = nabla_mu - i g_s T^a A^a_mu`; the flat quark interaction has positive g_s. This symbol is distinct from `SUNN`.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynGrav`YangMillsCoupling -> value
```

`expression` is a calculated Yang–Mills vertex or amplitude and `value` is the gauge coupling. A zero-coupling replacement removes terms proportional to this coupling, but does not remove gravitational interactions of the gauge or quark fields.

## Related documentation

- [Model or conventions](../../Models/YangMills/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
