# SUNN

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynCalc`SUNN``. **Baseline:** 4.2.0.

Number of colours N in FeynCalc SU(N) notation. The adjoint dimension is N² - 1 and `tr(T^a T^b) = delta^(ab)/2`. This is FeynCalc-owned, not a new FeynGrav constant.

## Initial value and use

**Initial/default status:** Retain symbolic. This is a parameter or convention symbol, not a function or an option.

Keep the symbol unassigned during package/model loading. Specialise a completed symbolic expression by replacement. An early assignment can enter imported vertices or interfere with symbolic tensor conventions; import records do not rewrite previously calculated expressions.

```mathematica
expression /. FeynCalc`SUNN -> 3
```

Here `expression` is a previously calculated colour expression. The replacement specialises its explicit `SUNN` dependence to three colours without assigning a session-wide value. It does not itself perform colour algebra; simplify the symbolic colour expression first.

## Related documentation

- [Model or conventions](../../Models/YangMills/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
