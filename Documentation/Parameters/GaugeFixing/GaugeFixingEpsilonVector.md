# GaugeFixingEpsilonVector

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`GaugeFixingEpsilonVector``. **Baseline:** 4.2.0.

Substituted into massless-vector libraries during `FeynGravImportVectors`. Assign the intended value before reimporting the required order.

## Initial value and use

**Initial/default status:** -1. This is a parameter or convention symbol, not a function or an option.

Load FeynGrav before inspecting this symbol. Use its bare name, without `[]`. Loading or reloading can reset defaults. The parameter performs no validation of gauge choices or singular limits.

```mathematica
InputForm[FeynGrav`GaugeFixingEpsilonVector]
```

After a fresh load, the inspection above displays `-1`. For a fresh session, import the first two installed orders at that gauge value:

```mathematica
<< FeynGrav`
FeynGrav`GaugeFixingEpsilonVector = -1;
FeynGravImportVectors[2, Verbose -> True];
FeynGravLibraryInformation[FeynGravImportVectors]
```

Choose the intended gauge value before the import. The final expression displays the import record so you can inspect the actual orders and recorded settings. Changing the parameter afterwards does not update imported vertices or expressions already calculated: reimport, then recompute dependent expressions. Reimporting direct rules does not rebuild any native FeynArts bundle.

## Related documentation

- [Model or conventions](../../Models/Vector/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
