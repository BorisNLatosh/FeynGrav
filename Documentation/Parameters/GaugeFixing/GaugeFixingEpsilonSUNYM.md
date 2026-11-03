# GaugeFixingEpsilonSUNYM

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`GaugeFixingEpsilonSUNYM``. **Baseline:** 4.2.0 development.

Substituted into Yang–Mills libraries during `FeynGravImportSUNYM`. Assign the intended value before importing. The native model is a separate fixed-gauge snapshot.

## Initial value and use

**Initial/default status:** -1. This is a parameter or convention symbol, not a function or an option.

Load FeynGrav before inspecting this symbol. Use its bare name, without `[]`. Loading or reloading can reset defaults. The parameter performs no validation of gauge choices or singular limits.

```mathematica
InputForm[FeynGrav`GaugeFixingEpsilonSUNYM]
```

After a fresh load, the inspection above displays `-1`. For a fresh session, import the first two installed orders at that gauge value:

```mathematica
<< FeynGrav`
FeynGrav`GaugeFixingEpsilonSUNYM = -1;
FeynGravImportSUNYM[2, Verbose -> True];
FeynGravLibraryInformation[FeynGravImportSUNYM]
```

Choose the intended gauge value before the import. The final expression displays the import record so you can inspect the actual orders and recorded settings. Changing the parameter afterwards does not update imported vertices or expressions already calculated: reimport, then recompute dependent expressions. Reimporting direct rules does not rebuild any native FeynArts bundle.

## Related documentation

- [Model or conventions](../../Models/YangMills/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
