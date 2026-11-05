# FeynGravInitialized

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynGravInitialized``. **Baseline:** 4.2.0.

This is an inspect-only state flag, not a command or configuration switch. Successful required startup imports set it to `True`; initialisation failures leave it false and report `InitializationFailed`.

```mathematica
<< FeynGrav`
TrueQ[FeynGrav`FeynGravInitialized]
```

Do not assign it manually to bypass failed initialisation. A true flag does not mean all optional sectors are imported or FORM is available. Inspect library records and use an explicit FORM check for those questions.
