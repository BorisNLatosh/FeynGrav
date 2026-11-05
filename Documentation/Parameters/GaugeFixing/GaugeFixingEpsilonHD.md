# GaugeFixingEpsilonHD

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`GaugeFixingEpsilonHD``. **Baseline:** 4.2.0.

Overall higher-derivative graviton gauge contribution. Its value is used by the HD quadratic-gravity propagator when called.

## Initial value and use

**Initial/default status:** Unassigned. This is a parameter or convention symbol, not a function or an option.

Load FeynGrav before inspecting this symbol. Use its bare name, without `[]`. Loading or reloading can reset defaults. The parameter performs no validation of gauge choices or singular limits.

```mathematica
InputForm[FeynGrav`GaugeFixingEpsilonHD]
```

After a fresh load, `InputForm` displays the symbol `GaugeFixingEpsilonHD` itself because no gauge value has been assigned. This is intentional, not a loading failure. The higher-derivative commands read the current settings when called. Keep them symbolic, or choose a consistent nonsingular set of all required HD gauge parameters before calculating; do not set an inverse gauge factor to zero by substitution. Recompute any previously stored propagators or vertices after changing the settings.

## Related documentation

- [Model or conventions](../../Models/QuadraticGravity/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
