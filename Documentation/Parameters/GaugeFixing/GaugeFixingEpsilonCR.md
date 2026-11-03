# GaugeFixingEpsilonCR

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`GaugeFixingEpsilonCR``. **Baseline:** 4.2.0 development.

The Cheung–Remmen graviton propagator uses its value when called. This is a distinct field parametrisation of GR.

## Initial value and use

**Initial/default status:** -1/2. This is a parameter or convention symbol, not a function or an option.

Load FeynGrav before inspecting this symbol. Use its bare name, without `[]`. Loading or reloading can reset defaults. The parameter performs no validation of gauge choices or singular limits.

```mathematica
InputForm[FeynGrav`GaugeFixingEpsilonCR]
```

After a fresh load, the inspection above displays `-1/2`. To use that value for one propagator call without changing the session setting:

```mathematica
Block[{FeynGrav`GaugeFixingEpsilonCR = -1/2},
  GravitonPropagatorCR[mu, nu, alpha, beta, p]]
```

Evaluate this after loading FeynGrav, with unassigned indices and momentum. It returns the symbolic propagator. This direct command reads the gauge setting when called; changing the setting does not rewrite a propagator already stored in a variable.

## Related documentation

- [Model or conventions](../../Models/GeneralRelativity/CheungRemmen/README.md)
- [Conventions report](../../Reference.md#Conventions%20report)
- [Import settings and provenance](../../Reference.md#Library%20import%20records)
