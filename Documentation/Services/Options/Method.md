# Method

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``System`Method``. **Baseline:** 4.2.0 development.

This page describes only FeynGrav’s use of this dependency-owned option.

## Meaning and accepted values

Selects `Automatic` or `"FORM"` for the full converter pipeline; `"Contractor"` or `"FORMBenchmark"` selects the corresponding restricted adapter. There is no automatic backend fallback. Restricted methods enforce their fixed full-pipeline option baseline and return `MethodCapability` on incompatible requests. This is ``System`Method``, not a new converter-owned symbol.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `Automatic` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
