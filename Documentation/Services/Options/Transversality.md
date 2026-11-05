# Transversality

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynCalc`Transversality``. **Baseline:** 4.2.0.

This page describes only FeynGrav’s use of this dependency-owned option.

## Meaning and accepted values

Boolean option of the three FeynGrav polarisation shortcuts. The declaration is `False`, but omission leaves the underlying `Polarization` object without an explicit option, so its current FeynCalc default applies. An explicit `True` sets the momentum–polarisation contraction to zero; it does not impose mass shell, null polarisation or normalisation.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| `PolarizationVectorD / PolarizationTensor / PolarizationTensorD` | `False (see omission rule below)` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
