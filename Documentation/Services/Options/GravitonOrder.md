# GravitonOrder

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`FeynArtsIntegration`GravitonOrder``. **Baseline:** 4.2.0.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

GravitonOrder is the FeynGravBuildModel option selecting the graviton vertex valence: 2 (default) builds the cubic and quartic vertices with the ghost sector, 3 additionally builds the five-graviton vertex. Values other than 2 or 3 are rejected before any import or rule construction.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| `FeynGravBuildModel` | `2` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
