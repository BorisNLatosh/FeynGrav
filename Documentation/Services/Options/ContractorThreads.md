# ContractorThreads

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGrav`ContractorThreads``. **Baseline:** 4.2.0.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

ContractorThreads is an option of FeynGravSimplify selecting the Contractor worker count: Automatic and positive integers are accepted; the effective bound follows the installed Contractor. It is resolved only for Method -> "Contractor"; unselected delayed values stay inactive. Ordinary argument evaluation still applies.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `8` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
