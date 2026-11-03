# OutputDirectory

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``FeynGravLibrariesGenerator`OutputDirectory``. **Baseline:** 4.2.0 development.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

OutputDirectory is an option for Generate* and Generate*Specific. Automatic (the default) writes extensionless libraries to the Libs directory containing this generator, independently of Directory[]. An explicit value must be an existing directory. WorkingDirectory instead controls the parent directory of temporary converter jobs. Check* commands always inspect the generator's Libs directory.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| `Generate* / Generate*Specific` | `Automatic` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
