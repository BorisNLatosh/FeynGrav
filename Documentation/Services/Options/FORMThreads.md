# FORMThreads

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`FORMThreads``. **Baseline:** 4.2.0.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

Accepts `Automatic` or a positive integer. Full-pipeline automatic discovery prefers TFORM with up to eight workers and falls back to serial FORM only if automatic TFORM discovery fails. An explicit executable with automatic threads runs one worker on that path. Explicit counts above one require TFORM. The restricted `FORMBenchmark` method instead chooses its automatic count from processors and does not silently fall back to serial; see the method guide. Worker count is not a cap on all computation threads.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `Automatic` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `Automatic` |
| [CalcFormCheck](../Calculation/CalcFormCheck.md) | `Automatic` |
| [FeynGravFORMCheck](../Calculation/FeynGravFORMCheck.md) | `Automatic` |
| [CalcFormInstall](../Calculation/CalcFormInstall.md) | `Automatic` |
| [FeynGravFORMInstall](../Calculation/FeynGravFORMInstall.md) | `Automatic` |
| `Generate* / Generate*Specific` | `Automatic` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
