# ReduceLoopCoefficients

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`ReduceLoopCoefficients``. **Baseline:** 4.2.0 development.

This is an option symbol; it is supplied as a rule to an owning command, not called as a function.

## Meaning and accepted values

ReduceLoopCoefficients is an option for CalcFormExport and CalcFormCalculate, default False. Enabled ReduceToScalarMasters includes this reduction regardless of this option. With master reduction off, True lowers repeated propagator powers in one-loop coefficients through partial fractions and integration-by-parts identities executed in FORM. It can also process existing LoopTensorCoefficient expressions without declaring a loop momentum. For symbolic D, subfamilies with at most four distinct routes use ordinary PaVe coefficients where possible; scaleless, singular and unsupported families remain formal. Linear results are collected by final integral coefficient, with rational dimension coefficients simplified in FORM and shared amplitude blocks for large commuting expressions. Denominators inside those shared blocks are not globally collected. Generic reductions may introduce inverse mass differences and Cayley determinants. No analytic integral evaluation is performed.

## Owners and initial defaults

| Command or family | Default |
| --- | --- |
| [CalcFormExport](../Calculation/CalcFormExport.md) | `False` |
| [FeynGravFORMExport](../Calculation/FeynGravFORMExport.md) | `False` |
| [CalcFormCalculate](../Calculation/CalcFormCalculate.md) | `False` |
| [FeynGravSimplify](../Calculation/FeynGravSimplify.md) | `False` |

These are initial defaults, not a report of the current kernel. Inspect `Options[command]` for commands with option stores. The optional FeynArts amplitude adapter accepts only its explicit forwarding whitelist; its wrapper behaviour is documented on its own page. Passing an option in one call does not change another command's defaults.

## Related documentation

- [Converter option reference](../Calculation/Guide.md)
- [Method policies](../Calculation/Methods.md)
- [Library generator](../LibraryGeneration/Guide.md)
- [Installation guide](../../UserGuide.md)
