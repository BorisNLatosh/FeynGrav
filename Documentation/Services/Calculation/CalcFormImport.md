# CalcFormImport

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``CalcFormConverter`CalcFormImport``. **Baseline:** 4.2.0 development.

Reconstructs FeynCalc internal notation from a dedicated FORM result and its matching JSON mapping.

## Syntax and arguments

```mathematica
CalcFormImport[resultFile, mappingFile]
```

Both arguments are filenames. The mapping is data, not executable Wolfram source. Import validates the recorded format and integrity, and returns an expression or `Failure`. It neither runs FORM nor performs tensor/integral reduction. Versions one through eight are supported; use the original matching result/mapping pair.

## Options

No options are accepted.

## Stored format and normalisation

Import preserves the recorded integral or saved-coefficient normalisation and scaleless setting. Rank-four epsilon reconstruction requires the exported `$LeviCivitaSign`. Grouped results are read incrementally, but the final expression and individual coefficients still require kernel memory. Import performs no new Dirac or colour tensor reduction.

## Example

This example assumes `job` describes an earlier export whose FORM execution has completed.

```mathematica
result = CalcFormImport[job["ResultFile"], job["MappingFile"]];
result
```

The final line displays the imported FeynCalc expression or a diagnostic `Failure`. For the contraction used on the export command page, the expected expression is `FCI[SPD[p, q]]`. Other exports have their own expected result; successful import alone does not verify the algebra or physics.



## Related documentation

- [Complete converter guide](./Guide.md)
- [Supported vocabulary](./Guide.md#Supported%20vocabulary)
- [Troubleshooting](./Guide.md#Troubleshooting)
- [Common normalisation conventions](../../Conventions.md)
