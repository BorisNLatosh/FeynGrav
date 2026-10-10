# Focused rational-coefficient experiment

This is a reproducible experiment for the first term displayed in
`One-Loop-Counterterms.nb`. A guarded version is now **enabled by `CalcFormExport` and
`CalcFormCalculate`**; these standalone files preserve the preliminary experiment. The user-approved first step was to reproduce the saved
simplification for this coefficient before assessing the complete calculation.

All simplification runs in FORM. No Wolfram `Simplify`, `FullSimplify`, `Factor`,
`Cancel` or `Together` is used. Wolfram is only used to recover the original
expression through the existing importer and to check reconstruction and sizes.

## Run the saved example

Copy this directory to an empty working directory, then run:

```sh
form -q Original.frm
form -q Candidate.frm
```

Or use `tform -w10 -q` with the same files. The programs require no FeynGrav
installation when executed by FORM.

- `baseline.out` is the existing converter output.
- `candidate.out` is the compact result, with the original mapping header.
- `difference.out` must contain `0`. This checks equality by rereading the
  printed numerator and denominator factors, cross-multiplying against the
  original expression, and reducing the residual in FORM.
- `numerator.inc` and `denominator.inc` are the intermediate factors used by
  that check. They are overwritten on a subsequent run.

After loading CalcFormConverter, use its existing importer:

```mathematica
compact = CalcFormImport[
  FileNameJoin[{directory, "candidate.out"}],
  FileNameJoin[{directory, "Original.map.json"}]
];
```

`Candidate.frm` includes equality verification. `Calculate.frm` performs only
the simplification and output stages and is the program used for timing.
These programs are specific to the supplied coefficient, not drop-in
replacements for the complete polarisation calculation.

## Algorithm

1. Reuse the existing exported FORM expression and its Lorentz processing.
2. Extract its single complete propagator product. Keep it opaque throughout.
3. Replace scalar products by distinct temporary scalar symbols.
4. Expose the recorded reciprocal polynomials in `D`; their mapped identifiers
   must not be treated as independent variables.
5. Move scalar monomials into `PolyRatFun` and combine the rational coefficient.
6. Factorise its numerator and denominator separately using native `Factorize`.
7. Restore scalar products in each printed factor and write ordinary products
   and a quotient. No temporary identifiers or `factor_` objects reach import.
8. Check the result against the original in FORM.

This is rational-function algebra at generic parameter values. It does not
assign a value at a pole, impose on-shell/transversality conditions or cancel a
propagator against a numerator. It applies no momentum-conservation identity.

## Rebuild and test

`prepare.py` is a test-only adapter for trusted converter-generated source and
mapping files. Its only source-layout dependency is the checked output-stage
boundary. It generates a new directory and refuses an existing destination:

```sh
python3 prepare.py Original.frm Original.map.json /tmp/new-coefficient-probe
python3 check.py /tmp/new-coefficient-checks
```

Python uses only its standard library and performs no symbolic algebra. It is
needed to rebuild/test the experiment, not to run the supplied FORM programs.
The checks exercise installed FORM and, when present, TFORM with ten workers.
Each small process has a 60-second timeout.

The deliberately narrow adapter accepts one version-one scalar coefficient.
Free indices, complex coefficients, non-rational abbreviations, unmapped scalar
symbols and newer matrix/colour/epsilon formats are rejected. Multiple
propagator products and residual scalar functions are rejected by the FORM
program. These restrictions belong to this experiment, not to the converter.

The synthetic control mappings in `check.py` are preparation-only dictionaries;
only the complete `Original.map.json` fixture is passed to the public importer.

`NotebookNumerator.inc` and `NotebookDenominator.inc` independently transcribe
the saved compact notebook output in the fixture's dictionary. `check.py`
compares that expression with the original as a second equality check.

See [the measured report](../Reports/RationalCoefficient.md). See the [integration report](../Reports/RationalCoefficientIntegration.md) for
the current selection limits, full-result coverage and remaining limitations.
