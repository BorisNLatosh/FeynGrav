# QuadraticGravityVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

`r >= 3` graviton legs in one flat list, followed by the two quadratic-gravity mass parameters.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
QuadraticGravityVertex[triples, m0, m2]
```

## Arguments

`triples` is one flat list `{mu1, nu1, k1, ..., mun, nun, kn}` of graviton index–index–momentum triples, not a nested list. The flat list contains `r >= 3` graviton triples; `m0, m2` are the mass parameters defined in the overview.

## Availability

Use `FeynGravImportQuadraticGravity[r - 2]` to load the required installed order. This family is not imported during ordinary initialisation. Imports read existing files; they do not generate missing rules. Inspect actual orders with `FeynGravLibraryInformation[FeynGravImportQuadraticGravity]`.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
FeynGravImportQuadraticGravity[1];
result = QuadraticGravityVertex[{mu1, nu1, p1, mu2, nu2, p2, mu3, nu3, p3}, m0, m2];
result =!= Null && FreeQ[result, _QuadraticGravityVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `QuadraticGravityVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

For `r` legs import coupling order `r - 2`, not `r`. This differs from the conventional GR command, whose triples are separate arguments. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
