# GravitonMassiveVectorVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Each vector is specified by index then momentum, followed by their mass.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonMassiveVectorVertex[pairs, lambda1, p1, lambda2, p2, m]
```

## Arguments

`pairs` is one flat list `{mu1, nu1, ..., mun, nun}` of graviton index pairs, not a nested list. Each vector leg is specified by Lorentz index then incoming momentum; `m` is the vector mass.

## Availability

Use `FeynGravImportVectors[n]` to load the required installed order. The family is loaded through order 2 during successful package initialisation. Imports read existing files; they do not generate missing rules. Inspect actual orders with `FeynGravLibraryInformation[FeynGravImportVectors]`.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
result = GravitonMassiveVectorVertex[{mu, nu}, lambda1, p1, lambda2, p2, m];
result =!= Null && FreeQ[result, _GravitonMassiveVectorVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `GravitonMassiveVectorVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

This is the Proca interaction. Massless gauge fixing and ghosts belong to the separate massless-vector commands. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
