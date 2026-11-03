# GravitonScalarPotentialVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Scalar-potential vertex with the supplied potential coupling; no scalar momentum arguments. The convention is the potential monomial `lambda phi^r/r!`.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
GravitonScalarPotentialVertex[pairs, lambda]
```

## Arguments

`pairs` is one flat list `{mu1, nu1, ..., mun, nun}` of graviton index pairs, not a nested list. `lambda` is the coefficient in the action monomial `+lambda phi^r/r!`; the scalar multiplicity `r` is not an argument. There are no scalar momenta.

## Availability

Use `FeynGravImportScalars[n]` to load the required installed order. The family is loaded through order 2 during successful package initialisation. Imports read existing files; they do not generate missing rules. Inspect actual orders with `FeynGravLibraryInformation[FeynGravImportScalars]`.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
result = GravitonScalarPotentialVertex[{mu, nu}, lambda];
result =!= Null && FreeQ[result, _GravitonScalarPotentialVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `GravitonScalarPotentialVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

The factorial from differentiating the scalar fields cancels `r!`. Supply the signed action coefficient; for an action term `-V`, translate the sign of the chosen potential explicitly. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
