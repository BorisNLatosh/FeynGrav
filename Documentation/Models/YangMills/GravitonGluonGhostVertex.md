# GravitonGluonGhostVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Gluon–ghost interaction in the existing library interface, with three index–colour–momentum groups. Retain all nine leg arguments, including the ghost-position Lorentz placeholders.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonGluonGhostVertex[pairs, lambda1, a1, p1, lambda2, a2, p2, lambda3, a3, p3]
```

## Arguments

`pairs` is one flat list `{mu1, nu1, ..., mun, nun}` of graviton index pairs, not a nested list. After the graviton list, the three groups are gluon, incoming ghost and outgoing ghost, in that order. Each group is Lorentz-index–adjoint-colour-index–momentum, with all momenta incoming.

## Availability

Use `FeynGravImportSUNYM[n]` to load the required installed order. This family is not imported during ordinary initialisation. Imports read existing files; they do not generate missing rules. Inspect actual orders with `FeynGravLibraryInformation[FeynGravImportSUNYM]`.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
FeynGravImportSUNYM[1];
result = GravitonGluonGhostVertex[{mu, nu}, lambda1, a1, p1, lambda2, a2, p2, lambda3, a3, p3];
result =!= Null && FreeQ[result, _GravitonGluonGhostVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `GravitonGluonGhostVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

Only the gluon Lorentz slot is used, but retain the two ghost Lorentz placeholders. This argument order differs from `GravitonGluonVertex`. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
