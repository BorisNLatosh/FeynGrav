# GravitonGluonVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

`l = 2, 3, 4` gluons, each supplied as momentum–Lorentz-index–colour-index. Selects the two-, three- or four-gluon library by argument count.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0 development.

## Syntax

```mathematica
GravitonGluonVertex[triples, p1, lambda1, a1, ..., pl, lambdal, al]
```

## Arguments

`triples` is one flat list `{mu1, nu1, k1, ..., mun, nun, kn}` of graviton index–index–momentum triples, not a nested list. After the graviton list, supply 2, 3 or 4 gluon legs as separate momentum–Lorentz-index–adjoint-colour-index triples. The number of arguments selects the library.

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
result = GravitonGluonVertex[{mu, nu, k}, p1, lambda1, a1, p2, lambda2, a2];
result =!= Null && FreeQ[result, _GravitonGluonVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `GravitonGluonVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

`GaugeFixingEpsilonSUNYM` is applied during import. Colour and coupling notation follows the installed library; do not assume every historical file has the notation of a freshly generated rule. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
