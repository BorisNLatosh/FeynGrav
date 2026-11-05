# GravitonAxionVectorVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Two vector index–momentum groups and axion coupling `theta`; no axion momentum argument.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonAxionVectorVertex[pairs, lambda1, p1, lambda2, p2, theta]
```

## Arguments

`pairs` is one flat list `{mu1, nu1, ..., mun, nun}` of graviton index pairs, not a nested list. Each vector leg is Lorentz index then incoming momentum; `theta` is the axion coupling. There is no axion momentum argument.

## Availability

Use `FeynGravImportAxionVectorVertex[n]` to load the required installed order. This family is not imported during ordinary initialisation. Imports read existing files; they do not generate missing rules. Inspect actual orders with `FeynGravLibraryInformation[FeynGravImportAxionVectorVertex]`.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
FeynGravImportAxionVectorVertex[1];
result = GravitonAxionVectorVertex[{mu, nu}, lambda1, p1, lambda2, p2, theta];
result =!= Null && FreeQ[result, _GravitonAxionVectorVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `GravitonAxionVectorVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

The epsilon-density/action convention remains unresolved. Existing multiplicity and notation checks do not certify the intended axion action. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
