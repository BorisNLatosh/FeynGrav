# ScalarGaussBonnet

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

`n >= 2` graviton legs, followed by the scalar–Gauss–Bonnet coupling; no separate scalar momentum argument.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
ScalarGaussBonnet[triples, g]
```

## Arguments

`triples` is one flat list `{mu1, nu1, k1, ..., mun, nun, kn}` of graviton index–index–momentum triples, not a nested list. The list contains at least two graviton triples. `g` is the scalar–Gauss–Bonnet coupling; there is no separate scalar momentum argument.

## Availability

Use `FeynGravImportScalarGaussBonnet[n]` to load the required installed order. This family is not imported during ordinary initialisation. Imports read existing files; they do not generate missing rules. Inspect actual orders with `FeynGravLibraryInformation[FeynGravImportScalarGaussBonnet]`.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
FeynGravImportScalarGaussBonnet[2];
result = ScalarGaussBonnet[{mu1, nu1, k1, mu2, nu2, k2}, g];
result =!= Null && FreeQ[result, _ScalarGaussBonnet | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `ScalarGaussBonnet` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

The one-graviton contribution vanishes around flat space, but this interface requires at least two graviton legs. Higher-order source rules and the stored order-three library are not certified by the two-graviton check. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
