# HorndeskiG5

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

`s = a + 2 b + 1`; graviton triples.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
HorndeskiG5[triples, momenta, b, lambda]
```

## Arguments

`triples` is one flat list `{mu1, nu1, k1, ..., mun, nun, kn}` of graviton index–index–momentum triples, not a nested list. `momenta` is the list of all incoming scalar momenta. `b` is the non-negative integer power of X; infer `a = Length[momenta] - 2 b - 1`. It must be non-negative. `lambda` is the coefficient of the monomial `phi^a X^b` in G5.

## Availability

Call `FeynGravImportHorndeskiG5[]` first. This imports every installed matching file; it takes no order argument. Imports read existing files; they do not generate missing rules. Inspect actual orders with `FeynGravLibraryInformation[FeynGravImportHorndeskiG5]`.

## Options and settings

No command options. Importer options belong to the importer. All settings and model-specific qualifications are described below and in the [model overview](./README.md).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented phase and coupling factors. Vertex momenta are incoming; momentum conservation, on-shell conditions, external states and diagram factors are not applied by the retrieval call.

## Example

In a fresh kernel, with symbolic indices, momenta, masses, couplings and `D`:

```mathematica
<< FeynGrav`
FeynGravImportHorndeskiG5[];
result = HorndeskiG5[{mu, nu, k}, {p1, p2, p3}, 1, lambda];
result =!= Null && FreeQ[result, _HorndeskiG5 | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `result` contains neither an unevaluated `HorndeskiG5` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `result` and the loading messages. Evaluate `result` on its own to display the expression.

## Limitations and verification

There is no implicit `1/a!`, `1/b!` or `2^-b`. Match an installed `(a,b,n)` selection. This four-argument library interface differs from the three-argument generator-side command of the same name. An unavailable library order or unmatched signature can remain unevaluated; this is not a zero interaction or a guaranteed structured `Failure`.

The [model overview](./README.md#Verified%20scope%20and%20limitations) distinguishes corrected source rules, migrated libraries and finite verification cases. No general Horndeski FeynArts bundle is supplied.

Read the model overview for action conventions, verification evidence and native-model limits. Availability of a rule is not a proof of its action derivation or a complete amplitude prescription.

## See also

- [Library imports](../../Reference.md#Library%20importers)
- [Import records](../../Reference.md#Library%20import%20records)
- Main interface source (`FeynGrav.wl`)
- [Worked examples](../../Examples/README.md)
