# GravitonVertex

[Model overview](./README.md) · [Common conventions](../../Conventions.md) · [Command index](../../Reference.md)

Returns a stored conventional Einstein–Hilbert graviton self-interaction vertex.

**Context:** ``FeynGrav` ``. **Documentation baseline:** 4.2.0.

## Syntax

```mathematica
GravitonVertex[mu1, nu1, p1, ..., mur, nur, pr]
```

## Arguments

| Argument | Meaning |
| --- | --- |
| `mui, nui` | Two Lorentz indices for graviton leg i. |
| `pi` | Incoming momentum of graviton leg i. |
| `r` | Number of graviton legs, at least three; inferred from the arguments, not supplied as an extra argument. |

## Availability

Pass separate index–index–momentum triples, with no enclosing list. The cubic and quartic libraries are loaded initially. For r legs, request `FeynGravImportGravitons[r - 2]`; this reads installed libraries, caps the request at availability, and does not generate missing rules. The current five-graviton library requires `FeynGravImportGravitons[3]`. Inspect actual imports with `FeynGravLibraryInformation[FeynGravImportGravitons]`.

## Options and settings

No vertex options. Conventional graviton self-interactions do not depend on `GaugeFixingEpsilon`. Importer options belong to the importer, not this command. See [gauge parameters and coupling](../../Reference.md#Gauge%20parameters%20and%20coupling).

## Returned expression

Returns a symbolic FeynCalc expression with the implemented Feynman-rule phase and coupling factor $\kappa^{r-2}$. All momenta are incoming. Momentum conservation, external on-shell conditions and polarisations are not imposed by this retrieval call.

Rules use the [shared momentum, dimension and normalisation conventions](../../Conventions.md). Diagram factors, loop measures and external-state choices belong to the assembled calculation.

## Example

Use a fresh kernel with unassigned index and momentum symbols, symbolic `D` and κ. This example retrieves the rule; it does not construct a physical amplitude.

```mathematica
<< FeynGrav`
v3 = GravitonVertex[mu1, nu1, p1, mu2, nu2, p2, mu3, nu3, p3];
v3 =!= Null && FreeQ[v3, _GravitonVertex | _Failure | $Failed | $Aborted]
```

Expected result: `True` when the required libraries are loaded and the call evaluates. This checks that `v3` contains neither an unevaluated `GravitonVertex` call nor one of the listed failure markers; it does not test the tensor identity or physical correctness. If it returns `False`, inspect `v3` and the loading messages. Evaluate `v3` on its own to display the expression.

## Limitations

A call at an unavailable order or with an unmatched signature can remain unevaluated; it is not a zero interaction and does not promise a structured `Failure`. The shipped native `GR` model stops at κ² per vertex even when the main package has imported the κ³ contact. This library-backed `FeynGrav` command differs from the same-named generator-side function in `GravitonVertex` context.

## Verification scope

The cubic/quartic native interactions and stored quintic contact have the checks described in [Scientific scope](../../ScientificScope.md#General%20relativity). The contact has a complete off-shell tensor comparison. The [order-three native tree](./HigherOrders.md) has separate four-dimensional amplitude, Ward and leading soft checks.

## See also

- [GravitonPropagator](./GravitonPropagator.md)
- [GravitonGhostVertex](./GravitonGhostVertex.md)
- Main-package source (`FeynGrav.wl`)
- [Graviton library import](../../Reference.md#Library%20importers)
- [Native GR guide](./NativeModel.md)
