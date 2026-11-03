# GravitonVertex

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``GravitonVertex`GravitonVertex``. **Baseline:** 4.2.0 development.

Constructs the conventional Einstein–Hilbert graviton interaction from one flat list of index–index–momentum triples, including quadratic order as allowed by the source contract.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/GravitonVertex.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
GravitonVertex[indexArray]
```

## Argument contract

- argument 1: flat list, block size 3, length 6 to Infinity.

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant performs the additional contraction/algebra implemented in its source; that can invoke FORM or Wolfram parallel evaluation. It is not loop integration. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).

For already generated libraries, use the distinct [main-package command](../../../Models/GeneralRelativity/GravitonVertex.md).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/GravitonVertex.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
