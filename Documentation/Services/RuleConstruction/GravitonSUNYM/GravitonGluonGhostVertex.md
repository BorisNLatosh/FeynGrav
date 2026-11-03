# GravitonGluonGhostVertex

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``GravitonSUNYM`GravitonGluonGhostVertex``. **Baseline:** 4.2.0 development.

Constructs the gluon–Yang–Mills-ghost coupling. Preserve the exact array/leg order in the signature and source contract.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/GravitonSUNYM.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
GravitonGluonGhostVertex[indexArray, array1, array2, array3]
```

## Argument contract

- argument 1: flat list, block size 2, length 0 to Infinity.
- argument 2: flat list, block size 1, length 3 to 3.
- argument 3: flat list, block size 1, length 3 to 3.
- argument 4: flat list, block size 1, length 3 to 3.

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant performs the additional contraction/algebra implemented in its source; that can invoke FORM or Wolfram parallel evaluation. It is not loop integration. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).

For already generated libraries, use the distinct [main-package command](../../../Models/YangMills/GravitonGluonGhostVertex.md).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/GravitonSUNYM.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
