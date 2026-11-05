# GravitonMassiveVectorVertexUncontracted

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``GravitonVectorVertex`GravitonMassiveVectorVertexUncontracted``. **Baseline:** 4.2.0.

Constructs the Proca coupling with two vector index–momentum groups and explicit mass.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/GravitonVectorVertex.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
GravitonMassiveVectorVertexUncontracted[indexArray, \[Lambda]1, p1, \[Lambda]2, p2, m]
```

## Argument contract

- argument 1: flat list, block size 2, length 0 to Infinity.
- argument 2: symbolic expression (not a list or association).
- argument 3: symbolic expression (not a list or association).
- argument 4: symbolic expression (not a list or association).
- argument 5: symbolic expression (not a list or association).
- argument 6: symbolic expression (not a list or association).

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant leaves contraction/algebra for the generator or converter. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).

For already generated libraries, use the distinct [main-package command](../../../Models/Vector/GravitonMassiveVectorVertex.md).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/GravitonVectorVertex.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
