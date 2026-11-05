# GravitonScalarVertexUncontracted

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``GravitonScalarVertex`GravitonScalarVertexUncontracted``. **Baseline:** 4.2.0.

Constructs the minimal two-scalar coupling, including the explicit scalar mass. The scalar momenta are incoming.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/GravitonScalarVertex.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
GravitonScalarVertexUncontracted[indexArray, p1, p2, m]
```

## Argument contract

- argument 1: flat list, block size 2, length 0 to Infinity.
- argument 2: symbolic expression (not a list or association).
- argument 3: symbolic expression (not a list or association).
- argument 4: symbolic expression (not a list or association).

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant leaves contraction/algebra for the generator or converter. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).

For already generated libraries, use the distinct [main-package command](../../../Models/Scalar/GravitonScalarVertex.md).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/GravitonScalarVertex.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
