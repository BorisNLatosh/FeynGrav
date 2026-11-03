# CTensorPlainGeneral

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``CTensorGeneral`CTensorPlainGeneral``. **Baseline:** 4.2.0 development.

Ordered coefficient of `Sqrt[-g]` times a product of zero through seven inverse metrics, without symmetrisation.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/CTensorGeneral.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
CTensorPlainGeneral[indexArrayExternal, indexArrayInternal]
```

## Argument contract

- argument 1: flat list, block size 2, length 0 to Infinity.
- argument 2: flat list, block size 2, length 0 to Infinity.
- At most seven external index pairs are implemented. Condition: Length[indexArrayExternal] <= 14.

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/CTensorGeneral.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
