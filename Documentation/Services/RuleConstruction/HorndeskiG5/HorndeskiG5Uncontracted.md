# HorndeskiG5Uncontracted

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``HorndeskiG5`HorndeskiG5Uncontracted``. **Baseline:** 4.2.0 development.

Constructs the G5 monomial interaction with unit monomial coupling. Infer the scalar power a from the scalar momentum count and b using the Horndeski overview. This three-argument rule differs from the four-argument main-package retrieval command.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/HorndeskiG5.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
HorndeskiG5Uncontracted[gravitonParameters, scalarMomenta, b]
```

## Argument contract

- argument 1: flat list, block size 3, length 3 to Infinity.
- argument 3: explicit integer >= 0.
- argument 2: flat list, block size 1, length 2 b + 1 to Infinity.
- All graviton orders use the complete connection expansion.
- b <= 4 at n = 1, b <= 3 at n = 2, and b <= 2 at n >= 3 (seven inverse-metric pairs).

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant leaves contraction/algebra for the generator or converter. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).

Read the [model conventions and verification limits](../../../Models/Horndeski/README.md).

For already generated libraries, use the distinct [main-package command](../../../Models/Horndeski/HorndeskiG5.md).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/HorndeskiG5.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
