# HorndeskiG4

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``HorndeskiG4`HorndeskiG4``. **Baseline:** 4.2.0.

Constructs the G4 monomial interaction with unit monomial coupling. Infer the scalar power a from the scalar momentum count and b using the Horndeski overview. This three-argument rule differs from the four-argument main-package retrieval command.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/HorndeskiG4.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
HorndeskiG4[gravitonParameters, scalarMomenta, b]
```

## Argument contract

- argument 1: flat list of n triples, n >= 1.
- argument 3: explicit integer b >= 0.
- argument 2: flat scalar list of length a + 2 b, a >= 0.
- b <= 5 for n = 1, b <= 4 otherwise. Corrected action and bounded verification are documented below.

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant performs the additional contraction/algebra implemented in its source; that can invoke FORM or Wolfram parallel evaluation. It is not loop integration. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).

Read the [model conventions and verification limits](../../../Models/Horndeski/README.md).

For already generated libraries, use the distinct [main-package command](../../../Models/Horndeski/HorndeskiG4.md).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/HorndeskiG4.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
