# HorndeskiG2

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``HorndeskiG2`HorndeskiG2``. **Baseline:** 4.2.0.

Constructs the G2 monomial interaction with unit monomial coupling. Infer the scalar power a from the scalar momentum count and b using the Horndeski overview. This three-argument rule differs from the four-argument main-package retrieval command.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/HorndeskiG2.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
HorndeskiG2[gravitonParameters, scalarMomenta, b]
```

## Argument contract

- argument 1: flat list, block size 2, length 0 to Infinity.
- argument 3: explicit integer b with 0 <= b <= 7.
- argument 2: flat list, block size 1, length 2 b to Infinity.

Here n = Length[gravitonParameters]/2 may be zero. The upper bound on b comes from the shared
`CTensorGeneral` (`Rules/CTensorGeneral.wl`) limit of seven
inverse-metric pairs: G2 uses b pairs.
This is a source construction limit, not a guarantee of practical runtime or
verification of every allowed case. See the
[Horndeski construction bounds](../../../Models/Horndeski/README.md#Rule%20generation%20and%20higher%20orders).

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant performs the additional contraction/algebra implemented in its source; that can invoke FORM or Wolfram parallel evaluation. It is not loop integration. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).

Read the [model conventions and verification limits](../../../Models/Horndeski/README.md).

For already generated libraries, use the distinct [main-package command](../../../Models/Horndeski/HorndeskiG2.md).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/HorndeskiG2.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
