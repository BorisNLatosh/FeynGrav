# GravitonThreeGluonVertex

[Group overview](./README.md) · [Documentation index](../../../README.md)

**Symbol:** ``GravitonSUNYM`GravitonThreeGluonVertex``. **Baseline:** 4.2.0 development.

Constructs the three-gluon coupling to gravitons. Each gluon is given as momentum–Lorentz-index–adjoint-colour-index.

**Use:** constructing additional interaction rules. Use a separate fresh kernel to avoid collisions with library-backed main-package names. Load the rule file (`Rules/GravitonSUNYM.wl`) by its absolute path; dependencies resolve relative to that file without changing `Directory[]` or `$Path`.

## Syntax

```mathematica
GravitonThreeGluonVertex[indexArray, p1, \[Lambda]1, a1, p2, \[Lambda]2, a2, p3, \[Lambda]3, a3]
```

## Argument contract

- argument 1: flat list, block size 2, length 0 to Infinity.
- argument 2: symbolic expression (not a list or association).
- argument 3: symbolic expression (not a list or association).
- argument 4: symbolic expression (not a list or association).
- argument 5: symbolic expression (not a list or association).
- argument 6: symbolic expression (not a list or association).
- argument 7: symbolic expression (not a list or association).
- argument 8: symbolic expression (not a list or association).
- argument 9: symbolic expression (not a list or association).
- argument 10: symbolic expression (not a list or association).

A block size of two means index pairs; three means index–index–momentum triples. Arrays are flat, not lists of nested legs. External index lists are kept distinct from perturbation lists. Non-negative orders/counts must be explicit integers where required. Symbolic masses, couplings and supported composite momenta remain allowed in expression slots.

## Result, normalisation and failures

Returns the constructed symbolic tensor/vertex expression. This variant performs the additional contraction/algebra implemented in its source; that can invoke FORM or Wolfram parallel evaluation. It is not loop integration. This constructor accepts no options.

Structural errors return `Failure`; dependency failures propagate before further algebra. Structural validation does not establish physical kinematics, invertibility, absence of singular limits or mathematical correctness.

The C/CE helpers return averaged coefficients. Functional differentiation multiplicities belong to the assembled rule; do not multiply an already summed permutation factor twice. See [Scientific scope](../../../ScientificScope.md#Vertex%20multiplicities).


## Sources and workflow

- [Group overview](./README.md)
- Source (`Rules/GravitonSUNYM.wl`)
- [Generator workflow](../../LibraryGeneration/Guide.md)
- [Rule contracts](../Contracts.md)
