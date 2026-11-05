# NieuwenhuizenOperator0Bar

[Group overview](./README.md) · [Documentation index](../../README.md)

**Symbol:** ``Nieuwenhuizen`NieuwenhuizenOperator0Bar``. **Baseline:** 4.2.0.

`omega(mu,nu) omega(alpha,beta)`.

## Syntax and arguments

```mathematica
NieuwenhuizenOperator0Bar[mu, nu, alpha, beta, p]
```

The index arguments are Lorentz indices and the final argument is momentum. Arguments are symbolic expressions, not lists or associations.

## Conventions and options

No command options, including no `LongForm` option. These helpers load with the main package. All tensor objects are D-dimensional. The implemented 0 and 2 operators retain fixed `1/3` coefficients; do not substitute `1/(D-1)` when interpreting this basis. The 0BarBar operator has no extra square-root normalisation.

The transverse tensor is `theta = eta - p p/p^2`; the longitudinal tensor is `omega = p p/p^2`. Inverse momentum squares use FeynCalc propagator notation.

## Result and limitations

`omega(mu,nu) omega(alpha,beta)`. Structural errors and dependency failures return `Failure`; check `FailureQ` before using a result. There is no automatic singularity diagnosis at `p^2 = 0` or at zero scalar denominators of an inverse. Decomposition applies only to tensors representable in the five-element basis; unsupported structures, inconclusive verification and calculation failures have distinct diagnostics.

## Related documentation

- [Projector conventions and workflow](./README.md)
- [Rule contracts](../RuleConstruction/Contracts.md)
- Implementation (`Rules/Nieuwenhuizen.wl`)
